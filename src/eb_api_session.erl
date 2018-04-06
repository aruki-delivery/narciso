%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_api_session).

-include("eb_constants.hrl").

-behaviour(eb_batch).

%% ====================================================================
%% Constants
%% ====================================================================
-define(CACHE_SESSIONS, eb_cache_sessions).

% Timer messages
-define(MESSAGE_PURGE_EXPIRED, purge_expired).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle_message/1]).
-export([login/3, login_external/1, get_auth_url/2, validate_api_key/1, validate_token/1, logout/1, logout/2, available/2, unavailable/1, update_session_info/2, create_notification/2,
         get_courier_counters/3, get_courier_details/6, get_push_types/0, get_notification_types/0, working/1, available/1,
         get_user_position/3, get_help/0, notify_users/7, notify_user/3, get_user_session_info/1, get_users/2, get_users/3, get_courier_accounts/1,
         get_session_cache_entry/1, init/1]).

%
% Initializations
%
init(MainNode) ->
	create_session_cache(MainNode),
	ok = create_expiration_timer(),
	ok.

%
% Login
%
login(ClientId, ClientSecret, ValidUserTypes) when is_binary(ClientId) andalso is_binary(ClientSecret) andalso is_list(ValidUserTypes) ->
	case eb_db_util:execute({get_authenticated_user, ClientId, ClientSecret}) of
		not_found -> {nok, authentication};
		#user{id_status=UserStatusId} when UserStatusId /= ?DB_USER_STATUS_ACTIVE -> {nok, user_status};
		#user{id=UserId, id_type=UserTypeId} ->
			create_user_session(UserId, UserTypeId, ValidUserTypes);
		_ -> {nok, error}
	end;
login(_ClientId, _ClientSecret, _ValidUserTypes) -> {nok, invalid_parameters}.

%
% Login Users Externos
%
login_external(AuthorizationInfo) ->
	try
		SessionTimeoutSecs = eb_cache_util:get_db_parameter(?DB_PARAM_SESSION_TIMEOUT_SECS, ?MODULE, login_external),
		% Sanitize input data
		#authorization_info{code=AuthorizationCode, redirect_uri=RedirectURI, token=TokenId} =
			case sanitize(AuthorizationInfo) of
				{ok, SanitizedRecord} -> SanitizedRecord;
				_ -> throw(missing_values)
		end,
		{NewUserTypeId, NewOauthProviderId} =
			case eb_db_util:execute({get_token_info, TokenId}) of
				{ok, #token{id_type=?DB_TOKEN_TYPE_LOGIN_EXTERNAL_USER}, [#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_TYPE_ID, value=BinUserTypeId},
																		  #token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_OAUTH_PROVIDER_ID, value=BinOauthProviderId}]} ->
					case eb_util:binary_to_int(BinUserTypeId) of
						{ok, NUserTypeId} ->
							case eb_util:binary_to_int(BinOauthProviderId) of
								{ok, NOauthProviderId} -> {NUserTypeId, NOauthProviderId};
								error -> throw(error)
							end;
						error -> throw(error)
					end;
				{ok, #token{id_type=TokenTypeId}, _} when TokenTypeId =/= ?DB_TOKEN_TYPE_LOGIN_EXTERNAL_USER -> throw(wrong_token_type);
				{nok, invalid} -> throw(invalid_token);
				not_found -> throw(not_found_token);
				_ -> throw(error)
			end,
		% Get Oauth Provider DB data
		#oauth_provider{client_id=ClientId, description=ProviderDescription, client_secret=ClientSecret,
							 authorize_endpoint=AuthorizeEndpoint, access_token_endpoint=AccessTokenEndpoint,
							 profile_endpoint=ProfileEndpoint} =
			case eb_db_util:execute({get_oauth_provider, NewOauthProviderId}) of
				OauthProviderRecord when is_record(OauthProviderRecord, oauth_provider) -> OauthProviderRecord;
				not_found -> throw(oauth_provider_not_found);
				_ -> throw(error)
			end,
		% Get Oauth AccessToken
		#auth_info{access_token=AccessToken} =
			case get_auth_info(AccessTokenEndpoint, AuthorizationCode, RedirectURI, ClientId, ClientSecret) of
				{ok, AuthInfoRecord} when is_record(AuthInfoRecord, auth_info) -> AuthInfoRecord;
				_ -> throw(auth_error)
			end,
		% Get Oauth User Profile
		#new_user_external{id_oauth=OauthId, email=Email, first_name=FirstName, last_name=LastName, telephone_nr=TelephoneNr,
								 fiscal_id=FiscalId, address=Address} =
			case get_user_profile(ProfileEndpoint, AccessToken, ProviderDescription) of
				{ok, NewUserExternalRecord} when is_record(NewUserExternalRecord, new_user_external) -> NewUserExternalRecord;
				_ -> throw(get_profile_error)
			end,
		% Verify the existence of the external user
		case eb_db_util:execute({get_user_external, OauthId}) of
			#user{id_type=UTypeId} when UTypeId /= NewUserTypeId -> throw(invalid_user_type);
			#user{id_status=UserStatusId} when UserStatusId =/= ?DB_USER_STATUS_ACTIVE -> throw(invalid_user_status);
			#user{id=UserId, id_type=UserTypeId} ->
				% User exists. Create User session
				case create_user_session(UserId, UserTypeId, [NewUserTypeId]) of
					{ok, SessionToken, SessionTimeoutSecs, UserId} -> {ok, SessionToken, SessionTimeoutSecs, UserId};
					{nok, Error} -> throw(Error)
				end;
			not_found ->
				TokenParameters = [
					#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_ID, value=OauthId},
					#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_TYPE_ID, value=erlang:integer_to_binary(NewUserTypeId)},
					#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_OAUTH_PROVIDER_ID, value=erlang:integer_to_binary(NewOauthProviderId)}
				],
				case eb_db_util:execute({create_token, NewOauthProviderId, ?DB_TOKEN_TYPE_CREATE_USER, TokenParameters}) of
					{ok, NTokenId} -> {ok, #info_user_external{token=NTokenId, email=Email, first_name=FirstName,
																			 last_name=LastName, telephone_nr=TelephoneNr, fiscal_id=FiscalId,
																			 address=Address}};
					_ -> throw(error)
				end;
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end.

%
% Get auth external url
%
get_auth_url(ProviderId, UserTypeId) when is_integer(ProviderId)
  andalso (UserTypeId =:= ?DB_USER_TYPE_EXTERNAL_COURIER) -> %orelse UserTypeId =:= ?DB_USER_TYPE_EXTERNAL_CLIENT_PRIVATE) ->
	case eb_db_util:execute({get_oauth_provider, ProviderId}) of
		#oauth_provider{client_id=ClientId, authorize_endpoint=AuthorizeEndpoint} ->
			TokenParameters = [
				#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_TYPE_ID, value=erlang:integer_to_binary(UserTypeId)},
				#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_OAUTH_PROVIDER_ID, value=erlang:integer_to_binary(ProviderId)}
			],
			case eb_db_util:execute({create_token, UserTypeId, ?DB_TOKEN_TYPE_LOGIN_EXTERNAL_USER, TokenParameters}) of
				{ok, TokenId} ->
					% FO coloca '&redirect_uri=<<uri_redirect>>' que contem distincao por tipo user
					Url = list_to_binary(http_uri:encode(binary_to_list(AuthorizeEndpoint) ++ "?client_id=" ++ binary_to_list(ClientId) ++ "?token=" ++ binary_to_list(TokenId))),
					{ok, Url};
				_ -> {nok, error}
			end;
		not_found -> {nok, oauth_provider_not_found};
		_ -> {nok, error}
	end.

%
% Validate API key
%
validate_api_key(ApiKey) when is_binary(ApiKey) ->
	case eb_cache_util:get_api_key(ApiKey) of
		{ok, _Entry} -> ok;
		not_found -> {nok, invalid_api_key};
		_ -> {nok, error}
	end;
validate_api_key(_ApiKey) -> {nok, invalid_parameters}.

%
% Validate token
%
validate_token(Token) when is_binary(Token) ->
	case touch_session_cache_entry(Token) of
		{ok, #session_info{id_user=UserId, id_user_type=UserTypeId}} -> {ok, UserId, UserTypeId};
		not_found -> {nok, invalid_token};
		_ -> {nok, error}
	end;
validate_token(_InvalidToken) -> {nok, invalid_parameters}.

%
% Logout
%
logout(Token) when is_binary(Token) ->
	eb_mnesia_util:delete(?CACHE_SESSIONS, Token),
	ok;
logout(_Token) -> {nok, invalid_parameters}.

%
% Logout for operators
%
logout(RequestingUserTypeId, TargetUserId) when is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) ->
	case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
		ok ->
			delete_session(TargetUserId),
			ok;
		nok -> {nok, forbidden}
	end;
logout(_RequestingUserTypeId, _TargetUserId) -> {nok, invalid_parameters}.

%
% Change online status to available
%
available(Token, UserSessionInfo) when is_binary(Token) ->
	try
		#update_session_info{push_info=PushInfo, position=Position} =
			case sanitize(true, UserSessionInfo) of
				{ok, SanitizedRecord} when is_record(SanitizedRecord, update_session_info) ->
					SanitizedRecord;
				_ -> throw(invalid_parameters)
			end,
		CourierId =
			case update_session_cache_entry(Token, ?COURIER_STATUS_AVAILABLE, PushInfo, Position, undefined) of
				{ok, #session_info{id_user=IdUser}} -> IdUser;
				not_found -> throw(invalid_token);
				_ -> throw(error)
			end,
		% Nao existe notificacao para a rede, por agora
		update_session_cache_entry(CourierId, false),
		TransportTypeId =
			case eb_db_util:execute({get_current_courier_transport, CourierId}) of
				#courier_transport{id_transport_type=IdTransportType} -> IdTransportType;
				not_found -> throw(missing_default_transport);
				_ -> throw(error)
			end,
		update_session_cache_entry(CourierId, TransportTypeId),
		CourierDeliveries =
			case eb_api_deliveries:get_assigned_deliveries(CourierId) of
				{ok, Deliveries} ->
					% If there is an assigned delivery, change the courier session status to working
					update_session_cache_entry(CourierId, ?COURIER_STATUS_WORKING),
					#courier_deliveries{assigned_deliveries=Deliveries};
				not_found ->
					#courier_deliveries{};
				_ ->
					throw(error)
			end,
		{ok, CourierDeliveries}
	catch
		throw:Error -> {nok, Error}
	end;
available(_Token, _UserSessionInfo) -> {nok, invalid_parameters}.

%
% Change online status to available
%
available(UserId) when is_integer(UserId) ->
	case update_session_cache_entry(UserId, ?COURIER_STATUS_AVAILABLE) of
		{ok, _SessionInfo} -> ok;
		not_found -> {nok, user_not_in_session};
		_Other -> {nok, error}
	end;
available(_UserId) -> {nok, invalid_parameters}.

%
% Change online status to unavailable
%
unavailable(Token) when is_binary(Token) ->
	case update_session_cache_entry(Token, ?COURIER_STATUS_UNAVAILABLE, undefined, undefined, undefined) of
		{ok, _SessionInfo} -> ok;
		not_found -> {nok, invalid_token};
		_Other -> {nok, error}
	end;
unavailable(_Token) -> {nok, invalid_parameters}.

%
% Update user position
%
update_session_info(Token, UpdateSessionInfo) when is_binary(Token) ->
	case sanitize(false, UpdateSessionInfo) of
		{ok, FUpdateSessionInfo} ->
			case update_session_cache_entry(Token, FUpdateSessionInfo) of
				{ok, _SessionInfo} -> ok;
				not_found -> {nok, invalid_token};
				_Other -> {nok, error}
			end;
		_ -> {nok, invalid_parameters}
	end;
update_session_info(_Token, _UpdateSessionInfo) -> {nok, invalid_parameters}.

%
% Send a message to the central
%
create_notification(Token, NewNotification) when is_binary(Token) ->
	case sanitize(NewNotification) of
		{ok, #new_notification{position=Position}} ->
			% Update user's position
			case update_session_cache_entry(Token, Position) of
				{ok, _SessionInfo} ->
 					case eb_db_util:execute({create_notification, NewNotification}) of
						{ok, NewNotificationId} -> {ok, NewNotificationId};
						_Other -> {nok, error}
					end;
				 not_found -> {nok, invalid_token};
				_Other -> {nok, error}
			end;
		_ -> {nok, invalid_parameters}
	end;
create_notification(_Token, _NewNotification) -> {nok, invalid_parameters}.

%
% Get courier counters
%
get_courier_counters(RequestingUserTypeId, TransportTypeId, Status) when is_integer(RequestingUserTypeId) ->
	case eb_api_util:is_optional_integer(TransportTypeId) of
		true ->
			case eb_api_util:trim_optional(Status) of
				{ok, FStatus} ->
					case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
						ok ->
							Couriers = get_online_users([?DB_USER_TYPE_COURIER], TransportTypeId, FStatus, undefined),
							SortFun = fun(#session_info{status=?COURIER_STATUS_UNAVAILABLE}, Counters=#courier_counters{unavailable=Count}) -> Counters#courier_counters{unavailable=Count+1};
							             (#session_info{status=?COURIER_STATUS_AVAILABLE}, Counters=#courier_counters{available=Count}) -> Counters#courier_counters{available=Count+1};
							             (#session_info{status=?COURIER_STATUS_WORKING}, Counters=#courier_counters{working=Count}) -> Counters#courier_counters{working=Count+1};
							             (_, Counters) -> Counters
							          end,
							CourierCounters = lists:foldl(SortFun, #courier_counters{}, Couriers),
							{ok, CourierCounters};
						nok -> {nok, forbidden}
					end;
				missing_value -> {nok, invalid_parameters};
				_ -> {nok, error}
			end;
		_ -> {nok, invalid_parameters}
	end;
get_courier_counters(_RequestingUserTypeId, _TransportTypeId, _Status) -> {nok, invalid_parameters}.

%
% Get courier details
%
get_courier_details(RequestingUserTypeId, TransportTypeId, Status, ZoneId, AccountId, Navigation) when is_integer(RequestingUserTypeId) ->
	try
		% Validate permissions
		eb_api_util:verify_operator_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),

		% Sanitize params
		case eb_api_util:is_optional_integer(TransportTypeId) of
			true -> ok;
			_ -> throw(invalid_parameters)
		end,
		FStatus =
			case eb_api_util:trim_optional(Status) of
				{ok, SanitizedStatus} -> SanitizedStatus;
				missing_value -> throw(invalid_parameters);
				_ -> throw(error)
			end,
		case eb_api_util:is_optional_integer(ZoneId) of
			true -> ok;
			_ -> throw(invalid_parameters)
		end,
		FNavigation =
			case eb_api_util:sanitize_navigation(Navigation, [?ORDER_USER_ID]) of
				{ok, SanitizedNavigation} -> SanitizedNavigation;
				_ -> throw(invalid_parameters)
			end,
		% Process result
		Couriers = get_online_users([?DB_USER_TYPE_COURIER], TransportTypeId, FStatus, undefined),
		FilteredCouriers =
			case filter_by_zone(Couriers, ZoneId) of
				FilterZoneResult when is_list(FilterZoneResult) ->
					case filter_by_account(FilterZoneResult, AccountId) of
						FilterAccountResult when is_list(FilterAccountResult) -> FilterAccountResult;
						_ -> throw(error)
					end;
				_ -> throw(error)
			end,
		SortedCouriers = order_and_paginate_online_users(FilteredCouriers, FNavigation),
		CouriersWithoutDeliveries =
			case eb_db_util:execute({get_online_courier_information, SortedCouriers}) of
				CourierInfoResults when is_list(CourierInfoResults) -> CourierInfoResults;
				_ -> throw(error)
			end,
		CouriersWithDeliveries = lists:map(
			fun(CourierWithoutDelivery) ->
				case eb_api_deliveries:get_assigned_deliveries(CourierWithoutDelivery#inf_online_courier.user#user.id) of
					{ok, Deliveries} -> CourierWithoutDelivery#inf_online_courier{assigned_deliveries=Deliveries};
					not_found -> CourierWithoutDelivery;
					_ -> throw(error)
				end
			end, CouriersWithoutDeliveries),
		{ok, CouriersWithDeliveries}
	catch
		throw:Error -> {nok, Error}
	end;
get_courier_details(_RequestingUserTypeId, _TransportTypeId, _Status, _ZoneId, _AccountId, _Navigation) -> {nok, invalid_parameters}.

%
% Get courier accounts
%
get_courier_accounts(RequestingUserTypeId) when is_integer(RequestingUserTypeId) ->
	try
		% Validate permissions
		case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
			ok -> ok;
			nok -> throw(forbidden)
		end,
		% Process result
		Couriers = get_online_users([?DB_USER_TYPE_COURIER], undefined, undefined, undefined),
		case eb_db_util:execute({get_online_courier_accounts, Couriers}) of
			Accounts when is_list(Accounts) -> {ok, Accounts};
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
get_courier_accounts(_RequestingUserTypeId) -> {nok, invalid_parameters}.

%
% Get all push notification types
%
get_push_types() -> {ok, [#reference_data{id=?PUSH_TYPE_ANDROID_ID, description=?PUSH_TYPE_ANDROID_DESCRIPTION}]}.

%
% Get all notification types
%
get_notification_types() ->
	case eb_db_util:execute({get_notification_types}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get user position
%
get_user_position(RequestingUserId, RequestingUserTypeId, UserId) when is_integer(UserId) ->
	case can_access_position_data(RequestingUserId, RequestingUserTypeId, UserId) of
		true ->
			case get_session_cache_entry(UserId) of
				{ok, #session_info{position=undefined}} -> {nok, no_position};
				{ok, #session_info{position=Position}} -> {ok, Position};
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		false -> {nok, forbidden}
	end;
get_user_position(_RequestingUserId, _RequestingUserTypeId, _UserId) -> {nok, invalid_parameters}.

%
% Get user session info
%
get_user_session_info(UserId) when is_integer(UserId) ->
	case get_session_cache_entry(UserId) of
		{ok, SessionInfo} -> {ok, SessionInfo};
		not_found -> {nok, not_found};
		_ -> {nok, error}
	end;
get_user_session_info(_UserId) -> {nok, invalid_parameters}.

%
% Change online status to working
%
working(UserId) when is_integer(UserId) ->
	case update_session_cache_entry(UserId, ?COURIER_STATUS_WORKING) of
		{ok, _SessionInfo} -> ok;
		not_found -> {nok, user_not_in_session};
		_Other -> {nok, error}
	end;
working(_UserId) -> {nok, invalid_parameters}.

%
% Get Help
%
get_help() ->
	case eb_cache_util:get_db_parameter(?DB_PARAM_CALL_CENTER) of
		{ok, CallCenter} -> {ok, CallCenter};
		Other ->
			error_logger:error_msg("~p:get_help(...): Unable to get parameter ?DB_PARAM_CALL_CENTER (~p): ~p~n", [?MODULE, ?DB_PARAM_CALL_CENTER, Other]),
			{nok, missing_db_parameter}
	end.

%
% Notify online users
%
notify_users(UserTypeId, TransportTypeId, OnlineStatus, Latitude, Longitude, Radius, Message) ->
	% Spawn a new process so the main process doesn't get locked while sending the notification
	Function = fun() ->
		error_logger:info_msg("GCM DEBUG notify_users: ~p ~p ~p ~p ~p ~p ~p\n", [UserTypeId, TransportTypeId, OnlineStatus, Latitude, Longitude, Radius, Message]),
		case get_online_users([UserTypeId], TransportTypeId, OnlineStatus, true) of
			[] -> noreply;
			SessionInfos ->
				error_logger:info_msg("GCM DEBUG SessionInfos: ~p\n", [SessionInfos]),
				if
					% Check if we need to filter by radius
					Latitude =/= undefined andalso Longitude =/= undefined andalso Radius =/= undefined ->
						FilterFun = fun(#session_info{position=#position{latitude=UserLatitude, longitude=UserLongitude}}) ->
							eb_util:distance_between(UserLatitude, UserLongitude, Latitude, Longitude) < Radius
						end;
					true ->
						FilterFun = fun(_SessionInfo) -> true end
				end,
				% Get the users that are inside the Radius. Get the push id as a string 
				case [binary_to_list(SessionInfo#session_info.push_info#push_info.id) || SessionInfo <- SessionInfos, FilterFun(SessionInfo)] of
					[] -> noreply;
					UserKeys -> send_user_notification(UserKeys, Message)
				end
		end
	end,
	spawn(Function).

%
% Notify a user
%
notify_user(UserId, UserTypeId, Message) ->
	% Spawn a new process so the main process doesn't get locked while sending the notification
	Function = fun() ->
		case get_session_cache_entry(UserId) of
			{ok, SessionInfo=#session_info{id_user=UserId, id_user_type=UserTypeId, status = Status}} when Status /= ?COURIER_STATUS_UNAVAILABLE ->
				% Get the push id as a string 
				UserKey = binary_to_list(SessionInfo#session_info.push_info#push_info.id),
				send_user_notification([UserKey], Message);
			_ -> noreply
		end
	end,
	spawn(Function).

%
% Get online users
%
get_users(UserTypeIds, OnlineStatus) when is_list(UserTypeIds) andalso is_binary(OnlineStatus) ->
	SessionInfos = get_online_users(UserTypeIds, undefined, OnlineStatus, undefined),
	{ok, SessionInfos};
get_users(_UserTypeIds, _OnlineStatus) -> {nok, invalid_parameters}.

%
% Get online users
%
get_users(UserTypeIds, TransportTypeId, OnlineStatus) when is_list(UserTypeIds), is_integer(TransportTypeId), is_binary(OnlineStatus) ->
	SessionInfos = get_online_users(UserTypeIds, TransportTypeId, OnlineStatus, undefined),
	{ok, SessionInfos};
get_users(_UserTypeIds, _TransportTypeId, _OnlineStatus) -> {nok, invalid_parameters}.

%
% Handle purge expired batch message
%
handle_message(?MESSAGE_PURGE_EXPIRED) ->
	purge_expired(),
	create_expiration_timer(),
	ok;

%
% Generic case
%
handle_message(Message) ->
	error_logger:error_msg("~p:handle_message(~p): Unknown message~n", [?MODULE, Message]),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

create_expiration_timer() ->
	case eb_cache_util:get_db_parameter(?DB_PARAM_PURGE_EXPIRED_SESSIONS_RUN_SECS) of
		{ok, PurgeRunSecs} ->
			eb_batch:create_timer(PurgeRunSecs * 1000, ?MODULE, ?MESSAGE_PURGE_EXPIRED),
			ok;
		Other ->
			error_logger:error_msg("~p:create_expiration_timer(): Unable to get parameter ?DB_PARAM_PURGE_EXPIRED_SESSIONS_RUN_SECS (~p): ~p~n", [?MODULE, ?DB_PARAM_PURGE_EXPIRED_SESSIONS_RUN_SECS, Other]),
			{nok, missing_db_parameter}
	end.

create_session_cache(undefined) ->
	TabDef = [
	            {type, set},
	            {attributes, record_info(fields, session_info)},
	            {record_name, session_info}
	         ],
	mnesia:create_table(?CACHE_SESSIONS, TabDef);
create_session_cache(MainNode) ->
	eb_mnesia_util:copy_cache_tables(MainNode, ?CACHE_SESSIONS).

delete_session(UserId) when is_integer(UserId) ->
	eb_mnesia_util:select_delete(?CACHE_SESSIONS, [{#session_info{id_user = '$1', session_token='$2', _='_'}, [{'=:=', '$1', UserId}], ['$2']}]),
	ok.

purge_expired() ->
	% sessions with session_info.invalid_after=undefined never expire
	CurrentSecs = get_current_seconds(),
	eb_mnesia_util:select_delete(?CACHE_SESSIONS, [{#session_info{invalid_after = '$1', session_token='$2', _='_'}, [{'<', '$1', CurrentSecs}], ['$2']}]).

create_session(UserId, UserTypeId) ->
	case eb_token_util:generate_token(UserId) of
		{ok, SessionToken} ->
			case eb_db_util:execute({set_login_date, UserId}) of
				ok ->
					case UserTypeId of
						?DB_USER_TYPE_COURIER -> Status = ?COURIER_STATUS_UNAVAILABLE;
						_ -> Status = undefined
					end,
					InvalidAfter = get_invalid_after_value(Status),
					SessionInfo = #session_info{session_token=SessionToken, id_user=UserId, id_user_type=UserTypeId, invalid_after=InvalidAfter, status=Status},
					eb_mnesia_util:write(?CACHE_SESSIONS, SessionInfo),
					{ok, SessionToken};
				_ -> {nok, error}
			end;
		_ -> {nok, error}
	end.

replace_session(UserId, UserTypeId) ->
	delete_session(UserId),
	create_session(UserId, UserTypeId).

inner_get_session_cache_entry(GetEntryFun) ->
  	case GetEntryFun() of
		[] -> not_found;
		[SessionInfo] when is_record(SessionInfo, session_info) -> {ok, SessionInfo};
		_Other -> system_error
	end.

inner_mnesia_transaction(Fun) ->
	case mnesia:transaction(Fun) of
		{atomic, Result} -> Result;
		Other -> Other
	end.

get_session_cache_entry(UserId) when is_integer(UserId) ->
	GetEntryFun = fun() -> eb_mnesia_util:select(?CACHE_SESSIONS, [{#session_info{id_user = '$1', _='_'}, [{'=:=', '$1', UserId}], ['$_']}]) end,
	inner_get_session_cache_entry(GetEntryFun);

get_session_cache_entry(Token) ->
	GetEntryFun = fun() -> eb_mnesia_util:read(?CACHE_SESSIONS, Token) end,
	inner_get_session_cache_entry(GetEntryFun).

get_lock_session_cache_entry(UserId) when is_integer(UserId) ->
	GetEntryFun = fun() -> mnesia:select(?CACHE_SESSIONS, [{#session_info{id_user = '$1', _='_'}, [{'=:=', '$1', UserId}], ['$_']}], write) end,
	inner_get_session_cache_entry(GetEntryFun);

get_lock_session_cache_entry(Token) ->
	GetEntryFun = fun() -> mnesia:read(?CACHE_SESSIONS, Token, write) end,
	inner_get_session_cache_entry(GetEntryFun).

% This function updates the session's timeout
touch_session_cache_entry(Token) ->
	Fun = fun() ->
		case get_lock_session_cache_entry(Token) of
			{ok, SessionInfo=#session_info{status=Status}} ->
				InvalidAfter = get_invalid_after_value(Status),
				NewSessionInfo = SessionInfo#session_info{invalid_after=InvalidAfter},
				mnesia:write(?CACHE_SESSIONS, NewSessionInfo, write),
				{ok, NewSessionInfo};
			Error -> Error
		end
	end,
	inner_mnesia_transaction(Fun).

update_session_cache_entry(UserId, Status) when is_integer(UserId) andalso is_binary(Status) ->
	Fun = fun() ->
		case get_lock_session_cache_entry(UserId) of
			{ok, SessionInfo} ->
				InvalidAfter = get_invalid_after_value(Status),
				NewSessionInfo = SessionInfo#session_info{invalid_after=InvalidAfter, status=Status},
				mnesia:write(?CACHE_SESSIONS, NewSessionInfo, write),
				{ok, NewSessionInfo};
			Error -> Error
		end
	end,
	inner_mnesia_transaction(Fun);

update_session_cache_entry(UserId, TransportTypeId) when is_integer(UserId) andalso is_integer(TransportTypeId) ->
	Fun = fun() ->
		case get_lock_session_cache_entry(UserId) of
			{ok, SessionInfo=#session_info{status=Status}} ->
				InvalidAfter = get_invalid_after_value(Status),
				NewSessionInfo = SessionInfo#session_info{invalid_after=InvalidAfter, id_transport_type=TransportTypeId},
				mnesia:write(?CACHE_SESSIONS, NewSessionInfo, write),
				{ok, NewSessionInfo};
			Error -> Error
		end
 	end,
	inner_mnesia_transaction(Fun);

update_session_cache_entry(Token, Position) when is_record(Position, position) ->
	Fun = fun() ->
		case get_lock_session_cache_entry(Token) of
			{ok, SessionInfo=#session_info{id_user=UserId, status=Status}} ->
				InvalidAfter = get_invalid_after_value(Status),
				NewSessionInfo = SessionInfo#session_info{invalid_after=InvalidAfter, position=Position},
				save_user_position(UserId, Status, Position),
	 			mnesia:write(?CACHE_SESSIONS, NewSessionInfo, write),
				{ok, NewSessionInfo};
			Error -> Error
		end
	end,
	inner_mnesia_transaction(Fun);

update_session_cache_entry(Token, #update_session_info{push_info=PushInfo, position=Position}) ->
	Fun = fun() ->
		case get_lock_session_cache_entry(Token) of
			{ok, SessionInfo=#session_info{id_user=UserId, push_info=OldPushInfo, position=OldPosition, status=Status}} ->
				FPushInfo = eb_api_util:get_update_value(PushInfo, OldPushInfo),
				FPosition = eb_api_util:get_update_value(Position, OldPosition),
				InvalidAfter = get_invalid_after_value(Status),
				NewSessionInfo = SessionInfo#session_info{invalid_after=InvalidAfter, push_info=FPushInfo, position=FPosition},
				save_user_position(UserId, Status, FPosition),
				mnesia:write(?CACHE_SESSIONS, NewSessionInfo, write),
				{ok, NewSessionInfo};
			Error -> Error
		end
	end,
	inner_mnesia_transaction(Fun);

update_session_cache_entry(UserId, Notify) when is_integer(UserId) andalso is_boolean(Notify) ->
	Fun = fun() ->
		case get_lock_session_cache_entry(UserId) of
			{ok, SessionInfo=#session_info{status=Status}} ->
				InvalidAfter = get_invalid_after_value(Status),
				NewSessionInfo = SessionInfo#session_info{invalid_after=InvalidAfter, notify=Notify},
				mnesia:write(?CACHE_SESSIONS, NewSessionInfo, write),
				{ok, NewSessionInfo};
			Error -> Error
		end
	end,
	inner_mnesia_transaction(Fun).

update_session_cache_entry(Token, Status, PushInfo, Position, Notify) ->
	Fun = fun() ->
		case get_lock_session_cache_entry(Token) of
			{ok, SessionInfo=#session_info{id_user=UserId}} ->
				InvalidAfter = get_invalid_after_value(Status),
				NewSessionInfo = SessionInfo#session_info{invalid_after=InvalidAfter, status=Status, push_info=PushInfo, position=Position,
				                                          notify=Notify},
				save_user_position(UserId, Status, Position),
				mnesia:write(?CACHE_SESSIONS, NewSessionInfo, write),
				{ok, NewSessionInfo};
			Error -> Error
		end
	end,
	inner_mnesia_transaction(Fun).

get_invalid_after_value(?COURIER_STATUS_WORKING) -> undefined; % When the courier is working, the session never expires
get_invalid_after_value(_OtherStatus) ->
	case eb_cache_util:get_db_parameter(?DB_PARAM_SESSION_TIMEOUT_SECS) of
		{ok, SessionTimeoutSecs} ->
			TimeInSecs = get_current_seconds(),
			TimeInSecs + SessionTimeoutSecs;
		Other ->
			error_logger:info_msg("~p:get_invalid_after_value(...): Unable to get parameter ?DB_PARAM_SESSION_TIMEOUT_SECS (~p): ~p. Assuming session never expires.~n", [?MODULE, ?DB_PARAM_SESSION_TIMEOUT_SECS, Other]),
			undefined % If there is an error getting ?DB_PARAM_SESSION_TIMEOUT_SECS we assume the session never expires
	end.

get_current_seconds() ->
	erlang:monotonic_time(seconds).

can_access_position_data(UserId, UserTypeId, CourierUserId) ->
	% Check user type
	case eb_api_util:verify_operator_permissions(UserTypeId) of
		ok -> true;
		_ -> 
			% Check if it's the same user
			case eb_api_util:verify_same_user(UserId, CourierUserId) of
				ok -> true;
				_ ->
					% Pprocura na cache se o estafeta fez check out do
					% ponto imediatamente anterior ao que pertence ao cliente
					% Check if user is the owner of the delivery
					case eb_api_deliveries:get_ongoing_deliveries(UserId, CourierUserId) of
						{ok, _Deliveries} -> true; 
						_ -> false
					end
			end
	end.

%
% Query sessions cache
%
get_online_users(UserTypeIds, undefined, undefined, undefined) ->
	% Function to create the 'OR' conditions
	MSFun = fun(UserTypeId, MatchSpecAcc) -> [{#session_info{id_user_type=UserTypeId, _='_'}, [], ['$_']}|MatchSpecAcc] end,
	MatchSpec = lists:foldl(MSFun, [], UserTypeIds),
	% Execute the query
	eb_mnesia_util:select(?CACHE_SESSIONS, MatchSpec);
get_online_users(UserTypeIds, undefined, undefined, Notify) ->
	% Function to create the 'OR' conditions
	MSFun = fun(UserTypeId, MatchSpecAcc) -> [{#session_info{id_user_type=UserTypeId, notify=Notify, _='_'}, [], ['$_']}|MatchSpecAcc] end,
	MatchSpec = lists:foldl(MSFun, [], UserTypeIds),
	% Execute the query
	eb_mnesia_util:select(?CACHE_SESSIONS, MatchSpec);
get_online_users(UserTypeIds, undefined, Status, undefined) ->
	% Function to create the 'OR' conditions
	MSFun = fun(UserTypeId, MatchSpecAcc) -> [{#session_info{id_user_type=UserTypeId, status=Status, _='_'}, [], ['$_']}|MatchSpecAcc] end,
	MatchSpec = lists:foldl(MSFun, [], UserTypeIds),
	% Execute the query
	eb_mnesia_util:select(?CACHE_SESSIONS, MatchSpec);
get_online_users(UserTypeIds, undefined, Status, Notify) ->
	% Function to create the 'OR' conditions
	MSFun = fun(UserTypeId, MatchSpecAcc) -> [{#session_info{id_user_type=UserTypeId, status=Status, notify=Notify, _='_'}, [], ['$_']}|MatchSpecAcc] end,
	MatchSpec = lists:foldl(MSFun, [], UserTypeIds),
	% Execute the query
	eb_mnesia_util:select(?CACHE_SESSIONS, MatchSpec);
get_online_users(UserTypeIds, TransportTypeId, undefined, undefined) ->
	% Function to create the 'OR' conditions
	MSFun = fun(UserTypeId, MatchSpecAcc) -> [{#session_info{id_user_type=UserTypeId, id_transport_type=TransportTypeId, _='_'}, [], ['$_']}|MatchSpecAcc] end,
	MatchSpec = lists:foldl(MSFun, [], UserTypeIds),
	% Execute the query
	eb_mnesia_util:select(?CACHE_SESSIONS, MatchSpec);
get_online_users(UserTypeIds, TransportTypeId, undefined, Notify) ->
	% Function to create the 'OR' conditions
	MSFun = fun(UserTypeId, MatchSpecAcc) -> [{#session_info{id_user_type=UserTypeId, id_transport_type=TransportTypeId, notify=Notify, _='_'}, [], ['$_']}|MatchSpecAcc] end,
	MatchSpec = lists:foldl(MSFun, [], UserTypeIds),
	% Execute the query
	eb_mnesia_util:select(?CACHE_SESSIONS, MatchSpec);
get_online_users(UserTypeIds, TransportTypeId, Status, undefined) ->
	% Function to create the 'OR' conditions
	MSFun = fun(UserTypeId, MatchSpecAcc) -> [{#session_info{id_user_type=UserTypeId, id_transport_type=TransportTypeId, status=Status, _='_'}, [], ['$_']}|MatchSpecAcc] end,
	MatchSpec = lists:foldl(MSFun, [], UserTypeIds),
	% Execute the query
	eb_mnesia_util:select(?CACHE_SESSIONS, MatchSpec);
get_online_users(UserTypeIds, TransportTypeId, Status, Notify) ->
	% Function to create the 'OR' conditions
	MSFun = fun(UserTypeId, MatchSpecAcc) -> [{#session_info{id_user_type=UserTypeId, id_transport_type=TransportTypeId, status=Status, notify=Notify, _='_'}, [], ['$_']}|MatchSpecAcc] end,
	MatchSpec = lists:foldl(MSFun, [], UserTypeIds),
	% Execute the query
	eb_mnesia_util:select(?CACHE_SESSIONS, MatchSpec).

get_sort_booleans(?NAVIGATION_SORT_ASC) -> {true, false};
get_sort_booleans(_) -> {false, true}.

order_and_paginate_online_users([], _Navigation) -> [];
order_and_paginate_online_users(Users, #rs_navigation{skip=Skip}) when length(Users) < (Skip + 1) -> [];
order_and_paginate_online_users(Users, Navigation=#rs_navigation{order=Order, sort=Sort}) ->
	% Order the results
	case Order of
		?ORDER_USER_ID ->
			{SortLessThan, SortBiggerThan} = get_sort_booleans(Sort),
			SortFun = fun(#session_info{id_user=UserIDA}, #session_info{id_user=UserIDB}) when UserIDA < UserIDB -> SortLessThan;
			             (_, _) -> SortBiggerThan
			          end,
			OrderedUsers = lists:sort(SortFun, Users);
		_ -> OrderedUsers = Users
	end,
	% Paginate
	case Navigation of
		#rs_navigation{skip=Skip, max=Max} when Max > 0 ->
			lists:sublist(OrderedUsers, Skip + 1, Max);
		_ ->
			% No limits
			OrderedUsers
	end.

get_user_keys_as_string([FirstUserKey|RemainingUserKeys]) ->
	lists:flatten(["\"" ++ FirstUserKey ++ "\"" | [[",", "\"" ++ UserKey ++ "\""] || UserKey <- RemainingUserKeys]]).

%% Save courier position
save_user_position(UserId, ?COURIER_STATUS_WORKING, #position{latitude=Latitude, longitude=Longitude}) ->
	case eb_api_deliveries:get_working_delivery(UserId) of
		{ok, #inf_delivery{delivery=#delivery{id=IdDelivery}}} ->
			% The return values are being ignored
			eb_db_util:execute({create_courier_route, UserId, IdDelivery, Latitude, Longitude}),
			ok;
		_Other -> ok
	end;
save_user_position(_UserId, _Status, _Position) -> ok.

get_auth_info(TokenEndPoint, AuthorizationCode, RedirectURI, ClientId, ClientSecret) ->
	% Obtain an access token
	RequestData = "?client_id=" ++ binary_to_list(ClientId) ++ "&client_secret=" ++ binary_to_list(ClientSecret) ++ "&code=" ++ binary_to_list(AuthorizationCode) ++ "&redirect_uri=" ++ binary_to_list(RedirectURI),
	CreateRequest = {binary_to_list(TokenEndPoint) ++ RequestData, [], "application/x-www-form-urlencoded", []},
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(post, CreateRequest, HTTPOptions, Options) of
		{ok, {_StatusCode, Body}} ->
			Body = "access_token=TGT-589-vvuBHRciZY0eHcUogbhC6MDGLyMtpd7uxcHlWaXnl4ovp4V59a-www.ctt.pt&expires=3403",
			Data=[{Label, Value}=list_to_tuple(string:tokens(Elem, "=")) || Elem <- string:tokens(Body, "&")],
			case parse_auth_info(Data) of
				{ok, AuthInfo} -> {ok, AuthInfo};
				{nok, Other} ->
					error_logger:error_msg("DEBUG parse_auth_info: Error parsing the query response: ~p~n", [Other]),
					{nok, Other}
			end;
		{error, Reason} ->
			error_logger:error_msg("~p:get_auth_info(~p, ~p, ~p}: Unexpected error querying: ~p~n", [?MODULE, CreateRequest, HTTPOptions, Options, Reason]),
			{nok, error}
	end.

parse_auth_info(Data) when is_list(Data) ->
	parse_auth_info(Data, #auth_info{}).

parse_auth_info([], AuthInfo) -> {ok, AuthInfo};
parse_auth_info([Element|Rest], AuthInfo) ->
	case set_auth_record(Element, AuthInfo) of
		{nok, Other} -> {nok, Other};
		NewAuthInfo ->
			parse_auth_info(Rest, NewAuthInfo)
	end.

set_auth_record({Label, Value}, NewRecord) ->
	case Label of
		"access_token" -> NewRecord#auth_info{access_token=Value};
		"expires" -> NewRecord#auth_info{expires=Value};
		"error" -> {nok, Value};
		_ -> NewRecord
	end.

get_user_profile(ProfileEndpoint, AccessToken, ProviderDescription) ->
	% Obtain profile
	RequestUrl = binary_to_list(ProfileEndpoint) ++ "?access_token=" ++ AccessToken,
	GetRequest = {RequestUrl, []},

	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],

	Body = [123,34,105,100,34,58,34,80,97,100,101,115,99,97,34,
                         44,34,97,116,116,114,105,98,117,116,101,115,34,58,91,
                         123,34,76,111,99,97,108,34,58,34,65,71,85,65,76,86,
                         65,45,67,65,67,195,137,77,34,125,44,123,34,71,85,73,
                         68,34,58,34,51,67,69,50,55,53,65,52,45,69,55,52,49,
                         45,52,51,49,70,45,65,48,55,53,45,48,54,57,68,54,51,
                         52,69,55,66,70,52,34,125,44,123,34,80,111,115,116,97,
                         108,68,101,115,105,103,34,58,34,65,71,85,65,76,86,65,
                         45,67,65,67,195,137,77,34,125,44,123,34,65,99,99,116,
                         78,111,110,76,111,99,107,101,100,34,58,34,49,34,125,
                         44,123,34,67,80,52,34,58,34,50,55,51,53,34,125,44,
                         123,34,67,80,51,34,58,34,51,51,54,34,125,44,123,34,
                         67,111,117,110,116,114,121,34,58,34,80,84,34,125,44,
                         123,34,76,97,115,116,83,117,99,99,101,115,115,76,111,
                         103,105,110,34,58,34,50,48,49,55,45,48,51,45,48,49,
                         32,49,54,58,51,50,58,51,52,46,54,48,52,34,125,44,123,
                         34,78,97,109,101,34,58,34,80,97,117,108,111,32,80,
                         101,114,101,105,114,97,34,125,44,123,34,65,99,116,
                         105,118,101,34,58,34,49,34,125,44,123,34,69,109,97,
                         105,108,34,58,34,112,97,117,108,111,46,112,97,100,
                         101,115,99,97,46,112,101,114,101,105,114,97,64,103,
                         109,97,105,108,46,99,111,109,34,125,44,123,34,67,114,
                         101,100,78,111,110,69,120,112,105,114,101,100,34,58,
                         34,49,34,125,44,123,34,114,111,108,101,115,34,58,34,
                         73,83,78,69,95,70,69,87,69,66,34,125,44,123,34,65,
                         100,100,114,101,115,115,34,58,34,69,115,116,114,97,
                         100,97,32,100,101,32,80,97,195,167,111,32,100,101,32,
                         65,114,99,111,115,44,32,54,54,32,101,32,54,54,45,65,
                         34,125,44,123,34,73,68,34,58,34,50,48,56,50,48,53,50,
                         34,125,44,123,34,77,111,98,105,108,101,80,104,111,
                         110,101,34,58,34,57,54,53,54,49,51,56,49,49,34,125,
                         44,123,34,65,99,99,116,78,111,110,69,120,112,105,114,
                         101,100,34,58,34,49,34,125,44,123,34,67,111,109,112,
                         97,110,121,67,111,117,110,116,114,121,34,58,34,80,84,
                         34,125,93,125],
	try
				JsonBody = jsondoc:decode(Body),
				case parse_external_user_info(JsonBody) of
					{ok, ExternalUserInfo} -> {ok, ExternalUserInfo};
					Other ->
						error_logger:error_msg("~p:get_user_profile(~p, ~p, ~p}: Error parsing the query response: ~p~n", [?MODULE, GetRequest, HTTPOptions, Options, Other]),
						{nok, Other}
				end
	catch
		_:_ -> {nok, error}
	end.

parse_external_user_info({Elements}) when is_list(Elements) ->
	parse_external_user_info(Elements, #new_user_external{}).

parse_external_user_info([], ExternalUserInfo) -> {ok, ExternalUserInfo};
parse_external_user_info([Element|Rest], ExternalUserInfo) ->
	NewExternalUserInfo = set_new_user_external_record(Element, ExternalUserInfo),
	parse_external_user_info(Rest, NewExternalUserInfo).

set_new_user_external_record(Element={Label, Value}, NewRecord) when is_record(NewRecord, new_user_external) ->
	case Label of
		<<"attributes">> ->
			case parse_external_attributes(Value, NewRecord) of
				{ok, #new_user_external{id_oauth=OauthId, email=Email, telephone_nr=TelephoneNr, address=Address}} ->
					FNewRecord = NewRecord#new_user_external{id_oauth=OauthId, email=Email, telephone_nr=TelephoneNr, address=Address};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end.

parse_external_attributes([], ExternalUserInfo) -> {ok, ExternalUserInfo};
parse_external_attributes([Element|Rest], ExternalUserInfo) ->
	NewExternalUserInfo = set_external_attributes_record(Element, ExternalUserInfo),
	parse_external_attributes(Rest, NewExternalUserInfo).

set_external_attributes_record(Element={[{Label, Value}]}, NewRecord) when is_record(NewRecord, new_user_external) ->
	case Label of
		<<"ID">> -> NewRecord#new_user_external{id_oauth=Value};
		<<"Email">> -> NewRecord#new_user_external{email=Value};
		<<"MobilePhone">> -> NewRecord#new_user_external{telephone_nr=Value};
		_ -> NewRecord
	end.

create_user_session(UserId, UserTypeId, ValidUserTypes) ->
	case eb_cache_util:get_db_parameter(?DB_PARAM_SESSION_TIMEOUT_SECS) of
		{ok, SessionTimeoutSecs} ->
			case lists:member(UserTypeId, ValidUserTypes) of
				true ->
					case get_session_cache_entry(UserId) of
						{ok, #session_info{id_user_type=?DB_USER_TYPE_COURIER, status=?COURIER_STATUS_WORKING}} -> {nok, in_use};
						{ok, SessionInfo} when is_record(SessionInfo, session_info) ->
							case replace_session(UserId, UserTypeId) of
								{ok, SessionToken} -> {ok, SessionToken, SessionTimeoutSecs, UserId};
								_ -> {nok, error}
							end;
						{ok, _OtherUserType} -> {nok, in_use};
						not_found ->
							case create_session(UserId, UserTypeId) of
								{ok, SessionToken} -> {ok, SessionToken, SessionTimeoutSecs, UserId};
								_ -> {nok, error}
							end;
						_ -> {nok, error}
					end;
				false -> {nok, invalid_user_type}
			end;
		Other ->
			error_logger:error_msg("~p:create_user_session(...): Unable to get parameter ?DB_PARAM_SESSION_TIMEOUT_SECS (~p): ~p~n", [?MODULE, ?DB_PARAM_SESSION_TIMEOUT_SECS, Other]),
			{nok, missing_db_parameter}
	end.

send_user_notification(UserKeys, Message) ->
	case eb_cache_util:get_db_parameters([?DB_PARAM_GOOGLE_API_KEY, ?DB_PARAM_GOOGLE_CLOUD_MESSAGING_ENDPOINT], ?MODULE, send_user_notification) of
		[ApiKey, Endpoint] ->
			% Get the user keys as a string
			UserKeysString = get_user_keys_as_string(UserKeys),
			% Get the message as a string
			JSON = binary_to_list(Message),
			% Build the notification payload
			Body = "{\"data\":" ++ JSON ++ ",\"registration_ids\":[" ++ UserKeysString ++ "],\"time_to_live\":" ++ integer_to_list(30) ++ "}",
			Request = {Endpoint, [{"Authorization", "key=" ++ ApiKey}], "application/json", Body},
			% Execute
			Response = httpc:request(post,
			                         Request,
			                         [{timeout, 15000}, {connect_timeout, 15000}],
			                         [{sync, true}]);
		_Other -> error
	end.

filter_by_zone(SessionInfos, undefined) -> SessionInfos;
filter_by_zone([], _ZoneId) -> [];
filter_by_zone(SessionInfos, ZoneId) ->
	CheckZone = fun(UserId) ->
		case eb_db_util:execute({exists_user_zone, UserId, ZoneId}) of
			true -> true;
			false -> false;
			_ -> throw(error)
		end
	end,
	try
		[SessionInfo || SessionInfo <- SessionInfos, CheckZone(SessionInfo#session_info.id_user)]
	catch
		throw:error -> nok
	end.

filter_by_account(SessionInfos, undefined) -> SessionInfos;
filter_by_account([], _AccountId) -> [];
filter_by_account(SessionInfos, AccountId) ->
	CheckAccount = fun(UserId) ->
		case eb_db_util:execute({exists_account_user, UserId, AccountId}) of
			true -> true;
			false -> false;
			_ -> throw(error)
		end
	end,
	try
		[SessionInfo || SessionInfo <- SessionInfos, CheckAccount(SessionInfo#session_info.id_user)]
	catch
		throw:error -> nok
	end.

sanitize(false, #update_session_info{push_info=PushInfo, position=Position}) ->
	try
		case PushInfo of
			undefined -> FPushInfo = undefined;
			_ -> {ok, FPushInfo} = sanitize(PushInfo)
		end,
		case Position of
			undefined -> FPosition = undefined;
			_ -> {ok, FPosition} = eb_api_util:sanitize_position(Position)
		end,
		% Build the return record
		SanitizedRecord = #update_session_info{push_info=FPushInfo, position=FPosition},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(true, #update_session_info{push_info=undefined}) -> nok;
sanitize(true, #update_session_info{position=undefined}) -> nok;
sanitize(true, #update_session_info{push_info=PushInfo, position=Position}) ->
	try
		% Trim binary strings
		{ok, FPushInfo} = sanitize(PushInfo),
		{ok, FPosition} = eb_api_util:sanitize_position(Position),
		% Build the return record
		SanitizedRecord = #update_session_info{push_info=FPushInfo, position=FPosition},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;
sanitize(_All, _Other) -> nok.

sanitize(#push_info{type=Type}) when Type =/= ?PUSH_TYPE_ANDROID_ID -> nok;
sanitize(#push_info{id=PushId}) ->
	try
		% Trim binary strings
		{ok, FPushId} = eb_api_util:trim_mandatory(PushId),
		% Build the return record
		SanitizedRecord = #push_info{type=?PUSH_TYPE_ANDROID_ID, id=FPushId},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#new_notification{delivery_id=DeliveryId}) when not is_integer(DeliveryId) -> nok;
sanitize(#new_notification{waypoint_id=WaypointId}) when not is_integer(WaypointId) -> nok;
sanitize(#new_notification{delivery_id=DeliveryId, waypoint_id=WaypointId, notification_type_id=NotificationTypeId, message=Message, position=Position}) ->
	try
		true = eb_api_util:is_optional_integer(NotificationTypeId),
		{ok, FMessage} = eb_api_util:trim_optional(Message),
		{ok, FPosition} = eb_api_util:sanitize_position(Position),
		% Check if one of NotificationId or NotificationMessage is present
		ok = eb_api_util:check_one_of(NotificationTypeId, FMessage),
		% Size validations
		ok = eb_api_util:validate_size_optional(FMessage, ?DB_FIELD_SIZE__NOTIFICATION__MESSAGE),
		% Build the return record
		SanitizedRecord = #new_notification{delivery_id=DeliveryId, waypoint_id=WaypointId, notification_type_id=NotificationTypeId,
                                            message=FMessage, position=FPosition},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(Record = #authorization_info{code=Code, redirect_uri=RedirectURI, token=Token}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FCode} = eb_api_util:trim_mandatory(Code, TrimPattern),
		{ok, FRedirectURI} = eb_api_util:trim_mandatory(RedirectURI, TrimPattern),
		{ok, FToken} = eb_api_util:trim_optional(Token, TrimPattern),
		% Build the return record
		SanitizedRecord = Record#authorization_info{code=FCode, redirect_uri=FRedirectURI, token=FToken},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(_Other) -> nok.
