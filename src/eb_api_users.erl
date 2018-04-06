%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_api_users).

-include("eb_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_user/3, create_user_external/1, confirm_user_email/1, count_users/3, get_users/4, get_user/3, change_user/4,
		 change_password/4, reset_password/3, set_password/2, get_user_types/0, get_user_statuses/0, get_user_photo/3,
		 get_user_documents/6, get_transport_types/0, get_user_transports/3, get_user_document/4,
		 create_user_document/4, change_user_document/5, get_document_types/0, get_document_statuses/0, get_user_ranking/3,
		 get_user_couriers/4, add_user_courier/5, remove_user_courier/5, get_user_locations/6, add_user_location/4,
		 get_user_used_couriers/6, remove_user_location/5, change_user_location/5, get_user_location/4,
		 create_courier_transport/4, remove_courier_transport/5, change_courier_transport/5,
		 get_user_notification_types/0, get_user_notifications/3, get_transport_type_statuses/0,
		 set_user_notifications/4, get_zones/0, change_current_courier_transport/5, get_mobileos/0,
		 get_commercial_info/3, change_commercial_info/4]).

%
% Create user
%
create_user(RequestingUserId, RequestingUserTypeId, NewUser) when is_record(NewUser, new_user) ->
	try
		SanitizedNewUser=#new_user{new_account=NewAccount, user_type_id=UserTypeId, mobileos_id=MobileOsId} =
			case sanitize(NewUser) of
				{ok, SanitizedRecord} -> SanitizedRecord;
				nok -> throw(missing_values)
			end,
		case verify_user_creation_permission(RequestingUserId, RequestingUserTypeId, UserTypeId) of
			ok -> ok;
			nok -> throw(forbidden)
		end,
		% Business accounts validations
		FUserRecord=#new_user{username=Username} =
			case validate_account_parameters(NewAccount, SanitizedNewUser) of
				{ok, NewUserRecord} -> NewUserRecord;
				{nok, Error} -> throw(Error)
			end,
		% Verify the existence of the user type
		case eb_db_util:execute({exists_user_type, UserTypeId}) of
			true -> ok;
			false -> throw(unknown_user_type_id);
			_ -> throw(invalid_user_type_id)
		end,
		% Verify the existence of the username
		case eb_db_util:execute({exists_user, Username}) of
			false -> ok;
			true -> throw(user_exists);
			_ -> throw(error)
		end,
		% Verify the existence of the user mobileosid
		case eb_db_util:execute({exists_mobileos, MobileOsId}) of
			true -> ok;
			false -> throw(unknown_mobileos_id);
			_ -> throw(invalid_mobileos_id)
		end,
		% Create the user
		case eb_db_util:execute({create_user, undefined, FUserRecord}) of
			{ok, NewUserId, _NewAccountVersion} ->
				{ok, NewUserId};
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end;
create_user(_RequestingUserId, _RequestingUserTypeId, _NewUser) -> {nok, invalid_parameters}.

%
% Create external user
%
create_user_external(NewUserExternal = #new_user_external{token=TokenId}) ->
	try
		% Get the User Redirect URL parameter from cache
		UserRedirectUrl = eb_cache_util:get_db_parameter(?DB_PARAM_USER_REDIRECT_URL, ?MODULE, create_user_external),
		{FOauthId, FOauthProviderId, FUserTypeId} =
			case eb_db_util:execute({get_token_info, TokenId}) of
				{ok, #token{id_type=?DB_TOKEN_TYPE_CREATE_USER}, [#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_ID, value=BinOauthId},
																				  #token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_TYPE_ID, value=BinUserTypeId},
																				  #token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_OAUTH_PROVIDER_ID, value=BinOauthProviderId}]} ->
					TOauthId = binary_to_list(BinOauthId),
					TOauthProviderId =
						case eb_util:binary_to_int(BinOauthProviderId) of
							{ok, OauthProviderId} -> OauthProviderId;
							error -> throw(error2)
						end,
					TUserTypeId =
						case eb_util:binary_to_int(BinUserTypeId) of
							{ok, UserTypeId} -> UserTypeId;
							error -> throw(error3)
						end,
					{TOauthId, TOauthProviderId, TUserTypeId};
				{ok, #token{id_type=TokenTypeId}, _} when TokenTypeId =/= ?DB_TOKEN_TYPE_CREATE_USER -> throw(wrong_token_type);
				{nok, invalid} -> throw(invalid_token);
				not_found -> throw(not_found_token);
				_ -> throw(error)
			end,
		TNewUserExternal = NewUserExternal#new_user_external{id_oauth=FOauthId, id_oauth_provider=FOauthProviderId,
																			  user_type_id=FUserTypeId},
		FNewUserExternal = #new_user_external{email=Email} =
			case sanitize(TNewUserExternal) of
				{ok, SanitizedRecord} -> SanitizedRecord;
				_ -> throw(missing_values)
			end,
		% Verify the existence of the user type
		case eb_db_util:execute({exists_user_type, FUserTypeId}) of
			true -> ok;
			false -> throw(unknown_user_type_id);
			_ -> throw(invalid_user_type_id)
		end,
		% Verify the existence of the external user
		% If user exists, invalidate token
		case eb_db_util:execute({exists_user_external, FOauthId, FOauthProviderId}) of
			false -> ok;
			true -> throw(invalid_token);
			_ -> throw(error)
		end,
		% Create the user
		case eb_db_util:execute({create_user_external, FNewUserExternal}) of
			{ok, NewTokenId} ->
				% New user notification
				eb_notification:create_user(Email, NewTokenId, UserRedirectUrl),
				ok;
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
create_user_external(_NewUserExternal) -> {nok, invalid_parameters}.

%
% User email is valid
%
confirm_user_email(TokenId) when is_binary(TokenId) andalso TokenId =/= <<>> ->
	case eb_db_util:execute({get_token_info, TokenId}) of
		{ok, #token{id_type=?DB_TOKEN_TYPE_CREATE_USER}, [#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_ID, value=BinUserId}]} ->
			case eb_util:binary_to_int(BinUserId) of
				{ok, UserId} ->
					case eb_db_util:execute({confirm_user_email, UserId}) of
						ok -> ok;
						not_found -> {nok, not_found_user};
						_ -> {nok, error}
					end;
				error -> {nok, error}
			end;
		{ok, #token{id_type=TokenTypeId}, _} when TokenTypeId =/= ?DB_TOKEN_TYPE_CREATE_USER -> {nok, wrong_type};
		{nok, invalid} -> {nok, invalid};
		not_found -> {nok, not_found_token};
		_Other -> {nok, error}
	end;
confirm_user_email(_TokenId) -> {nok, invalid_parameters}.

%
% Count all users
%
count_users(RequestingUserTypeId, UserTypeId, UserStatusId) when is_integer(RequestingUserTypeId) ->
	case eb_api_util:is_optional_integer(UserTypeId) of
		true ->
			case eb_api_util:is_optional_integer(UserStatusId) of
				true ->
					case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
						ok ->
							case eb_db_util:execute({count_users, UserTypeId, UserStatusId}) of
								Count when is_integer(Count) -> {ok, Count};
								_ -> {nok, error}
							end;
						nok -> {nok, forbidden}
					end;
				_ -> {nok, invalid_parameters}
			end;
		_ -> {nok, invalid_parameters}
	end;
count_users( _RequestingUserTypeId, _UserTypeId, _UserStatusId) -> {nok, invalid_parameters}.

%
% Get all users
%
get_users(RequestingUserTypeId, UserTypeId, UserStatusId, Navigation) when is_integer(RequestingUserTypeId) ->
	try
		UserTypeIdList=
			case UserTypeId of
				undefined -> undefined;
				_ -> [begin {Int,_}=string:to_integer(Elem), Int end|| Elem <- string:tokens(UserTypeId,",")] % "," values separator
			end,
		case eb_api_util:is_optional_integer(UserStatusId) of
			true -> ok;
			_ -> throw(invalid_parameters)
		end,
		case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
			ok -> ok;
			nok -> throw(forbidden)
		end,
		FNavigation =
			case eb_api_util:sanitize_navigation(Navigation, [?ORDER_USER_ID, ?ORDER_USER_TYPE_ID, ?ORDER_USER_STATUS_ID]) of
				{ok, SanitizedNavigation} -> SanitizedNavigation;
				_ -> throw(invalid_parameters)
			end,
		case eb_db_util:execute({get_users_information, UserTypeIdList, UserStatusId, FNavigation}) of
			Results when is_list(Results) -> {ok, Results};
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
get_users(_RequestingUserTypeId, _UserTypeId, _UserStatusId, _Navigation) -> {nok, invalid_parameters}.

%
% Get user
%
get_user(RequestingUserId, RequestingUserTypeId, TargetUserId) when is_integer(RequestingUserId)
                                                            andalso is_integer(RequestingUserTypeId)
                                                            andalso is_integer(TargetUserId) ->
	try
		% Verify permissions
		verify_data_access(RequestingUserId, RequestingUserTypeId, TargetUserId) =:= ok orelse
			eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		
		% Get user information
		case eb_db_util:execute({get_user_information, TargetUserId}) of
			Result when is_record(Result, inf_user) ->
				_User = filter_user_information(RequestingUserId, RequestingUserTypeId, TargetUserId, Result#inf_user.user),
				{ok, Result};
			not_found -> throw(not_found);
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;	
get_user(_RequestingUserId, _RequestingUserTypeId, _TargetUserId) -> {nok, invalid_parameters}.

%
% Modify the user information
%
change_user(RequestingUserId, RequestingUserTypeId, TargetUserId, ChangeUserInfo) when is_integer(RequestingUserId)
                                                                               andalso is_integer(RequestingUserTypeId)
                                                                               andalso is_integer(TargetUserId) ->
	try
		% Get the Email Redirect URL parameter from cache
		EmailRedirectUrl = eb_cache_util:get_db_parameter(?DB_PARAM_EMAIL_REDIRECT_URL, ?MODULE, change_user),
		% Verify user permissions
		eb_api_util:verify_user_or_account_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) =:= ok orelse throw(forbidden),
		% Sanitize record
		SanitizedChangeUserInfo = #change_user{username=Username, user_status_id=UserStatusId, email=NewEmail,
		                                       mobileos_id=MobileosId, zone_ids=ZoneIds, version=Version} =
			case sanitize(ChangeUserInfo) of
				{ok, SanitizedRecord} -> SanitizedRecord;
				nok -> throw(missing_values)
			end,
		% Verify the existence of the user, version and get the old user record
		OldUser =
			case eb_db_util:execute({get_user, TargetUserId}) of
				UserRecord=#user{version=Version} -> UserRecord;
				UserRecord when is_record(UserRecord, user) -> throw(version);
				not_found -> throw(not_found);
				_ -> throw(get_user)
			end,
		% Verify the existence of the username
		case Username of
			undefined -> ok;
			_ ->
				case eb_db_util:execute({exists_user, Username}) of
					true -> throw(user_exists);
					false -> ok;
					_ -> throw(exists_user)
				end
		end,
		% Validate user_status_id
		case UserStatusId of
			undefined -> ok;
			_ ->
				% User is not allowed the change his own status
				RequestingUserId =:= TargetUserId andalso throw(forbidden),
				% Verify that the user_status_id exists
				case eb_db_util:execute({exists_user_status, UserStatusId}) of
					true -> ok;
					false -> throw(invalid_user_status);
					_ -> throw(exists_user_status)
				end
		end,
		% Validate mobileos_id
		case MobileosId of
			undefined -> ok;
			_ ->
				case eb_db_util:execute({exists_mobileos, MobileosId}) of
					true -> ok;
					false -> throw(invalid_mobileos);
					_ -> throw(error)
				end
		end,
		% Validate zone_ids
		case ZoneIds of
			undefined -> ok;
			_ ->
				ValidateZoneId = fun(ZoneId) ->
					case eb_db_util:execute({exists_zone, ZoneId}) of
						true -> ok;
						false -> throw(invalid_zone);
						_ -> throw(error)
					end
				end,
				lists:map(ValidateZoneId, ZoneIds)
		end,
		% Change the user
		case eb_db_util:execute({change_user, OldUser, SanitizedChangeUserInfo}) of
			{ok, TokenId, NewVersion} ->
				{ok, NewVersion};
			{ok, NewVersion} -> {ok, NewVersion};
			_ -> throw(change_user)
		end
	catch
		throw:Error -> {nok, Error}
	end;
change_user(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _ChangeUserInfo) -> {nok, invalid_parameters}.

%
% Change the password
%
change_password(RequestingUserId, RequestingUserTypeId, TargetUserId, ChangePasswordInfo) when is_integer(RequestingUserId)
                                                                                       andalso is_integer(RequestingUserTypeId)
                                                                                       andalso is_integer(TargetUserId) ->
	case eb_api_util:verify_same_user(RequestingUserId, TargetUserId) of
		ok ->
			case sanitize(ChangePasswordInfo) of
				{ok, #change_password{old_password=OldPassword, new_password=NewPassword, version=Version}} ->
					% Verify user credentials
					case eb_db_util:execute({get_authenticated_user, TargetUserId, OldPassword}) of
						#user{email=Email} ->
							% Change the password
							case eb_db_util:execute({set_password, TargetUserId, Version, NewPassword}) of
								ok ->
									% Password change notification
									eb_notification:change_password(Email),
									ok;
								_ -> {nok, error}
							end;
						not_found -> {nok, not_found};
						_ -> {nok, error}
					end;
				nok -> {nok, missing_values}
			end;
		nok -> {nok, forbidden}
	end;
change_password(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _ChangeUserInfo) -> {nok, invalid_parameters}.

%
% Send reset password email
%
reset_password(RequestingUserId, RequestingUserTypeId, ResetPassword)
    when (is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId))
  orelse (RequestingUserId =:= undefined andalso RequestingUserTypeId =:= undefined) ->
	case eb_cache_util:get_db_parameter(?DB_PARAM_PASSWORD_REDIRECT_URL) of
		{ok, PasswordRedirectUrl} ->
			case sanitize(ResetPassword) of
				{ok, #reset_password{user_id=TargetUserId, username=TargetUsername}} ->
					% Only operators and users without session can use reset_password
					% Users without session can only reset password by username
					case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
						ok ->
							case TargetUserId of
								TargetUserId when is_integer(TargetUserId) -> Parameter = TargetUserId;
								_ -> Parameter = TargetUsername
							end,
							Action = continue;
						Nok when Nok =:= nok andalso RequestingUserId =:= undefined andalso TargetUsername =/= undefined ->
							Parameter = TargetUsername,
							Action = continue;
						_ ->
							Parameter = none,
							Action = stop
					end,
					case Action of
						continue ->
							case eb_db_util:execute({get_user, Parameter}) of
								#user{id=UserId, email=Email, version=Version} ->
									TokenParameters = [
										#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_ID, value=erlang:integer_to_binary(UserId)},
										#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_VERSION, value=erlang:integer_to_binary(Version)}
									],
									case eb_db_util:execute({create_token, UserId, ?DB_TOKEN_TYPE_RESET_PASSWORD, TokenParameters}) of
										{ok, TokenId} ->
											% Reset password notification
											eb_notification:reset_password(Email, TokenId, PasswordRedirectUrl),
											ok;
										_ -> {nok, error}
									end;
								not_found -> {nok, not_found};
								_ -> {nok, error}
							end;
						stop -> {nok, forbidden}
					end;
				_Other -> {nok, missing_values}
			end;
		Other ->
			error_logger:error_msg("~p:reset_password(...): Unable to get parameter ?DB_PARAM_PASSWORD_REDIRECT_URL (~p): ~p~n", [?MODULE, ?DB_PARAM_PASSWORD_REDIRECT_URL, Other]),
			{nok, missing_db_parameter}
	end;
reset_password(_RequestingUserId, _RequestingUserTypeId, _ResetPassword) -> {nok, invalid_parameters}.

%
% Change the password after reset password
%
set_password(NewPassword, TokenId) when is_binary(NewPassword) andalso NewPassword =/= <<>> andalso is_binary(TokenId) andalso TokenId =/= <<>> ->
	case eb_db_util:execute({get_token_info, TokenId}) of
		{ok, #token{id_type=?DB_TOKEN_TYPE_RESET_PASSWORD}, [#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_ID, value=BinUserId},
		                                                     #token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_VERSION, value=BinVersion}]} ->
			case eb_util:binary_to_int(BinUserId) of
				{ok, UserId} ->
					case eb_util:binary_to_int(BinVersion) of
						{ok, Version} ->
							case eb_db_util:execute({set_password, UserId, Version, NewPassword}) of
								ok -> ok;
								_ -> {nok, error}
							end;
						error -> {nok, error}
					end;
				error -> {nok, error}
			end;
		{ok, #token{id_type=?DB_TOKEN_TYPE_CREATE_USER}, [#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_ID, value=BinUserId}]} ->
			case eb_util:binary_to_int(BinUserId) of
				{ok, UserId} ->
					case eb_db_util:execute({set_password_and_activate_user, UserId, 1, NewPassword}) of
						ok -> ok;
						_ -> {nok, error}
					end;
				error -> {nok, error}
			end;
		{ok, #token{id_type=TokenTypeId}, _} when TokenTypeId =/= ?DB_TOKEN_TYPE_RESET_PASSWORD
		                                        , TokenTypeId =/= ?DB_TOKEN_TYPE_CREATE_USER ->
			{nok, wrong_type};
		{nok, invalid} -> {nok, invalid};
		not_found -> {nok, not_found_token};
		_Other -> {nok, error}
	end;
set_password(_NewPassword, _TokenId) -> {nok, invalid_parameters}.

%
% Get all user types
%
get_user_types() ->
	case eb_db_util:execute({get_user_types}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all user statuses
%
get_user_statuses() ->
	case eb_db_util:execute({get_user_statuses}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Create courier transport
create_courier_transport(RequestingUserId, RequestingUserTypeId, TargetUserId, NewCourierTransport)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) ->
	case sanitize(NewCourierTransport) of
		{ok, SanitizedNewCourierTransport=#new_courier_transport{transport_type_id=TransportTypeId, version=Version}} ->
			case eb_api_util:verify_user_or_account_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				ok ->
					% Verify the existence of the user
					case eb_db_util:execute({get_user, TargetUserId}) of
						#user{version=Version} ->
							% Verify the existence of the transport_type
							case eb_db_util:execute({exists_transport_type, TransportTypeId}) of
								true ->
									% Create the courier transport
									case eb_db_util:execute({create_courier_transport, TargetUserId, SanitizedNewCourierTransport, Version}) of
										{ok, NewCourierTransportTypeId, NewVersion} -> {ok, NewCourierTransportTypeId, NewVersion};
										_ -> {nok, error}
									end;
								false -> {nok, unknown_transport_type_id};
								_ -> {nok, invalid_transport_type_id}
							end;
						User when is_record(User, user) -> {nok, version};
						not_found -> {nok, not_found_user};
						_ -> {nok, error}
					end;
				nok -> {nok, forbidden}
			end;
		nok -> {nok, missing_values}
	end;
create_courier_transport(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _NewCourierTransport) -> {nok, invalid_parameters}.

%
% Remove courier transport
%
remove_courier_transport(RequestingUserId, RequestingUserTypeId, TargetUserId, TransportId, Version)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) andalso is_integer(TransportId)
 andalso is_integer(Version) ->
	case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok ->
			% Verify the existence of the user
			case eb_db_util:execute({get_user, TargetUserId}) of
				#user{version=Version} ->
					%Get courier transport
					case eb_db_util:execute({get_courier_transport, TransportId}) of
						#courier_transport{id_status=?DB_COURIER_TRANSPORT_STATUS_ACTIVE} ->
							% At the moment, put the courier transport_type inactive
							case eb_db_util:execute({update_courier_transport, TargetUserId, ?DB_COURIER_TRANSPORT_STATUS_INACTIVE, TransportId, Version}) of
								{ok, NewVersion} -> {ok, NewVersion};
								_ -> {nok, error}
							end;
						#courier_transport{id_status=StatusId} when StatusId =/= ?DB_COURIER_TRANSPORT_STATUS_ACTIVE -> {nok, wrong_status};
						not_found -> {nok, not_found_transport};
						_ -> {nok, error}
					end;
				User when is_record(User, user) -> {nok, version};
				not_found -> {nok, not_found_user};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
remove_courier_transport(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _TransportId, _Version) -> {nok, invalid_parameters}.

%
% Update courier transport
%
change_current_courier_transport(RequestingUserId, RequestingUserTypeId, TargetUserId, NewTransportId, Version)
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId)
  andalso is_integer(Version) ->
	try
		case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
			ok -> ok;
			nok -> throw(forbidden)
		end,
		% Verify the existence of the user
		case eb_db_util:execute({get_user, TargetUserId}) of
			#user{version=Version} -> ok;
			User when is_record(User, user) -> throw(version);
			not_found -> throw(not_found_user);
			_ -> throw(error)
		end,
		%Check if new transport is active.
		case eb_db_util:execute({get_courier_transport, NewTransportId}) of
			#courier_transport{id_status=StatusId} when StatusId /=?DB_COURIER_TRANSPORT_STATUS_ACTIVE -> throw(wrong_status);
			#courier_transport{id_status=?DB_COURIER_TRANSPORT_STATUS_ACTIVE} -> ok;
			not_found -> throw(not_found_transport);
			_ -> throw(error)
		end,
		FOldTransportId =
			case eb_db_util:execute({get_current_courier_transport, TargetUserId}) of
				#courier_transport{id=OldTransportId, id_status=?DB_COURIER_TRANSPORT_STATUS_ACTIVE} -> OldTransportId;
				OldCourierTransport when is_record(OldCourierTransport, courier_transport) -> throw(wrong_status);
				not_found -> throw(not_found_transport);
				_ -> throw(error)
			end,
		case eb_db_util:execute({update_current_courier_transport, FOldTransportId, NewTransportId, TargetUserId, Version}) of
			{ok, NewVersion} -> {ok, NewVersion};
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
change_current_courier_transport( _RequestingUserId, _RequestingUserTypeId, _TargetUserId, _TransportId, _Version) -> {nok, invalid_parameters}.

%
% Change a courier transport
%
change_courier_transport(RequestingUserId, RequestingUserTypeId, TargetUserId, TransportId, ChangeUserTransport)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) andalso is_integer(TransportId) ->
	try
		case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
			ok -> ok;
			nok -> throw(forbidden)
		end,
		#change_courier_transport{transport_status_id=TransportStatusId, version=Version} =
			case sanitize(ChangeUserTransport) of
				{ok, SanitizedRecord} when is_record(SanitizedRecord, change_courier_transport) -> SanitizedRecord;
				_Other -> throw(missing_values)
			end,
		case eb_db_util:execute({exists_courier_transport_status, TransportStatusId}) of
			true -> ok;
			false -> throw(unknown_transport_status_id);
			_ -> throw(invalid_transport_status_id)
		end,
		% Verify the existence of the user
		case eb_db_util:execute({get_user, TargetUserId}) of
			#user{version=Version} -> ok;
			User when is_record(User, user) -> throw(version);
			not_found -> throw(not_found_user);
			_ -> throw(error)
		end,
		%Check if transport exists
		case eb_db_util:execute({get_courier_transport, TransportId}) of
			#courier_transport{current=?DB_COURIER_TRANSPORT_CURRENT_YES} -> throw(current_transport);
			CourierTransport when is_record(CourierTransport, courier_transport) -> ok;
			not_found -> throw(not_found_transport);
			_ -> throw(error)
		end,
		% Update the courier transport
		case eb_db_util:execute({update_courier_transport, TargetUserId, TransportStatusId, TransportId, Version}) of
			{ok, NewVersion} -> {ok, NewVersion};
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
change_courier_transport(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _TransportId, _ChangeCourierTransport) -> {nok, invalid_parameters}.

%
% Get all transport types
%
get_transport_types() ->
	case eb_db_util:execute({get_transport_types}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all transport type statuses
%
get_transport_type_statuses() ->
	case eb_db_util:execute({get_courier_transport_type_status}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get user transports
%
get_user_transports(RequestingUserId, RequestingUserTypeId, TargetUserId) when is_integer(RequestingUserId)
                                                                       andalso is_integer(RequestingUserTypeId)
                                                                       andalso is_integer(TargetUserId) ->
	case eb_api_util:verify_user_or_account_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok -> Action = {continue, get_courier_transports};
		nok ->
			case can_access_others_data(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				true -> Action = {continue, get_active_courier_transports};
				false -> Action = {stop, forbidden};
				_ -> Action = {stop, error}
			end
	end,
	case Action of
		{stop, Reason} -> {nok, Reason};
		{continue, Query} ->
			case eb_db_util:execute({Query, TargetUserId}) of
				Results when is_list(Results) -> {ok, Results};
				_ -> {nok, error}
			end
	end;
get_user_transports(_RequestingUserId, _RequestingUserTypeId, _TargetUserId) -> {nok, invalid_parameters}.

%
% Get user photo
%
get_user_photo(RequestingUserId, RequestingUserTypeId, TargetUserId) when is_integer(RequestingUserId)
                                                                  andalso is_integer(RequestingUserTypeId)
                                                                  andalso is_integer(TargetUserId) ->
	case verify_data_access(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok ->
			case eb_db_util:execute({get_user_photo, TargetUserId}) of
				#document{name=Name, mimetype=Mimetype, base64_data=Base64Data} ->
					Photo = #file{name=Name, mimetype=Mimetype, base64_data=Base64Data},
					{ok, Photo};
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		Other -> Other
	end;
get_user_photo(_RequestingUserId, _RequestingUserTypeId, _TargetUserId) -> {nok, invalid_parameters}.

%
% Get user documents
%
get_user_documents(RequestingUserId, RequestingUserTypeId, TargetUserId, DocumentTypeId, DocumentStatusId, Navigation)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) ->
	case eb_api_util:is_optional_integer(DocumentTypeId) of
		true ->
			case eb_api_util:is_optional_integer(DocumentStatusId) of
				true ->
					case eb_api_util:sanitize_navigation(Navigation, [?ORDER_USER_ID, ?ORDER_USER_TYPE_ID, ?ORDER_USER_STATUS_ID]) of
						{ok, FNavigation} ->
							case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
								ok ->
									case eb_db_util:execute({get_user_documents_summary, TargetUserId, DocumentTypeId, DocumentStatusId, FNavigation}) of
										Results when is_list(Results) -> {ok, Results};
										_ -> {nok, error}
									end;
								nok -> {nok, forbidden}
							end;
						_ -> {nok, invalid_parameters}
					end;
				_ -> {nok, invalid_parameters}
			end;
		_ -> {nok, invalid_parameters}
	end;
get_user_documents(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _DocumentTypeId, _DocumentStatusId, _Navigation) ->
	{nok, invalid_parameters}.

%
% Get a user document
%
get_user_document(RequestingUserId, RequestingUserTypeId, TargetUserId, DocumentId) when is_integer(RequestingUserId)
                                                                                 andalso is_integer(RequestingUserTypeId)
                                                                                 andalso is_integer(TargetUserId)
                                                                                 andalso is_integer(DocumentId) ->
	case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok ->
			case eb_db_util:execute({get_user_document, TargetUserId, DocumentId}) of
				Result when is_record(Result, document) -> {ok, Result};
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
get_user_document(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _DocumentId) -> {nok, invalid_parameters}.

%
% Create a user document
%
create_user_document(RequestingUserId, RequestingUserTypeId, TargetUserId, NewDocument)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) ->
	case sanitize(NewDocument) of
		{ok, SanitizedNewDocument=#new_document{document_type_id=DocumentTypeId}} ->
			case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				ok ->
					% Verify the existence of the document type
					case eb_db_util:execute({exists_document_type, DocumentTypeId}) of
						true ->
							% Create the document
							case eb_db_util:execute({create_user_document, TargetUserId, SanitizedNewDocument}) of
								{ok, NewDocumentId} -> {ok, NewDocumentId};
								_ -> {nok, error}
							end;
						false -> {nok, unknown_document_type_id};
						_ -> {nok, invalid_document_type_id}
					end;
				nok -> {nok, forbidden}
			end;
		_Other -> {nok, missing_values}
	end;
create_user_document(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _NewDocument) -> {nok, invalid_parameters}.

%
% Change a user document
%
change_user_document(RequestingUserId, RequestingUserTypeId, TargetUserId, DocumentId, ChangeDocument)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) andalso is_integer(DocumentId) ->
	case sanitize(ChangeDocument) of
		{ok, SanitizedChangeDocument=#change_document{document_status_id=DocumentStatusId, version=Version}} ->
			case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				ok ->
					% Verify the existence of the document type
					case eb_db_util:execute({exists_document_status, DocumentStatusId}) of
						true ->
							% Validate document's version
							case eb_db_util:execute({get_user_document_summary, TargetUserId, DocumentId}) of
								#document{version=Version} ->
									% Update the document
									case eb_db_util:execute({change_user_document, TargetUserId, DocumentId, SanitizedChangeDocument}) of
										{ok, NewVersion} -> {ok, NewVersion};
										_ -> {nok, error}
									end;
								Document when is_record(Document, document) -> {nok, wrong_version};
								not_found -> {nok, not_found};
								_ -> {nok, error}
							end;
						false -> {nok, unknown_document_type_id};
						_ -> {nok, invalid_document_type_id}
					end;
				nok -> {nok, forbidden}
			end;
		_Other -> {nok, missing_values}
	end;
change_user_document(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _DocumentId, _ChangeDocument) -> {nok, invalid_parameters}.

%
% Get user ranking
%
get_user_ranking(RequestingUserId, RequestingUserTypeId, TargetUserId) when is_integer(RequestingUserId)
                                                                    andalso is_integer(RequestingUserTypeId)
                                                                    andalso is_integer(TargetUserId) ->
	case verify_data_access(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok ->
			case eb_db_util:execute({get_user_ranking, TargetUserId}) of
				Ranking when is_float(Ranking) -> {ok, Ranking};
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		Other -> Other
	end;
get_user_ranking(_RequestingUserId, _RequestingUserTypeId, _TargetUserId) -> {nok, invalid_parameters}.

%
% Get all user couriers
%
get_user_couriers(RequestingUserId, RequestingUserTypeId, TargetUserId, Navigation)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) ->
	case eb_api_util:sanitize_navigation(Navigation, [?ORDER_NAME]) of
		{ok, FNavigation} ->
			case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				ok ->
					case eb_db_util:execute({get_user_couriers, TargetUserId, FNavigation}) of
						Results when is_list(Results) -> {ok, Results};
						_ -> {nok, error}
					end;
				nok -> {nok, forbidden}
			end;
		_ -> {nok, invalid_parameters}
	end;
get_user_couriers(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _Navigation) -> {nok, invalid_parameters}.

%
% Add a courier to the user's list
%
add_user_courier(RequestingUserId, RequestingUserTypeId, TargetUserId, CourierId, Version)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) andalso is_integer(CourierId)
 andalso is_integer(Version) ->
	case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok ->
			% Verify the existence of the user
			case eb_db_util:execute({get_user, TargetUserId}) of
				#user{version=Version} ->
					% Verify the existence of the courier
					case eb_db_util:execute({get_user, CourierId}) of
						Courier when is_record(Courier, user) ->
							% Check if courier is already in the list
							case eb_db_util:execute({get_user_courier, TargetUserId, CourierId}) of
								not_found ->
									% Add the courier to the list
									case eb_db_util:execute({create_user_courier, TargetUserId, CourierId, Version}) of
										{ok, NewVersion} -> {ok, NewVersion};
										_ -> {nok, error}
									end;
								UserCourier when is_record(UserCourier, user_courier) -> {nok, courier_exists};
								_ -> {nok, error}
							end;
						not_found -> {nok, not_found_courier};
						_ -> {nok, error}
					end;
				User when is_record(User, user) -> {nok, version};
				not_found -> {nok, not_found_user};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
add_user_courier(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _CourierId, _Version) -> {nok, invalid_parameters}.

%
% Delete a courier from the user's list
%
remove_user_courier(RequestingUserId, RequestingUserTypeId, TargetUserId, CourierId, Version)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) andalso is_integer(CourierId)
 andalso is_integer(Version) ->
	case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok ->
			% Verify the existence of the user
			case eb_db_util:execute({get_user, TargetUserId}) of
				#user{version=Version} ->
					% Remove the courier from the list
					case eb_db_util:execute({delete_user_courier, TargetUserId, CourierId, Version}) of
						{ok, NewVersion} -> {ok, NewVersion};
						_ -> {nok, error}
					end;
				User when is_record(User, user) -> {nok, version};
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
remove_user_courier(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _CourierId, _Version) -> {nok, invalid_parameters}.

%
% Get all user used couriers
%
get_user_used_couriers(RequestingUserId, RequestingUserTypeId, TargetUserId, DateFrom, DateTo, Navigation)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) ->
	case eb_util:verify_dates(DateFrom, DateTo) of
		ok ->
			case eb_api_util:sanitize_navigation(Navigation, [?ORDER_NAME]) of
				{ok, FNavigation} ->
					case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
						ok ->
							case eb_db_util:execute({get_user_used_couriers, TargetUserId, DateFrom, DateTo, FNavigation}) of
								Results when is_list(Results) -> {ok, Results};
								_ -> {nok, error}
							end;
						nok -> {nok, forbidden}
					end;
				_ -> {nok, invalid_parameters}
			end;
		_ -> {nok, invalid_parameters}
	end;
get_user_used_couriers(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _DateFrom, _DateTo, _Navigation) -> {nok, invalid_parameters}.

%
% Get all document types
%
get_document_types() ->
	case eb_db_util:execute({get_document_types}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all document statuses
%
get_document_statuses() ->
	case eb_db_util:execute({get_document_statuses}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all user notification types
%
get_user_notification_types() ->
	case eb_db_util:execute({get_user_notification_types}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all mobile operating systems
%
get_mobileos() ->
	case eb_db_util:execute({get_mobileos}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all user locations
%
get_user_locations(RequestingUserId, RequestingUserTypeId, TargetUserId, Name, PhoneNr, Navigation)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) ->
	case eb_api_util:sanitize_navigation(Navigation, []) of
		{ok, FNavigation} ->
			case eb_api_util:trim_optional(Name) of
				{ok, FName}  ->
					case eb_api_util:trim_optional(PhoneNr) of
						{ok, FPhoneNr} ->
							case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
								ok ->
									case eb_db_util:execute({get_user_locations_information, TargetUserId, FName, FPhoneNr, FNavigation}) of
										Results when is_list(Results) -> {ok, Results};
										_ -> {nok, error}
									end;
								nok -> {nok, forbidden}
							end;
						_ -> {nok, invalid_parameters}
					end;
			_ -> {nok, invalid_parameters}
			end;
		_ -> {nok, invalid_parameters}
	end;
get_user_locations(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _Name, _PhoneNr, _Navigation) -> {nok, invalid_parameters}.

%
% Add a location to the user's list
%
add_user_location(RequestingUserId, RequestingUserTypeId, TargetUserId, NewLocation)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId)  ->
	case sanitize(NewLocation) of
		{ok, SanitizedNewLocation=#new_location{version=Version}} ->
			case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				ok ->
					% Verify the existence of the user
					case eb_db_util:execute({get_user, TargetUserId}) of
						#user{version=Version} ->
							% Add the new location to the list
							case eb_db_util:execute({create_user_location, TargetUserId, SanitizedNewLocation}) of
								{ok, NewLocationId, NewVersion} -> {ok, NewLocationId, NewVersion};
								_ -> {nok, error}
							end;
						User when is_record(User, user) -> {nok, version};
						not_found -> {nok, not_found_user};
						_ -> {nok, error}
					end;
				nok -> {nok, forbidden}
			end;
		_Other -> {nok, missing_values}
	end;
add_user_location(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _NewLocation) -> {nok, invalid_parameters}.

%
% Delete a location from the user's list
%
remove_user_location(RequestingUserId, RequestingUserTypeId, TargetUserId, LocationId, Version)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) andalso is_integer(LocationId)
 andalso is_integer(Version) ->
	case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok ->
			% Verify the existence of the user
			case eb_db_util:execute({get_user, TargetUserId}) of
				#user{version=Version} ->
					% Remove the location from the list
					case eb_db_util:execute({delete_user_location, TargetUserId, LocationId, Version}) of
						{ok, NewVersion} -> {ok, NewVersion};
						_ -> {nok, error}
					end;
				User when is_record(User, user) -> {nok, version};
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
remove_user_location(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _LocationId, _Version) -> {nok, invalid_parameters}.

%
% Change a user's location
%
change_user_location(RequestingUserId, RequestingUserTypeId, TargetUserId, LocationId, ChangeLocation)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) andalso is_integer(LocationId) ->
	case sanitize(ChangeLocation) of
		{ok, SanitizedChangeLocation=#change_location{version=Version}} ->
			case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				ok ->
					% Verify the existence of the user
					case eb_db_util:execute({get_user, TargetUserId}) of
						#user{version=Version} ->
							% Verify the existence of the location
							case eb_db_util:execute({get_user_location, TargetUserId, LocationId}) of
								OldLocation when is_record(OldLocation, user_location) ->
									% Update the location
									case eb_db_util:execute({change_user_location, OldLocation, SanitizedChangeLocation}) of
										{ok, NewVersion} -> {ok, NewVersion};
										_ -> {nok, error}
									end;
								not_found -> {nok, not_found_location};
								_ -> {nok, error}
							end;
						User when is_record(User, user) -> {nok, version};
						not_found -> {nok, not_found_user};
						_ -> {nok, error}
					end;
				nok -> {nok, forbidden}
			end;
		_Other -> {nok, missing_values}
	end;
change_user_location(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _LocationId, _ChangeLocation) -> {nok, invalid_parameters}.

%
% Get a user's location
%
get_user_location(RequestingUserId, RequestingUserTypeId, TargetUserId, LocationId)
	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetUserId) andalso is_integer(LocationId) ->
	case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok ->
			case eb_db_util:execute({get_user_location_information, TargetUserId, LocationId}) of
				Result when is_record(Result, inf_location) -> {ok, Result};
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
get_user_location(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _LocationId) -> {nok, invalid_parameters}.

%
% Get user's notifications
%
get_user_notifications(RequestingUserId, RequestingUserTypeId, TargetUserId) 
	when is_integer(RequestingUserId), is_integer(RequestingUserTypeId), is_integer(TargetUserId) ->
	case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok ->
			case eb_db_util:execute({get_user, TargetUserId}) of
				User when is_record(User, user) ->
					case eb_db_util:execute({get_user_notifications, TargetUserId}) of
						Result when is_record(Result, inf_user_notifications) -> {ok, Result};
						_ -> {nok, error}
					end;
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
get_user_notifications(_RequestingUserId, _RequestingUserTypeId, _TargetUserId) -> {nok, invalid_parameters}.

%
% Set user's notifications
%
set_user_notifications(RequestingUserId, RequestingUserTypeId, TargetUserId, UpdateUserNotifications)
	when is_integer(RequestingUserId), is_integer(RequestingUserTypeId), is_integer(TargetUserId)
	   , is_record(UpdateUserNotifications, update_user_notifications) ->
	case sanitize(UpdateUserNotifications) of
		{ok, SanitizedUpdateUserNotifications=#update_user_notifications{notification_type_ids=NotificationTypeIds, version=Version}} ->
			case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				ok ->
					% Verify the existence of the user
					case eb_db_util:execute({get_user, TargetUserId}) of
						#user{version=Version} ->
							% Verify the existence of the notification types
							case verify_user_notification_types(NotificationTypeIds) of
								ok ->
									case eb_db_util:execute({set_user_notifications, TargetUserId, SanitizedUpdateUserNotifications}) of
										{ok, NewVersion} -> {ok, NewVersion};
										_ -> {nok, error}
									end;
								nok -> {nok, invalid_user_notification_type}
							end;
						User when is_record(User, user) -> {nok, version};
						not_found -> {nok, not_found};
						_ -> {nok, error}
					end;
				nok -> {nok, forbidden}
			end;
		_Other -> {nok, missing_values}
	end;
set_user_notifications(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _UpdateUserNotifications) -> {nok, invalid_parameters}.

%
% Get user's commercial information
%
get_commercial_info(RequestingUserId, RequestingUserTypeId, TargetUserId)
  when is_integer(RequestingUserId), is_integer(RequestingUserTypeId), is_integer(TargetUserId) ->
	try
		case eb_api_util:verify_user_or_account_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
			ok -> ok;
			nok -> throw(forbidden)
		end,
		case eb_db_util:execute({get_user, TargetUserId}) of
			User when is_record(User, user) -> ok;
			not_found -> throw(not_found);
			_ -> throw(error)
		end,
		case eb_db_util:execute({get_commercial_info, TargetUserId}) of
			Result when is_record(Result, commercial_info) -> {ok, Result};
			not_found -> {ok, #commercial_info{id_user=TargetUserId, about=null}};
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
get_commercial_info(_RequestingUserId, _RequestingUserTypeId, _TargetUserId) -> {nok, invalid_parameters}.

%
% Set user's commercial information
%
change_commercial_info(RequestingUserId, RequestingUserTypeId, TargetUserId, ChangeCommercialInfo)
  when is_integer(RequestingUserId), is_integer(RequestingUserTypeId), is_integer(TargetUserId)
     , is_record(ChangeCommercialInfo, change_commercial_info) ->
	try
		SanitizedChangeCommercialInfo = #change_commercial_info{version=Version} =
			case sanitize(ChangeCommercialInfo) of
				{ok, SanitizedRecord} -> SanitizedRecord;
				_ -> throw(missing_values)
			end,
		case eb_api_util:verify_user_or_account_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
			ok -> ok;
			nok -> throw(forbidden)
		end,
		% Verify the existence of the user
		case eb_db_util:execute({get_user, TargetUserId}) of
			#user{version=Version} -> ok;
			User when is_record(User, user) -> throw(version);
			not_found -> throw(not_found);
			_ -> throw(error)
		end,
		Query =
			case eb_db_util:execute({get_commercial_info, TargetUserId}) of
				Result when is_record(Result, commercial_info) -> change_commercial_info;
				not_found -> create_commercial_info;
				_ -> throw(error)
			end,
		case eb_db_util:execute({Query, TargetUserId, SanitizedChangeCommercialInfo}) of
			{ok, NewVersion} -> {ok, NewVersion};
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
change_commercial_info(_RequestingUserId, _RequestingUserTypeId, _TargetUserId, _ChangeCommercialInfo) -> {nok, invalid_parameters}.

%
% Get all zones
%
get_zones() ->
	case eb_db_util:execute({get_zones}) of
		Results when is_list(Results) ->{ok, Results};
		_ -> {nok, error}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
validate_account_parameters(NewAccount, #new_user{user_type_id=UserTypeId}) when is_record(NewAccount, new_account)
                                                                               , not ?IS_BUSINESS_CLIENT(UserTypeId) ->
	{nok, wrong_user_type};
validate_account_parameters(NewAccount, NewUser=#new_user{user_type_id=UserTypeId}) ->
	% Sanitize account related parameters. If AccountId or NewAccount is present, the user has to be a business user.
	case NewAccount of
		#new_account{unique_key=UniqueKey} ->
			% If new account info is present, the new user will allways be the new account administrator.
			% We will also validate if the unique key is already registered
			case eb_db_util:execute({exists_account, UniqueKey}) of
				false ->
					SanitizedNewUser = NewUser#new_user{account_user_type_id=?DB_ACCOUNT_USER_TYPE_ADMINISTRATOR},
					Action = {continue_account, SanitizedNewUser};
				true -> Action = {nok, unique_key_exists};
				_ -> Action = {nok, invalid_unique_key}
			end;
		_ ->
			% No NewAccount. Clear account_user_type_id
			case UserTypeId of
				?DB_USER_TYPE_CLIENT_BUSINESS ->
					Action = {nok, missing_account_information};
				_ ->
					SanitizedNewUser = NewUser#new_user{account_user_type_id=undefined},
					Action = {continue_no_account, SanitizedNewUser}
			end
	end,
	% If this is a business user, validate the account_user_type_id
	case Action of
		{continue_account, SanitizedUser}  ->
			% Verify the existence of the account user type
			case SanitizedUser#new_user.account_user_type_id of
				undefined -> {nok, missing_account_user_type_id};
				AccountUserTypeId ->
					case eb_db_util:execute({exists_account_user_type, AccountUserTypeId}) of
						true -> {ok, SanitizedUser};
						false -> {nok, unknown_account_user_type_id};
						_ -> {nok, invalid_account_user_type_id}
					end
			end;
		{continue_no_account, SanitizedUser}  -> {ok, SanitizedUser};
		_ -> Action
	end.

verify_data_access(RequestingUserId, RequestingUserTypeId, TargetUserId) ->
	case eb_api_util:verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) of
		ok -> ok;
		nok ->
			case eb_db_util:execute({verify_account_admin, RequestingUserId, TargetUserId}) of
				true -> ok;
				false ->
					case can_access_others_data(RequestingUserId, RequestingUserTypeId, TargetUserId) of
						true -> ok;
						false -> {nok, forbidden};
						_ -> {nok, error}
					end
			end
	end.

verify_user_creation_permission(_RequestingUserId, _RequestingUserTypeId, UserTypeId) when ?IS_PRIVATE_CLIENT(UserTypeId) -> ok;
verify_user_creation_permission(undefined, _RequestingUserTypeId, _UserTypeId) -> nok;
verify_user_creation_permission(_RequestingUserId, RequestingUserTypeId, _UserTypeId) ->
	case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
		ok -> ok;
		_ -> nok
	end.

verify_user_notification_types([]) -> ok;
verify_user_notification_types([NotificationTypeId|Rest]) ->
	case eb_db_util:execute({exists_user_notification_type, NotificationTypeId}) of
		true -> verify_user_notification_types(Rest);
		_ -> nok
	end.

can_access_others_data(ClientUserId, UserTypeId, CourierUserId) when ?IS_CLIENT(UserTypeId) ->
	% Verify if TargetUserId is a courier from any delivery of the client
	eb_db_util:execute({exists_client_courier_relation, ClientUserId, CourierUserId});
can_access_others_data(CourierUserId, UserTypeId, ClientUserId) when ?IS_COURIER(UserTypeId) ->
	% Verify if TargetUserId is a client from any delivery of the courier
	eb_db_util:execute({exists_client_courier_relation, ClientUserId, CourierUserId});
can_access_others_data(_CourierUserId, _UserTypeId, _ClientUserId) -> false.

filter_user_information(UserId, _UserTypeId, UserId, User) -> User;
filter_user_information(_RequestingUserId, RequestingUserTypeId, _TargetUserId, User) when ?IS_OPERATOR(RequestingUserTypeId) -> User;
filter_user_information(_RequestingUserId, _RequestingUserTypeId, _TargetUserId,
                        #user{id=UserId, id_type=UserTypeId, first_name=FirstName, last_name=LastName, email=Email, telephone_nr=TelephoneNr,
                              creation_date=CreationDate}) ->
	#user{id=UserId, id_type=UserTypeId, username=null, id_status=null, creation_date=CreationDate, status_date=null, email=Email,
	      first_name=FirstName, last_name=LastName, telephone_nr=TelephoneNr, fiscal_id=null, version=null}.

sanitize(#new_user{user_type_id=UserTypeId}) when not is_integer(UserTypeId) -> nok;
sanitize(Record = #new_user{username=Username, password=Password, email=Email, user_type_id=UserTypeId,
							first_name=FirstName, last_name=LastName, telephone_nr=TelephoneNr,
							fiscal_id=FiscalId, reference=Reference, mobileos_id=MobileOsId, birth_day=BirthDay,
							birth_month=BirthMonth, birth_year=BirthYear, national_id=NationalId, country=Country,
							account_user_type_id=AccountUserTypeId, new_account=NewAccount}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FUsername} = eb_api_util:trim_mandatory(Username, TrimPattern),
		{ok, FPassword} = eb_api_util:trim_optional(Password, TrimPattern),
		{ok, FEmail} = eb_api_util:trim_mandatory(Email, TrimPattern),
		{ok, FFirstName} = eb_api_util:trim_optional(FirstName, TrimPattern),
		{ok, FLastName} = eb_api_util:trim_optional(LastName, TrimPattern),
		{ok, FTelephoneNr} = eb_api_util:trim_optional(TelephoneNr, TrimPattern),
		{ok, FFiscalId} = eb_api_util:trim_optional(FiscalId, TrimPattern),
		{ok, FReference} = eb_api_util:trim_optional(Reference, TrimPattern),
		{ok, FNationalId} = eb_api_util:trim_optional(NationalId, TrimPattern),
		{ok, FCountry} = eb_api_util:trim_optional(Country, TrimPattern),
		% Size validations
		ok = eb_util:validate_size(FUsername, ?DB_FIELD_SIZE__USER_AUTH__USERNAME),
		ok = eb_api_util:validate_size_optional(FPassword, ?DB_FIELD_SIZE__USER_AUTH__PASSWORD),
		ok = eb_util:validate_size(FEmail, ?DB_FIELD_SIZE__USER__EMAIL),
		ok = eb_api_util:validate_size_optional(FFirstName, ?DB_FIELD_SIZE__USER__FIRST_NAME),
		ok = eb_api_util:validate_size_optional(FLastName, ?DB_FIELD_SIZE__USER__LAST_NAME),
		ok = eb_api_util:validate_size_optional(FTelephoneNr, ?DB_FIELD_SIZE__USER__TELEPHONE_NR),
		ok = eb_api_util:validate_size_optional(FFiscalId, ?DB_FIELD_SIZE__USER__FISCAL_ID),
		ok = eb_api_util:validate_size_optional(FReference, ?DB_FIELD_SIZE__USER__REFERENCE),
		ok = eb_api_util:validate_size_optional(FNationalId, ?DB_FIELD_SIZE__USER__NATIONAL_ID),
		ok = eb_api_util:validate_size_optional(FCountry, ?DB_FIELD_SIZE__USER__COUNTRY),
		% Extra validations
		true = eb_api_util:is_optional_integer(MobileOsId),
		true = eb_api_util:is_optional_integer(BirthDay),
		true = eb_api_util:is_optional_integer(BirthMonth),
		true = eb_api_util:is_optional_integer(BirthYear),
		true = eb_util:is_valid_email(FEmail),
		% Validate new_account
		case NewAccount of
			#new_account{unique_key=AUniqueKey, name=AName, fiscal_id=AFiscalId, address=AAddress, email=AEmail,
			             telephone_nr=ATelephoneNr, contract_nr=AContractNr, additional_infos=AAdditionalInfos} ->
				% Trim binary strings
				{ok, FAUniqueKey} = eb_api_util:trim_mandatory(AUniqueKey, TrimPattern),
				{ok, FAName} = eb_api_util:trim_mandatory(AName, TrimPattern),
				{ok, FAFiscalId} = eb_api_util:trim_optional(AFiscalId, TrimPattern),
				{ok, FAEmail} = eb_api_util:trim_optional(AEmail, TrimPattern),
				{ok, FATelephoneNr} = eb_api_util:trim_optional(ATelephoneNr, TrimPattern),
				{ok, FAContractNr} = eb_api_util:trim_optional(AContractNr, TrimPattern),
				% Size validations
				ok = eb_util:validate_size(FAUniqueKey, ?DB_FIELD_SIZE__ACCOUNT__UNIQUE_KEY),
				ok = eb_util:validate_size(FAName, ?DB_FIELD_SIZE__ACCOUNT__ACCOUNT_NAME),
				ok = eb_api_util:validate_size_optional(FAFiscalId, ?DB_FIELD_SIZE__ACCOUNT__FISCAL_ID),
				ok = eb_api_util:validate_size_optional(FATelephoneNr, ?DB_FIELD_SIZE__ACCOUNT__TELEPHONE_NR),
				ok = eb_api_util:validate_size_optional(FAEmail, ?DB_FIELD_SIZE__ACCOUNT__EMAIL),
				ok = eb_api_util:validate_size_optional(FAContractNr, ?DB_FIELD_SIZE__ACCOUNT__CONTRACT_NR),
				% Extra validations
				true = eb_util:is_valid_optional_email(FAEmail),
				% Validate address
				{ok, FAAddress} = eb_api_util:sanitize_address_components(AAddress, TrimPattern),
				{ok, FAAdditionalInfos} = eb_api_util:sanitize_additional_infos(AAdditionalInfos, TrimPattern),
				% Build record
				FNewAccount = #new_account{unique_key=FAUniqueKey, name=FAName, fiscal_id=FAFiscalId, address=FAAddress, email=FAEmail,
				                           telephone_nr=FATelephoneNr, contract_nr=FAContractNr, additional_infos=FAAdditionalInfos};
			_ -> FNewAccount = undefined
		end,
		% Build the return record
		SanitizedRecord = Record#new_user{username=FUsername, password=FPassword, email=FEmail,
										  first_name=FFirstName, last_name=FLastName, telephone_nr=FTelephoneNr,
										  fiscal_id=FFiscalId, reference=FReference, new_account=FNewAccount},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(Record = #new_user_external{token=Token, email=Email, first_name=FirstName, last_name=LastName,
									 telephone_nr=TelephoneNr, fiscal_id=FiscalId, address=Address}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),

		% Trim binary strings
		{ok, FToken} = eb_api_util:trim_mandatory(Token, TrimPattern),
		{ok, FEmail} = eb_api_util:trim_mandatory(Email, TrimPattern),
		{ok, FFirstName} = eb_api_util:trim_optional(FirstName, TrimPattern),
		{ok, FLastName} = eb_api_util:trim_optional(LastName, TrimPattern),
		{ok, FTelephoneNr} = eb_api_util:trim_optional(TelephoneNr, TrimPattern),
		{ok, FFiscalId} = eb_api_util:trim_optional(FiscalId, TrimPattern),

		% Size validations
		ok = eb_util:validate_size(FEmail, ?DB_FIELD_SIZE__USER__EMAIL),
		ok = eb_api_util:validate_size_optional(FFirstName, ?DB_FIELD_SIZE__USER__FIRST_NAME),
		ok = eb_api_util:validate_size_optional(FLastName, ?DB_FIELD_SIZE__USER__LAST_NAME),
		ok = eb_api_util:validate_size_optional(FTelephoneNr, ?DB_FIELD_SIZE__USER__TELEPHONE_NR),
		ok = eb_api_util:validate_size_optional(FFiscalId, ?DB_FIELD_SIZE__USER__FISCAL_ID),
		% Extra validations
		true = eb_util:is_valid_email(FEmail),
		% Validate address
		{ok, FAddress} = sanitize_address(Address, TrimPattern),
		% Build the return record
		SanitizedRecord = Record#new_user_external{token=FToken, email=FEmail, first_name=FFirstName, last_name=FLastName,
																 telephone_nr=FTelephoneNr, fiscal_id=FFiscalId, address=FAddress},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#change_password{version=Version}) when not is_integer(Version) -> nok;
sanitize(#change_password{old_password=OldPassword, new_password=NewPassword, version=Version}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FOldPassword} = eb_api_util:trim_mandatory(OldPassword, TrimPattern),
		{ok, FNewPassword} = eb_api_util:trim_mandatory(NewPassword, TrimPattern),
		% Size validations
		ok = eb_util:validate_size(FNewPassword, ?DB_FIELD_SIZE__USER_AUTH__PASSWORD),
		% Build the return record
		SanitizedRecord = #change_password{old_password=FOldPassword, new_password=FNewPassword, version=Version},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#reset_password{user_id=UserId, username=Username}) ->
	try
		true = eb_api_util:is_optional_integer(UserId),
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FUsername} = eb_api_util:trim_optional(Username, TrimPattern),
		% Check if user_id or username is present
		ok = eb_api_util:check_one_of(UserId, FUsername),
		% Size validations
		ok = eb_api_util:validate_size_optional(FUsername, ?DB_FIELD_SIZE__USER_AUTH__USERNAME),
		% Build the return record
		SanitizedRecord = #reset_password{user_id=UserId, username=FUsername},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#change_user{version=Version}) when not is_integer(Version) -> nok;
sanitize(ChangeUser = #change_user{username=Username, user_status_id=UserStatusId, email=Email, first_name=FirstName, last_name=LastName,
                                   telephone_nr=TelephoneNr, fiscal_id=FiscalId, mobileos_id=MobileosId,
                                   birth_day=BirthDay, birth_month=BirthMonth, birth_year=BirthYear, national_id=NationalId,
                                   zone_ids=ZoneIds, address=Address}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FUsername} = eb_api_util:trim_optional(Username, TrimPattern),
		{ok, FEmail} = eb_api_util:trim_optional(Email, TrimPattern),
		{ok, FFirstName} = eb_api_util:trim_optional(FirstName, TrimPattern),
		{ok, FLastName} = eb_api_util:trim_optional(LastName, TrimPattern),
		{ok, FTelephoneNr} = eb_api_util:trim_optional(TelephoneNr, TrimPattern),
		{ok, FFiscalId} = eb_api_util:trim_optional(FiscalId, TrimPattern),
		{ok, FNationalId} = eb_api_util:trim_optional(NationalId, TrimPattern),
		% Size validations
		ok = eb_api_util:validate_size_optional(FUsername, ?DB_FIELD_SIZE__USER_AUTH__USERNAME),
		ok = eb_api_util:validate_size_optional(FEmail, ?DB_FIELD_SIZE__USER__EMAIL),
		ok = eb_api_util:validate_size_optional(FFirstName, ?DB_FIELD_SIZE__USER__FIRST_NAME),
		ok = eb_api_util:validate_size_optional(FLastName, ?DB_FIELD_SIZE__USER__LAST_NAME),
		ok = eb_api_util:validate_size_optional(FTelephoneNr, ?DB_FIELD_SIZE__USER__TELEPHONE_NR),
		ok = eb_api_util:validate_size_optional(FFiscalId, ?DB_FIELD_SIZE__USER__FISCAL_ID),
		ok = eb_api_util:validate_size_optional(FNationalId, ?DB_FIELD_SIZE__USER__NATIONAL_ID),
		% Sanitize lists
		{ok, FAddress} = sanitize_address(Address, TrimPattern),
		% Extra validations
		true = eb_api_util:is_optional_integer(UserStatusId),
		true = eb_api_util:is_optional_integer(MobileosId),
		true = eb_api_util:is_optional_integer(BirthDay),
		true = eb_api_util:is_optional_integer(BirthMonth),
		true = eb_api_util:is_optional_integer(BirthYear),
		case FEmail of
			undefined -> do_nothing;
			_ -> true = eb_util:is_valid_email(FEmail)
		end,
		case ZoneIds of
			undefined -> ok;
			_ -> true = lists:all(fun(ZoneId) -> eb_api_util:is_optional_integer(ZoneId) end, ZoneIds)
		end,
		% Build the return record
		SanitizedRecord = ChangeUser#change_user{username=FUsername,
		                                         email=FEmail,
		                                         first_name=FFirstName,
		                                         last_name=FLastName,
		                                         telephone_nr=FTelephoneNr,
		                                         fiscal_id=FFiscalId,
		                                         address=FAddress,
		                                         national_id=FNationalId},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#new_document{document_type_id=DocumentTypeId}) when not is_integer(DocumentTypeId) -> nok;
sanitize(#new_document{document_type_id=DocumentTypeId, document=Document}) ->
	try
		% Other records
		{ok, FDocument} = eb_api_util:sanitize_file(Document),
		% Build the return record
		SanitizedRecord = #new_document{document_type_id=DocumentTypeId, document=FDocument},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#change_document{document_status_id=DocumentStatusId}) when not is_integer(DocumentStatusId) -> nok;
sanitize(#change_document{version=Version}) when not is_integer(Version) -> nok;
sanitize(Record) when is_record(Record, change_document) -> {ok, Record};

sanitize(#new_location{version=Version}) when not is_integer(Version) -> nok;
sanitize(#new_location{description=Description, components=Components, position=Position, contacts=Contacts, version=Version}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FDescription} = eb_api_util:trim_mandatory(Description, TrimPattern),
		% Size validations
		ok = eb_util:validate_size(FDescription, ?DB_FIELD_SIZE__USER_LOCATION__DESCRIPTION),
		% Extra validations
		{ok, FPosition} = eb_api_util:sanitize_position(Position),
		case Components of
			undefined -> FComponents = undefined;
			_ -> {ok, FComponents} = sanitize(Components, [], TrimPattern)
		end,
		case Contacts of
			undefined -> FContacts = undefined;
			_ -> {ok, FContacts} = sanitize(Contacts, [], TrimPattern)
		end,
		% Build the return record
		SanitizedRecord = #new_location{description=FDescription, components=FComponents, position=FPosition, contacts=FContacts, version=Version},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#change_location{version=Version}) when not is_integer(Version) -> nok;
sanitize(#change_location{description=Description, components=Components, position=Position, contacts=Contacts, version=Version}) ->
	error_logger:info_msg("sanitize change_location start~p~n", []),
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		error_logger:info_msg("sanitize change_location description~p~n", []),
		{ok, FDescription} = eb_api_util:trim_optional(Description, TrimPattern),
		% Size validations
		ok = eb_api_util:validate_size_optional(FDescription, ?DB_FIELD_SIZE__USER_LOCATION__DESCRIPTION),
		% Extra validations
		error_logger:info_msg("sanitize change_location position~p~n", []),
		case Position of
			undefined -> FPosition = undefined;
			_ -> {ok, FPosition} = eb_api_util:sanitize_position(Position)
		end,
		error_logger:info_msg("sanitize change_location components~p~n", []),
		case Components of
			undefined -> FComponents = undefined;
			_ -> {ok, FComponents} = sanitize(Components, [], TrimPattern)
		end,
		error_logger:info_msg("sanitize change_location contacts~p~n", []),
		case Contacts of
			undefined -> FContacts = undefined;
			_ -> {ok, FContacts} = sanitize(Contacts, [], TrimPattern)
		end,
		error_logger:info_msg("sanitize change_location build record~p~n", []),
		% Build the return record
		SanitizedRecord = #change_location{description=FDescription, components=FComponents, position=FPosition, contacts=FContacts, version=Version},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	after
		error_logger:info_msg("sanitize change_location catch~p~n", [])
	end;

sanitize(#new_courier_transport{version=Version}) when not is_integer(Version) -> nok;
sanitize(#new_courier_transport{transport_type_id=TransportTypeId}) when not is_integer(TransportTypeId) -> nok;
sanitize(Record=#new_courier_transport{description=Description, registration_id=RegistrationId, color=Color}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FDescription} = eb_api_util:trim_optional(Description, TrimPattern),
		{ok, FRegistrationId} = eb_api_util:trim_optional(RegistrationId, TrimPattern),
		% Size validations
		ok = eb_api_util:validate_size_optional(FDescription, ?DB_FIELD_SIZE__COURIER_TRANSPORT_TYPE__DESCRIPTION),
		ok = eb_api_util:validate_size_optional(FRegistrationId, ?DB_FIELD_SIZE__COURIER_TRANSPORT_TYPE__REGISTRATION_ID),
		% Other validations
		true = eb_api_util:is_optional_color(Color),
		% Build the return record
		SanitizedRecord = Record#new_courier_transport{description=FDescription, registration_id=FRegistrationId},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#change_courier_transport{transport_status_id=TransportStatusId}) when not is_integer(TransportStatusId) -> nok;
sanitize(#change_courier_transport{version=Version}) when not is_integer(Version) -> nok;
sanitize(Record) when is_record(Record, change_courier_transport) -> {ok, Record};

sanitize(UpdateFile = #update_file{file=File, version=Version}) when is_integer(Version) ->
	try
		{ok, SanitizedFile} = eb_api_util:sanitize_file(File),
		% Build the return record
		SanitizedRecord = UpdateFile#update_file{file=SanitizedFile},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#update_user_notifications{enabled=Enabled, version=Version}) when not is_boolean(Enabled); not is_integer(Version) -> nok;
sanitize(#update_user_notifications{notification_type_ids=NotificationTypeIds})
  when NotificationTypeIds =/= undefined, not is_list(NotificationTypeIds) -> nok;
sanitize(UpdateUserNotifications = #update_user_notifications{notification_type_ids=undefined}) ->
	SanitizedRecord = UpdateUserNotifications#update_user_notifications{notification_type_ids=[]},
	{ok, SanitizedRecord};
sanitize(UpdateUserNotifications = #update_user_notifications{notification_type_ids=NotificationTypeIds}) ->
	IntListFlag = lists:all(fun(Id) -> is_integer(Id) end, NotificationTypeIds),
	if IntListFlag -> {ok, UpdateUserNotifications}
	 ; not IntListFlag -> nok
	end;

sanitize(#change_commercial_info{version=Version}) when not is_integer(Version) -> nok;
sanitize(ChangeCommercialInfo = #change_commercial_info{about=About}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FAbout} = eb_api_util:trim_mandatory(About, TrimPattern),
		% Build the return record
		SanitizedRecord = ChangeCommercialInfo#change_commercial_info{about=FAbout},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(_Other) -> nok.

sanitize(#component_info{component=Component, value=Value}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FComponent} = eb_api_util:trim_mandatory(Component, TrimPattern),
		{ok, FValue} = eb_api_util:trim_mandatory(Value, TrimPattern),
		% Build the return record
		SanitizedRecord = #component_info{component=FComponent, value=FValue},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#contact_info{name=Name, phone_nr=PhoneNr, email=Email}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FName} = eb_api_util:trim_mandatory(Name, TrimPattern),
		{ok, FPhoneNr} = eb_api_util:trim_optional(PhoneNr, TrimPattern),
		{ok, FEmail} = eb_api_util:trim_optional(Email, TrimPattern),
		% Size validations
		ok = eb_util:validate_size(FName, ?DB_FIELD_SIZE__USER_LOCATION_CONTACT_NAME),
		ok = eb_api_util:validate_size_optional(FPhoneNr, ?DB_FIELD_SIZE__USER_LOCATION_CONTACT_PHONE_NR),
		ok = eb_api_util:validate_size_optional(FEmail, ?DB_FIELD_SIZE__USER_LOCATION_CONTACT_EMAIL),
		% Extra validations
error_logger:info_msg("sanitize contact validate email~p~n", []),
		case FEmail of
			FEmail when is_binary(FEmail) -> true = eb_util:is_valid_email(FEmail);
			_ -> no_validation
		end,% Build the return record
error_logger:info_msg("sanitize contact build record~p~n", []),
		SanitizedRecord = #contact_info{name=FName, phone_nr=FPhoneNr, email=FEmail},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(_Other, _TrimPattern) -> nok.

sanitize([], SanitizedList, _TrimPattern) -> {ok, SanitizedList};
sanitize([Element|Rest], SanitizedList, TrimPattern) ->
	case sanitize(Element, TrimPattern) of
		{ok, SanitizedElement} -> sanitize(Rest, [SanitizedElement|SanitizedList], TrimPattern);
		_Error -> nok
	end;
sanitize(_Other, _, _TrimPattern) -> nok.

sanitize_address(undefined, _TrimPattern) -> {ok, undefined};
sanitize_address([], _TrimPattern) -> {ok, []};
sanitize_address(AddressComponents, TrimPattern) when is_list(AddressComponents) ->
	sanitize_address([], AddressComponents, TrimPattern);
sanitize_address(_AddressComponents, _TrimPattern) -> nok.

sanitize_address(Acc, [], _TrimPattern) -> {ok, lists:reverse(Acc)};
sanitize_address(Acc, [#component_info{component=Component, value=Value}|Rest], TrimPattern) ->
	try
		% Trim binary strings
		{ok, FComponent} = eb_api_util:trim_mandatory(Component, TrimPattern),
		{ok, FValue} = eb_api_util:trim_mandatory(Value, TrimPattern),
		% Build the return record
		SanitizedRecord = #component_info{component=FComponent, value=FValue},
		% Sanitize next element
		sanitize_address([SanitizedRecord|Acc], Rest, TrimPattern)
	catch
		_:_ -> nok
	end.
