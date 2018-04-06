%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_rest_session).

-include("eb_constants.hrl").

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Update user position
%% PATCH /session
handle(<<"PATCH">>, [], Request) ->
	case get_session_token(Request) of
		{ok, Token, Request1} ->
			case eb_rest_util:get_record_parameter(Request1, #update_session_info{}) of
				{ok, UserSessionInfo, Request2} ->
					case eb_api_session:update_session_info(Token, UserSessionInfo) of
						ok -> eb_rest_util:return_success(Request2);
						{nok, invalid_token} -> eb_rest_util:return_error(401, <<"invalid_client">>, <<"Invalid authentication token">>, Request2);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request2);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request2)
					end;
				{nok, _Error, Request2} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The position record is invalid.">>, Request2)
			end;
		{nok, Code, Value, Description, Request1} -> eb_rest_util:return_error(Code, Value, Description, Request1)
	end;

%% Login
%% POST /session/<user_type>
handle(<<"POST">>, [<<"clients">>], Request) ->
	process_login(Request, [?DB_USER_TYPE_CLIENT_BUSINESS, ?DB_USER_TYPE_CLIENT_PRIVATE, ?DB_USER_TYPE_CLIENT_ECOMMERCE, ?DB_USER_TYPE_CLIENT_COURIER_BUSINESS]);
handle(<<"POST">>, [<<"couriers">>], Request) ->
	process_login(Request, [?DB_USER_TYPE_COURIER, ?DB_USER_TYPE_EXTERNAL_COURIER]);
handle(<<"POST">>, [<<"operators">>], Request) ->
	process_login(Request, [?DB_USER_TYPE_ADMINISTRATOR, ?DB_USER_TYPE_OPERATOR, ?DB_USER_TYPE_DOCUMENT_VALIDATOR,
							?DB_USER_TYPE_DISPATCHER, ?DB_USER_TYPE_CALL_CENTER]);

%% Get the delivery types
%% GET /session/push-types
handle(<<"GET">>, [<<"push-types">>], Request) ->
	case validate_session(Request) of
		{ok, _UserId, _UserTypeId, Request1} ->
			case eb_api_session:get_push_types() of
				{ok, Results} -> eb_rest_util:return_success(Results, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
			end;
		{nok, Code, Value, Description, Request1} -> eb_rest_util:return_error(Code, Value, Description, Request1)
	end;

%% Get the notification types
%% GET /session/notification-types
handle(<<"GET">>, [<<"notification-types">>], Request) ->
	case validate_session(Request) of
		{ok, _UserId, _UserTypeId, Request1} ->
			case eb_api_session:get_notification_types() of
				{ok, Results} -> eb_rest_util:return_success(Results, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
			end;
		{nok, Code, Value, Description, Request1} -> eb_rest_util:return_error(Code, Value, Description, Request1)
	end;

%% Get callcenter contact
%% GET /session/help/couriers
handle(<<"GET">>, [<<"help">>, <<"couriers">>], Request) ->
	case validate_session(Request) of
		{ok, _UserId, _UserTypeId, Request1} ->
			case eb_api_session:get_help() of
				{ok, Callcenter} -> eb_rest_util:return_json(200, [{<<"callcenter">>, Callcenter}], Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
			end;
		{nok, Code, Value, Description, Request1} -> eb_rest_util:return_error(Code, Value, Description, Request1)
	end;

%% Courier available
%% PUT /session/couriers/available
handle(<<"PUT">>, [<<"couriers">>, <<"available">>], Request) ->
	case get_session_token(Request) of
		{ok, Token, Request1} ->
			case eb_rest_util:get_record_parameter(Request1, #update_session_info{}) of
				{ok, UserSessionInfo, Request2} ->
					case eb_api_session:available(Token, UserSessionInfo) of
						{ok, Result} -> eb_rest_util:return_success(Result, Request2);
						{nok, invalid_token} -> eb_rest_util:return_error(401, <<"invalid_client">>, <<"Invalid authentication token">>, Request2);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request2);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request2)
					end;
				{nok, _Error, Request2} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The courier available record is invalid.">>, Request2)
			end;
		{nok, Code, Value, Description, Request1} -> eb_rest_util:return_error(Code, Value, Description, Request1)
	end;

%% Courier unavailable
%% DELETE /session/couriers/available
handle(<<"DELETE">>, [<<"couriers">>, <<"available">>], Request) ->
	case get_session_token(Request) of
		{ok, Token, Request1} ->
			case eb_api_session:unavailable(Token) of
				ok -> eb_rest_util:return_success(Request1);
				{nok, invalid_token} -> eb_rest_util:return_error(401, <<"invalid_client">>, <<"Invalid authentication token">>, Request1);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
			end;
		{nok, Code, Value, Description, Request1} -> eb_rest_util:return_error(Code, Value, Description, Request1)
	end;

%% Send a message to the central
%% POST /session/notification
handle(<<"POST">>, [<<"notification">>], Request) ->
	case get_session_token(Request) of
		{ok, Token, Request1} ->
			case eb_rest_util:get_record_parameter(Request1, #new_notification{}) of
				{ok, NewNotification, Request2} ->
					case eb_api_session:create_notification(Token, NewNotification) of
						{ok, NotificationId} ->eb_rest_util:return_json(201, [{<<"notification_id">>, NotificationId}], Request2);
						{nok, invalid_token} -> eb_rest_util:return_error(401, <<"invalid_client">>, <<"Invalid authentication token">>, Request2);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request2);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request2)
					end;
				{nok, _Error, Request2} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The new notification record is invalid.">>, Request2)
			end;
		{nok, Code, Value, Description, Request1} -> eb_rest_util:return_error(Code, Value, Description, Request1)
	end;

%% Get counters for online couriers
%% GET /session/counters/couriers
handle(<<"GET">>, [<<"counters">>, <<"couriers">>], Request) ->
	case validate_session(Request) of
		{ok, _UserId, UserTypeId, Request1} ->
			% Get query parameters
			{Args, Request2} = kb_action_helper:get_args(Request1),
			case eb_rest_util:get_optional_binary_arg_value(Args, <<"status">>) of
				{ok, Status} ->
					case eb_rest_util:get_optional_integer_arg_value(Args, <<"transport_type_id">>) of
						{ok, TransportTypeId} ->
							case eb_api_session:get_courier_counters(UserTypeId, TransportTypeId, Status) of
								{ok, Results} -> eb_rest_util:return_success(Results, Request2);
								{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request2);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get counter information.">>, Request2);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request2)
							end;
						_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid transport_type_id parameter.">>, Request2)
					end;
				_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid status parameter.">>, Request2)
			end;
		{nok, Code, Value, Description, Request1} -> eb_rest_util:return_error(Code, Value, Description, Request1)
	end;

%% Get details for online couriers
%% GET /session/details/couriers
handle(<<"GET">>, [<<"details">>, <<"couriers">>], Request) ->
	try
		% Validate Session
		UserTypeId =
			case validate_session(Request) of
				{ok, _UserId, SessionUserTypeId, Request1} -> SessionUserTypeId;
				{nok, Code, Value, Description, Request1} -> throw({validate_session, Request1, Code, Value, Description})
			end,
		% Get query parameters
		{Args, Request2} = kb_action_helper:get_args(Request1),
		% Get query arguments
		Status =
			case eb_rest_util:get_optional_binary_arg_value(Args, <<"status">>) of
				{ok, StatusArg} -> StatusArg;
				_ -> throw({invalid_status_param, Request2})
			end,
		TransportTypeId =
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"transport_type_id">>) of
				{ok, TransportTypeIdArg} -> TransportTypeIdArg;
				_ -> throw({invalid_transport_type_id_param, Request2})
			end,
		ZoneId =
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"zone_id">>) of
				{ok, ZoneIdArg} -> ZoneIdArg;
				_ -> throw({invalid_zone_id_param, Request2})
			end,
		AccountId =
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"account_id">>) of
				{ok, AccountIdArg} -> AccountIdArg;
				_ -> throw({invalid_account_id_param, Request2})
			end,
		Navigation =
			case eb_rest_util:get_rs_navigation(Args) of
				{ok, NavigationArgs} -> NavigationArgs;
				nok -> throw({invalid_navigation_params, Request2})
			end,
		% Get courier information
		case eb_api_session:get_courier_details(UserTypeId, TransportTypeId, Status, ZoneId, AccountId, Navigation) of
			{ok, Results} -> eb_rest_util:return_success(Results, Request2);
			{nok, Error} -> throw({Error, Request2});
			_ -> throw({unexpected, Request2})
		end
	catch
		throw:{validate_session, LastRequest, RetCode, RetValue, RetDescription} -> eb_rest_util:return_error(RetCode, RetValue, RetDescription, LastRequest);
		throw:{invalid_status_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid status parameter.">>, LastRequest);
		throw:{invalid_transport_type_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid transport_type_id parameter.">>, LastRequest);
		throw:{invalid_zone_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid zone_id parameter.">>, LastRequest);
		throw:{invalid_account_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid account_id parameter.">>, LastRequest);
		throw:{invalid_navigation_params, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The navigation parameters are invalid.">>, LastRequest);
		throw:{invalid_parameters, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, LastRequest);
		throw:{forbidden, LastRequest} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get detail information.">>, LastRequest);
		throw:{_, LastRequest} -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, LastRequest)
	end;

%% Get accounts of online couriers
%% GET /session/details/couriers/accounts
handle(<<"GET">>, [<<"details">>, <<"couriers">>, <<"accounts">>], Request) ->
	try
		% Validate Session
		UserTypeId =
			case validate_session(Request) of
				{ok, _UserId, SessionUserTypeId, Request1} -> SessionUserTypeId;
				{nok, Code, Value, Description, Request1} -> throw({validate_session, Request1, Code, Value, Description})
			end,
		% Get courier information
		case eb_api_session:get_courier_accounts(UserTypeId) of
			{ok, Results} -> eb_rest_util:return_success(Results, Request1);
			{nok, Error} -> throw({Error, Request1});
			_ -> throw({unexpected, Request1})
		end
	catch
		throw:{validate_session, LastRequest, RetCode, RetValue, RetDescription} -> eb_rest_util:return_error(RetCode, RetValue, RetDescription, LastRequest);
		throw:{invalid_parameters, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, LastRequest);
		throw:{forbidden, LastRequest} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this information.">>, LastRequest);
		throw:{_, LastRequest} -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, LastRequest)
	end;

%% Logout
%% DELETE /session
handle(<<"DELETE">>, [], Request) ->
	case get_session_token(Request) of
		{ok, Token, Request1} ->
			case eb_api_session:logout(Token) of
				ok -> eb_rest_util:return_success(Request1);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
			end;
		{nok, Code, Value, Description, Request1} -> eb_rest_util:return_error(Code, Value, Description, Request1)
	end;

handle(Method, Path, Request) ->
	Attributes = kb_action_helper:get_attributes(Request),
	error_logger:info_msg("\nMethod [~p]\nPath [~p]\nRequest [~p]\nAttributes [~p]\n", [Method, Path, Request, Attributes]),
	{raw, 404, [], "Request malandro...", Request}.

%% ====================================================================
%% Internal functions
%% ====================================================================
get_credentials_from_token(Token) ->
	try
		DecodedValue = base64:decode(Token),
		case binary:split(DecodedValue, <<":">>) of
			[ClientId, ClientSecret] when is_binary(ClientId) andalso is_binary(ClientSecret) -> {ok, ClientId, ClientSecret};
			_ -> {nok, invalid_token_content}
		end
	catch
		error: _ -> {nok, invalid_token}
	end.

get_username_and_password(Args) ->
	case proplists:get_value(<<"username">>, Args) of
		undefined -> {nok, missing_username};
		ClientId ->
			case proplists:get_value(<<"password">>, Args) of
				undefined -> {nok, missing_password};
				ClientSecret -> {ok, ClientId, ClientSecret}
			end
	end.

get_credentials(Request) ->
	{Args, Request1} = kb_action_helper:get_args(Request),
	case proplists:get_value(<<"grant_type">>, Args) of
		<<"client_credentials">> ->
			case eb_rest_util:get_header(Request1, <<"authorization">>) of
				{ok, Value, Request2} ->
					case binary:split(Value, <<" ">>) of
						[<<"Basic">>, TokenValue] ->
							case get_credentials_from_token(TokenValue) of
								{ok, ClientId, ClientSecret} -> {ok, ClientId, ClientSecret, Request2};
								{nok, Error} ->
									Desc = <<<<"Invalid request: unknown basic authentication values: ">>/binary, (atom_to_binary(Error, latin1))/binary>>,
									{nok, <<"invalid_request">>, Desc, Request2}
							end;
						_ -> {nok, <<"invalid_request">>, <<"Invalid request: unknown basic authentication values">>, Request2}
					end;
				{Error, Request2} ->
					Desc = <<<<"Invalid request: authentication header: ">>/binary, (atom_to_binary(Error, latin1))/binary>>,
					{nok, <<"invalid_request">>, Desc, Request2}
			end;
		<<"password">> ->
			case get_username_and_password(Args) of
				{ok, ClientId, ClientSecret} -> {ok, ClientId, ClientSecret, Request1};
				{nok, Error} ->
					Desc = <<<<"Invalid request: password authentication: ">>/binary, (atom_to_binary(Error, latin1))/binary>>,
					{nok, <<"invalid_request">>, Desc, Request1}
			end;
		_Other -> {nok, <<"unsupported_grant_type">>, <<"Invalid request: unknown grant_type">>, Request1}
	end.

process_login(Request, ValidUserTypes) ->
	case get_credentials(Request) of
		{ok, ClientId, ClientSecret, Request1} ->
			case eb_api_session:login(ClientId, ClientSecret, ValidUserTypes) of
				{ok, SessionToken, SessionTimeoutSecs, UserId} ->
					eb_rest_util:return_json(200, [
					                               {<<"access_token">>, SessionToken},
					                               {<<"token_type">>, <<"bearer">>},
					                               {<<"expires_in">>, SessionTimeoutSecs},
					                               {<<"user_id">>, UserId}
					                              ], Request1);
				{nok, not_found, ExternalUserInfo} -> eb_rest_util:return_error(401, ExternalUserInfo, Request1);
				{nok, authentication} -> eb_rest_util:return_error(401, <<"invalid_client">>, <<"Unknown username/password combination.">>, Request1);
				{nok, user_status} -> eb_rest_util:return_error(403, <<"user_status">>, <<"User not active.">>, Request1);
				{nok, invalid_user_type} -> eb_rest_util:return_error(403, <<"user_type">>, <<"Invalid user type for this login operation.">>, Request1);
				{nok, in_use} -> eb_rest_util:return_error(403, <<"session">>, <<"User already in session.">>, Request1);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
			end;
		{nok, Error, Description, Request1} -> eb_rest_util:return_error(400, Error, Description, Request1)
	end.

get_session_token(Request) ->
	case eb_rest_util:get_authorization_token(Request) of
		{ok, TokenValue, Request1} -> {ok, TokenValue, Request1};
		{Error, Request1} ->{nok, 400, <<"invalid_request">>, <<<<"Invalid request: authentication header: ">>/binary, (atom_to_binary(Error, latin1))/binary>>, Request1}
	end.

validate_session(Request) ->
	case get_session_token(Request) of
		{ok, TokenValue, Request1} ->
			case eb_api_session:validate_token(TokenValue) of
				{ok, UserId, UserTypeId} -> {ok, UserId, UserTypeId, Request1};
				{nok, invalid_token} -> {nok, 401, <<"invalid_client">>, <<"Invalid authentication token">>, Request1};
				_Other -> {nok, 500, <<"system_error">>, <<"Unexpected system error">>, Request1}
			end;
		Error -> Error
	end.
