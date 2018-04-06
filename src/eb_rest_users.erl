%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_rest_users).

-include("eb_constants.hrl").

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Get users
%% GET /users
handle(<<"GET">>, [], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_optional_string_arg_value(Args, <<"user_type_id">>) of
		{ok, UserTypeId} ->
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"user_status_id">>) of
				{ok, UserStatusId} ->
					case eb_rest_util:get_rs_navigation(Args) of
						{ok, Navigation} ->
							case eb_api_users:get_users(RequestingUserTypeId, UserTypeId, UserStatusId, Navigation) of
								{ok, Results} -> eb_rest_util:return_success(Results, Request1);
								{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get other users information.">>, Request1);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
							end;
						nok -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The navigation parameters are invalid.">>, Request1)
					end;
				_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid user_status_id parameter.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid user_type_id parameter.">>, Request1)
	end;

%% Register user
%% POST /users
handle(<<"POST">>, [], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% These attributes can be undefined
	case eb_rest_util:get_record_parameter(Request, #new_user{}) of
		{ok, NewUser, Request1} ->
			case eb_api_users:create_user(RequestingUserId, RequestingUserTypeId, NewUser) of
				{ok, UserId} -> eb_rest_util:return_json(201, [{<<"user_id">>, UserId}], Request1);
				{nok, user_exists} -> eb_rest_util:return_error(409, <<"user_exists">>, <<"The user already exists.">>, Request1);
				{nok, unique_key_exists} -> eb_rest_util:return_error(409, <<"unique_key_exists">>, <<"The unique key already exists.">>, Request1);
				{nok, ParameterError} when ParameterError =:= unknown_user_type_id orelse
				                           ParameterError =:= invalid_user_type_id orelse
				                           ParameterError =:= unknown_account_user_type_id orelse
				                           ParameterError =:= invalid_account_user_type_id orelse
				                           ParameterError =:= missing_account_user_type_id orelse
				                           ParameterError =:= missing_account_information orelse
				                           ParameterError =:= wrong_user_type orelse
				                           ParameterError =:= invalid_unique_key orelse
				                           ParameterError =:= missing_values orelse
				                           ParameterError =:= unknown_mobileos_id orelse
				                           ParameterError =:= invalid_mobileos_id orelse
				                           ParameterError =:= invalid_parameters ->
					BinParameterError = atom_to_binary(ParameterError, latin1),
					Description = <<<<"The user record has invalid parameters: ">>/binary, BinParameterError/binary>>,
					eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to create this user type.">>, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error creating the user.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The user record is invalid.">>, Request1)
	end;

%% Confirm user email
%% PUT /users
handle(<<"PUT">>, [], Request) ->
	case eb_rest_util:get_single_parameter(Request, <<"token">>, undefined) of
		{ok, TokenId, Request1} ->
			case eb_api_users:confirm_user_email(TokenId) of
				ok -> eb_rest_util:return_success(Request1);
				{nok, not_found_token} -> eb_rest_util:return_error(404, <<"not_found_token">>, <<"Token ID not found.">>, Request1);
				{nok, not_found_user} -> eb_rest_util:return_error(404, <<"not_found_user">>, <<"User ID not found.">>, Request1);
				{nok, wrong_type} -> eb_rest_util:return_error(400, <<"wrong_type">>, <<"Invalid token type for email confirmation.">>, Request1);
				{nok, invalid} -> eb_rest_util:return_error(400, <<"invalid_token">>, <<"Token ID invalid / expired.">>, Request1);
				{nok, ParameterError} when ParameterError =:= missing_values orelse
				                           ParameterError =:= invalid_parameters ->
					BinParameterError = atom_to_binary(ParameterError, latin1),
					Description = <<<<"The user email confirmation record has invalid parameters: ">>/binary, BinParameterError/binary>>,
					eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error confirming user email.">>, Request1)
			end;
		{nok, _Error, Request1} ->
			eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The email confirmation record is invalid.">>, Request1)
	end;

%% Count users
%% GET /users/total
handle(<<"GET">>, [<<"total">>], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_optional_integer_arg_value(Args, <<"user_type_id">>) of
		{ok, UserTypeId} ->
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"user_status_id">>) of
				{ok, UserStatusId} ->
					case eb_api_users:count_users(RequestingUserTypeId, UserTypeId, UserStatusId) of
						{ok, Count} -> eb_rest_util:return_json(200, [{<<"total">>, Count}], Request1);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get user totals.">>, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error creating the user.">>, Request1)
					end;
				_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid user_status_id parameter.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid user_type_id parameter.">>, Request1)
	end;

% GET /users/types
handle(<<"GET">>, [<<"types">>], Request) ->
	case eb_api_users:get_user_types() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

% GET /users/statuses
handle(<<"GET">>, [<<"statuses">>], Request) ->
	case eb_api_users:get_user_statuses() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

% GET /users/transport-types
handle(<<"GET">>, [<<"transport-types">>], Request) ->
	case eb_api_users:get_transport_types() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

% GET /users/transport-type-statuses
handle(<<"GET">>, [<<"transport-type-statuses">>], Request) ->
	case eb_api_users:get_transport_type_statuses() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

% GET /users/document-types
handle(<<"GET">>, [<<"document-types">>], Request) ->
	case eb_api_users:get_document_types() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

% GET /users/document-statuses
handle(<<"GET">>, [<<"document-statuses">>], Request) ->
	case eb_api_users:get_document_statuses() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

% GET /users/notification-types
handle(<<"GET">>, [<<"notification-types">>], Request) ->
	case eb_api_users:get_user_notification_types() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

% GET /users/zones
handle(<<"GET">>, [<<"zones">>], Request) ->
	case eb_api_users:get_zones() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

% GET /users/mobileos
handle(<<"GET">>, [<<"mobileos">>], Request) ->
	case eb_api_users:get_mobileos() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Reset password - step 1/2
%% POST /users/new-password
handle(<<"POST">>, [<<"new-password">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% These attributes can be undefined
	case eb_rest_util:get_record_parameter(Request, #reset_password{}) of
		{ok, RecoverPassword, Request1} ->
			case eb_api_users:reset_password(RequestingUserId, RequestingUserTypeId, RecoverPassword) of
				ok -> eb_rest_util:return_success(Request1);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID / Username not found.">>, Request);
				{nok, ParameterError} when ParameterError =:= missing_values orelse
				                           ParameterError =:= invalid_parameters ->
					BinParameterError = atom_to_binary(ParameterError, latin1),
					Description = <<<<"The reset password record has invalid parameters: ">>/binary, BinParameterError/binary>>,
					eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to reset password for this user type.">>, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error creating the user.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The reset password record is invalid.">>, Request1)
	end;

%% Reset password - step 2/2
%% PUT /users/new-password
handle(<<"PUT">>, [<<"new-password">>], Request) ->
	case eb_rest_util:get_record_parameter(Request, #new_password{}) of
		{ok, #new_password{password=NewPassword, token=TokenId}, Request1} ->
			case eb_api_users:set_password(NewPassword, TokenId) of
				ok -> eb_rest_util:return_success(Request1);
				{nok, not_found_token} -> eb_rest_util:return_error(404, <<"not_found_token">>, <<"Token ID not found.">>, Request);
				{nok, wrong_type} -> eb_rest_util:return_error(400, <<"wrong_type">>, <<"Invalid token type for email confirmation.">>, Request);
				{nok, invalid} -> eb_rest_util:return_error(400, <<"invalid_token">>, <<"Token ID invalid / expired.">>, Request);
				{nok, ParameterError} when ParameterError =:= missing_values orelse
				                           ParameterError =:= invalid_parameters ->
					BinParameterError = atom_to_binary(ParameterError, latin1),
					Description = <<<<"The new password record has invalid parameters: ">>/binary, BinParameterError/binary>>,
					eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error setting the user password.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The new password record is invalid.">>, Request1)
	end;

%% Get user
%% GET /users/<id>
handle(<<"GET">>, [UserId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_api_users:get_user(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Change user
%% PATCH /users/<id>
handle(<<"PATCH">>, [UserId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_rest_util:get_record_parameter(Request, #change_user{}) of
				{ok, ChangeUser, Request1} ->
					case eb_api_users:change_user(RequestingUserId, RequestingUserTypeId, TargetUserId, ChangeUser) of
						{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request1);
						{nok, version} -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong user version">>, Request1);
						{nok, user_exists} -> eb_rest_util:return_error(409, <<"user_exists">>, <<"Duplicate username">>, Request1);
						{nok, ParameterError} when ParameterError =:= invalid_user_status
						                         ; ParameterError =:= missing_values
						                         ; ParameterError =:= invalid_mobileos
						                         ; ParameterError =:= invalid_zone
						                         ; ParameterError =:= invalid_parameters ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The change user record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change other users.">>, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the user.">>, Request1)
					end;
				{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change user record is invalid.">>, Request1)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Change Password
%% PUT /users/<id>/password
handle(<<"PUT">>, [UserId, <<"password">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_rest_util:get_record_parameter(Request, #change_password{}) of
				{ok, ChangePassword, Request1} ->
					case eb_api_users:change_password(RequestingUserId, RequestingUserTypeId, TargetUserId, ChangePassword) of
						ok -> eb_rest_util:return_success(Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID and password not found.">>, Request);
						{nok, ParameterError} when ParameterError =:= missing_values orelse ParameterError =:= invalid_parameters ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The change password record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"Users are only allowed to change their own password.">>, Request);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the password.">>, Request1)
					end;
				{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change password record is invalid.">>, Request1)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Logout
%% DELETE /users/<id>/session
handle(<<"DELETE">>, [UserId, <<"session">>], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_api_session:logout(RequestingUserTypeId, TargetUserId) of
				ok -> eb_rest_util:return_success(Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to logout other users.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Get user photo
%% GET /users/<userid>/photo
handle(<<"GET">>, [UserId, <<"photo">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_api_users:get_user_photo(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Resource not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user documents.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Get all user documents
%% GET /users/<id>/documents
handle(<<"GET">>, [UserId, <<"documents">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_optional_integer_arg_value(Args, <<"document_type_id">>) of
		{ok, DocumentTypeId} ->
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"document_status_id">>) of
				{ok, DocumentStatusId} ->
					case eb_rest_util:get_rs_navigation(Args) of
						{ok, Navigation} ->
							case eb_util:binary_to_int(UserId) of
								{ok, TargetUserId} ->
									case eb_api_users:get_user_documents(RequestingUserId, RequestingUserTypeId, TargetUserId, DocumentTypeId, DocumentStatusId, Navigation) of
										{ok, Results} -> eb_rest_util:return_success(Results, Request1);
										{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
										{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user documents.">>, Request1);
										_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
									end;
								_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
							end;
						nok -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The navigation parameters are invalid.">>, Request1)
					end;
				_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid user_status_id parameter.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid user_type_id parameter.">>, Request1)
	end;

%% Create a new user document
%% POST /users/<id>/documents
handle(<<"POST">>, [UserId, <<"documents">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_rest_util:get_record_parameter(Request, #new_document{}) of
				{ok, NewDocument, Request1} ->
					case eb_api_users:create_user_document(RequestingUserId, RequestingUserTypeId, TargetUserId, NewDocument) of
						{ok, DocumentId} -> eb_rest_util:return_json(201, [{<<"document_id">>, DocumentId}], Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to create documents for this user.">>, Request1);
						{nok, ParameterError} when ParameterError =:= unknown_document_type_id orelse
						                           ParameterError =:= invalid_document_type_id orelse
						                           ParameterError =:= missing_values orelse
						                           ParameterError =:= invalid_parameters ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The new document record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, BinParameterError, Description, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error creating the document.">>, Request1)
					end;
				{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The new document record is invalid.">>, Request1)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Get a user document
%% GET /users/<userid>/documents/<documentid>
handle(<<"GET">>, [UserId, <<"documents">>, DocumentId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_util:binary_to_int(DocumentId) of
				{ok, IDocumentId} ->
					case eb_api_users:get_user_document(RequestingUserId, RequestingUserTypeId, TargetUserId, IDocumentId) of
						{ok, Result} -> eb_rest_util:return_success(Result, Request);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Resource not found.">>, Request);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user documents.">>, Request);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Document ID.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Change a user document's status
%% PATCH /users/<userid>/documents/<documentid>
handle(<<"PATCH">>, [UserId, <<"documents">>, DocumentId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_util:binary_to_int(DocumentId) of
				{ok, IDocumentId} ->
					case eb_rest_util:get_record_parameter(Request, #change_document{}) of
						{ok, ChangeDocument, Request1} ->
							case eb_api_users:change_user_document(RequestingUserId, RequestingUserTypeId, TargetUserId, IDocumentId, ChangeDocument) of
								{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change this user documents.">>, Request1);
								{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Resource not found.">>, Request1);
								{nok, ParameterError} when ParameterError =:= unknown_document_status_id orelse
						                           ParameterError =:= invalid_document_status_id orelse
						                           ParameterError =:= missing_values orelse
						                           ParameterError =:= invalid_parameters ->
									BinParameterError = atom_to_binary(ParameterError, latin1),
									Description = <<<<"The change document record has invalid parameters: ">>/binary, BinParameterError/binary>>,
									eb_rest_util:return_error(400, BinParameterError, Description, Request1);
								{nok, wrong_version} -> eb_rest_util:return_error(409, <<"wrong_version">>, <<"Document has a different version.">>, Request1);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
							end;
						{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change document record is invalid.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Document ID.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Get user transports
%% GET /users/<id>/transports
handle(<<"GET">>, [UserId, <<"transports">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_api_users:get_user_transports(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user transport types.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Create a new user transport
%% POST /users/<id>/transport
handle(<<"POST">>, [UserId, <<"transports">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_rest_util:get_record_parameter(Request, #new_courier_transport{}) of
				{ok, NewCourierTransport, Request1} ->
					case eb_api_users:create_courier_transport(RequestingUserId, RequestingUserTypeId, TargetUserId, NewCourierTransport) of
						{ok, NewVersion} -> eb_rest_util:return_json(201, [{<<"version">>, NewVersion}], Request1);
						{ok, NewCourierTransportId, NewVersion} -> eb_rest_util:return_json(201, [{<<"transport_id">>, NewCourierTransportId},
						                                                                          {<<"version">>, NewVersion}], Request1);
						{nok, not_found_user} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to create courier transports for this user.">>, Request1);
						{nok, ParameterError} when ParameterError =:= unknown_transport_type_id orelse
						                           ParameterError =:= invalid_transport_type_id orelse
						                           ParameterError =:= missing_values orelse
						                           ParameterError =:= invalid_parameters ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The new courier transport type record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, BinParameterError, Description, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error creating the courier transport.">>, Request1)
					end;
				{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The new courier transport record is invalid.">>, Request1)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Delete a transport_type from the user's list
%% DELETE /users/<userid>/transports/<id>
handle(<<"DELETE">>, [UserId, <<"transports">>, TransportId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_integer_arg_value(Args, <<"version">>) of
		{ok, Version} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_util:binary_to_int(TransportId) of
						{ok, FTransportId} ->
							case eb_api_users:remove_courier_transport(RequestingUserId, RequestingUserTypeId, TargetUserId, FTransportId, Version) of
								{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
								{nok, not_found_user} -> eb_rest_util:return_error(404, <<"not_found_user">>, <<"User Id not found.">>, Request1);
								{nok, version} -> eb_rest_util:return_error(409, <<"version">>, <<"Wrong user version.">>, Request1);
								{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
								{nok, not_found_transport} -> eb_rest_util:return_error(404, <<"not_found_transport">>, <<"Transport Id not found.">>, Request1);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to delete this user transport.">>, Request1);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
							end;
						_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Transport Id.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User Id.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The version parameter is invalid.">>, Request1)
	end;

%% Update a transport from the user's list
%% PUT /users/<userid>/transports/<transportid>
handle(<<"PUT">>, [UserId, <<"transports">>, OldTransportId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_integer_arg_value(Args, <<"version">>) of
		{ok, Version} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_util:binary_to_int(OldTransportId) of
						{ok, FOldTransportId} ->
							case eb_api_users:change_current_courier_transport(RequestingUserId, RequestingUserTypeId, TargetUserId, FOldTransportId, Version) of
								{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
								{nok, not_found_user} -> eb_rest_util:return_error(404, <<"not_found_user">>, <<"User Id not found.">>, Request1);
								{nok, version} -> eb_rest_util:return_error(409, <<"version">>, <<"Wrong user version.">>, Request1);
								{nok, not_found_transport} -> eb_rest_util:return_error(404, <<"not_found_transport">>, <<"Transport Id not found.">>, Request1);
								{nok, ParameterError} when ParameterError =:= invalid_parameters orelse ParameterError =:= wrong_status ->
									BinParameterError = atom_to_binary(ParameterError, latin1),
									Description = <<<<"The request has invalid parameters: ">>/binary, BinParameterError/binary>>,
									eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to delete this user transport.">>, Request1);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
							end;
						_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Old Transport Id.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User Id.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The version parameter is invalid.">>, Request1)
	end;

%% Change a user transport's status
%% PATCH /users/<userid>/transports/<transportid>
handle(<<"PATCH">>, [UserId, <<"transports">>, TransportId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_util:binary_to_int(TransportId) of
				{ok, ITransportId} ->
					case eb_rest_util:get_record_parameter(Request, #change_courier_transport{}) of
						{ok, ChangeCourierTransport, Request1} ->
							case eb_api_users:change_courier_transport(RequestingUserId, RequestingUserTypeId, TargetUserId, ITransportId, ChangeCourierTransport) of
								{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change this user transports.">>, Request1);
								{nok, not_found_user} -> eb_rest_util:return_error(404, <<"not_found_user">>, <<"User Id not found.">>, Request1);
								{nok, version} -> eb_rest_util:return_error(409, <<"version">>, <<"Wrong user version.">>, Request1);
								{nok, not_found_transport} -> eb_rest_util:return_error(404, <<"not_found_transport">>, <<"Transport Id not found.">>, Request1);
								{nok, current_transport} -> eb_rest_util:return_error(404, <<"current_transport">>, <<"User is not allowed to change the current transport.">>, Request1);
								{nok, ParameterError} when ParameterError =:= unknown_transport_status_id orelse
															   ParameterError =:= invalid_transport_status_id orelse
															   ParameterError =:= missing_values orelse
															   ParameterError =:= invalid_parameters ->
									BinParameterError = atom_to_binary(ParameterError, latin1),
									Description = <<<<"The change courier transport record has invalid parameters: ">>/binary, BinParameterError/binary>>,
									eb_rest_util:return_error(400, BinParameterError, Description, Request1);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
							end;
						{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change courier transport record is invalid.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Transport Id.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User Id.">>, Request)
	end;

%% Get the user position
%% GET /users/<id>/position
handle(<<"GET">>, [UserId, <<"position">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_api_session:get_user_position(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request);
				{nok, no_position} -> eb_rest_util:return_error(404, <<"no_position">>, <<"User ID no position found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user position.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Get user ranking
%% GET /users/<id>/ranking
handle(<<"GET">>, [UserId, <<"ranking">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_api_users:get_user_ranking(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				{ok, Ranking} -> eb_rest_util:return_json(200, [{<<"ranking">>, Ranking}], Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user ranking.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Get all user couriers
%% GET /users/<id>/couriers
handle(<<"GET">>, [UserId, <<"couriers">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_rs_navigation(Args) of
		{ok, Navigation} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_api_users:get_user_couriers(RequestingUserId, RequestingUserTypeId, TargetUserId, Navigation) of
						{ok, Results} -> eb_rest_util:return_success(Results, Request1);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user couriers.">>, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
			end;
		nok -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The navigation parameters are invalid.">>, Request1)
	end;

%% Add a courier to the user's list
%% PUT /users/<userid>/couriers/<courierid>
handle(<<"PUT">>, [UserId, <<"couriers">>, CourierId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_rest_util:get_single_parameter(Request, <<"version">>, undefined) of
		{ok, Version, Request1} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_util:binary_to_int(CourierId) of
						{ok, FCourierId} ->
							case eb_api_users:add_user_courier(RequestingUserId, RequestingUserTypeId, TargetUserId, FCourierId, Version) of
								{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1); 
								{nok, NotFound} when NotFound =:= not_found_user orelse NotFound =:= not_found_courier ->
									BinNotFound = atom_to_binary(NotFound, latin1),
									Description = <<<<"Not found: ">>/binary, BinNotFound/binary>>,
									eb_rest_util:return_error(404, BinNotFound, Description, Request1);
								{nok, Conflict} when Conflict =:= version orelse Conflict =:= courier_exists ->
									BinConflict = atom_to_binary(Conflict, latin1),
									Description = <<<<"Conflict: ">>/binary, BinConflict/binary>>,
									eb_rest_util:return_error(409, BinConflict, Description, Request1);
								{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user documents.">>, Request1);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
							end;
						_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Courier ID.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The add courier to list record is invalid.">>, Request1)
	end;

%% Delete a courier from the user's list
%% DELETE /users/<userid>/couriers/<courierid>
handle(<<"DELETE">>, [UserId, <<"couriers">>, CourierId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_integer_arg_value(Args, <<"version">>) of
		{ok, Version} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_util:binary_to_int(CourierId) of
						{ok, FCourierId} ->
							case eb_api_users:remove_user_courier(RequestingUserId, RequestingUserTypeId, TargetUserId, FCourierId, Version) of
								{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
								{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request1);
								{nok, version} -> eb_rest_util:return_error(409, <<"version">>, <<"Wrong user version.">>, Request1);
								{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to delete this user documents.">>, Request1);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
							end;
						_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Courier ID.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The version parameter is invalid.">>, Request1)
	end;

%% Get all user used couriers
%% GET /users/<id>/used-couriers
handle(<<"GET">>, [UserId, <<"used-couriers">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_optional_datetime(Args, <<"date_from">>) of
		{ok, DateFrom} ->
			case eb_rest_util:get_optional_datetime(Args, <<"date_to">>) of
				{ok, DateTo} ->
					case eb_rest_util:get_rs_navigation(Args) of
						{ok, Navigation} ->
							case eb_util:binary_to_int(UserId) of
								{ok, TargetUserId} ->
									case eb_api_users:get_user_used_couriers(RequestingUserId, RequestingUserTypeId, TargetUserId, DateFrom, DateTo, Navigation) of
										{ok, Results} -> eb_rest_util:return_success(Results, Request1);
										{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
										{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user couriers.">>, Request1);
										_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
									end;
								_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
							end;
						nok -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The navigation parameters are invalid.">>, Request1)
					end;
				_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid date_to parameter.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid date_from parameter.">>, Request1)
	end;

%% Get all user locations
%% GET /users/<id>/locations
handle(<<"GET">>, [UserId, <<"locations">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_optional_binary_arg_value(Args, <<"name">>) of
		{ok, Name} ->
			case eb_rest_util:get_optional_binary_arg_value(Args, <<"phone_nr">>) of
				{ok, PhoneNr} ->
					case eb_rest_util:get_rs_navigation(Args) of
						{ok, Navigation} ->
							case eb_util:binary_to_int(UserId) of
								{ok, TargetUserId} ->
									case eb_api_users:get_user_locations(RequestingUserId, RequestingUserTypeId, TargetUserId, Name, PhoneNr, Navigation) of
										{ok, Results} -> eb_rest_util:return_success(Results, Request1);
										{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
										{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user locations.">>, Request1);
										_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
									end;
								_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
							end;
						nok -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The navigation parameters are invalid.">>, Request1)
					end;
				_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid phone_nr parameter.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid name parameter.">>, Request1)
	end;

%% Create a new user location
%% POST /users/<id>/locations
handle(<<"POST">>, [UserId, <<"locations">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_rest_util:get_record_parameter(Request, #new_location{}) of
		{ok, NewLocation, Request1} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_api_users:add_user_location(RequestingUserId, RequestingUserTypeId, TargetUserId, NewLocation) of
						{ok, NewLocationId, NewVersion} -> eb_rest_util:return_json(201, [{<<"location_id">>, NewLocationId},
						                                                                  {<<"version">>, NewVersion}], Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request1);
						{nok, ParameterError} when ParameterError =:= missing_values orelse ParameterError =:= invalid_parameters ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The new location record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, BinParameterError, Description, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to add locations to this user.">>, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The new location record is invalid.">>, Request1)
	end;

%% Get a user location
%% GET /users/<userid>/locations/<locationid>
handle(<<"GET">>, [UserId, <<"locations">>, LocationId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_util:binary_to_int(LocationId) of
				{ok, FLocationId} ->
					case eb_api_users:get_user_location(RequestingUserId, RequestingUserTypeId, TargetUserId, FLocationId) of
						{ok, Result} -> eb_rest_util:return_success(Result, Request);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User or Location ID not found.">>, Request);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user locations.">>, Request);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Location ID.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Delete a user location
%% DELETE /users/<userid>/locations/<locationid>
handle(<<"DELETE">>, [UserId, <<"locations">>, LocationId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_integer_arg_value(Args, <<"version">>) of
		{ok, Version} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_util:binary_to_int(LocationId) of
						{ok, FLocationId} ->
							case eb_api_users:remove_user_location(RequestingUserId, RequestingUserTypeId, TargetUserId, FLocationId, Version) of
								{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
								{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request1);
								{nok, version} -> eb_rest_util:return_error(409, <<"version">>, <<"Wrong user version.">>, Request1);
								{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to delete this user locations.">>, Request1);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
							end;
						_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Location ID.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The version parameter is invalid.">>, Request1)
	end;

%% Change a user location
%% PATCH /users/<userid>/locations/<locationid>
handle(<<"PATCH">>, [UserId, <<"locations">>, LocationId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_rest_util:get_record_parameter(Request, #change_location{}) of
		{ok, ChangeLocation, Request1} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_util:binary_to_int(LocationId) of
						{ok, FLocationId} ->
							case eb_api_users:change_user_location(RequestingUserId, RequestingUserTypeId, TargetUserId, FLocationId, ChangeLocation) of
								{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
								{nok, NotFound} when NotFound =:= not_found_user orelse NotFound =:= not_found_location ->
									BinNotFound = atom_to_binary(NotFound, latin1),
									Description = <<<<"Not found: ">>/binary, BinNotFound/binary>>,
									eb_rest_util:return_error(404, BinNotFound, Description, Request1);
								{nok, version} -> eb_rest_util:return_error(409, <<"version">>, <<"Wrong user version.">>, Request1);
								{nok, ParameterError} when ParameterError =:= missing_values orelse ParameterError =:= invalid_parameters ->
									BinParameterError = atom_to_binary(ParameterError, latin1),
									Description = <<<<"The change location record has invalid parameters: ">>/binary, BinParameterError/binary>>,
									eb_rest_util:return_error(400, BinParameterError, Description, Request1);
								{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change this user locations.">>, Request1);
								_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
							end;
						_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Location ID.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change location record is invalid.">>, Request1)
	end;

%% Get user's notifications configuration
%% GET /users/<id>/notifications
handle(<<"GET">>, [UserId, <<"notifications">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_api_users:get_user_notifications(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Update user's notifications configuration
%% PUT /users/<id>/notifications
handle(<<"PUT">>, [UserId, <<"notifications">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_rest_util:get_record_parameter(Request, #update_user_notifications{}) of
		{ok, UpdateUserNotifications, Request1} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_api_users:set_user_notifications(RequestingUserId, RequestingUserTypeId, TargetUserId, UpdateUserNotifications) of
						{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
						{nok, missing_values} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The update user notifications record has invalid parameters: missing_values">>, Request1);
						{nok, invalid_user_notification_type} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The update user notifications record has invalid parameters: invalid_user_notification_type">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change this user information.">>, Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request1);
						{nok, version} -> eb_rest_util:return_error(409, <<"version">>, <<"Wrong user version.">>, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The update user notifications record is invalid.">>, Request1)
	end;

%% Get user's commercial information
%% GET /users/<id>/commercial-info
handle(<<"GET">>, [UserId, <<"commercial-info">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(UserId) of
		{ok, TargetUserId} ->
			case eb_api_users:get_commercial_info(RequestingUserId, RequestingUserTypeId, TargetUserId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this user information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request)
	end;

%% Update user's commercial information
%% PUT /users/<id>/commercial-info
handle(<<"PUT">>, [UserId, <<"commercial-info">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_rest_util:get_record_parameter(Request, #change_commercial_info{}) of
		{ok, ChangeCommercialInfo, Request1} ->
			case eb_util:binary_to_int(UserId) of
				{ok, TargetUserId} ->
					case eb_api_users:change_commercial_info(RequestingUserId, RequestingUserTypeId, TargetUserId, ChangeCommercialInfo) of
						{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
						{nok, missing_values} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change commercial info record has invalid parameters: missing_values">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change this user information.">>, Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"User ID not found.">>, Request1);
						{nok, version} -> eb_rest_util:return_error(409, <<"version">>, <<"Wrong user version.">>, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid User ID.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change commercial info record is invalid.">>, Request1)
	end;

handle(Method, Path, Request) ->
	Attributes = kb_action_helper:get_attributes(Request),
	error_logger:info_msg("\nMethod [~p]\nPath [~p]\nRequest [~p]\nAttributes [~p]\n", [Method, Path, Request, Attributes]),
	{raw, 404, [], "Request malandro...", Request}.

%% ====================================================================
%% Internal functions
%% ====================================================================
