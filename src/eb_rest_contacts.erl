%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_rest_contacts).

-include("eb_constants.hrl").

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Get all contact requests
%% GET /contacts
handle(<<"GET">>, [], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_api_contacts:get_contact_requests(RequestingUserTypeId) of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request)
	end;

%% Create contact request 
%% POST /contacts
handle(<<"POST">>, [], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% These attributes can be undefined
	case eb_rest_util:get_record_parameter(Request, #new_contact_request{}) of
		{ok, NewContactRequest, Request1} ->
			case eb_api_contacts:create_contact_request(RequestingUserId, RequestingUserTypeId, NewContactRequest) of
				{ok, ContactRequestId} -> eb_rest_util:return_json(201, [{<<"contact_request_id">>, ContactRequestId}], Request1);
				{nok, ParameterError} when ParameterError =:= missing_values orelse
				                           ParameterError =:= invalid_parameters ->
					BinParameterError = atom_to_binary(ParameterError, latin1),
					Description = <<<<"The contact request record has invalid parameters: ">>/binary, BinParameterError/binary>>,
					eb_rest_util:return_error(400, ParameterError, Description, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error creating the contact request.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The contact request record is invalid.">>, Request1)
	end;

%% Get the contact statuses
%% GET /contacts/statuses
handle(<<"GET">>, [<<"statuses">>], Request) ->
	case eb_api_contacts:get_contact_request_statuses() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request)
	end;

%% Get contact request
%% GET /contacts/<id>
handle(<<"GET">>, [ContactRequestId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(ContactRequestId) of
		{ok, ParamContactRequestId} ->
			case eb_api_contacts:get_contact_request(RequestingUserId, RequestingUserTypeId, ParamContactRequestId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Contact request ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this contact request information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_credentials">>, <<"Invalid Contact Request ID.">>, Request)
	end;

%% Change contact request
%% PATCH /contacts/<id>
handle(<<"PATCH">>, [ContactRequestId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_rest_util:get_record_parameter(Request, #change_contact_request{}) of
		{ok, ChangeContactRequest, Request1} ->
			case eb_util:binary_to_int(ContactRequestId) of
				{ok, ParamContactRequestId} ->
					case eb_api_contacts:update_contact_request(RequestingUserId, RequestingUserTypeId, ParamContactRequestId, ChangeContactRequest) of
						{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
						{nok, version} -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong contact request version">>, Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Contact request ID not found.">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this contact request information.">>, Request1);
						{nok, ParameterError} when ParameterError =:= unknown_contact_request_status orelse
				              		               ParameterError =:= invalid_contact_request_status orelse
												   ParameterError =:= missing_values orelse
												   ParameterError =:= invalid_parameters ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The contact request record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, ParameterError, Description, Request1);						
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_credentials">>, <<"Invalid Contact Request ID.">>, Request1)
			end;
		{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change location record is invalid.">>, Request1)
	end;

%% Delete contact request
%% DELETE /contacts/<id>
handle(<<"DELETE">>, [ContactRequestId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	% Get query parameters
	{Args, Request1} = kb_action_helper:get_args(Request),
	case eb_rest_util:get_integer_arg_value(Args, <<"version">>) of
		{ok, Version} ->
			case eb_util:binary_to_int(ContactRequestId) of
				{ok, ParamContactRequestId} ->
					case eb_api_contacts:delete_contact_request(RequestingUserId, RequestingUserTypeId, ParamContactRequestId, Version) of
						ok -> eb_rest_util:return_success(Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Contact request ID not found.">>, Request1);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this contact request information.">>, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request1)
					end;
				_Error -> eb_rest_util:return_error(400, <<"invalid_credentials">>, <<"Invalid Contact Request ID.">>, Request1)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The version parameter is invalid.">>, Request1)
	end;

handle(Method, Path, Request) ->
	Attributes = kb_action_helper:get_attributes(Request),
	error_logger:info_msg("\nMethod [~p]\nPath [~p]\nRequest [~p]\nAttributes [~p]\n", [Method, Path, Request, Attributes]),
	{raw, 404, [], "Request malandro...", Request}.
