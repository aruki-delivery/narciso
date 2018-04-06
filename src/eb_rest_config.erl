%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_rest_config).

-include("eb_constants.hrl").

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%%
%% Parameterization operations
%%

%% Get all parameterizations
%% GET /config/parameterizations
handle(<<"GET">>, [<<"parameterizations">>], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_api_config:get_parameterizations(RequestingUserTypeId) of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid parameters.">>, Request);
		{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get parameterization information.">>, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request)
	end;

%% Get a parameterization
%% GET /config/parameterizations/<id>
handle(<<"GET">>, [<<"parameterizations">>, Id], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(Id) of
		{ok, ParameterizationId} ->
			case eb_api_config:get_parameterization(RequestingUserTypeId, ParameterizationId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Parameterization ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid parameterization.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get parameterization information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid parameterization ID.">>, Request)
	end;

%% Modify a parameterization
%% PATCH /config/parameterizations/<id>
handle(<<"PATCH">>, [<<"parameterizations">>, Id], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(Id) of
		{ok, ParameterizationId} ->
			case eb_rest_util:get_record_parameter(Request, #change_parameterization{}) of
				{ok, ChangeParameterization, Request1} ->
					case eb_api_config:update_parameterization(RequestingUserTypeId, ParameterizationId, ChangeParameterization) of
						{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Parameterization ID not found.">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change parameterization information.">>, Request1);
						{nok, ParameterError} when ParameterError =:= missing_values orelse ParameterError =:= invalid_parameters ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The update parameterization record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
						{nok, version} -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong parameterization version.">>, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request1)
					end;
				{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The update parameterization record is invalid.">>, Request1)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid parameterization ID.">>, Request)
	end;

%%
%% Token type operations
%%

%% Get all parameterizations
%% GET /config/token-types
handle(<<"GET">>, [<<"token-types">>], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_api_config:get_token_types(RequestingUserTypeId) of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid parameters.">>, Request);
		{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get token type information.">>, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request)
	end;

%% Get a token type
%% GET /config/token-types/<id>
handle(<<"GET">>, [<<"token-types">>, Id], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(Id) of
		{ok, TokenTypeId} ->
			case eb_api_config:get_token_type(RequestingUserTypeId, TokenTypeId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Token type ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid token type.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get token type information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid token type ID.">>, Request)
	end;

%% Modify a token type
%% PATCH /config/token-types/<id>
handle(<<"PATCH">>, [<<"token-types">>, Id], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(Id) of
		{ok, TokenTypeId} ->
			case eb_rest_util:get_record_parameter(Request, #update_token_type{}) of
				{ok, UpdateTokenType, Request1} ->
					case eb_api_config:update_token_type(RequestingUserTypeId, TokenTypeId, UpdateTokenType) of
						{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Token type ID not found.">>, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change token type information.">>, Request1);
						{nok, ParameterError} when ParameterError =:= missing_values orelse
						                           ParameterError =:= invalid_parameters ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The update token type record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
						{nok, version} -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong token type version.">>, Request1);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error updating the token type.">>, Request1)
					end;
				{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The update token type record is invalid.">>, Request1)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid token type ID.">>, Request)
	end;

%% Get the configuration for the courier app
%% GET /config/apps/courier
handle(<<"GET">>, [<<"apps">>, <<"courier">>], Request) ->
	case eb_api_config:get_courier_app_config() of
		{ok, Result} -> eb_rest_util:return_success(Result, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error">>, Request)
	end;

%handle(_Method, _Path, Request) ->
handle(Method, Path, Request) ->
	Attributes = kb_action_helper:get_attributes(Request),
	error_logger:info_msg("\nMethod [~p]\nPath [~p]\nRequest [~p]\nAttributes [~p]\n", [Method, Path, Request, Attributes]),
	{raw, 404, [], "Request malandro...", Request}.
