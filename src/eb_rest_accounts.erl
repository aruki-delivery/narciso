%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_rest_accounts).

-include("eb_constants.hrl").

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% GET all accounts
%% GET /accounts
handle(<<"GET">>, [], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_api_accounts:get_accounts(RequestingUserTypeId) of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
		{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get other accounts information.">>, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get the account information
%% GET /accounts/<id>
handle(<<"GET">>, [AccountId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(AccountId) of
		{ok, TargetAccountId} ->
			case eb_api_accounts:get_account(RequestingUserId, RequestingUserTypeId, TargetAccountId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Account ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this account information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Account ID.">>, Request)
	end;

%% Get a restaurant slots availability
%% GET /accounts/<id>/availability
handle(<<"GET">>, [AccountId, <<"availability">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	{Args, Request1} = kb_action_helper:get_args(Request),  % Get query parameters
	try
		TargetAccountId =
			case eb_util:binary_to_int(AccountId) of
				{ok, IntAccountId} -> IntAccountId;
				_ -> throw(invalid_account_id)
			end,
		IdType =
			case eb_rest_util:get_integer_arg_value(Args, <<"idType">>) of
				{ok, IdTypeArg} -> IdTypeArg;
				_ -> throw(invalid_parameters)
			end,
		case eb_api_accounts:get_account_slots(RequestingUserId, RequestingUserTypeId, TargetAccountId, IdType) of
			{ok, Result} -> eb_rest_util:return_success(Result, Request);
			{nok, ParameterError} -> throw(ParameterError);
			_ -> throw(unexpected)
		end
	catch
		throw:invalid_account_id -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Account ID.">>, Request1);
		throw:invalid_parameters -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"GET /accounts/availability has been called with invalid parameters.">>, Request1);
		throw:forbidden -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change account.">>, Request1);
		throw:not_found -> eb_rest_util:return_error(404, <<"not_found">>, <<"Account ID not found.">>, Request1);
		throw:version -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong account version">>, Request1);
		throw:_ -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the account.">>, Request1)
	end;

%% Change a restaurant occupancy rate
%% PATCH /accounts/<id>/occupancy
handle(<<"PATCH">>, [AccountId, <<"occupancy">>], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	{RecordResp, ChangeOccupancy, Request1} = eb_rest_util:get_record_parameter(Request, #change_occupancy{}),
	try
		TargetAccountId =
			case eb_util:binary_to_int(AccountId) of
				{ok, IntAccountId} -> IntAccountId;
				_ -> throw(invalid_account_id)
			end,
		RecordResp =:= ok orelse throw(invalid_record),
		case eb_api_accounts:change_occupancy(RequestingUserTypeId, TargetAccountId, ChangeOccupancy) of
			{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
			{nok, ParameterError} -> throw(ParameterError);
			_ -> throw(unexpected)
		end
	catch
		throw:invalid_account_id -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Account ID.">>, Request);
		throw:invalid_record -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change occupancy record is invalid.">>, Request1);
		throw:invalid_occupancy_type -> eb_rest_util:return_error(400, <<"invalid_occupancy_type">>, <<"The change occupancy record has invalid parameters.">>, Request1);
		throw:invalid_parameters -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change occupancy record has invalid parameters.">>, Request1);
		throw:forbidden -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change account.">>, Request1);
		throw:not_found -> eb_rest_util:return_error(404, <<"not_found">>, <<"Account ID not found.">>, Request1);
		throw:version -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong account version">>, Request1);
		throw:_ -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the account.">>, Request1)
	end;

%handle(_Method, _Path, Request) ->
handle(Method, Path, Request) ->
	Attributes = kb_action_helper:get_attributes(Request),
	error_logger:info_msg("\nMethod [~p]\nPath [~p]\nRequest [~p]\nAttributes [~p]\n", [Method, Path, Request, Attributes]),
	{raw, 404, [], "Request malandro...", Request}.

%% ====================================================================
%% Internal functions
%% ====================================================================

