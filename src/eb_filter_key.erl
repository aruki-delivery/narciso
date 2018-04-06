%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_filter_key).

-behaviour(kb_filter_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

handle(_Method, _Path, Request) ->
	% Verify API Key
	case eb_rest_util:get_api_key(Request) of
		{ok, ApiKey, Request1} ->
			case eb_api_session:validate_api_key(ApiKey) of
				ok -> {next, [], Request1};
				{nok, invalid_api_key} -> eb_rest_util:return_error(401, <<"api_key">>, <<"Invalid api key">>, Request1);
				_ -> eb_rest_util:return_error(500, <<"api_key">>, <<"Unexpected system error">>, Request1)
			end;
		{missing, Request1} -> eb_rest_util:return_error(403, <<"api_key">>, <<"Missing api key header">>, Request1);
		{invalid, Request1} -> eb_rest_util:return_error(400, <<"api_key">>, <<"Invalid request: invalid api key header">>, Request1)
	end.
