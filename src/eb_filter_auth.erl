%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_filter_auth).

-behaviour(kb_filter_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

-define(UNAUTHENTICATED_OPERATIONS, [
                                     {<<"POST">>, <<"users">>, []},
                                     {<<"POST">>, <<"users">>, [<<"new-password">>]},
                                     {<<"POST">>, <<"contacts">>, []},
                                     {<<"PUT">>, <<"users">>, []},
                                     {<<"PUT">>, <<"users">>, [<<"new-password">>]},
                                     {<<"PUT">>, <<"users">>, [<<"profile">>]},
                                     {<<"GET">>, <<"config">>, [<<"apps">>, <<"courier">>]}
                                    ]).

handle(Method, Path, Request) ->
	case eb_rest_util:get_authorization_token(Request) of
		{ok, TokenValue, Request1} ->
			case eb_api_session:validate_token(TokenValue) of
				{ok, UserId, UserType} -> {next, [UserId, UserType], Request1};
				{nok, invalid_token} -> eb_rest_util:return_error(401, <<"token">>, <<"Invalid authentication token">>, Request1);
				_Other -> eb_rest_util:return_error(500, <<"token">>, <<"Unexpected system error">>, Request1)
			end;
		{missing, Request1} ->
			ActionPrefix = kb_action_helper:get_action_prefix(Request),
			case lists:member({Method, ActionPrefix, Path}, ?UNAUTHENTICATED_OPERATIONS) of
				true -> {next, [undefined, undefined], Request1};
				false -> eb_rest_util:return_error(403, <<"token">>, <<"Missing authentication header">>, Request1)
			end;
		{invalid, Request1} -> eb_rest_util:return_error(400, <<"token">>, <<"Invalid request: invalid authentication header">>, Request1)
	end.
