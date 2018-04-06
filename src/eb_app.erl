%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_app).

-include("eb_constants.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	MainNode = get_main_node(),
	{ok, Pid} = eb_sup:start_link(),
	ok = eb_cache_util:init(),
	ok = eb_notification:init(),
	ok = eb_api_deliveries:init(MainNode),
	ok = eb_api_orders:init(),
	ok = eb_api_session:init(MainNode),
	ok = start_webserver(),
	{ok, Pid}.

stop(_State) -> ok.

start_webserver() ->
	{ok, Port} = application:get_env(eb, rest_port),
	ServerConfig  = {server_config, default, [{port, Port}]},
	{ok, Server} = kill_bill:config_server(ServerConfig),
	WebAppConfig = {webapp_config, eb, [
	                                    {context, "/api"},
	                                    {action, [
	                                              {eb_filter_key, [
	                                                               {"session", eb_rest_session},
	                                                               {eb_filter_auth, [
																					 {"accounts", eb_rest_accounts},
	                                                                                 {"config", eb_rest_config},
	                                                                                 {"contacts", eb_rest_contacts},
	                                                                                 {"deliveries", eb_rest_deliveries},
                                                                                     {"orders", eb_rest_orders},
	                                                                                 {"users", eb_rest_users}
	                                                               ]}
	                                             ]}
	                                   ]}
	                ]},
	ok = kill_bill:deploy(Server, WebAppConfig),
	ok = kill_bill:start_server(Server),
	ok.

get_main_node() ->
	case init:get_argument(?START_ARG_MAIN_NODE) of
		{ok, [[TempMainNode|_]]} ->
			MainNode = list_to_atom(TempMainNode),
			% Check if it is alive or die
			pong = net_adm:ping(MainNode),
			MainNode;
		_ -> undefined
	end.