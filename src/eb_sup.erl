%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> supervisor:start_link(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, DBHostname} = application:get_env(eb_wrapper, db_hostname),
	{ok, DBPort} = application:get_env(eb_wrapper, db_port),
	{ok, DBDatabase} = application:get_env(eb_wrapper, db_database),
	{ok, DBUsername} = application:get_env(eb_wrapper, db_username),
	{ok, DBPassword} = application:get_env(eb_wrapper, db_password),
	{ok, DBPoolSize} = application:get_env(eb_wrapper, db_pool_size),
	{ok, DBPoolMaxOverflow} = application:get_env(eb_wrapper, db_pool_max_overflow),
	error_logger:info_msg("~p:init([]): DB Configuration: hostname=~p; port=~p; database=~p; username=~p; password=(hidden); pool size=~p; pool max overflow=~p~n", [?MODULE, DBHostname, DBPort, DBDatabase, DBUsername, DBPoolSize, DBPoolMaxOverflow]),
	ConnectionPool = {poolboy, {poolboy, start_link, [
	                                                  [{name, {local, eb_db_pool}},
	                                                   {worker_module, eb_db_worker},
	                                                   {size, DBPoolSize},
	                                                   {max_overflow, DBPoolMaxOverflow}],
	                                                   [{db_hostname, DBHostname},
	                                                    {db_port,     DBPort},
	                                                    {db_database, DBDatabase},
	                                                    {db_username, DBUsername},
	                                                    {db_password, DBPassword}]
	                                                  ]},
	                  permanent, 2000, worker, [poolboy]},
	Batch = {eb_batch, {eb_batch, start_link, []}, permanent, 2000, worker, [eb_batch]},
	Procs = [ConnectionPool, Batch],
	{ok, {{one_for_one, 5, 10}, Procs}}.
