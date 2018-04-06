%%
%% Copyright 2017 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_cache_util).

-include("eb_constants.hrl").

%% ====================================================================
%% Constants
%% ====================================================================
-define(CACHE_DB_PARAMETERIZATION, eb_cache_db_parameterization).
-define(CACHE_API_KEYS, eb_cache_api_keys).
-define(CACHE_GLOBAL, eb_cache_global).

-record(cache_api_key, {id_api_client, ip_address}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0, get_db_parameter/1, get_db_parameter/3, get_db_parameters/3, flush_db_parameter_cache/0, get_api_key/1, get_global/1, put_global/2]).

init() ->
	% DB parameterization cache
	ok = create_cache(?CACHE_DB_PARAMETERIZATION, build_cache_options(fun load_db_parameter/1, 600, 60)),
	% API Key cache
	ok = create_cache(?CACHE_API_KEYS, build_cache_options(fun load_api_key/1, 600, 60)),
	% Global application cache
	ok = create_cache(?CACHE_GLOBAL, build_cache_options(none, 0, none)),
	ok.

get_db_parameter(Parameter) ->
	case g_cache:get(?CACHE_DB_PARAMETERIZATION, Parameter) of
		{ok, Value, _Version} -> Value;
		Other -> Other
	end.

% Use this one inside try .. catch
get_db_parameter(Parameter, Module, Function) ->
	case get_db_parameter(Parameter) of
		{ok, Value} -> Value;
		Other ->
			error_logger:error_msg("~p:~p(...): Unable to get parameter (~p): ~p~n", [Module, Function, Parameter, Other]),
			throw(missing_db_parameter)
	end.

get_db_parameters(Parameters, Module, Function) -> get_db_parameters(Parameters, [], Module, Function).

flush_db_parameter_cache() ->
	g_cache:flush(?CACHE_DB_PARAMETERIZATION),
	ok.

get_api_key(ApiKey) ->
	case g_cache:get(?CACHE_API_KEYS, ApiKey) of
		{ok, Entry, _Version} -> {ok, Entry};
		not_found -> not_found;
		_ -> system_error
	end.

get_global(Key) ->
	case g_cache:get(?CACHE_GLOBAL, Key) of
		{ok, Entry, _Version} -> {ok, Entry};
		not_found -> not_found;
		_ -> system_error
	end.

put_global(Key, Value) ->
	case g_cache:store(?CACHE_GLOBAL, Key, Value) of
		{ok, _Version} -> ok;
		_ -> system_error
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
build_cache_options(GetValueFun, MaxAge, PurgeInterval) ->
	[{get_value_function, GetValueFun},
	 {max_age, MaxAge},
	 {purge_interval, PurgeInterval},
	 {cluster_nodes, all}].

create_cache(CacheName, Options) ->
	case gibreel:create_cache(CacheName, Options) of
		ok -> ok;
		{error, duplicated} -> ok;
		Other -> Other
	end.

% gibreel only stores a value in cache if it is not the atom error or not_found
load_db_parameter(Parameter={_Id, string}) -> load_db_parameter(Parameter, fun erlang:binary_to_list/1);
load_db_parameter(Parameter={_Id, integer}) -> load_db_parameter(Parameter, fun erlang:binary_to_integer/1);
load_db_parameter(Parameter={_Id, _OtherType}) -> load_db_parameter(Parameter, fun(Value) -> Value end).

load_db_parameter({Id, Type}, ParsingFunction) ->
	case eb_db_util:execute({get_parameterization, Id}) of
		#parameterization{value=BinValue} = Param ->
			try
				ParsedValue = ParsingFunction(BinValue),
				{ok, ParsedValue}
			catch
				error: badarg ->
					error_logger:error_msg("~p:load_db_parameter({~w, ~w}, ...): Invalid parameter for the type. DB Record: ~p~n", [?MODULE, Id, Type, Param]),
					error
			end;
		Error ->
			error_logger:error_msg("~p:load_db_parameter({~w, ~w}, ...): Error getting the parameter: ~w~n", [?MODULE, Id, Type, Error]),
			error
	end.

get_db_parameters([], Values, _Module, _Function) -> lists:reverse(Values);
get_db_parameters([Parameter|Rest], Values, Module, Function) ->
	case get_db_parameter(Parameter) of
		{ok, Value} -> get_db_parameters(Rest, [Value|Values], Module, Function);
		Other ->
			error_logger:error_msg("~p:~p(...): Unable to get parameter ~p: ~p~n", [Module, Function, Parameter, Other]),
			{nok, missing_db_parameter}
	end.

load_api_key(Key) ->
	case eb_db_util:execute({get_active_api_key, Key}) of
		#api_key{id_api_client=ApiClientId, domain=Domain} ->
			% Get the current IP address for the domain
			case inet:getaddr(binary_to_list(Domain), inet) of
				{ok, IP} ->
					IpAddress = list_to_binary(inet:ntoa(IP)),
					#cache_api_key{id_api_client=ApiClientId, ip_address=IpAddress};
				_ -> error
			end;
		not_found -> not_found;
		_ -> error
	end.
