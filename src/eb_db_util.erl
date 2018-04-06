%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
%% Código retirado do projecto SETools - SysVision Erlang Tools
%%
-module(eb_db_util).

-include("eb_constants.hrl").

-define(DB_POOL_NAME, eb_db_pool).

%% ====================================================================
%% API functions
%% ====================================================================
-export([execute/1, execute/2, get_parameter_values/1]).

execute(Command) -> execute(Command, 4000). % Less than the default 5000 ms timeout for gen_server:call

execute(Command, Timeout) ->
	case poolboy:checkout(?DB_POOL_NAME) of
		full ->
			error_logger:error_msg("~p:execute(~p, ~p): Unable to get a Worker from pool ~p.", [?MODULE, Command, Timeout, ?DB_POOL_NAME]),
			system_error;
		Worker ->
			Reply = try
			            gen_server:call(Worker, Command, Timeout) 
			        catch
			            Error:Reason ->
			                error_logger:error_msg("~p:execute(~p, ~p): Unexpected error executing worker: {~p, ~p}.", [?MODULE, Command, Timeout, Error, Reason]),
			                system_error
			         end,
			poolboy:checkin(?DB_POOL_NAME, Worker),
			Reply
	end.

get_parameter_values(Parameters) ->
	get_parameter_values(Parameters, []).

%% ====================================================================
%% Internal functions
%% ====================================================================
get_parameter_values([], Values) ->
	lists:reverse(Values);
get_parameter_values([Parameter|Rest], Values) ->
	case get_parameter_value(Parameter) of
		{ok, Value} -> get_parameter_values(Rest, [Value|Values]);
		_ -> error
	end.

get_parameter_value(Parameter={_Id, binary}) -> get_parameter_value(Parameter, fun(Value) -> Value end);
get_parameter_value(Parameter={_Id, string}) -> get_parameter_value(Parameter, fun(Value) -> binary_to_list(Value) end);
get_parameter_value(Parameter={_Id, integer}) -> get_parameter_value(Parameter, fun(Value) -> binary_to_integer(Value) end).

get_parameter_value({Id, Type}, ParsingFunction) ->
	case execute({get_parameterization, Id}) of
		#parameterization{value=BinValue} ->
			try
				ParsedValue = ParsingFunction(BinValue),
				{ok, ParsedValue}
			catch
				error: badarg ->
					error_logger:error_msg("~p:get_parameter_value({~p, ~p}, ...): Invalid parameter for the type.", [?MODULE, Id, Type]),
					invalid_parameter
			end;
		Error ->
			error_logger:error_msg("~p:get_parameter_value({~p, ~p}, ...): Error getting the parameter: ~p", [?MODULE, Id, Type, Error]),
			Error
	end.
