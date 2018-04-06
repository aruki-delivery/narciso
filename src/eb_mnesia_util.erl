%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
%% Código retirado do projecto SETools - SysVision Erlang Tools
%%
-module(eb_mnesia_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([delete/2, select_delete/2, write/2, select/2, read/2, copy_cache_tables/2]).

delete(Table, Key) ->
	mnesia:transaction(fun() -> mnesia:delete(Table, Key, write) end),
	ok.

% MatchSpec has to return a list of Keys
select_delete(Table, MatchSpec) ->
	mnesia:transaction(
		fun() ->
			case mnesia:select(Table, MatchSpec) of
				Keys when is_list(Keys) ->
					[mnesia:delete(Table, Key, write) || Key <- Keys],
					removed;
				_Error -> error
			end
		end
	),
	ok.

write(Table, Record) ->
	mnesia:transaction(fun() -> mnesia:write(Table, Record, write) end),
	ok.

select(Table, MatchSpec) ->
	case mnesia:transaction(fun() -> mnesia:select(Table, MatchSpec) end) of
		{atomic, Results} -> Results;
		Other -> Other
	end.

read(Table, Key) ->
	case mnesia:transaction(fun() -> mnesia:read(Table, Key) end) of
		{atomic, Results} -> Results;
		Other -> Other
	end.

copy_cache_tables(FromNode, Table) ->
	% If the is a node allready running, we copy the table to our node
	mnesia:change_config(extra_db_nodes, [FromNode]),
	mnesia:add_table_copy(Table, node(), ram_copies).
