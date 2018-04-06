%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
%% Código retirado do projecto SETools - SysVision Erlang Tools
%%
-module(eb_token_util).

-define(TABLES,
	[
		{
			12,
			[
				[29, 8, 27, 19, 22, 3, 20, 24, 30, 1, 14, 12, 21, 17, 9, 28, 2, 26, 11, 5, 13, 10, 16, 25, 6, 7, 18, 23, 4, 15],
				[30, 22, 11, 13, 25, 21, 20, 4, 16, 14, 23, 12, 26, 6, 28, 1, 15, 10, 24, 3, 18, 17, 8, 29, 27, 9, 2, 7, 5, 19],
				[1, 20, 8, 9, 5, 2, 16, 17, 27, 15, 13, 12, 14, 19, 10, 26, 4, 6, 11, 24, 30, 22, 25, 23, 28, 7, 18, 3, 29, 21],
				[8, 25, 15, 19, 9, 14, 18, 10, 6, 16, 27, 12, 20, 1, 2, 4, 13, 23, 22, 3, 7, 24, 26, 17, 28, 5, 30, 29, 11, 21],
				[14, 10, 8, 5, 3, 26, 9, 23, 18, 2, 16, 12, 22, 17, 30, 1, 24, 13, 15, 29, 6, 19, 28, 21, 11, 4, 25, 27, 7, 20],
				[26, 19, 17, 27, 30, 3, 9, 5, 6, 15, 23, 12, 21, 1, 13, 7, 2, 29, 8, 25, 11, 10, 4, 28, 24, 22, 18, 16, 20, 14],
				[1, 17, 30, 29, 23, 25, 15, 10, 8, 9, 20, 12, 18, 16, 13, 26, 27, 4, 28, 11, 19, 22, 5, 21, 7, 24, 6, 3, 2, 14],
				[27, 9, 23, 13, 7, 24, 10, 29, 30, 3, 25, 12, 28, 17, 5, 19, 26, 16, 2, 21, 20, 6, 18, 11, 14, 15, 4, 22, 8, 1],
				[23, 7, 29, 26, 27, 17, 24, 11, 16, 13, 21, 12, 1, 8, 10, 4, 22, 28, 14, 19, 9, 25, 15, 5, 30, 3, 6, 20, 2, 18],
				[13, 7, 5, 27, 22, 25, 26, 17, 4, 3, 8, 12, 29, 11, 23, 24, 9, 18, 14, 19, 30, 20, 6, 2, 16, 1, 15, 28, 21, 10]
			]
		},
		{
			22,
			[
				[9, 3, 8, 12, 30, 6, 26, 20, 23, 24, 17, 19, 1, 15, 10, 11, 4, 13, 21, 7, 25, 22, 28, 14, 5, 2, 18, 27, 16, 29],
				[1, 10, 25, 28, 30, 8, 19, 18, 24, 20, 5, 6, 12, 2, 26, 29, 23, 16, 14, 17, 13, 22, 4, 7, 27, 3, 15, 11, 21, 9],
				[20, 19, 10, 30, 15, 13, 1, 7, 18, 8, 6, 23, 29, 2, 5, 25, 4, 11, 14, 9, 21, 22, 17, 27, 3, 16, 24, 28, 26, 12],
				[5, 28, 14, 6, 7, 21, 24, 23, 19, 4, 9, 13, 27, 10, 25, 8, 30, 11, 12, 2, 29, 22, 18, 26, 16, 20, 3, 17, 15, 1],
				[21, 20, 8, 5, 30, 6, 3, 24, 10, 27, 14, 12, 13, 11, 9, 7, 15, 19, 26, 16, 23, 22, 17, 18, 25, 1, 2, 29, 28, 4],
				[3, 28, 25, 9, 21, 17, 4, 11, 7, 20, 27, 14, 29, 16, 5, 13, 10, 26, 23, 24, 19, 22, 18, 1, 15, 8, 12, 6, 2, 30],
				[4, 20, 5, 7, 23, 3, 14, 29, 16, 19, 8, 9, 10, 18, 12, 26, 13, 27, 11, 1, 28, 22, 21, 17, 24, 15, 25, 30, 6, 2],
				[9, 1, 17, 12, 6, 8, 20, 29, 28, 30, 11, 5, 18, 4, 24, 27, 19, 3, 25, 26, 2, 22, 16, 10, 23, 21, 7, 13, 14, 15],
				[2, 10, 23, 6, 13, 7, 25, 17, 30, 26, 1, 8, 9, 29, 20, 28, 24, 12, 11, 19, 3, 22, 21, 18, 16, 4, 27, 5, 14, 15],
				[18, 25, 11, 12, 10, 28, 20, 6, 16, 14, 4, 17, 8, 24, 3, 29, 21, 27, 9, 15, 30, 22, 1, 26, 2, 13, 7, 19, 23, 5]
			]
		},
		{
			4,
			[
				[8, 14, 11, 4, 24, 13, 1, 2, 26, 30, 10, 16, 21, 9, 15, 20, 22, 3, 28, 18, 17, 7, 12, 19, 29, 25, 5, 27, 23, 6],
				[9, 26, 22, 4, 24, 28, 7, 13, 23, 11, 3, 18, 14, 1, 20, 25, 8, 15, 30, 2, 29, 6, 27, 10, 19, 12, 21, 5, 17, 16],
				[18, 5, 3, 4, 8, 30, 6, 23, 15, 28, 17, 12, 1, 20, 10, 14, 26, 19, 2, 13, 29, 16, 27, 22, 9, 25, 24, 21, 7, 11],
				[24, 16, 21, 4, 2, 8, 28, 13, 29, 11, 6, 26, 19, 3, 17, 14, 12, 22, 7, 27, 15, 23, 5, 9, 18, 25, 10, 20, 30, 1],
				[3, 14, 18, 4, 9, 1, 13, 23, 2, 8, 10, 28, 12, 22, 24, 15, 21, 29, 7, 16, 26, 20, 17, 30, 27, 19, 11, 25, 5, 6],
				[22, 30, 14, 4, 26, 28, 19, 25, 10, 5, 9, 20, 27, 13, 7, 29, 6, 2, 11, 15, 21, 1, 8, 23, 18, 12, 24, 16, 17, 3],
				[15, 6, 26, 4, 27, 22, 8, 24, 10, 16, 17, 1, 20, 18, 30, 3, 12, 7, 5, 11, 23, 9, 14, 19, 28, 29, 21, 2, 25, 13],
				[19, 14, 10, 4, 22, 15, 8, 3, 5, 2, 13, 20, 26, 25, 27, 7, 23, 16, 6, 28, 17, 18, 12, 1, 21, 11, 24, 9, 29, 30],
				[1, 30, 6, 4, 29, 13, 24, 17, 22, 12, 23, 19, 11, 2, 3, 21, 25, 27, 26, 8, 5, 10, 9, 15, 28, 14, 18, 16, 7, 20],
				[16, 7, 25, 4, 27, 13, 20, 26, 18, 8, 23, 30, 11, 19, 15, 3, 5, 24, 21, 10, 1, 14, 9, 29, 2, 6, 22, 12, 17, 28]
			]
		}
	]
).

-define(ID_MULTIPLIER, 10000000000).
-define(RANDOM_MIN, 10000000000000000000).
-define(RANDOM_MAX, 99999999999999999999).
-define(NUMBER_MULTIPLIER, 100000000000000000000000000000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate_token/1, get_id_from_token/1]).

generate_token(SomeId) when is_integer(SomeId) andalso SomeId > 0 andalso SomeId < ?ID_MULTIPLIER ->
	try
		Number = crypto:rand_uniform(?RANDOM_MIN, ?RANDOM_MAX),
		BigNumber = Number * ?ID_MULTIPLIER + SomeId,
		IntegerList = get_digits_list(BigNumber),
		Scrambled = scramble(IntegerList),
		FinalInteger = get_integer_from_digits_list(Scrambled),
		Token = integer_to_binary(FinalInteger, 36),
		{ok, Token}
	catch
		Error ->
			error_logger:error_msg("~p:generate_token(~p): Error generating a token: ~p", [?MODULE, SomeId, Error]),
			error
	end;
generate_token(_InvalidId) ->
	error.

get_id_from_token(Token) ->
	try
		BigNumber = binary_to_integer(Token, 36),
		IntegerList = get_digits_list(BigNumber),
		Unscrambled = unscramble(IntegerList),
		FinalInteger = get_integer_from_digits_list(Unscrambled),
		Id = FinalInteger rem ?ID_MULTIPLIER,
		{ok, Id}
	catch
		Error ->
			error_logger:error_msg("~p:get_id_from_token(~p): Error getting the ID from the token: ~p", [?MODULE, Token, Error]),
			error
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

scramble(Input) ->
	IndexSearchFunction = fun(Line, Position) -> lists:nth(Position, Line) end,
	swap(?TABLES, IndexSearchFunction, Input).

unscramble(Input) ->
	IndexSearchFunction = fun(Line, Position) -> index_of(Line, Position) end,
	swap(lists:reverse(?TABLES), IndexSearchFunction, Input).

index_of(Line, Position) -> index_of(Line, Position, 1).
index_of([], _, _) -> -1;
index_of([Position|_], Position, Index) -> Index;
index_of([_|Rest], Position, Index) -> index_of(Rest, Position, Index + 1).

swap([], _IndexSearchFunction, Result) -> Result;
swap([{Position, Table}|Rest], IndexSearchFunction, Input) ->
	Key = lists:nth(Position, Input),
	Line = lists:nth(Key + 1, Table),
	Output = swap(Input, Line, 1, IndexSearchFunction, []),
	swap(Rest, IndexSearchFunction, Output).

swap(Input, Line, Position, IndexSearchFunction, Processed) when Position =< length(Input) ->
	Index = IndexSearchFunction(Line, Position),
	NewValue = lists:nth(Index, Input),
	swap(Input, Line, Position + 1, IndexSearchFunction, [NewValue|Processed]);
swap(_, _, _, _, Processed) -> lists:reverse(Processed).

get_digits_list(BigNumber) -> get_digits_list(BigNumber, ?NUMBER_MULTIPLIER, []).
get_digits_list(_Number, Divisor, Acc) when Divisor < 1 -> lists:reverse(Acc);
get_digits_list(Number, Divisor, Acc) ->
	Part = Number div Divisor,
	Rest = Number rem Divisor,
	get_digits_list(Rest, Divisor div 10, [Part|Acc]).

get_integer_from_digits_list(List) ->
	{0, FinalInteger} = lists:foldl(fun(Digit, {Multiplier, Acc}) ->
	                                    NewAcc = Digit * Multiplier + Acc,
	                                    {Multiplier div 10, NewAcc}
	                                end, {?NUMBER_MULTIPLIER, 0}, List),
	FinalInteger.
