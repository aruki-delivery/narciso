%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
%% Código retirado do projecto SETools - SysVision Erlang Tools
%%
-module(eb_util).

-define(PATTERN_EMAIL_POINT_FOLLOWED_BY_POINT, ".+\\.\\..+").
-define(PATTERN_EMAIL_POINT_FOLLOWED_BY_AT, ".+\\.@.+").
-define(PATTERN_EMAIL_AT_FOLLOWED_BY_POINT, ".+@\\..+").
-define(PATTERN_EMAIL_REGULAR_PATTERN, "^[a-zA-Z0-9]+[a-zA-Z0-9+_.-]*@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,63}$").
-define(PATTERN_TRIM, "^\\s+|\\s+$").
-define(PATTERN_HOUR_MINUTE, "^([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$").

%% ====================================================================
%% API functions
%% ====================================================================
-export([binary_to_int/1, binary_to_str/1, trim/2, trim/1, is_valid_email/2, is_valid_email/1, get_trim_pattern/0, get_email_patterns/0,
         round_float/2, trunc_float/2, distance_between/4, get_hour_minute_pattern/0, is_valid_hour_minute/1,
         is_valid_hour_minute/2, hhmm_to_seconds/1, add_to_hhmm/2, hours_minutes_to_bin/2, datetime_to_yyyymmdd_hhmmss/1,
         datetime_to_string/1, localtime_to_utc/1, utc_time_offset/2, get_current/0,
         get_timestamp_as_secs_microsecs/1, split/2, is_valid_optional_email/1, get_fullname/2,
         get_epoch_timestamp/1, timestamp_from_epoch/1]).

-export([validate_size/2, val_interval/3, verify_dates/2]).

binary_to_int(BinValue) ->
	try
		IntValue = binary_to_integer(BinValue),
		{ok, IntValue}
	catch
		_: _ -> error
	end.

binary_to_str(BinValue) ->
	try
		StrValue = binary_to_list(BinValue),
		{ok, StrValue}
	catch
		_: _ -> error
	end.

get_trim_pattern() ->
	case compile_patterns([?PATTERN_TRIM]) of
		[Trim] -> {ok, Trim};
		_ -> error
	end.

get_email_patterns() ->
	case compile_patterns([?PATTERN_EMAIL_POINT_FOLLOWED_BY_POINT, ?PATTERN_EMAIL_POINT_FOLLOWED_BY_AT, ?PATTERN_EMAIL_AT_FOLLOWED_BY_POINT,
	                       ?PATTERN_EMAIL_REGULAR_PATTERN]) of
		[PointFollwedByPoint, PointFollowedByAt, AtFollowedByPoint, Regular] ->
			{ok, {PointFollwedByPoint, PointFollowedByAt, AtFollowedByPoint, Regular}};
		_ -> error
	end.

get_hour_minute_pattern() ->
	case compile_patterns([?PATTERN_HOUR_MINUTE]) of
		[HHMM] -> {ok, HHMM};
		_ -> error
	end.

trim(Binary) when is_binary(Binary) ->
	try
		{ok, TrimPattern} = get_trim_pattern(),
		trim(Binary, TrimPattern)
	catch
		_:Error ->
			error_logger:error_msg("~p:trim(~p): Error trimming: ~p", [?MODULE, Binary, Error]),
			error
	end;
trim(_Invalid) -> error.

trim(Binary, TrimPattern) when is_binary(Binary) ->
	try
		Trimmed = re:replace(Binary, TrimPattern, <<>>, [{return, binary}, global]),
		{ok, Trimmed}
	catch
		_:Error ->
			error_logger:error_msg("~p:trim(~p, ~p): Error trimming: ~p", [?MODULE, Binary, TrimPattern, Error]),
			error
	end;
trim(_Invalid, _) -> error.

is_valid_email(Email) when is_binary(Email) ->
	try
		{ok, EmailPatterns} = get_email_patterns(),
		is_valid_email(Email, EmailPatterns)
	catch
		_:Error ->
			error_logger:error_msg("~p:is_valid_email(~p): Error validating email: ~p", [?MODULE, Email, Error]),
			false
	end;
is_valid_email(_Invalid) -> false.

is_valid_email(Email, {PointFollwedByPoint, PointFollowedByAt, AtFollowedByPoint, Regular}) when is_binary(Email) ->
	try
		nomatch = re:run(Email, PointFollwedByPoint),
		nomatch = re:run(Email, PointFollowedByAt),
		nomatch = re:run(Email, AtFollowedByPoint),
		{match, [Email]} = re:run(Email, Regular, [{capture, all, binary}]),
		true
	catch
		_:_ -> false
	end;
is_valid_email(_Invalid, _) -> false.

is_valid_optional_email(undefined) -> true;
is_valid_optional_email(Email) -> is_valid_email(Email).

is_valid_hour_minute(HHMM) when is_binary(HHMM) ->
	try
		{ok, HHMMPattern} = get_hour_minute_pattern(),
		is_valid_hour_minute(HHMM, HHMMPattern)
	catch
		_:Error ->
			error_logger:error_msg("~p:is_valid_hour_minute(~p): Error validating hour/minute: ~p", [?MODULE, HHMM, Error]),
			false
	end;
is_valid_hour_minute(_Invalid) -> false.

is_valid_hour_minute(HHMM, HHMMPattern) when is_binary(HHMM) ->
	try
		{match, [HHMM]} = re:run(HHMM, HHMMPattern, [{capture, first, binary}]),
		true
	catch
		_:_ -> false
	end;
is_valid_hour_minute(_Invalid, _) -> false.

hhmm_to_seconds(HHMM) when is_binary(HHMM) ->
	case get_hh_mm(HHMM) of
		{Hours, Minutes} -> (Hours * 3600) + (Minutes * 60);
		_ -> invalid_hhmm
	end;
hhmm_to_seconds(_Other) -> invalid_hhmm.

add_to_hhmm(HHMM, MinutesToAdd) when is_binary(HHMM) andalso is_integer(MinutesToAdd) ->
	case get_hh_mm(HHMM) of
		{Hours, Minutes} ->
			Secs = calendar:time_to_seconds({Hours, Minutes, 0}),
			NewSecs = Secs + (MinutesToAdd * 60),
			{NewHours, NewMinutes, _NewSeconds} = calendar:seconds_to_time(NewSecs),
			hours_minutes_to_bin(NewHours, NewMinutes);
		_ -> invalid_hhmm
	end;
add_to_hhmm(_Other, _Minutes) -> invalid.

get_hh_mm(<<BinHours:2/binary, ":", BinMinutes:2/binary>>) ->
	case binary_to_int(BinHours) of
		{ok, Hours} ->
			case binary_to_int(BinMinutes) of
				{ok, Minutes} -> {Hours, Minutes};
				_ -> invalid_hhmm
			end;
		_ -> invalid_hhmm
	end;
get_hh_mm(_Other) -> invalid_hhmm.

hours_minutes_to_bin(Hours, Minutes) when is_integer(Hours) andalso Hours >= 0 andalso Hours < 24
                                  andalso is_integer(Minutes) andalso Minutes >= 0 andalso Minutes < 60 ->
	BinHours = time_int_to_bin(Hours),
	BinMinutes = time_int_to_bin(Minutes),
	<<BinHours/binary, ":", BinMinutes/binary>>;
hours_minutes_to_bin(_Hours, _Minuts) -> invalid_hhmm.

datetime_to_yyyymmdd_hhmmss({{Year, Month, Day}, {Hours, Minutes, Seconds}}) ->
	case verify_date({{Year, Month, Day}, {Hours, Minutes, Seconds}}) of
		ok ->
			BinYear = time_int_to_bin(Year),
			BinMonth = time_int_to_bin(Month),
			BinDay = time_int_to_bin(Day),
			BinHours = time_int_to_bin(Hours),
			BinMinutes = time_int_to_bin(Minutes),
			BinSeconds = time_int_to_bin(trunc(Seconds)),
			{<<BinYear/binary, BinMonth/binary, BinDay/binary>>, <<BinHours/binary, BinMinutes/binary, BinSeconds/binary>>};
		_ -> invalid_datetime
	end.

datetime_to_string({{Year, Month, Day}, {Hours, Minutes, Seconds}}) ->
	case verify_date({{Year, Month, Day}, {Hours, Minutes, Seconds}}) of
		ok ->
			[BinDate|[BinTime|_]] = binary:split(iso8601:format({{Year, Month, Day}, {Hours, Minutes, Seconds}}), [<<"T">>, <<".">>], [global]),
			<<BinDate/binary, " ", BinTime/binary>>;
		_ -> invalid_datetime
	end.

validate_size(Str,Tam) ->
	val_stringsize((byte_size(Str)),Tam).

val_stringsize(Size,Tam) when Size =< Tam ->
	ok;
val_stringsize(_Size,_Tam) ->
	nok.

val_interval(Num,Val1,Val2) when Val1 =< Num andalso Num =< Val2 ->
	in_interval;
val_interval(_Num,_Val1,_Val2) ->
	out_interval.

% http://www.codecodex.com/wiki/Round_a_number_to_a_specific_decimal_place#Erlang
round_float(Value, DecimalPlaces) when is_float(Value) andalso is_integer(DecimalPlaces) ->
	P = math:pow(10, DecimalPlaces),
	round(Value * P) / P.

trunc_float(Value, DecimalPlaces) when is_float(Value) andalso is_integer(DecimalPlaces) ->
	P = math:pow(10, DecimalPlaces),
	trunc(Value * P) / P.

distance_between(Latitude1, Longitude1, Latitude2, Longitude2) ->
	Lat1 = rad(Latitude1),
	Lat2 = rad(Latitude2),
	Long1 = rad(Longitude1),
	Long2 = rad(Longitude2),
	L = 2 * math:asin(math:sqrt(math:pow(math:sin((Lat1 - Lat2)/2), 2) + math:cos(Lat1) * math:cos(Lat2) * math:pow(math:sin((Long1 - Long2)/2), 2))),
	Nm = L*180*60/math:pi(),
	Nm * 1.852.

%% Timezone and others
localtime_to_utc(Localtime) ->
	case calendar:local_time_to_universal_time_dst(Localtime) of
		[] -> Localtime;
		[UTCDST, _UTC] -> UTCDST;
		[UTC] -> UTC
	end.

utc_time_offset(UTC, Localtime) ->
	UTCSecs = calendar:datetime_to_gregorian_seconds(UTC),
	LocalSecs = calendar:datetime_to_gregorian_seconds(Localtime),
	case {UTCSecs, LocalSecs} of
		{UTCSecs, LocalSecs} when UTCSecs > LocalSecs ->
			Signal = minus,
			Diff = UTCSecs - LocalSecs;
		{UTCSecs, LocalSecs} ->
			Signal = plus,
			Diff = LocalSecs - UTCSecs
	end,
	{Hours, Minutes, _Seconds} = calendar:seconds_to_time(Diff),
	{Signal, Hours, Minutes}.

get_current() ->
	Timestamp = {_, _, Micro} = erlang:timestamp(),
	{Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Timestamp),
	{Date, {Hours, Minutes, (Seconds + (Micro / 1000000))}}.

get_timestamp_as_secs_microsecs({Date, {Hours, Minutes, Seconds}}) ->
	TruncatedSeconds = trunc(Seconds),
	MicroSecondsToAdd = eb_util:round_float((Seconds - TruncatedSeconds), 6),
	SystemTimestamp = {Date, {Hours, Minutes, TruncatedSeconds}},
	CurrentSeconds = calendar:datetime_to_gregorian_seconds(SystemTimestamp),
	{CurrentSeconds, MicroSecondsToAdd}.

get_epoch_timestamp({Date, {Hours, Minutes, Seconds}}) ->
	TruncatedSeconds = trunc(Seconds),
	MiliSecondsToAdd = round((Seconds - TruncatedSeconds) * 1000),
	LocalTimestamp = {Date, {Hours, Minutes, TruncatedSeconds}},
	UTCTimestamp = eb_util:localtime_to_utc(LocalTimestamp),
	SecondsFromEpoch = calendar:datetime_to_gregorian_seconds(UTCTimestamp) - 62167219200,
	(SecondsFromEpoch * 1000) + MiliSecondsToAdd.

timestamp_from_epoch(MillisecondsFromEpochUTC) when is_integer(MillisecondsFromEpochUTC) ->
	UTCSeconds = MillisecondsFromEpochUTC div 1000,
	Fraction = (MillisecondsFromEpochUTC rem 1000) / 1000,
	UTCDateTime = calendar:gregorian_seconds_to_datetime(UTCSeconds + 62167219200),
	{Date, {Hours, Minutes, Seconds}} = calendar:universal_time_to_local_time(UTCDateTime),
	{Date, {Hours, Minutes, Seconds + Fraction}}.

split(List, Size) when is_list(List) andalso is_integer(Size) andalso Size > 0 ->
	inner_split(List, Size);
split(_, _) ->
	error.

verify_dates(undefined, undefined) -> ok;
verify_dates(DateFrom, DateTo) ->
	try
		ok = verify_date(DateFrom),
		ok = verify_date(DateTo),
		true = DateFrom =< DateTo,
		ok
	catch
		_:_ -> nok
	end.

get_fullname(null, null) -> null;
get_fullname(null, undefined) -> null;
get_fullname(null, LastName) -> LastName;
get_fullname(undefined, null) -> null;
get_fullname(undefined, undefined) -> null;
get_fullname(undefined, LastName) -> LastName;
get_fullname(FirstName, null) -> FirstName;
get_fullname(FirstName, undefined) -> FirstName;
get_fullname(FirstName, LastName) when is_binary(FirstName) andalso is_binary(LastName) -> <<FirstName/binary, " ", LastName/binary>>;
get_fullname(FirstName, LastName) when not is_binary(FirstName) andalso is_binary(LastName) ->
	get_fullname(list_to_binary(FirstName), LastName);
get_fullname(FirstName, LastName) when is_binary(FirstName) andalso not is_binary(LastName) ->
	get_fullname(FirstName, list_to_binary(LastName));
get_fullname(FirstName, LastName) -> FirstName ++ " " ++ LastName.

%% ====================================================================
%% Internal functions
%% ====================================================================
compile_patterns(Patterns) -> compile_patterns(Patterns, []).

compile_patterns([], CompiledPatterns) -> lists:reverse(CompiledPatterns);
compile_patterns([Pattern|Rest], CompiledPatterns) ->
	case re:compile(Pattern) of
		{ok, CompiledPattern} -> compile_patterns(Rest, [CompiledPattern|CompiledPatterns]);
		Error ->
			error_logger:error_msg("~p:compile_patterns([~p | ~p], ...): Error compiling pattern: ~p", [?MODULE, Pattern, Rest, Error]),
			error
	end.

rad(Decimal) ->
	Decimal * math:pi() / 180.

time_int_to_bin(Int) when Int < 10 ->
	BinInt = integer_to_binary(Int),
	<<"0", BinInt/binary>>;
time_int_to_bin(Int) -> integer_to_binary(Int).

inner_split([], _) -> {ok, []};
inner_split(List, Size) when Size >= length(List) -> {ok, [List]};
inner_split(List, Size) -> inner_split(List, Size, 0, [], []).

inner_split([], _, _, Acc, GlobalAcc) ->
	{ok, lists:reverse([lists:reverse(Acc)|GlobalAcc])};
inner_split(List, Size, Size, Acc, GlobalAcc) ->
	inner_split(List, Size, 0, [], [lists:reverse(Acc)|GlobalAcc]);
inner_split([Element|Rest], Size, Count, Acc, GlobalAcc) ->
	inner_split(Rest, Size, Count + 1, [Element|Acc], GlobalAcc).

verify_date({{Year, Month, Day}, {Hour, Minutes, Seconds}}) when is_integer(Hour) andalso Hour >= 0 andalso Hour =< 23
                                                         andalso is_integer(Minutes) andalso Minutes >= 0 andalso Minutes =< 59 
                                                         andalso (is_integer(Seconds) orelse is_float(Seconds)) andalso Seconds >= 0
                                                                                                                andalso Seconds < 60 ->
	try 
		true = calendar:valid_date(Year, Month, Day),
		ok
	catch
		_:_ -> nok
	end;
verify_date(_Other) -> nok.
