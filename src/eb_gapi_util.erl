%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
%% Google API utils
%%
%% Código retirado do projecto SETools - SysVision Erlang Tools
%%
-module(eb_gapi_util).

-include("eb_gapi_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_directions/8, get_travel_info/5, match_to_road/3, get_geocode_address/4, get_geocode_components/4, get_distance_matrix/5]).

-spec get_directions(Endpoint :: string(), ApiKey :: string(), Origin :: #gapi_waypoint{}, Destination :: #gapi_waypoint{}, Waypoints :: [#gapi_waypoint{}], TravelMode :: string(), Region :: string(), Optimize :: boolean()) -> {ok, #gapi_directions{}} | {nok, Error :: atom()}.
get_directions(Endpoint, ApiKey, Origin, Destination, Waypoints, TravelMode, Region, Optimize) ->
	EncOrigin = url_encode(Origin),
	EncDestination = url_encode(Destination),
	EncWaypoints = encode_waypoints(Waypoints, Optimize),
	RequestUrl = Endpoint ++ "json?origin=" ++ EncOrigin ++ "&destination=" ++ EncDestination ++ EncWaypoints ++ "&mode=" ++ TravelMode ++ "&alternatives=false&units=metric&region=" ++ Region ++ "&key=" ++ ApiKey,
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} -> build_directions(ResponseBody);
		Other -> {nok, Other}
	end.

-spec get_travel_info(Endpoint :: string(), ApiKey :: string(), Origin :: #gapi_waypoint{}, Destination :: #gapi_waypoint{}, TravelMode :: string()) -> {ok, #gapi_travel_info{}} | {nok, Error :: atom()}.
get_travel_info(Endpoint, ApiKey, Origin, Destination, TravelMode) ->
	EncOrigin = url_encode(Origin),
	EncDestination = url_encode(Destination),
	RequestUrl = Endpoint ++ "json?units=metric&origins=" ++ EncOrigin ++ "&destinations=" ++ EncDestination ++ "&mode=" ++ TravelMode ++ "&key=" ++ ApiKey,
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} ->
			case build_travel_info(ResponseBody) of
				{ok, [[TravelInfo]]} -> {ok, TravelInfo};
				Other -> Other
			end;
		Other -> {nok, Other}
	end.

-spec get_distance_matrix(Endpoint :: string(), ApiKey :: string(), Origins :: [#gapi_waypoint{}], Destinations :: [#gapi_waypoint{}], TravelMode :: string()) -> {ok, [[#gapi_travel_info{}]]} | {nok, Error :: atom()}.
get_distance_matrix(Endpoint, ApiKey, Origins, Destinations, TravelMode) ->
	EncOrigins = encode_origins(Origins),
	EncDestinations= encode_destinations(Destinations),
	RequestUrl = Endpoint ++ "json?units=metric&origins=" ++ EncOrigins ++ "&destinations=" ++ EncDestinations ++ "&mode=" ++ TravelMode ++ "&key=" ++ ApiKey,
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} -> build_travel_info(ResponseBody);
		Other -> {nok, Other}
	end.

-spec match_to_road(Endpoint :: string(), ApiKey :: string(), Positions :: list()) -> {ok, NewPositions :: list()} | {nok, Error :: atom()}.
match_to_road(Endpoint, ApiKey, Positions) ->
	case eb_util:split(Positions, ?GAPI_SNAP_TO_ROAD_MAX_POSITIONS) of
		{ok, Lists} -> match_to_road(Endpoint, ApiKey, Lists, []);
		Other -> {nok, Other}
	end.

-spec get_geocode_address(Endpoint :: string(), ApiKey :: string(), Region :: string(), Address :: string()) -> {ok, #gapi_geocode{}} | {nok, Error :: atom()}.
get_geocode_address(_Endpoint, ApiKey, Region, Address) when is_binary(Address) ->
	EncAddress = http_uri:encode(binary_to_list(Address)),
	RequestUrl = "https://maps.googleapis.com/maps/api/geocode/" ++ "json?address=" ++ EncAddress ++ "&region=" ++ Region ++ "&key=" ++ ApiKey,
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} -> build_geocode(ResponseBody);
		Other -> {nok, Other}
	end.

-spec get_geocode_components(Endpoint :: string(), ApiKey :: string(), Region :: string(), Address :: list()) -> {ok, #gapi_geocode{}} | {nok, Error :: atom()}.
get_geocode_components(_Endpoint, ApiKey, Region, AddressComponents) when is_list(AddressComponents) ->
	EncAddressComponents = url_encode(AddressComponents),
	RequestUrl = "https://maps.googleapis.com/maps/api/geocode/" ++ "json?components=" ++ EncAddressComponents ++ "&region=" ++ Region ++ "&key=" ++ ApiKey,
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} -> build_geocode(ResponseBody);
		Other -> {nok, Other}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
match_to_road(_Endpoint, _ApiKey, [], Processed) ->
	% Reverse and flatten Processed list
	Positions = lists:flatten(lists:reverse(Processed)),
	Distance = calculate_distance(Positions),
	DistanceInMeters = round(Distance * 1000),
	Route = eb_polyline_util:encode(Positions),
	#gapi_route{distance=DistanceInMeters, route=Route};
match_to_road(Endpoint, ApiKey, [Positions|Rest], Processed) ->
	EncodedPositions = [encode_position(Latitude, Longitude) || #gapi_position{latitude=Latitude, longitude=Longitude} <- Positions],
	RequestUrl = Endpoint ++ "?interpolate=true&key="++ ApiKey ++ "&path=" ++ string:join(EncodedPositions, "|"),
	HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
	Options = [{sync, true}, {full_result, false}],
	case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
		{ok, {200, ResponseBody}} ->
			case build_position_list(ResponseBody) of
				{ok, NewPositions} -> match_to_road(Endpoint, ApiKey, Rest, [NewPositions|Processed]);
				Other -> Other
			end;
		Other -> {nok, Other}
	end.

url_encode(#gapi_waypoint{latitude=Latitude, longitude=Longitude}) when is_float(Latitude) andalso is_float(Longitude) ->
	encode_position(Latitude, Longitude);
url_encode(#gapi_waypoint{address=Address}) when is_binary(Address) -> http_uri:encode(binary_to_list(Address));
url_encode(AddressComponents) when is_list(AddressComponents) -> encode_address_components(AddressComponents, "").

encode_waypoints(Waypoints, Optimize) when is_list(Waypoints) -> encode_waypoints(Waypoints, Optimize, "").

encode_waypoints([], _Optimize, EncodedWaypointsString) -> EncodedWaypointsString;
encode_waypoints([Waypoint|Rest], Optimize, "") ->
	OptimizePart = get_optimize_param(Optimize),
	encode_waypoints(Rest, Optimize, "&waypoints=" ++ OptimizePart ++ url_encode(Waypoint));
encode_waypoints([Waypoint|Rest], Optimize, EncodedWaypointsString) -> encode_waypoints(Rest, Optimize, EncodedWaypointsString ++ "|" ++ url_encode(Waypoint)).

encode_origins(Waypoints) when is_list(Waypoints) -> encode_other_waypoints(Waypoints, "").
encode_destinations(Waypoints) when is_list(Waypoints) -> encode_other_waypoints(Waypoints, "").

encode_other_waypoints([], EncodedWaypointsString) -> EncodedWaypointsString;
encode_other_waypoints([Waypoint|Rest], "") ->
	encode_other_waypoints(Rest, url_encode(Waypoint));
encode_other_waypoints([Waypoint|Rest], EncodedWaypointsString) ->
	encode_other_waypoints(Rest, EncodedWaypointsString ++ "|" ++ url_encode(Waypoint)).

encode_position(Latitude, Longitude) ->
	L1 = float_to_list(Longitude, [{decimals, 8}]),
	L2 = [$, | L1],
	http_uri:encode(lists:append(float_to_list(Latitude, [{decimals, 8}]), L2)).

encode_address_components([], EncodedComponentsString) -> EncodedComponentsString;
encode_address_components([#gapi_address_component{component=Component, value=Value}|Rest], "") ->
	encode_address_components(Rest, binary_to_list(Component) ++ ":" ++ url_encode(#gapi_waypoint{address=Value}));
encode_address_components([#gapi_address_component{component=Component, value=Value}|Rest], EncodedComponentsString) ->
	encode_address_components(Rest, EncodedComponentsString ++ "|" ++ binary_to_list(Component) ++ ":" ++ url_encode(#gapi_waypoint{address=Value})).

get_optimize_param(true) -> "optimize:true|";
get_optimize_param(_Other) -> "".

build_directions(ResponseBody) ->
	JSON = jsondoc:decode(ResponseBody),
	parse_directions(JSON).

parse_directions({Elements}) when is_list(Elements) -> parse_directions(Elements, #gapi_directions{}).

parse_directions([], GAPIDirections) -> {ok, GAPIDirections};
parse_directions([Element|Rest], GAPIDirections) ->
	case parse_element(Element, GAPIDirections) of
		{nok, Error} -> {nok, Error};
		NewGAPIDirections -> parse_directions(Rest, NewGAPIDirections)
	end.

parse_element({<<"routes">>, Routes}, GAPIDirections) -> parse_routes(Routes, GAPIDirections);
parse_element({<<"status">>, <<"OK">>}, GAPIDirections) -> GAPIDirections;
parse_element({<<"status">>, <<"NOT_FOUND">>}, _GAPIDirections) -> {nok, invalid_location};
parse_element({<<"status">>, <<"ZERO_RESULTS">>}, _GAPIDirections) -> {nok, no_route};
parse_element({<<"status">>, <<"MAX_WAYPOINTS_EXCEEDED">>}, _GAPIDirections) -> {nok, max_waypoints_exceeded};
parse_element({<<"status">>, Unknown}, _GAPIDirections) ->
	error_logger:info_msg("~p:parse_element(..., ~p): Unknown value\n", [?MODULE, Unknown]),
	{nok, error};
parse_element({_Other, _Ignore}, GAPIDirections) -> GAPIDirections.

parse_routes([], GAPIDirections) -> GAPIDirections;
parse_routes([Route|Rest], GAPIDirections) ->
	NewGAPIDirections = parse_route(Route, GAPIDirections),
	parse_routes(Rest, NewGAPIDirections).

parse_route({Elements}, GAPIDirections) when is_list(Elements) -> parse_route_elements(Elements, GAPIDirections).

parse_route_elements([], GAPIDirections) -> GAPIDirections;
parse_route_elements([Element|Rest], GAPIDirections) ->
	NewGAPIDirections = parse_route_element(Element, GAPIDirections),
	parse_route_elements(Rest, NewGAPIDirections).

parse_route_element({<<"legs">>, Legs}, GAPIDirections) -> parse_route_legs(Legs, no_address, GAPIDirections);
parse_route_element({<<"overview_polyline">>, {[{<<"points">>, Route}]}}, GAPIDirections=#gapi_directions{route=OldRoute}) ->
	NewRoute = <<OldRoute/binary, Route/binary>>,
	NewGAPIDirections = GAPIDirections#gapi_directions{route=NewRoute},
	NewGAPIDirections;
parse_route_element({<<"waypoint_order">>, WaypointOrderList}, GAPIDirections) ->
	NewGAPIDirections = GAPIDirections#gapi_directions{waypoints_new_order=WaypointOrderList},
	NewGAPIDirections;
parse_route_element({_Other, _Ignore}, GAPIDirections) -> GAPIDirections.

parse_route_legs([], no_address, GAPIDirections) -> GAPIDirections;
parse_route_legs([], {FinalWaypoint, PreviousDistance, PreviousDuration}, GAPIDirections) ->
	NewFinalWaypoint = FinalWaypoint#gapi_waypoint{distance=PreviousDistance, duration=PreviousDuration},
	NewGAPIDirections = GAPIDirections#gapi_directions{destination=NewFinalWaypoint},
	NewGAPIDirections;
parse_route_legs([Leg|Rest], AddressToCarry, GAPIDirections=#gapi_directions{waypoints=Waypoints, distance=OldDistance, duration=OldDuration}) ->
	#gapi_leg{start_address=StartAddress, end_address=EndAddress, distance=Distance, duration=Duration} = parse_route_leg(Leg, #gapi_leg{}),
	NewDistance = OldDistance + Distance,
	NewDuration = OldDuration + Duration,
	case AddressToCarry of
		no_address -> NewGAPIDirections = GAPIDirections#gapi_directions{origin=StartAddress, distance=NewDistance, duration=NewDuration};
		{_PreviousAddress, PreviousDistance, PreviousDuration} ->
			NewStartAddress = StartAddress#gapi_waypoint{distance=PreviousDistance, duration=PreviousDuration},
			NewWaypoints = Waypoints ++ [NewStartAddress],
			NewGAPIDirections = GAPIDirections#gapi_directions{waypoints=NewWaypoints, distance=NewDistance, duration=NewDuration}
	end,
	parse_route_legs(Rest, {EndAddress, Distance, Duration}, NewGAPIDirections).

parse_route_leg({Leg}, GAPILeg) when is_list(Leg) -> parse_route_leg_keys(Leg, GAPILeg).

parse_route_leg_keys([], GAPILeg) -> GAPILeg;
parse_route_leg_keys([Key|Rest], GAPILeg) ->
	NewGAPILeg = parse_route_leg_key(Key, GAPILeg),
	parse_route_leg_keys(Rest, NewGAPILeg).

parse_route_leg_key({<<"distance">>, {KeyValueList}}, GAPILeg=#gapi_leg{distance=OldValue}) ->
	Value = get_value(KeyValueList),
	NewValue = Value + OldValue,
	NewGAPILeg = GAPILeg#gapi_leg{distance=NewValue},
	NewGAPILeg;
parse_route_leg_key({<<"duration">>, {KeyValueList}}, GAPILeg=#gapi_leg{duration=OldValue}) ->
	Value = get_value(KeyValueList),
	NewValue = Value + OldValue,
	NewGAPILeg = GAPILeg#gapi_leg{duration=NewValue},
	NewGAPILeg;
parse_route_leg_key({<<"start_address">>, Address}, GAPILeg=#gapi_leg{start_address=StartAddress}) ->
	NewStartAddress = StartAddress#gapi_waypoint{address=Address},
	NewGAPILeg = GAPILeg#gapi_leg{start_address=NewStartAddress},
	NewGAPILeg;
parse_route_leg_key({<<"end_address">>, Address}, GAPILeg=#gapi_leg{end_address=EndAddress}) ->
	NewEndAddress = EndAddress#gapi_waypoint{address=Address},
	NewGAPILeg = GAPILeg#gapi_leg{end_address=NewEndAddress},
	NewGAPILeg;
parse_route_leg_key({<<"start_location">>, {KeyValueList}}, GAPILeg=#gapi_leg{start_address=Address}) ->
	NewAddress = build_coordinates(KeyValueList, Address),
	NewGAPILeg = GAPILeg#gapi_leg{start_address=NewAddress},
	NewGAPILeg;
parse_route_leg_key({<<"end_location">>, {KeyValueList}}, GAPILeg=#gapi_leg{end_address=Address}) ->
	NewAddress = build_coordinates(KeyValueList, Address),
	NewGAPILeg = GAPILeg#gapi_leg{end_address=NewAddress},
	NewGAPILeg;
parse_route_leg_key({_Other, _Ignore}, GAPILeg) -> GAPILeg.

get_value([]) -> 0;
get_value([{<<"value">>, Value}|_Rest]) -> Value;
get_value([_Other|Rest]) -> get_value(Rest).

build_coordinates([], Address) -> Address;
build_coordinates([{<<"lat">>, Value}|Rest], Address) ->
	NewEndAddress = Address#gapi_waypoint{latitude=Value},
	build_coordinates(Rest, NewEndAddress);
build_coordinates([{<<"lng">>, Value}|Rest], Address) ->
	NewEndAddress = Address#gapi_waypoint{longitude=Value},
	build_coordinates(Rest, NewEndAddress).

build_travel_info(ResponseBody) ->
	JSON = jsondoc:decode(ResponseBody),
	parse_travel_info(JSON).

parse_travel_info({Elements}) when is_list(Elements) ->
	try
		<<"OK">> = get_json_element(Elements, <<"status">>),
		Rows = get_json_element(Elements, <<"rows">>),
		DistanceMatrix = parse_travel_info_rows(Rows, []),
		{ok, DistanceMatrix}
	catch
		_:_ ->
			error_logger:info_msg("~p:parse_travel_info({~p}): Error parsing result~n", [?MODULE, Elements]),
			error_logger:info_msg("Backtrace ~p~n", [erlang:get_stacktrace()]),
			{nok, error}
	end.

parse_travel_info_rows([], Acc) -> lists:reverse(Acc);
parse_travel_info_rows([{Row}|Rest], Acc) ->
	Elements = get_json_element(Row, <<"elements">>),
	ParsedElements = parse_travel_info_elements(Elements, []),
	parse_travel_info_rows(Rest, [ParsedElements|Acc]).

parse_travel_info_elements([], Acc) -> lists:reverse(Acc);
parse_travel_info_elements([{Element}|Rest], Acc) ->
	<<"OK">> = get_json_element(Element, <<"status">>),
	{Distance} = get_json_element(Element, <<"distance">>),
	{Duration} = get_json_element(Element, <<"duration">>),
	DistanceValue = get_json_element(Distance, <<"value">>),
	DurationValue = get_json_element(Duration, <<"value">>),
	parse_travel_info_elements(Rest, [#gapi_travel_info{distance=DistanceValue, duration=DurationValue}|Acc]).

get_json_element([{Match, Element}|_], Match) -> Element;
get_json_element([_|Rest], Match) -> get_json_element(Rest, Match).

build_position_list(ResponseBody) ->
	JSON = jsondoc:decode(ResponseBody),
	parse_position_list(JSON).

parse_position_list({Elements}) when is_list(Elements) -> parse_position_list(Elements, []).

parse_position_list([], Processed) -> {ok, lists:reverse(Processed)};
parse_position_list([Element|Rest], Processed) ->
	case parse_position_list_element(Element, Processed) of
		{nok, Error} -> {nok, Error};
		NewProcessed -> parse_position_list(Rest, NewProcessed)
	end.

parse_position_list_element({<<"snappedPoints">>, SnappedPoint}, _) -> parse_snapped_points(SnappedPoint, []);
parse_position_list_element({_Other, _Ignore}, Processed) -> Processed.

parse_snapped_points([], Processed) -> Processed;
parse_snapped_points([Point|Rest], Processed) ->
	NewProcessed = parse_snapped_point(Point, Processed),
	parse_snapped_points(Rest, NewProcessed).

parse_snapped_point({Elements}, Processed) when is_list(Elements) -> parse_snapped_point(Elements, Processed);
parse_snapped_point([], Processed) -> Processed;
parse_snapped_point([Point|Rest], Processed) ->
	NewProcessed = parse_snapped_point_element(Point, Processed),
	parse_snapped_point(Rest, NewProcessed).

parse_snapped_point_element({<<"location">>, Location}, Processed) ->
	Position = parse_snapped_point_position(Location),
	[Position|Processed];
parse_snapped_point_element({_Other, _Ignore}, Processed) -> Processed.

parse_snapped_point_position({Elements}) when is_list(Elements) -> parse_snapped_point_position(Elements, {0, 0}).

parse_snapped_point_position([], GAPIPosition) -> GAPIPosition;
parse_snapped_point_position([Element|Rest], GAPIPosition) ->
	NewGAPIPosition = parse_snapped_point_position_element(Element, GAPIPosition),
	parse_snapped_point_position(Rest, NewGAPIPosition).

parse_snapped_point_position_element({<<"latitude">>, Latitude}, {Longitude, _}) -> {Longitude, Latitude};
parse_snapped_point_position_element({<<"longitude">>, Longitude}, {_, Latitude}) ->  {Longitude, Latitude};
parse_snapped_point_position_element({_Other, _Ignore}, Position) -> Position.

calculate_distance([]) -> 0;
calculate_distance([First|Rest]) -> calculate_distance(Rest, First, 0).

calculate_distance([], _, Distance) -> Distance;
calculate_distance([EndPosition={ELongitude, ELatitude}|Rest], {SLongitude, SLatitude}, Distance) ->
	NewDistance = eb_util:distance_between(SLatitude, SLongitude, ELatitude, ELongitude),
	calculate_distance(Rest, EndPosition, Distance + NewDistance).

build_geocode(ResponseBody) ->
	JSON = jsondoc:decode(ResponseBody),
	parse_geocode(JSON).

parse_geocode({Elements}) when is_list(Elements) -> parse_geocode(Elements, #gapi_geocode{}).

parse_geocode([], GAPIGeocode) -> {ok, GAPIGeocode};
parse_geocode([Element|Rest], GAPIGeocode) ->
	case parse_geocode_element(Element, GAPIGeocode) of
		{nok, Error} -> {nok, Error};
		NewGAPIGeocode -> parse_geocode(Rest, NewGAPIGeocode)
	end.

parse_geocode_element({<<"results">>, Results}, GAPIGeocode) -> parse_results(Results, GAPIGeocode);
parse_geocode_element({<<"status">>, <<"OK">>}, GAPIGeocode) -> GAPIGeocode;
parse_geocode_element({<<"status">>, <<"ZERO_RESULTS">>}, _GAPIPosition) -> {nok, no_address};
parse_geocode_element({<<"status">>, Unknown}, _GAPIGeocode) ->
	error_logger:info_msg("~p:parse_element(..., ~p): Unknown value\n", [?MODULE, Unknown]),
	{nok, error};
parse_geocode_element({_Other, _Ignore}, GAPIGeocode) -> GAPIGeocode.

parse_results(L, _GAPIGeocode) when length(L) > 1 -> {nok, too_many_results};
parse_results([], GAPIGeocode) -> GAPIGeocode;
parse_results([Element|_Rest], GAPIGeocode) ->
	NewGAPIGeocode = parse_result(Element, GAPIGeocode),
	NewGAPIGeocode.

parse_result({Elements}, GAPIGeocode) when is_list(Elements) -> parse_result_elements(Elements, GAPIGeocode).

parse_result_elements([], GAPIGeocode) -> GAPIGeocode;
parse_result_elements([Element|Rest], GAPIGeocode) ->
	NewGAPIGeocode = parse_result_element(Element, GAPIGeocode),
	parse_result_elements(Rest, NewGAPIGeocode).

parse_result_element({<<"geometry">>, Geometry}, GAPIGeocode) -> parse_geometry(Geometry, GAPIGeocode);
parse_result_element({<<"address_components">>, AddressComponents}, GAPIGeocode) -> parse_address_components(AddressComponents, GAPIGeocode);
parse_result_element({_Other, _Ignore}, GAPIGeocode) -> GAPIGeocode.

parse_geometry({Elements}, GAPIGeocode) when is_list(Elements) -> parse_geometry_elements(Elements, GAPIGeocode).

parse_geometry_elements([], GAPIGeocode) -> GAPIGeocode;
parse_geometry_elements([Element|Rest], GAPIGeocode) ->
	NewGAPIGeocode = parse_geometry_element(Element, GAPIGeocode),
	parse_geometry_elements(Rest, NewGAPIGeocode).

parse_geometry_element({<<"location">>, {KeyValueList}}, GAPIGeocode) ->
	NewGAPIPosition = build_position(KeyValueList, #gapi_position{}),
	NewGAPIGeocode = GAPIGeocode#gapi_geocode{position=NewGAPIPosition},
	NewGAPIGeocode;
parse_geometry_element({_Other, _Ignore}, GAPIGeocode) -> GAPIGeocode.

build_position([], GAPIPosition) -> GAPIPosition;
build_position([{<<"lat">>, Value}|Rest], GAPIPosition) ->
	NewGAPIPosition = GAPIPosition#gapi_position{latitude=Value},
	build_position(Rest, NewGAPIPosition);
build_position([{<<"lng">>, Value}|Rest], GAPIPosition) ->
	NewGAPIPosition = GAPIPosition#gapi_position{longitude=Value},
	build_position(Rest, NewGAPIPosition).

parse_address_components(Elements, GAPIGeocode) when is_list(Elements) ->
	AddressComponentTypeList = [?ADDRESS_COMPONENT_ROUTE, ?ADDRESS_COMPONENT_STREET_NUMBER,
										 ?ADDRESS_COMPONENT_EXTRA_INFO, ?ADDRESS_COMPONENT_LOCALITY, ?ADDRESS_COMPONENT_COUNTRY],
	NewAddressComponents = [#gapi_address_component{component=AddressComponent, value=proplists:get_value(<<"long_name">>, Elem)} || AddressComponent <- AddressComponentTypeList, {Elem} <- Elements, lists:member(AddressComponent, proplists:get_value(<<"types">>, Elem))],
	NewGAPIGeocode = GAPIGeocode#gapi_geocode{address=NewAddressComponents},
	NewGAPIGeocode.
