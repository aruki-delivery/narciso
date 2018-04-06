%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
%% Google API Constants
%%
%% Código retirado do projecto SETools - SysVision Erlang Tools
%%

%% ====================================================================
%% Constants
%% ====================================================================
-define(GAPI_TRAVEL_MODE_DRIVING,   "driving").
-define(GAPI_TRAVEL_MODE_WALKING,   "walking").
-define(GAPI_TRAVEL_MODE_BICYCLING, "bicycling").

-define(GAPI_SNAP_TO_ROAD_MAX_POSITIONS, 100).

-define(ADDRESS_COMPONENT_ROUTE, <<"route">>).
-define(ADDRESS_COMPONENT_STREET_NUMBER, <<"street_number">>).
-define(ADDRESS_COMPONENT_EXTRA_INFO, <<"extra_info">>).
-define(ADDRESS_COMPONENT_LOCALITY, <<"locality">>).
-define(ADDRESS_COMPONENT_COUNTRY, <<"country">>).

%% ====================================================================
%% Records
%% ====================================================================
-record(gapi_waypoint, {address, latitude, longitude, address_components=[], distance=0, duration=0}).
-record(gapi_leg, {start_address=#gapi_waypoint{}, end_address=#gapi_waypoint{}, distance=0, duration=0}).
-record(gapi_directions, {origin, destination, waypoints=[], waypoints_new_order, distance=0, duration=0, route = <<>>}).
-record(gapi_travel_info, {distance, duration}).
-record(gapi_position, {latitude, longitude}).
-record(gapi_route, {distance, route}).
-record(gapi_geocode, {position=#gapi_position{}, address=[]}).
-record(gapi_address_component, {component, value}).