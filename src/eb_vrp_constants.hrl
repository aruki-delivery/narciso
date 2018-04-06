%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
%% VRP API Constants
%%

%% ====================================================================
%% Constants
%% ====================================================================
-define(VRP_OVER_CAPACITY_COEFICIENT, 10000).
-define(VRP_OVER_NODE_COEFICIENT    , 5000).
-define(VRP_OVER_WINDOW_COEFICIENT  , 1000).
-define(VRP_NUMBER_OF_RUNS          , 10).
-define(VRP_ITERATIONS              , 40).
-define(VRP_MUTATE_PROBABILITY      , 80).

-define(SHOW_VRP_LOG, true).
-define(VRP_LOG_FILENAME, "vrp.log").

%% ====================================================================
%% Records
%% ====================================================================

-record(vrp_chromosome, {repr, fit, isFitActual}).
-record(vrp_problem, {nodes, distancemap, depot, capacity, overcapcoef, popcount, mutateprob, tournament, maxnodes, overnodecoef, overwindowcoef}).
-record(vrp_arc, {origin, destination, cost}).
-record(vrp_node, {id, capacity, cost, start_window, end_window}).

%% ====================================================================
%% Log functions
%% ====================================================================
-define(VRP_LOG(Text), (?SHOW_VRP_LOG andalso file:write_file(?VRP_LOG_FILENAME, io_lib:fwrite(Text, []), [append]))).
-define(VRP_LOG_2(Text, Params), (?SHOW_VRP_LOG andalso file:write_file(?VRP_LOG_FILENAME, io_lib:fwrite(Text, Params), [append]))).
