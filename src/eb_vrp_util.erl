%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
%% Baseado em https://github.com/garretraziel/cvrp
%%
%% VRP Algorithm utils
%%
-module(eb_vrp_util).

-include("eb_vrp_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([solve/5]).
-export([individual/4, tournament_selector/4, generations_iterate/4]).

solve(Nodes, DistanceMap, Cars, Capacity, MaxNodes) ->
	?VRP_LOG("VRP Initial Parameters~n"),
	?VRP_LOG_2("Nodes: ~p~n", [Nodes]),
	?VRP_LOG_2("DistanceMap: ~p~n", [DistanceMap]),
	?VRP_LOG_2("Cars: ~p~n", [Cars]),
	?VRP_LOG_2("Capacity: ~p~n", [Capacity]),
	?VRP_LOG_2("MaxNodes: ~p~n", [MaxNodes]),
	NodesPropList = [{Id, {NodeCapacity, NodeCost, StartWindow, EndWindow}}
	                 || #vrp_node{id=Id, capacity=NodeCapacity, cost=NodeCost, start_window=StartWindow, end_window=EndWindow}
	                 <- Nodes],
	Depot = 1,
	DistanceMapPropList = [{{Origin, Destination}, Cost} || #vrp_arc{origin=Origin, destination=Destination, cost=Cost} <- DistanceMap],
	PopCount = length(Nodes) * length(Nodes),
	Tournament = PopCount div 2,
	VRP = #vrp_problem{nodes=NodesPropList, distancemap=DistanceMapPropList,
	                   depot=Depot, capacity=Capacity, overcapcoef=?VRP_OVER_CAPACITY_COEFICIENT,
	                   popcount=PopCount, mutateprob=?VRP_MUTATE_PROBABILITY,
	                   tournament=Tournament,
	                   maxnodes=MaxNodes, overnodecoef=?VRP_OVER_NODE_COEFICIENT,
	                   overwindowcoef=?VRP_OVER_WINDOW_COEFICIENT},
	
	Solutions = [solve(VRP, Cars, ?VRP_ITERATIONS) || _ <- lists:seq(1, ?VRP_NUMBER_OF_RUNS)],
	CmpFunc =
		fun({Solution1, Fit1}, {Solution2, Fit2}) ->
			if Fit1 =:= infinity -> {Solution2, Fit2};
			   Fit2 =:= infinity -> {Solution1, Fit1};
			   Fit1 < Fit2 -> {Solution1, Fit1};
			   Fit1 >= Fit2 -> {Solution2, Fit2}
			end
		end,
	{BestSolution, _BestFit} = lists:foldl(CmpFunc, {none, infinity}, Solutions),
	?VRP_LOG_2("Selected: ~p~n", [BestSolution]),
	
	case BestSolution of
		none -> {nok, nosolution};
		_ ->
			FormattedSolution = [format_solution(SolutionRoute, Nodes) || SolutionRoute <- BestSolution, length(SolutionRoute) > 0],
			{ok, FormattedSolution}
	end.

solve(VRP = #vrp_problem{popcount=PopCount, depot=Depot, nodes=Nodes}, Cars, Iterations) ->
	InitPopulation = create_init_population(PopCount, Cars, lists:delete(Depot, proplists:get_keys(Nodes))),
	
	Processes = [spawn(?MODULE, individual,
	                   [{none, none, none},
	                    #vrp_chromosome{repr=I, isFitActual=false, fit=0},
	                    VRP,
	                    {none, none}])
	             || I <- InitPopulation],
	create_tree(Processes),
	
	?VRP_LOG("initial population created~n"),
	
	[H|_] = Processes,
	
	spawn(?MODULE, generations_iterate, [self(), H, Iterations, VRP]),
	
	receive
		{soln, {BestFit, BestPid}} ->
			BestPid ! {repr, self()},
			receive
				BestSolution ->
					?VRP_LOG_2("Best: ~p, fitness: ~p~n", [BestSolution, BestFit])
			end;
		die ->
			BestSolution = none,
			BestFit = 1000000
	end,
	
	[Pid ! die || Pid <- Processes],
	{BestSolution, BestFit}.

individual(Pids, C = #vrp_chromosome{repr=X, isFitActual=false}, P, _) ->
	Fit = fitness(X, P),
	individual(Pids, C#vrp_chromosome{isFitActual=true, fit=Fit}, P, {none, none});
individual(Pids = {Left, Right, Root}, C = #vrp_chromosome{fit=Fit}, P, S = {RightSelected, LeftSelected}) ->
	receive
		{left, NewLeft} ->
			individual({NewLeft, Right, Root}, C, P, S);
		{right, NewRight} ->
			individual({Left, NewRight, Root}, C, P, S);
		{selection, NewRoot} ->
			case {Left, Right} of
				{none, none} ->
					NewRoot ! {selected, self(), [{Fit, self()}]},
					individual({Left, Right, NewRoot}, C, P, S);
				{none, RightValue} ->
					RightValue ! {selection, self()},
					individual({Left, Right, NewRoot}, C, P, S);
				{LeftValue, none} ->
					LeftValue ! {selection, self()},
					individual({Left, Right, NewRoot}, C, P, S);
				{LeftValue, RightValue} ->
					RightValue ! {selection, self()},
					LeftValue ! {selection, self()},
					individual({Left, Right, NewRoot}, C, P, S)
			end;
		{selected, Left, SortedList} ->
			if
				Right == none ->
					Sorted = lists:keymerge(1, SortedList, [{Fit, self()}]),
					Root ! {selected, self(), Sorted},
					individual(Pids, C, P, {none, none});
				RightSelected /= none ->
					First = lists:keymerge(1, RightSelected, [{Fit, self()}]),
					Sorted = lists:keymerge(1, SortedList, First),
					Root ! {selected, self(), Sorted},
					individual(Pids, C, P, {none, none});
				true ->
					individual(Pids, C, P, {RightSelected, SortedList})
			end;
		{selected, Right, SortedList} ->
			if
				Left == none ->
					Sorted = lists:keymerge(1, SortedList, [{Fit, self()}]),
					Root ! {selected, self(), Sorted},
					individual(Pids, C, P, {none, none});
				LeftSelected /= none ->
					First = lists:keymerge(1, LeftSelected, [{Fit, self()}]),
					Sorted = lists:keymerge(1, SortedList, First),
					Root ! {selected, self(), Sorted},
					individual(Pids, C, P, {none, none});
				true ->
					individual(Pids, C, P, {SortedList, LeftSelected})
			end;
		{repr, Pid} ->
			Pid ! C#vrp_chromosome.repr,
			individual(Pids, C, P, S);
		{reprChange, NewRepr} ->
			individual(Pids, C#vrp_chromosome{repr=NewRepr, isFitActual=false}, P, S);
		die ->
			true
	end.

tournament_selector(Individuals, Master, TournamentCount, Length) ->
	tournament_selector(Individuals, Master, TournamentCount, Length, none, none).

tournament_selector(_, _, 0, _, none, none) ->
	erlang:error(bad_tournament_count);
tournament_selector(Individuals, Master, 0, _, I, J) ->
	{_, PidA} = lists:nth(I, Individuals),
	{_, PidB} = lists:nth(J, Individuals),
	PidA ! {repr, self()},
	PidB ! {repr, self()},
	crosser(Master, none, none);
tournament_selector(Individuals, Master, C, Length, I, J) ->
	First = rand:uniform(Length),
	Second = rand:uniform(Length),
	MinI =
		if First < I -> First;
		   true -> I
		end,
	MinJ =
		if Second < J -> Second;
		   true -> J
		end,
	tournament_selector(Individuals, Master, C-1, Length, MinI, MinJ).

generations_iterate(Master, H, Count, VRP) ->
	H ! {selection, self()},
	receive
		{selected, _, Sorted} ->
			true
	end,
	generations_iterate(Master, H, Count, VRP, 0, Sorted).

generations_iterate(Master, _, Total, _, Total, Acc) ->
	Master ! {soln, hd(Acc)};
generations_iterate(Master, H, Total, VRP = #vrp_problem{popcount=PopCount,
                                                         mutateprob=MutateProb,
                                                         tournament=Tournament},
                    Count, Acc) ->
	SelectorLength = round(PopCount/3),
	[spawn(?MODULE, tournament_selector, [Acc, self(), Tournament, PopCount]) || _ <- lists:seq(1, SelectorLength)],
	Children = receive_children(SelectorLength, MutateProb),
	replace_worst(Acc, Children),
	H ! {selection, self()},
	receive
		{selected, _, NewValues} ->
			true
	end,
	{BestFit, _} = hd(NewValues),
	?VRP_LOG_2("~p. generation: Avg fitness: ~p, Best: ~p~n", [Count, round(average_fitness(NewValues)), BestFit]),
	generations_iterate(Master, H, Total, VRP, Count+1, NewValues).

%% ====================================================================
%% Internal functions
%% ====================================================================

receive_children(Count, MutateProb) ->
	receive_children(Count, MutateProb, 0, []).

receive_children(Total, _, Total, Acc) ->
	Acc;
receive_children(Total, MutateProb, Count, Acc) ->
	receive
		{children, X, Y} ->
			case rand:uniform(100) =< MutateProb of
				true ->
					receive_children(Total, MutateProb,  Count+1, [mutate(X), mutate(Y) | Acc]);
				false ->
					receive_children(Total, MutateProb, Count+1, [X, Y | Acc])
			end
	end.

replace_worst([], []) ->
	ok;
replace_worst(Values = [_|T], Children) when length(Values) > length(Children) ->
	replace_worst(T, Children);
replace_worst([{_, Pid}|TV], [HC|TC]) ->
	Pid ! {reprChange, HC},
	replace_worst(TV, TC).

fitness(Chromosome, #vrp_problem{nodes=Nodes, distancemap=DistanceMap,
                                 depot=Depot, capacity=Capacity, overcapcoef=CapCoef,
                                 maxnodes=MaxNodes, overnodecoef=NodeCoef,
                                 overwindowcoef=TWCoef})
  when length(Chromosome) > 0 ->
	fitness(Chromosome, Nodes, DistanceMap, Depot, Capacity, CapCoef, MaxNodes, NodeCoef, TWCoef, 0, 0, 0, 0);
fitness(_, _) ->
	erlang:error(bad_chromosome).

fitness([], _, _, _, _, CapCoef, _, NodeCoef, TWCoef, Cost, OverCapacity, OverNodes, OverTW) ->
	Cost+CapCoef*OverCapacity+NodeCoef*OverNodes+TWCoef*OverTW;
fitness([H|T], Nodes, DistanceMap, Depot, Capacity, CapCoef, MaxNodes, NodeCoef, TWCoef, Cost, OverCapacity, OverNodes, OverTW) ->
	{ActualCost, ActualCap, ActualOverTW} = fitness_dist([Depot|H], Nodes, DistanceMap),
	ActualOverCap = if ActualCap > Capacity -> (ActualCap - Capacity); true -> 0 end,
	ActualOverNodes = if length(H) > MaxNodes -> (length(H) - MaxNodes); true -> 0 end,
	fitness(T, Nodes, DistanceMap, Depot, Capacity, CapCoef, MaxNodes, NodeCoef, TWCoef,
	        Cost+ActualCost, OverCapacity+ActualOverCap, OverNodes+ActualOverNodes, OverTW+ActualOverTW).

fitness_dist([], _, _) ->
	{0, 0, 0};
fitness_dist([H|T], Nodes, DistanceMap) ->
	fitness_dist(H, T, Nodes, DistanceMap, 0, 0, 0).

fitness_dist(H, [], Nodes, _, Cost, CapUsed, OverTW) ->
	{_, {Cap, NodeCost, StartWindow, EndWindow}} = lists:keyfind(H, 1, Nodes),
	TWCost = max(Cost, StartWindow),
	TWExceeded = calc_over_time_window(TWCost, EndWindow),
	{round(TWCost+NodeCost), CapUsed+Cap, OverTW+TWExceeded};
fitness_dist(Last, [H|T], Nodes, DistanceMap, Cost, CapUsed, OverTW) ->
	{_, {Cap, NodeCost, StartWindow, EndWindow}} = lists:keyfind(Last, 1, Nodes),
	{_, Distance} = lists:keyfind({Last, H}, 1, DistanceMap),
	TWCost = max(Cost, StartWindow),
	TWExceeded = calc_over_time_window(TWCost, EndWindow),
	fitness_dist(H, T, Nodes, DistanceMap, TWCost+Distance+NodeCost, CapUsed+Cap, OverTW+TWExceeded).

calc_over_time_window(_, infinity) -> 0;
calc_over_time_window(Cost, Limit) when Cost < Limit -> 0;
calc_over_time_window(Cost, Limit) -> Cost - Limit.

average_fitness(Values) ->
	lists:foldl(fun({V, _}, Acc) -> Acc + V end, 0, Values)/length(Values).

shuffle(List) ->
	[X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

create_init_population(0, _, _) -> [];
create_init_population(Count, CarCount, Nodes) ->
    create_init_population(Count, CarCount, Nodes, []).

create_init_population(0, _, _, List) -> List;
create_init_population(Count, CarCount, Nodes, List) ->
	Shuffled = shuffle(Nodes),
	Segments = create_segments(CarCount, length(Nodes)),
	Cutted = cut_list(Shuffled, Segments),
	create_init_population(Count-1, CarCount, Nodes, [Cutted|List]).

create_segments(CarCount, NodeCount) ->
	CarNum = rand:uniform(CarCount),
	Segments = [{N, 0} || N <- lists:seq(1, CarNum)],
	PopulatedSegments = populate_segments(NodeCount, CarNum, Segments),
	[Seg || {_, Seg} <- PopulatedSegments].

populate_segments(0, _CarNum, Segments) -> Segments;
populate_segments(NodeCount, CarNum, Segments) ->
	SelectedCar = rand:uniform(CarNum),
	{SelectedCar, Load} = lists:keyfind(SelectedCar, 1, Segments),
	NewSegments = lists:keyreplace(SelectedCar, 1, Segments, {SelectedCar, Load + 1}),
	populate_segments(NodeCount - 1, CarNum, NewSegments).

cut_list(List, Segments) ->
	cut_list(List, Segments, []).

cut_list([], [], Acc) ->
	lists:reverse(Acc);
cut_list(_, [], _) ->
	erlang:error(bad_segment_count);
cut_list([], [_|T], Acc) ->
	cut_list([], T, [[]|Acc]);
cut_list(List, [_], Acc) ->
	lists:reverse([List|Acc]);
cut_list(List, [H|T], Acc) when H > length(List) ->
	cut_list([], T, [List|Acc]);
cut_list(List, [H|T], Acc) ->
	{First, Others} = lists:split(H, List),
	cut_list(Others, T, [First|Acc]).

crosser(Master, none, none) ->
	receive
		Repr ->
			crosser(Master, Repr, none)
	end;
crosser(Master, RepA, none) ->
	receive
		Repr ->
			crosser(Master, RepA, Repr)
	end;
crosser(Master, RepA, RepB) ->
	{ChildA, ChildB} = crossover(RepA, RepB),
	Master ! {children, ChildA, ChildB}.

create_tree([]) ->
	erlang:error(no_processes);
create_tree(Processes=[_|Rest]) ->
	create_tree(Processes, Rest).

create_tree([], _) -> ok;
create_tree(_, []) -> ok;
create_tree([H|_], [L]) ->
	H ! {left, L},
	ok;
create_tree([H | T], [L, R | Rest]) ->
	H ! {left, L},
	H ! {right, R},
	create_tree(T, Rest).

crossover(ChromosomeA, ChromosomeB) ->
	IndexTable = lists:sort(lists:flatten(ChromosomeA)),
	IndexChromA = create_ichromosome(ChromosomeA, IndexTable),
	IndexChromB = create_ichromosome(ChromosomeB, IndexTable),
	SplitIndex = rand:uniform(length(IndexChromA)),
	{FirstA, SecondA} = lists:split(SplitIndex, IndexChromA),
	{FirstB, SecondB} = lists:split(SplitIndex, IndexChromB),
	FirstIndexChrom = from_ichrom(FirstA ++ SecondB, IndexTable),
	SecondIndexChrom = from_ichrom(FirstB ++ SecondA, IndexTable),
	ChildA = unflatten_by(FirstIndexChrom, ChromosomeA),
	ChildB = unflatten_by(SecondIndexChrom, ChromosomeB),
	{ChildA, ChildB}.

create_ichromosome(Chromosome, IndexTable) ->
	create_ichromosome(Chromosome, IndexTable, []).

create_ichromosome([], _, Acc) -> lists:reverse(Acc);
create_ichromosome([[]|T], IndexTable, Acc) ->
	create_ichromosome(T, IndexTable, Acc);
create_ichromosome([[H|TC]|TR], IndexTable, Acc) ->
	Index = index_of(H, IndexTable),
	IndexTableChanged = lists:delete(H, IndexTable),
	create_ichromosome([TC|TR], IndexTableChanged, [Index|Acc]).

from_ichrom(IndexChromosome, IndexTable) ->
	from_ichrom(IndexChromosome, IndexTable, []).

from_ichrom([], _, Acc) -> lists:reverse(Acc);
from_ichrom([H|T], IndexTable, Acc) ->
	Value = lists:nth(H, IndexTable),
	IndexTableChanged = lists:delete(Value, IndexTable),
	from_ichrom(T, IndexTableChanged, [Value|Acc]).

index_of(Item, List) ->
	index_of(Item, List, 1).

index_of(_, [], _) ->
	erlang:error(cannot_find_value);
index_of(Item, [Item|_], I) -> I;
index_of(Item, [_|T], I) ->
	index_of(Item, T, I+1).

unflatten_by(FlatList, Chromosome) ->
	unflatten_by(FlatList, Chromosome, []).

unflatten_by([], _, Acc) ->
	lists:reverse(Acc);
unflatten_by(FlatList, [H|T], Acc) ->
	{First, Second} = lists:split(length(H), FlatList),
	unflatten_by(Second, T, [First|Acc]).

mutate(Chromosome) ->
	I = rand:uniform(length(Chromosome)),
	J = rand:uniform(length(Chromosome)),
	case rand:uniform(2) of
		1 ->
			if I == J ->
					Splited = lists:split(I-1, Chromosome),
					mutate(Chromosome, Splited);
			   true ->
					As = lists:split(I-1, Chromosome),
					B = lists:nth(J, Chromosome),
					mutate(Chromosome, As, B, J)
			end;
		2 ->
			Length = length(lists:nth(I, Chromosome)),
			if Length == 0 ->
					mutate(Chromosome);
			   true ->
					K = rand:uniform(Length),
					mutate_give(Chromosome, I, J, K)
			end
	end.

mutate(Chromosome, {_, [N|_]}) when length(N) == 0 ->
	mutate(Chromosome);
mutate(_, {NPrev, [N|NPost]}) ->
	I = rand:uniform(length(N)),
	J = rand:uniform(length(N)),
	NPrev ++ [swap(N, I, J) | NPost].

mutate(Chromosome, {_, [A|_]}, B, _) when length(A) == 0; length(B) == 0 ->
	mutate(Chromosome);
mutate(_, {APrev, [A|APost]}, B, BIndex) ->
	I = rand:uniform(length(A)),
	J = rand:uniform(length(B)),
	{N, M} = swap_inside(A, I, B, J),
	LT = APrev ++ [N|APost],
	{List3, [_|List4]} = lists:split(BIndex-1, LT),
	List3 ++ [M|List4].

swap(L, I, J) ->
	{List1, [F|List2]} = lists:split(I-1, L),
	LT = List1 ++ [lists:nth(J, L)|List2],
	{List3, [_|List4]} = lists:split(J-1, LT),
	List3 ++ [F|List4].

swap_inside(N, I, M, J) ->
	{NPrev, [NVal|NPost]} = lists:split(I-1, N),
	{MPrev, [MVal|MPost]} = lists:split(J-1, M),
	{NPrev ++ [MVal|NPost], MPrev ++ [NVal|MPost]}.

mutate_give(Chromosome, I, J, K) ->
	{List1, [FL|List2]} = lists:split(I-1, Chromosome),
	{ListIn1, [F|ListIn2]} = lists:split(K-1, FL),
	LT = List1 ++ [(ListIn1++ListIn2)|List2],
	{List3, [G|List4]} = lists:split(J-1, LT),
	List3 ++ [[F|G]|List4].

format_solution(Route, Nodes) ->
	NodesPropList = [{Id, Node} || Node = #vrp_node{id=Id} <- Nodes],
	format_solution(Route, NodesPropList, []).

format_solution([], _, Result) -> lists:reverse(Result);
format_solution([NodeId|Rest], Nodes, Result) ->
	Node = proplists:get_value(NodeId, Nodes),
	format_solution(Rest, Nodes, [Node|Result]).
