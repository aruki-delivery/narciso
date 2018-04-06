%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_api_deliveries).

-include("eb_constants.hrl").
-include("eb_gapi_constants.hrl").

-behaviour(eb_batch).

%% ====================================================================
%% Constants
%% ====================================================================
-define(CACHE_DELIVERIES, eb_cache_deliveries).
-define(CACHE_CANDIDATES, eb_cache_candidates).

-define(MESSAGE_SCHEDULED_ORDERS_PUSH, scheduled_orders_push).
-define(MESSAGE_UNASSIGNED_DELIVERY(DeliveryId), {unassigned_delivery, DeliveryId}).
-define(MESSAGE_ASSIGN_DELIVERY(DeliveryId), {assign_delivery, DeliveryId}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle_message/1]).
-export([get_delivery_types/0, get_delivery_statuses/0,  get_waypoint_statuses/0, get_delivery/3]).
-export([get_object_statuses/0, get_object_actions/0]).
-export([get_object_types/0, get_assigned_deliveries/1]).
-export([get_delivery_waypoint/4, get_working_delivery/1]).
-export([get_deliveries/6, get_ongoing_deliveries/2]).
-export([change_delivery/4, change_delivery_waypoint/5, assign_delivery_to_courier/4]).
-export([notify_courier/3]).
-export([get_additional_courier_info/1, get_estimated_delivery_time/2]).
-export([get_delivery_cache_entry/1, update_ongoing_delivery/1, update_ongoing_delivery/2, delete_ongoing_delivery/1, init/1]).

-record(candidate, {key, dummy}).
-record(cached_delivery, {id, inf_delivery}).

%
% Initializations
%
init(MainNode) ->
	create_deliveries_cache(MainNode),
	create_candidates_cache(MainNode),
	case load_ongoing_deliveries(MainNode) of
		ok -> ok;
		_ -> {nok, error_loading_deliveries}
	end.

%
% Get delivery
%
get_delivery(RequestingUserId, RequestingUserTypeId, DeliveryId)
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(DeliveryId) ->
	try
		eb_api_util:verify_operator_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_courier_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		Query =
			case ?IS_COURIER(RequestingUserTypeId) of
				true -> {get_courier_delivery_information, RequestingUserId, DeliveryId};
				_ -> {get_delivery_information, DeliveryId}
			end,
		case eb_db_util:execute(Query) of
			Result when is_record(Result, inf_delivery) -> {ok, Result};
			not_found -> throw(not_found);
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end;
get_delivery(_RequestingUserId, _RequestingUserTypeId, _DeliveryId) -> {nok, invalid_parameters}.

%
% Get deliveries
%
get_deliveries(RequestingUserId, RequestingUserTypeId, StatusId, CourierId, OrderId, Navigation)
  	when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) ->
	try
		% Validar permissoes
		eb_api_util:verify_operator_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_courier_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		
		% Validar filtros
		eb_api_util:is_optional_integer(StatusId) orelse 
		eb_api_util:is_optional_integer(CourierId) orelse
		eb_api_util:is_optional_integer(OrderId) orelse throw(invalid_parameters),

		% Get the parameters from cache
		BufferTime = eb_cache_util:get_db_parameter(?DB_PARAM_BUFFER_TIME, ?MODULE, get_deliveries),
		ProductionTime = eb_cache_util:get_db_parameter(?DB_PARAM_PRODUCTION_TIME, ?MODULE, get_deliveries),

		FNavigation =
			case eb_api_util:sanitize_navigation(Navigation, [?ORDER_DELIVERY_ID, ?ORDER_COURIER_ID]) of
				{ok, SanitizedNavigation} -> SanitizedNavigation;
				missing_value -> throw(invalid_parameters);
				_ -> throw(error)
			end,
		Query =
			case ?IS_COURIER(RequestingUserTypeId) of
				true -> {get_courier_deliveries_information, RequestingUserId, StatusId, FNavigation};
				_ -> 
					case ?IS_OPERATOR(RequestingUserTypeId) of
						true -> {get_deliveries_information, StatusId, CourierId, FNavigation};
						_ -> {get_dispatcher_deliveries_information, StatusId, CourierId, ProductionTime, BufferTime, FNavigation}
					end
			end,
		FResults =
			case eb_db_util:execute(Query) of
				Results when is_list(Results) -> 
					get_additional_delivery_info(Results, delivery, not_relevant);
				_ -> throw(error)
			end,
 		Reply =
			case ?IS_DISPATCHER(RequestingUserTypeId) of
				true ->
					case OrderId =:= undefined of
						true -> FResults;
						false ->
							ProcessDelivery =
								fun(#addinfo_delivery{inf_delivery=#inf_delivery{orders=Orders}}) ->
									lists:any(fun(#inf_order{order=#order{id_order=NOrderId}}) -> NOrderId =:= OrderId end, Orders)
								end,
							lists:filter(ProcessDelivery, FResults)
					end;
				false -> FResults
			end,
		{ok, Reply}
	catch
		throw:FError -> {nok, FError}
	end;
get_deliveries(_RequestingUserId, _RequestingUserTypeId, _StatusId, _CourierId, _OrderId, _Navigation) -> {nok, invalid_parameters}.

%
% Get all delivery types
%
get_delivery_types() ->
	case eb_db_util:execute({get_delivery_types}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all delivery statuses
%
get_delivery_statuses() ->
	case eb_db_util:execute({get_delivery_statuses}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all waypoint statuses
%
get_waypoint_statuses() ->
	case eb_db_util:execute({get_waypoint_statuses}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all object statuses
%
get_object_statuses() ->
	case eb_db_util:execute({get_object_statuses}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all object actions
%
get_object_actions() ->
	case eb_db_util:execute({get_object_actions}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all object types
%
get_object_types() ->
	case eb_db_util:execute({get_object_types}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get the working delivery.
%
get_working_delivery(CourierId) when is_integer(CourierId) ->
	try
		case get_assigned_deliveries_from_cache(CourierId) of
			not_found -> throw(not_found);
			{ok, Deliveries} when is_list(Deliveries) ->
				{ok, hd(Deliveries)};
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end;
get_working_delivery(_CourierId) -> {nok, invalid_parameters}.

%
% Get the assigned deliveries
%
get_assigned_deliveries(CourierId) when is_integer(CourierId) -> get_assigned_deliveries_from_cache(CourierId);
get_assigned_deliveries(_CourierId) -> {nok, invalid_parameters}.

%
% Get ongoing delivery
%
get_ongoing_deliveries(UserId, CourierUserId) ->
	case eb_mnesia_util:select(?CACHE_DELIVERIES, [{#cached_delivery{inf_delivery=#inf_delivery{delivery = #delivery{id_courier = CourierUserId, _='_'}, _='_'}, _='_'}, [], ['$_']}]) of
		[] -> not_found;
		CachedDeliveries when is_list(CachedDeliveries) ->
			ProcessDelivery =
				fun(#cached_delivery{inf_delivery=#inf_delivery{waypoints=Waypoints}}) ->
					ProcessWaypoint =
						fun(#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{id_status=StatusId}}) ->
								StatusId =:= ?DB_WAYPOINT_STATUS_LEFT
						end,
					case lists:dropwhile(ProcessWaypoint, Waypoints) of
						[] -> false;
						Wps when is_list(Wps) ->
							case hd(Wps) of
								#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{id_waypoint=1}} -> false;
								#inf_delivery_waypoint{details=[#u_delivery_waypoint_detail{user_id=UserId}]} -> true;
								_ -> false
							end
					end
				end,
			Deliveries = lists:filter(ProcessDelivery, CachedDeliveries),
			FDeliveries = [Delivery || #cached_delivery{inf_delivery=Delivery} <- Deliveries],
			case FDeliveries of
				[] -> not_found;
				_ -> {ok, FDeliveries}
			end;
		_ -> error
	end.

%
% Get waypoint
%
get_delivery_waypoint(RequestingUserId, RequestingUserTypeId, DeliveryId, WaypointId)
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(DeliveryId) andalso is_integer(WaypointId) ->
	try
		eb_api_util:verify_operator_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_courier_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		case ?IS_COURIER(RequestingUserTypeId) of
			true ->
				case eb_db_util:execute({get_courier_delivery_information, RequestingUserId, DeliveryId}) of
					Result1 when is_record(Result1, inf_delivery) -> ok;
					not_found -> throw(forbidden);
					_ -> throw(error)
				end;
			_ -> ok
		end,
		case eb_db_util:execute({get_delivery_waypoint_information, DeliveryId, WaypointId}) of
			Result2 when is_record(Result2, inf_delivery_waypoint) -> {ok, Result2};
			not_found -> throw(not_found);
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end;
get_delivery_waypoint(_RequestingUserId, _RequestingUserTypeId, _DeliveryId, _WaypointId) -> {nok, invalid_parameters}.

%
% Change the delivery
%
change_delivery(RequestingUserId, RequestingUserTypeId, DeliveryId,
                ChangeDelivery = #change_delivery{delivery_status_id=DeliveryStatusId, version=Version})
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId)
  andalso is_integer(DeliveryId) andalso is_integer(DeliveryStatusId) andalso is_integer(Version) ->
	try
		eb_api_util:verify_courier_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		case eb_api_util:validate_id(DeliveryStatusId, exists_delivery_status) of
			ok -> ok;
			nok -> throw(invalid_delivery_status_id);
			missing -> throw(unknown_delivery_status_id);
			_ -> throw(error)
		end,
		case get_delivery_cache_entry(DeliveryId) of
			{ok, #inf_delivery{delivery=#delivery{version=OldVersion}}} when OldVersion =/= Version -> throw(version);
			{ok, OldInfDelivery} when is_record(OldInfDelivery, inf_delivery) ->
				change_delivery_rule(RequestingUserId, RequestingUserTypeId, ChangeDelivery, OldInfDelivery);
			not_found -> throw(not_found);
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end;
change_delivery(_RequestingUserId, _RequestingUserTypeId, _DeliveryId, _ChangeDelivery) -> {nok, invalid_parameters}.


%
% Change the waypoint
%
change_delivery_waypoint(RequestingUserId, RequestingUserTypeId, DeliveryId, WaypointId, ChangeDeliveryWaypoint)
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(DeliveryId) andalso is_integer(WaypointId) ->
	try
		%Verificar permissoes
		eb_api_util:verify_courier_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		
		% Sanitize dos dados
		#change_delivery_waypoint{status_id=NewWaypointStatusId, position=Position, rating=Rating,
								  rating_notes=RatingNotes, signature_file=SignatureFile,
								  version=Version} =
			case sanitize(ChangeDeliveryWaypoint) of
				{ok, SanitizedChangeDeliveryWaypoint} -> SanitizedChangeDeliveryWaypoint;
				nok -> throw(missing_values)
			end,
		% Verify the existence of the delivery
		case get_delivery_cache_entry(DeliveryId) of
			{ok, #inf_delivery{delivery=#delivery{version=OldVersion}}}
			  when OldVersion =/= Version ->
				throw(version);
			% Courier
			{ok, #inf_delivery{delivery = #delivery{id_courier=RequestingUserId,
													id_status=DeliveryStatusId},
							   waypoints=Waypoints, orders=Orders}} ->
				% Verify the existence of the waypoint
				FoundWaypoints = [Waypoint || Waypoint <- Waypoints, Waypoint#inf_delivery_waypoint.waypoint#u_delivery_waypoint.id_waypoint =:= WaypointId],
				case FoundWaypoints of
					[] -> throw(not_found_waypoint);
					[#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{status_date=OldWaypointStatusDate,
																		  latitude=OldLatitude,
																		  longitude=OldLongitude},
					 						details=Details}] ->
						#u_delivery_waypoint_detail{id_order=OrderId, signature=NeedsSignature} = hd(Details),
						if
							DeliveryStatusId =:= ?DB_DELIVERY_STATUS_ACCEPTED ->
								NewDeliveryStatusId = ?DB_DELIVERY_STATUS_EXECUTING;
							NewWaypointStatusId =:= ?DB_WAYPOINT_STATUS_LEFT andalso WaypointId =:= length(Waypoints) ->
								NewDeliveryStatusId = ?DB_DELIVERY_STATUS_WAITING_CONFIRMATION;
							true ->
								NewDeliveryStatusId = null
						end,
						{FSignatureFile, FStopDuration, FTravelTime} =
							case NewWaypointStatusId of
								?DB_WAYPOINT_STATUS_LEFT ->
									TSignatureFile =
										if
											NeedsSignature =:= true andalso SignatureFile =:= undefined ->
												throw(missing_signature);
											NeedsSignature =:= false -> undefined;
											true -> SignatureFile
										end,
									{CurrentSeconds, _} = eb_util:get_timestamp_as_secs_microsecs(eb_util:get_current()),
									{OldSeconds, _} = eb_util:get_timestamp_as_secs_microsecs(OldWaypointStatusDate),
									StopDurationSecs = CurrentSeconds - OldSeconds,
									StopDuration = StopDurationSecs div 60,
									TravelTime = null,
									{TSignatureFile, StopDuration, TravelTime};
								_ ->
									TSignatureFile = undefined,
									StopDuration = null,
									TTravelTime =
										case WaypointId of
											WaypointId when WaypointId > 1 ->
												OldWaypointId = WaypointId - 1,
												[PreviousWaypoint] = [OldWaypoint || OldWaypoint <- Waypoints, OldWaypoint#inf_delivery_waypoint.waypoint#u_delivery_waypoint.id_waypoint =:= OldWaypointId],
												LastWaypointStatusDate = PreviousWaypoint#inf_delivery_waypoint.waypoint#u_delivery_waypoint.status_date,
												{CurrentSeconds, _} = eb_util:get_timestamp_as_secs_microsecs(eb_util:get_current()),
												{OldSeconds, _} = eb_util:get_timestamp_as_secs_microsecs(LastWaypointStatusDate),
												TravelTimeSecs = CurrentSeconds - OldSeconds,
												TravelTime = TravelTimeSecs div 60,
												TravelTime;
											_ ->
												{CurrentSeconds, _} = eb_util:get_timestamp_as_secs_microsecs(eb_util:get_current()),
												{OldSeconds, _} = eb_util:get_timestamp_as_secs_microsecs(OldWaypointStatusDate),
												TravelTimeSecs = CurrentSeconds - OldSeconds,
												TravelTime = TravelTimeSecs div 60,
												TravelTime
										end,
									{TSignatureFile, StopDuration, TTravelTime}
							end,
						FOrders = lists:filter(
							fun(#inf_order{order=#order{id_order=NOrderId}}) ->
								NewDeliveryStatusId =:= ?DB_DELIVERY_STATUS_EXECUTING orelse NOrderId =:= OrderId
							end, Orders),
						case update_ongoing_delivery(DeliveryId,
							{update_delivery_waypoint, DeliveryId, WaypointId, NewWaypointStatusId,
													   FStopDuration, FTravelTime,
													   NewDeliveryStatusId, Rating, RatingNotes,
													   Position, FSignatureFile, Version, FOrders}) of
							{ok, #inf_delivery{delivery=#delivery{version=NewVersion}}} ->
								if NewWaypointStatusId =:= ?DB_WAYPOINT_STATUS_LEFT orelse
									   NewWaypointStatusId =:= ?DB_WAYPOINT_STATUS_ARRIVED ->
									CheckDistance = eb_util:distance_between(Position#position.latitude, 
																			Position#position.longitude,
																			OldLatitude, OldLongitude),
									MaxDistDiffBetwWPCheckP = eb_cache_util:get_db_parameter(?DB_PARAM_MAX_DISTANCE_DIFF_BETWEEN_WP_CHECKP,
									                                                         ?MODULE, change_delivery_waypoint),
									ok = check_position_difference(DeliveryId, WaypointId,
																   Position,
																   MaxDistDiffBetwWPCheckP,
																   CheckDistance);
									true -> do_nothing
								end,
								{ok, NewVersion};
							_ -> throw(error1)
						end;
					_Other -> throw(error2)
				end;
			{ok, Delivery} when is_record(Delivery, inf_delivery) -> throw(forbidden);
			not_found -> throw(not_found_delivery);
			_Other -> throw(error3)
		end
	catch
		throw:FError -> {nok, FError}
	end;
change_delivery_waypoint(_RequestingUserId, _RequestingUserTypeId, _DeliveryId, _WaypointId, _ChangeWaypoint) -> {nok, invalid_parameters}.

%
% Dispatcher: Assign a delivery to a courier
%
assign_delivery_to_courier(RequestingUserId, RequestingUserTypeId, DeliveryId, #change_delivery_courier{id_courier=CourierId, version=Version})
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(DeliveryId) andalso
	   is_integer(CourierId) andalso is_integer(Version) ->
	try
		eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		case get_delivery_cache_entry(DeliveryId) of
			{ok, #inf_delivery{delivery=#delivery{id_status=StatusId, version=OldVersion, id_courier=OldCourierId}, orders=Orders}} ->
				Version =:= OldVersion orelse throw(version),
				CourierId =/= OldCourierId orelse throw(same_courier), 
				lists:all(
					fun(#inf_order{order=#order{id_status=NStatusId}}) ->
						NStatusId =:= ?DB_ORDER_STATUS_DISPATCH
					end, Orders) orelse throw(invalid_order_status),
				if 
					StatusId =:= ?DB_DELIVERY_STATUS_CREATED orelse	
					StatusId =:= ?DB_DELIVERY_STATUS_ACCEPTED -> 
						case assigning_delivery_to_courier(DeliveryId, CourierId, OldCourierId, Version) of
							{ok, Result} -> {ok, Result};
							{nok, Result} -> throw(Result)
						end;
					true -> throw(invalid_delivery_status)
				end;
	 		_Other -> throw(not_found)
	 	end
	catch
		throw:Error -> {nok, Error}
	end;
 assign_delivery_to_courier(_RequestingUserId, _RequestingUserTypeId, _DeliveryId, _ChangeDeliveryCourier) -> {nok, invalid_parameters}.

%
% Dispatcher: courier's aditional information
%
get_additional_courier_info(#session_info{id_user=IdCourier, position=Position, id_transport_type=IdTransportType}) ->
	FPosition =
		case Position of
			Position when is_record(Position, position) -> Position;
			_ -> undefined
		end,

	case eb_db_util:execute({get_user, IdCourier}) of
		Result when is_record(Result, user) ->
			FirstName = Result#user.first_name,
			LastName  = Result#user.last_name,
			TelephoneNr  = Result#user.telephone_nr,

			case eb_db_util:execute({get_user_photo, IdCourier}) of
				#document{name=Name, mimetype=Mimetype, base64_data=Base64Data} ->
					Photo = #file{name=Name, mimetype=Mimetype, base64_data=Base64Data};
				_ -> Photo = undefined
			end,	
		
			#dispatcher_courier_info{id_courier=IdCourier, first_name=FirstName, last_name=LastName, telephone_nr=TelephoneNr,
			                         position=FPosition,
			                         transport_type=IdTransportType,
			                         courier_photo=Photo};
		not_found -> {nok, not_found};
		_ -> {nok, error}
	end.

%
% Dispatcher: assign courier to a delivery
%
assigning_delivery_to_courier(DeliveryId, CourierId, OldCourierId, Version) ->
	case eb_api_session:get_session_cache_entry(CourierId) of
		{ok, #session_info{status=?COURIER_STATUS_AVAILABLE}} ->
			case update_ongoing_delivery(DeliveryId, {assign_delivery_dispatcher, DeliveryId, CourierId, ?DB_DELIVERY_STATUS_ACCEPTED, Version}) of
				{ok, #inf_delivery{delivery=#delivery{version=NewVersion}}} ->
  					% notificar courier da atribuicao do pedido
 					notify_courier(CourierId, DeliveryId, ?PUSH_NOTIF_TYPE_ASSIGN),
 					% Update courier online status
 					eb_api_session:working(CourierId),
					if 
						OldCourierId =/= null andalso OldCourierId =/= undefined ->
							% notificar antigo courier que ja nao tem pedido
							notify_courier(OldCourierId, DeliveryId, ?PUSH_NOTIF_TYPE_UNASSIGN),
							% mudar o estado do antigo courier para available
							eb_api_session:available(OldCourierId);
						true -> noop
					end,
 					{ok, NewVersion};
 				_ -> {nok, error}
 			end;
 		_Other -> {nok, courier_not_found}
 	end.

%
% Handle delivery assignment message
%
handle_message(?MESSAGE_ASSIGN_DELIVERY(DeliveryId)) ->
	case get_best_candidate_from_cache(DeliveryId) of
		not_found -> do_nothing;
		CourierId ->
			case get_delivery_cache_entry(DeliveryId) of
				{ok, #inf_delivery{delivery=#delivery{version=Version, id_status=DeliveryStatusId}}}
					when DeliveryStatusId =/= ?DB_DELIVERY_STATUS_COMPLETED_CANCELED ->
					% Update delivery's db record
					case update_ongoing_delivery(DeliveryId, {assign_delivery, DeliveryId, CourierId, Version}) of
						{ok, #inf_delivery{delivery=_Delivery}} ->
							eb_mnesia_util:select_delete(?CACHE_CANDIDATES, [{#candidate{key={'$1', '$2', '$3', '$4', '$5', '$6'}}, [{'=:=', '$1', DeliveryId}], [{{'$1', '$2', '$3', '$4', '$5', '$6'}}]}]),
							eb_api_session:working(CourierId);
						_ -> do_nothing
					end;
				_ -> do_nothing
			end
	end,
	ok;

%
% Handle delivery was not accepted message
%
handle_message(?MESSAGE_UNASSIGNED_DELIVERY(DeliveryId)) ->
	error_logger:info_msg("DEBUG: unassigned_delivery(~p)~n", [DeliveryId]),
	case get_delivery_cache_entry(DeliveryId) of
		{ok, #inf_delivery{delivery=#delivery{id_status=?DB_DELIVERY_STATUS_CREATED, version=Version}}} ->
			case get_best_candidate_from_cache(DeliveryId) of
				not_found ->
					case update_ongoing_delivery(DeliveryId, {update_delivery_status, DeliveryId, Version, ?DB_DELIVERY_STATUS_COMPLETED_REFUSED}) of
						{ok, _Delivery} ->
							eb_mnesia_util:delete(?CACHE_DELIVERIES, DeliveryId);
						_ -> do_nothing
					end;
				_ ->
					do_nothing
			end;
		{ok, #inf_delivery{delivery=#delivery{id_status=?DB_DELIVERY_STATUS_SCHEDULED}}} ->
			case get_best_candidate_from_cache(DeliveryId) of
				not_found -> eb_mnesia_util:delete(?CACHE_DELIVERIES, DeliveryId);
				_ -> do_nothing
			end;
		{ok, #inf_delivery{delivery=#delivery{id_status=?DB_DELIVERY_STATUS_COMPLETED_CANCELED}}} ->
			eb_mnesia_util:delete(?CACHE_DELIVERIES, DeliveryId);
		_ ->
			do_nothing
	end,
	ok;

%
% Handle scheduled orders push batch message
%
handle_message(?MESSAGE_SCHEDULED_ORDERS_PUSH) ->
	process_scheduled_orders_push(),
	create_scheduled_orders_push_timer(),
	ok;

%
% Generic case
%
handle_message(Message) ->
	error_logger:error_msg("~p:handle_message(~p): Unknown message~n", [?MODULE, Message]),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
create_scheduled_orders_push_timer() ->
	case eb_cache_util:get_db_parameter(?DB_PARAM_SCHEDULED_ORDERS_PUSH_RUN_SECS) of
		{ok, PushRunSecs} ->
			eb_batch:create_timer(PushRunSecs * 1000, ?MODULE, ?MESSAGE_SCHEDULED_ORDERS_PUSH),
			ok;
		Other ->
			error_logger:error_msg("~p:create_scheduled_orders_push_timer(...): Unable to get parameter ?DB_PARAM_SCHEDULED_ORDERS_PUSH_RUN_SECS (~p): ~p~n", [?MODULE, ?DB_PARAM_SCHEDULED_ORDERS_PUSH_RUN_SECS, Other]),
			{nok, missing_db_parameter}
	end.

%
% Batch that sends scheduled deliveries to the courier network
%
process_scheduled_orders_push() ->
error_logger:info_msg("DEBUG PROCESS PUSH~n"),
	case eb_cache_util:get_db_parameters([?DB_PARAM_WAIT_FOR_COURIERS, ?DB_PARAM_SCHEDULED_ORDERS_PUSH_BOUND_LOWER_MINS,
	                                      ?DB_PARAM_SCHEDULED_ORDERS_PUSH_BOUND_UPPER_MINS], ?MODULE, process_scheduled_orders_push) of
		[WaitForCouriers, LowerMins, UpperMins] ->
			% Get pending orders
			case eb_db_util:execute({get_scheduled_deliveries, LowerMins, UpperMins}) of
				[] ->
					ok;
				Deliveries when is_list(Deliveries) ->
					ProcessDelivery = fun(Delivery=#inf_delivery{delivery=#delivery{id=DeliveryId}}) ->
						% Put the deliveries in cache
						eb_mnesia_util:write(?CACHE_DELIVERIES, #cached_delivery{id=DeliveryId, inf_delivery=Delivery}),
						% Create the timer to change delivery status if not accepted
						eb_batch:create_timer(WaitForCouriers, ?MODULE, ?MESSAGE_UNASSIGNED_DELIVERY(DeliveryId))
					end,
					% Run the function on all deliveries
					[ProcessDelivery(Delivery) || Delivery <- Deliveries],
					ok;
				Error ->
					error_logger:error_msg("~p:process_scheduled_orders_push(): Error getting get_scheduled_deliveries: ~p~n", [?MODULE, Error]),
					error
			end;
		_Other -> error
	end.

%
% Cache creation
%
create_deliveries_cache(undefined) ->
	TabDef = [
	            {type, set},
	            {attributes, record_info(fields, cached_delivery)},
	            {record_name, cached_delivery}
	         ],
	mnesia:create_table(?CACHE_DELIVERIES, TabDef);
create_deliveries_cache(MainNode) ->
	eb_mnesia_util:copy_cache_tables(MainNode, ?CACHE_DELIVERIES).

create_candidates_cache(undefined) ->
	TabDef = [
	            {type, ordered_set},
	            {attributes, record_info(fields, candidate)},
	            {record_name, candidate}
	         ],
	mnesia:create_table(?CACHE_CANDIDATES, TabDef);
create_candidates_cache(MainNode) ->
	eb_mnesia_util:copy_cache_tables(MainNode, ?CACHE_CANDIDATES).

load_ongoing_deliveries(undefined) ->
	case eb_db_util:execute({get_ongoing_deliveries_information}) of
		OngoingDeliveries when is_list(OngoingDeliveries) ->
			Fun = fun() ->
				[
				 mnesia:write(?CACHE_DELIVERIES, #cached_delivery{id=Delivery#inf_delivery.delivery#delivery.id, inf_delivery=Delivery}, write)
				|| Delivery <- OngoingDeliveries]
			end,
			mnesia:transaction(Fun),
			ok;
		_ -> error
	end;
load_ongoing_deliveries(_MainNode) ->
	ok.

get_assigned_deliveries_from_cache(CourierId) ->
	case eb_mnesia_util:select(?CACHE_DELIVERIES, [{#cached_delivery{inf_delivery=#inf_delivery{delivery = #delivery{id_courier = CourierId, _='_'}, _='_'}, _='_'}, [], ['$_']}]) of
		[] -> not_found;
		CachedDeliveries when is_list(CachedDeliveries) ->
			Deliveries = [Delivery || #cached_delivery{inf_delivery=Delivery} <- CachedDeliveries],
			{ok, Deliveries};
		_ -> error
	end.

get_estimated_delivery_time(DeliveryId, UserId) ->
	FWaypoints =
		case get_delivery_cache_entry(DeliveryId) of
			{ok, #inf_delivery{waypoints=Waypoints}} -> Waypoints;
			_Other -> []
		end,
	ProcessWaypoint =
		fun(#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{id_waypoint=WaypointId, id_status=StatusId}, details=Details}) ->
			#u_delivery_waypoint_detail{user_id=FUserId} = hd(Details),
			WaypointId =:= 1 orelse FUserId =/= UserId orelse StatusId =:= ?DB_WAYPOINT_STATUS_LEFT
		end,
	{Wps1, Wps2} = lists:splitwith(ProcessWaypoint, FWaypoints),
	
	TEstimatedDeliveryTime =
		case Wps2 of
			[] -> 0;
			[#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{id_status=?DB_WAYPOINT_STATUS_ARRIVED}}|_Rest] -> 0;
			[#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{id_status=?DB_WAYPOINT_STATUS_WAITING,
																  travel_time_est=TravelTimeEst}}|_Rest] -> TravelTimeEst
		end,
	CountEstimatedDeliveryTime =
		fun(#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{id_status=?DB_WAYPOINT_STATUS_WAITING,
																 travel_time_est=TravelTimeEst,
																 stop_duration_est=StopDurationEst}}, Acc) ->
				TravelTimeEst + StopDurationEst + Acc;
		   (#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{id_status=?DB_WAYPOINT_STATUS_ARRIVED,
																 stop_duration_est=StopDurationEst}}, Acc) ->
				StopDurationEst + Acc;
		   (#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{id_status=?DB_WAYPOINT_STATUS_LEFT}}, Acc) ->
				Acc
		end,
	EstimatedDeliveryTime = lists:foldl(CountEstimatedDeliveryTime, TEstimatedDeliveryTime, Wps1),
	EstimatedDeliveryTime.

change_delivery_rule(RequestingUserId, _RequestingUserTypeId,
					 #change_delivery{delivery_status_id=DeliveryStatusId},
					 #inf_delivery{delivery=#delivery{id=DeliveryId, id_courier=RequestingUserId,
													  id_status=?DB_DELIVERY_STATUS_WAITING_CONFIRMATION,
													  version=Version}}) ->
	try
		Reply =
			case DeliveryStatusId =:= ?DB_DELIVERY_STATUS_COMPLETED_SUCCESS orelse
					DeliveryStatusId =:= ?DB_DELIVERY_STATUS_COMPLETED_DISTRIBUTION_ERROR of
				true ->
					case update_ongoing_delivery(DeliveryId, {update_delivery_status, DeliveryId, Version,
															  DeliveryStatusId}) of
						{ok, #inf_delivery{delivery=#delivery{version=NewVersion}}} ->
							% Remove the delivery from the cache
							eb_mnesia_util:delete(?CACHE_DELIVERIES, DeliveryId),
							eb_api_session:available(RequestingUserId),
							{ok, NewVersion};
						_ -> throw(error)
					end;
				false -> {nok, status}
			end,
		Reply
	catch
		throw:FError -> {nok, FError}
	end;
change_delivery_rule(_RequestingUserId, RequestingUserTypeId, _ChangeDelivery, _OldInfDelivery)
  when ?IS_OPERATOR(RequestingUserTypeId) ->
	{nok, unimplemented};
change_delivery_rule(_RequestingUserId, _RequestingUserTypeId, _ChangeDelivery, _OldInfDelivery) ->
	{nok, forbidden}.

notify_courier(CourierId, DeliveryId, PushNotificationType) ->
	% Spawn a new process so the main process doesn't get locked while sending the notification,
	Function = fun() ->
		case get_delivery_cache_entry(DeliveryId) of
			{ok, Delivery} ->
				% Build the message to send
				Message = get_delivery_json_message(Delivery, PushNotificationType),
				% Notify specific courier
				eb_api_session:notify_user(CourierId, ?DB_USER_TYPE_COURIER, Message);
			Other ->
				% This should never happen
				error_logger:error_msg("~p:notify_courier(~p): Error getting delivery id from cache: ~p", [?MODULE, DeliveryId, Other])
		end
	end,
	spawn(Function).

get_delivery_json_message(Delivery=#inf_delivery{waypoints=Waypoints}, PushNotificationType) ->
	% Remove object information from the Delivery
	NewWaypoints = [#inf_delivery_waypoint{waypoint=Waypoint, details=[]} || #inf_delivery_waypoint{waypoint=Waypoint} <- Waypoints],
	NewDelivery = Delivery#inf_delivery{waypoints=NewWaypoints, objects=[]},
	NewDeliveryInformation = get_additional_delivery_info(NewDelivery, push, PushNotificationType),
	io:format("NewDeliveryInformation: ~p~n", [NewDeliveryInformation]),
	JSON = eb_rest_util:to_json(NewDeliveryInformation),
 	jsondoc:encode([{delivery, JSON}]).

get_delivery_cache_entry(DeliveryId) ->
	case eb_mnesia_util:read(?CACHE_DELIVERIES, DeliveryId) of
		[] -> not_found;
		[#cached_delivery{inf_delivery=Delivery}] when is_record(Delivery, inf_delivery) -> {ok, Delivery};
		Other ->
			error_logger:error_msg("~p:get_delivery_cache_entry(~p): Unexpected error: ~p\n", [?MODULE, DeliveryId, Other]),
			system_error
	end.

get_candidate_cache_entries(DeliveryId) ->
	eb_mnesia_util:select(?CACHE_CANDIDATES, [{#candidate{key={'$1', '$2', '$3', '$4', '$5', '$6'}}, [{'=:=', '$1', DeliveryId}], ['$_']}]).

get_best_candidate_from_cache(DeliveryId) ->
	case get_candidate_cache_entries(DeliveryId) of
		[#candidate{key={_, _, _, _, _, CourierId}} | _Rest] -> CourierId;
		_ -> not_found
	end.

update_ongoing_delivery(DeliveryId) ->
	% Get the newest DB record
	case eb_db_util:execute({get_delivery_information, DeliveryId}) of
		Delivery when is_record(Delivery, inf_delivery) ->
			% Update the cache record
			eb_mnesia_util:write(?CACHE_DELIVERIES, #cached_delivery{id=DeliveryId, inf_delivery=Delivery});
		_ -> {nok, error}
	end.

delete_ongoing_delivery(DeliveryId) ->
	case eb_db_util:execute({get_delivery_information, DeliveryId}) of
		Delivery when is_record(Delivery, inf_delivery) -> {nok, error};
		_ -> eb_mnesia_util:delete(?CACHE_DELIVERIES, DeliveryId)
	end.

update_ongoing_delivery(DeliveryId, DBStatement) ->
	% Update the DB record
	case eb_db_util:execute(DBStatement) of
		{ok, _Version} ->
			% Get the newest DB record
			case eb_db_util:execute({get_delivery_information, DeliveryId}) of
				Delivery when is_record(Delivery, inf_delivery) ->
					% Update the cache record
					eb_mnesia_util:write(?CACHE_DELIVERIES, #cached_delivery{id=DeliveryId, inf_delivery=Delivery}),
					{ok, Delivery};
				_ -> {nok, error}
			end;
		Error -> Error
	end.

check_position_difference(DeliveryId, WaypointId, Position, MaxDistDiffBetwWPCheckP, CheckDistance) when CheckDistance > MaxDistDiffBetwWPCheckP ->
	NotificationMessage = "Excedida distancia entre o ponto do checkin/checkout e o Waypoint.",
	NotificationTypeId = 5, 
	NewNotification=#new_notification{delivery_id=DeliveryId, waypoint_id=WaypointId,
									  notification_type_id=NotificationTypeId,
									  message=NotificationMessage, position=Position},
	case eb_db_util:execute({create_notification, NewNotification}) of
		{ok, _NewContactRequestId} -> ok;
		_ -> ok
	end;
check_position_difference(_DeliveryId, _WaypointId, _Position, _MaxDistDiffBetwWPCheckP, _CheckDistance) ->
	ok.

get_additional_delivery_info(NewDelivery, Source=push, PushNotificationType) when is_record(NewDelivery, inf_delivery) ->
	get_additional_info(NewDelivery, Source, PushNotificationType);
get_additional_delivery_info(_NewDelivery, push, _PushNotificationType) ->
	error;
get_additional_delivery_info(NewDelivery, Source, PushNotificationType) when is_list(NewDelivery) ->
	[get_additional_info(NewDel, Source, PushNotificationType) || NewDel <- NewDelivery];
get_additional_delivery_info(_NewDelivery, _Source, _PushNotificationType) ->
	error.

get_additional_info(NewDelivery, Source, PushNotificationType) ->
	Delivery = NewDelivery#inf_delivery.delivery,
	DeliveryId = Delivery#delivery.id,

	case eb_db_util:execute({get_object_qty_by_type_by_delivery, DeliveryId}) of
		Result when is_list(Result) -> ObjectsByDelivery = Result;
		_ -> ObjectsByDelivery = []
	end,

	case eb_db_util:execute({get_address_component_by_delivery, DeliveryId, ?ADDRESS_COMPONENT_05}) of
		Result1 when is_record(Result1, streets_by_delivery) -> StreetsByDelivery = Result1;
		_ -> StreetsByDelivery = #streets_by_delivery{}
	end,

	case eb_db_util:execute({get_estimated_delivery_time, DeliveryId}) of
		Result2 when is_record(Result2, time_count_by_delivery) -> 
			EstimatedTimeCount = Result2,
			EstimateRidingTime = EstimatedTimeCount#time_count_by_delivery.estimated_riding_time,
			EstimateWaitingTime = EstimatedTimeCount#time_count_by_delivery.estimated_waiting_time,
			EstimateTime = EstimateRidingTime + EstimateWaitingTime,
			NumWaypoints = EstimatedTimeCount#time_count_by_delivery.num_waypoints;
		_ -> 
			EstimateTime=null,
			NumWaypoints=null
	end,

	case Source of
		push ->
			case PushNotificationType of
				?PUSH_NOTIF_TYPE_UNASSIGN ->
					#push_delivery{push_notification_type=PushNotificationType, 
								   id_delivery=DeliveryId,
								   distance = null,
								   latitude = null,
								   longitude = null,
								   addinfo=#info_additional{num_waypoints=null, 
															estimated_time=null,
															objects_by_delivery=null, 
															streets_by_delivery=#streets_by_delivery{}}};
				?PUSH_NOTIF_TYPE_ASSIGN ->
					Waypoints = NewDelivery#inf_delivery.waypoints,
					FoundWaypoints = [Waypoint || Waypoint <- Waypoints, Waypoint#inf_delivery_waypoint.waypoint#u_delivery_waypoint.id_waypoint =:= 1],
					case FoundWaypoints of
						[#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{latitude=Latitude, longitude=Longitude}}] ->
							Distance = Delivery#delivery.distance,
							#push_delivery{push_notification_type=PushNotificationType, 
										   id_delivery=DeliveryId, 
										   distance=Distance, 
										   latitude=Latitude, 
										   longitude=Longitude,
										   addinfo=#info_additional{num_waypoints=NumWaypoints, 
																	estimated_time=EstimateTime,
																	objects_by_delivery=ObjectsByDelivery, 
																	streets_by_delivery=StreetsByDelivery}};
						_Other -> {nok, error}
					end;
				_Other -> {nok, error}
			end;
		delivery ->
			#addinfo_delivery{inf_delivery=NewDelivery, 
					  addinfo=#info_additional{num_waypoints=NumWaypoints,
											   estimated_time=EstimateTime,
											   objects_by_delivery=ObjectsByDelivery, 
											   streets_by_delivery=StreetsByDelivery}};
		_Other ->

			L = [Quantity || #objects_by_delivery{quantity=Quantity} <- ObjectsByDelivery],
			#addinfo_dispatcher{inf_delivery=NewDelivery, 
			  addinfo=#info_dispatcher{num_waypoints=NumWaypoints, 
									   estimated_time=EstimateTime, 
									   objects_by_delivery=ObjectsByDelivery,
									   number_of_objects=lists:sum(L),
									   weight_of_objects=0,
									   volume_of_objects=0}}
	end.

sanitize(#change_delivery{version=Version}) when not is_integer(Version) -> nok;
sanitize(ChangeDelivery=#change_delivery{delivery_status_id=DeliveryStatusId, version=_Version}) ->
	try
		true = eb_api_util:is_optional_integer(DeliveryStatusId),
		% Return the record
		{ok, ChangeDelivery}
	catch
		_:_ -> nok
	end;

sanitize(#change_delivery_waypoint{version=Version}) when not is_integer(Version) -> nok;
sanitize(Record = #change_delivery_waypoint{status_id=StatusId, position=Position, rating=Rating,
											rating_notes=RatingNotes, signature_file=SignatureFile,
											version=Version}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		
		% Trim binary strings
		{ok, FRatingNotes} = eb_api_util:trim_optional(RatingNotes, TrimPattern),
		
		% Check optional integers
		true = eb_api_util:is_optional_integer(StatusId),
		true = eb_api_util:is_optional_integer(Rating),
		
		% Other records
		case Position of
			undefined -> FPosition = undefined;
			_ -> {ok, FPosition} = eb_api_util:sanitize_position(Position)
		end,
		case SignatureFile of
			undefined -> FSignatureFile = undefined;
			_ -> {ok, FSignatureFile} = eb_api_util:sanitize_file(SignatureFile, TrimPattern)
		end,
		% Size validations. 
		ok = eb_api_util:validate_size_optional(FRatingNotes, ?DB_FIELD_SIZE__DELIVERY_WAYPOINT_RATING__RATING_NOTES_COURIER),

		% Build the return record
		SanitizedRecord = Record#change_delivery_waypoint{position=FPosition, rating_notes=FRatingNotes,
														  signature_file=FSignatureFile, version=Version},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(_) -> io:format("sanitize...bum\n"), nok.
