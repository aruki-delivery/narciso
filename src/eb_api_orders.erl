%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_api_orders).

-include("eb_constants.hrl").
-include("eb_gapi_constants.hrl").
-include("eb_vrp_constants.hrl").

-behaviour(eb_batch).

%% ====================================================================
%% Constants
%% ====================================================================
-define(GET_ORDERS_TIMEOUT_MS, 20000).
-define(GLOBAL_KEY_NOTIFY_VRP_CRASH, notify_vrp_crash).

% Timer messages
-define(MESSAGE_SCHEDULED_ORDERS_CREATE, scheduled_orders_create).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle_message/1]).
-export([init/0, create_order/3, get_order/3, get_orders/6, get_order_statuses/0, get_order_origins/0,
		 get_order_types/0, get_order_payment_methods/0, change_order_status/4, reallocate_order/4,
		 change_order_delay/4]).
-export([get_order_backoffice/3, get_todays_slots/1, update_order_occupancy/2]).

-record(inf_vrp_solution, {orders, gapi_waypoints, distance, duration, rel_distances=[], rel_durations=[]}).

%
% Initializations
%
init() ->
	case eb_cache_util:put_global(?GLOBAL_KEY_NOTIFY_VRP_CRASH, false) of
		ok ->
			case eb_cache_util:get_db_parameter(?DB_PARAM_SCHEDULED_ORDERS_CREATE_RUN_SECS) of
				{ok, RunSecs} ->
					create_scheduled_orders_create_timer(RunSecs),
					ok;
				Other ->
					error_logger:error_msg("~p:init(): Unable to get parameter ?DB_PARAM_SCHEDULED_ORDERS_CREATE_RUN_SECS (~p): ~p~n", [?MODULE, ?DB_PARAM_SCHEDULED_ORDERS_CREATE_RUN_SECS, Other]),
					{nok, missing_db_parameter}
			end;
		Other ->
			error_logger:error_msg("~p:init(): Unable to set initial GLOBAL_KEY_NOTIFY_VRP_CRASH value: ~p~n", [?MODULE, Other]),
			{nok, error_put_cache_global}
	end.

%
% Create a order
%
create_order(RequestingUserId, RequestingUserTypeId, NewOrder) when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) ->
	try
		io:format("NEW ORDER: ~p~n", [NewOrder]),
		eb_api_util:verify_client_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_operator_permissions(RequestingUserTypeId) =:= ok orelse
			throw(forbidden),
		SanitizedNewOrder=#new_order{payment_provider_id=PaymentProviderId, payment_method_id=PaymentMethodId,
									 payment_id=PaymentId, gross_total=GrossTotal, cut_time=CutTime} =
			case sanitize(NewOrder) of
				{ok, SanitizedRecord} -> SanitizedRecord;
				nok -> throw(missing_values)
			end,
		Now = eb_util:get_current(),
		CutTime > Now orelse throw(invalid_cut_time),
		case eb_db_util:execute({get_last_user_order, RequestingUserId}) of
			#order{creation_date=CreationDate} ->
				NowMili = eb_util:get_epoch_timestamp(Now),
				CreationDateMili = eb_util:get_epoch_timestamp(CreationDate),
			  if
				  NowMili =< CreationDateMili + 10000 -> throw(invalid_time);
				  true -> ok
			  end;
			not_found -> ok;
			_ -> throw(error)
		end,
		case validate(SanitizedNewOrder) of
			ok -> ok;
			{nok, ValidateError} -> throw(ValidateError)
		end,
		UpdatedNewOrder =
			case fill_time_window(SanitizedNewOrder, RequestingUserId) of
				{ok, TimeWindowNewOrder} -> TimeWindowNewOrder;
				{nok, FillTimeWindowError} -> throw(FillTimeWindowError)
			end,
		CheckPayment =
			case PaymentMethodId =:= ?DB_ORDER_PAYMENT_METHOD_CREDIT_CARD of
				true -> (PaymentId =/= null andalso PaymentId =/= undefined) orelse throw(invalid_payment_id);
				false -> false
			end,
		% Verify the existence of the payment_id (optional)
		% if payment_method_id =:= DB_ORDER_PAYMENT_METHOD_CREDIT_CARD
		case CheckPayment of
			true ->
				case eb_db_util:execute({exists_order_payment, PaymentId}) of
					false -> ok;
					true -> throw(already_used_payment_id);
					_ -> throw(invalid_payment_id)
				end;
			false -> do_nothing
		end,

		{FPaymentStatusId, FAuthorizationOnly, FProviderPaymentGetDetailsEndpoint, FPaymentRequest, FPaymentResponse} =
			case CheckPayment of
				false -> {undefined, undefined, undefined, undefined, undefined};
				true ->
					#payment_provider{id=PaymentProviderId, client_id=PaymentProviderClientId,
									  client_secret=PaymentProviderClientSecret, entity_id=PaymentProviderEntityId,
									  get_details_endpoint=ProviderPaymentGetDetailsEndpoint} =
						case eb_db_util:execute({get_payment_provider, PaymentProviderId}) of
							PaymentProvider when is_record(PaymentProvider, payment_provider) -> PaymentProvider;
							not_found -> throw(payment_provider_not_found);
							_ -> throw(error)
						end,
					GetPaymentResult =
						case PaymentProviderId of
							?DB_PAYMENT_PROVIDER_SIBS ->
								{PaymentStatus, PaymentRequest, PaymentResponse} =
									case eb_sibs_payments:get_payment_details(ProviderPaymentGetDetailsEndpoint,
																			  PaymentProviderClientId,
																			  PaymentProviderClientSecret,
																			  PaymentProviderEntityId, PaymentId,
																			  GrossTotal) of
										{ok, GetPaymentDetailsRequest, GetPaymentDetailsResponse} ->
											{ok, GetPaymentDetailsRequest, GetPaymentDetailsResponse};
										{nok, GetPaymentDetailsRequest, GetPaymentDetailsResponse} ->
											{nok, GetPaymentDetailsRequest, GetPaymentDetailsResponse};
										{nok, Error} -> throw(Error)
									end,
								PaymentStatusId =
									case PaymentStatus of
										ok -> ?DB_PAYMENT_STATUS_FINISHED_OK;
										nok -> throw({PaymentStatus, invalid_payment_status_id})
									end,
								AuthorizationOnly = false,
								{PaymentStatusId, AuthorizationOnly, ProviderPaymentGetDetailsEndpoint, PaymentRequest, PaymentResponse};
							_ -> throw(unknown_payment_provider_id)
						end,
					GetPaymentResult
			end,
		case eb_db_util:execute({create_order, RequestingUserId, UpdatedNewOrder, CheckPayment, FPaymentStatusId,
				FAuthorizationOnly, FProviderPaymentGetDetailsEndpoint, FPaymentRequest, FPaymentResponse}) of
			{ok, NewOrderId} -> {ok, NewOrderId};
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end;
create_order(_RequestingUserId, _RequestingUserTypeId, _Neworder) -> {nok, invalid_parameters}.

%
% Get orders
%
get_orders(RequestingUserId, RequestingUserTypeId, StatusId, TypeId, PhoneNr, Navigation)
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) ->
	try
		eb_api_util:verify_client_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_operator_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse
			throw(forbidden),
		eb_api_util:is_optional_integer(StatusId) orelse throw(invalid_parameters),
		eb_api_util:is_optional_integer(TypeId) orelse throw(invalid_parameters),
		% Get the parameters from cache
		BufferTime = eb_cache_util:get_db_parameter(?DB_PARAM_BUFFER_TIME, ?MODULE, get_orders),
		ProductionTime = eb_cache_util:get_db_parameter(?DB_PARAM_PRODUCTION_TIME, ?MODULE, get_orders),
		FPhoneNr =
			case eb_api_util:trim_optional(PhoneNr) of
				{ok, TPhoneNr} -> TPhoneNr;
				_ -> throw(invalid_parameters)
			end,
		FNavigation =
			case eb_api_util:sanitize_navigation(Navigation, [?ORDER_ID]) of
				{ok, TNavigation} -> TNavigation;
				missing_value -> throw(invalid_parameters);
				_ -> throw(error)
			end,
		Query =
			case ?IS_CLIENT(RequestingUserTypeId) of
				true -> {get_user_orders_information, RequestingUserId, StatusId, TypeId, FPhoneNr, FNavigation};
				_ ->
					case ?IS_OPERATOR(RequestingUserTypeId) of
						true -> {get_orders_information, StatusId, TypeId, FPhoneNr, FNavigation};
						_ -> {get_dispatcher_orders_information, StatusId, TypeId, FPhoneNr, ProductionTime, BufferTime, FNavigation}
					end
			end,
		case eb_db_util:execute(Query, ?GET_ORDERS_TIMEOUT_MS) of
			Results when is_list(Results) -> {ok, Results};
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
get_orders(_RequestingUserId, _RequestingUserTypeId, _StatusId, _TypeId, _PhoneNr, _Navigation) -> {nok, invalid_parameters}.

%
% Get all order statuses
%
get_order_statuses() ->
	case eb_db_util:execute({get_order_statuses}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all order origins
%
get_order_origins() ->
	case eb_db_util:execute({get_order_origins}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all order types
%
get_order_types() ->
	case eb_db_util:execute({get_order_types}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all order payment methods
%
get_order_payment_methods() ->
	case eb_db_util:execute({get_order_payment_methods}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get a order
%
get_order(RequestingUserId, RequestingUserTypeId, OrderId)
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(OrderId) ->
	try
		eb_api_util:verify_client_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_operator_permissions(RequestingUserTypeId) =:= ok orelse
			eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		Query =
			case ?IS_CLIENT(RequestingUserTypeId) of
				true -> {get_order_user_information, OrderId, RequestingUserId};
				_ -> {get_order_information, OrderId}
			end,
		InfOrder=#inf_order{order=#order{id_status=OrderStatusId}, courier_info=CourierInfo} =
			case eb_db_util:execute(Query) of
				InfOrderRecord when is_record(InfOrderRecord, inf_order) -> InfOrderRecord;
				not_found -> throw(not_found);
				_ -> throw(error)
			end,
		OrderCompleted = lists:member(OrderStatusId, [?DB_ORDER_STATUS_COMPLETED_SUCCESS,
													  ?DB_ORDER_STATUS_COMPLETED_REFUSED,
													  ?DB_ORDER_STATUS_CANCELED]),
		NewCourierInfo =
			if
				CourierInfo == [] orelse OrderCompleted -> [];
				true ->
					ProcessCourier =
						fun(#u_order_delivery_courier{id_delivery=DeliveryId, id_courier=CourierId}) ->
							get_order_tracking_info(RequestingUserId, RequestingUserTypeId, CourierId, DeliveryId)
						end,
					lists:filter(fun(Elem) -> Elem =/= #dispatcher_courier_info{} end, lists:map(ProcessCourier, CourierInfo))
			end,
		io:format("NewCourierInfo: ~p~n", [NewCourierInfo]),
		{ok, InfOrder#inf_order{courier_info=NewCourierInfo}}
	catch
		throw:FError -> {nok, FError}
	end;
get_order(_RequestingUserId, _RequestingUserTypeId, _OrderId) -> {nok, invalid_parameters}.

%
% Get an order for the backoffice (Temporary Service - Backoffice)
%
get_order_backoffice(RequestingUserId, RequestingUserTypeId, OrderId)
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(OrderId) ->
	try
		eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),	
		
		Deliveries =
			case eb_db_util:execute({get_generated_deliveries_by_id_order, OrderId}) of
				GeneratedDeliveries when is_list(GeneratedDeliveries) -> GeneratedDeliveries;
				_ -> {nok, error}
			end,

		ProcessDelivery =
			fun(#generated_delivery{id_delivery=DeliveryId}, IdOrder) ->
				case eb_db_util:execute({get_delivery_information, DeliveryId}) of
					InfDelivery when is_record(InfDelivery, inf_delivery) ->
						DeliveryId = InfDelivery#inf_delivery.delivery#delivery.id,
						case get_waypoint_detail(DeliveryId, IdOrder) of
							ListWaypointDetails when is_list(ListWaypointDetails) -> ListWaypointDetails;
							_ -> throw(error)
						end;
					_ -> throw(error)
				end
			end,
		WaypointDetails = [ProcessDelivery(Delivery, OrderId) || Delivery <- Deliveries],
		
		ProcessDeliveryOrders =
			fun(#generated_delivery{id_delivery=DeliveryId}) ->
				case eb_db_util:execute({get_generated_delivery_by_id_delivery, DeliveryId}) of
					GeneratedDeliveriesOrders when is_list(GeneratedDeliveriesOrders) -> GeneratedDeliveriesOrders;
					_ -> throw(error)
				end
			end,
		DeliveriesOrders = [ProcessDeliveryOrders(Delivery) || Delivery <- Deliveries],

		InfOrder = 
			case eb_db_util:execute({get_order_information, OrderId}) of
				InfOrderRecord when is_record(InfOrderRecord, inf_order) -> InfOrderRecord;
				not_found -> throw(not_found);
				_ -> throw(error)
			end,
		
		ClientName = InfOrder#inf_order.order#order.client_name,
		ClientPhone = InfOrder#inf_order.order#order.client_phone_nr,
		ClientEmail = InfOrder#inf_order.order#order.client_email,
		ClientNif = InfOrder#inf_order.order#order.client_fiscal_id,
		ClientOrderWaypoints = InfOrder#inf_order.waypoints,   % #inf_order_waypoint
		ClientOrderProducts = InfOrder#inf_order.order_prods, % #inf_order_prod

		ProcessAddress2 =
			fun(#order_waypoint_address{component=Component, value=Value}) ->
				#component_info{component=Component, value=Value}
			end,
		ProcessAddress1 =
			fun(#inf_order_waypoint{address=AddressList}) ->
				[ProcessAddress2(Address) || Address <- AddressList]
			end,
		ClientAddress = [ProcessAddress1(ClientWaypoint) || ClientWaypoint <- ClientOrderWaypoints, ClientWaypoint#inf_order_waypoint.waypoint#order_waypoint.id_waypoint > 1],
		
		ProcessProds1 =
			fun(#inf_order_prod{order_prod=OrderProd}) -> OrderProd
			end,
		ClientProducts = [ProcessProds1(ClientOrderProduct) || ClientOrderProduct <- ClientOrderProducts],
		
		NewRecord=#order_backoffice{client_name=ClientName, client_phone=ClientPhone, client_email=ClientEmail, 
									client_nif=ClientNif, client_address=ClientAddress,	client_prods=ClientProducts,
									waypoint_detail=WaypointDetails, deliveries=DeliveriesOrders},
		{ok, NewRecord}
	catch
		throw:FError -> {nok, FError}
	end;
get_order_backoffice(_RequestingUserId, _RequestingUserTypeId, _OrderId) -> {nok, invalid_parameters}.

%
% Get today's slots' information (Draft - Backoffice)
%
get_todays_slots(RequestingUserTypeId) ->
	try
		eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		
		Slots =
			case eb_db_util:execute({get_todays_slots, RequestingUserTypeId}) of
				TodaysSlots when is_list(TodaysSlots) -> TodaysSlots;
				_ -> throw(not_found)
			end,
		{ok, Slots}
	catch
		throw:FError -> {nok, FError}
	end.

%
% Close a slot (Draft - Backoffice)
%
update_order_occupancy(RequestingUserTypeId, SlotId) when is_integer(SlotId) ->
	try
		eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),

		% Verify slot existance
		case eb_db_util:execute({verify_todays_slot, SlotId}) of
			true -> ok;
			false -> throw(slot_not_found);
			_ -> throw(invalid_slot_id)
		end,
		
		case eb_db_util:execute({update_slot_occupancy, SlotId}) of
			{ok, _} -> ok;
			_Other -> throw(erro)
		end
	catch
		throw:FError -> {nok, FError}
	end;
update_order_occupancy(_RequestingUserTypeId, _SlotId) -> {nok, invalid_parameters}.

%
% Update order
%
change_order_status(RequestingUserId, RequestingUserTypeId, OrderId, ChangeOrderStatus=#change_order_status{status_id=StatusId, version=Version})
  when is_integer(OrderId) andalso is_integer(StatusId) andalso is_integer(Version) ->
	try
		eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		case eb_api_util:validate_id(StatusId, exists_order_status) of
			ok -> ok;
			nok -> throw(invalid_order_status_id);
			missing -> throw(unknown_order_status_id);
			_ -> throw(error)
		end,
		case eb_db_util:execute({get_order, OrderId}) of
			#order{version=OldVersion} when OldVersion =/= Version -> throw(version);
			OldOrder when is_record(OldOrder, order) ->
				change_order_status_rule(RequestingUserTypeId, ChangeOrderStatus, OldOrder);
			not_found -> throw(not_found);
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end;
change_order_status(_RequestingUserId, _RequestingUserTypeId, _OrderId, _ChangeOrderStatus) -> {nok, invalid_parameters}.

%
% Change order delay
%
change_order_delay(RequestingUserId, RequestingUserTypeId, OrderId, #change_order_delay{cut_time=CutTime, version=Version})
  when is_integer(OrderId) andalso is_integer(Version) ->
	try
		eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		FCutTime = {Date, _} =
			case eb_rest_util:get_datetime(CutTime) of
				{ok, NewCutTime} -> NewCutTime;
				_ -> throw(invalid_order_cut_time)
			end,
		{Today, _} = eb_util:get_current(),
		case eb_db_util:execute({get_order_information, OrderId}) of
			#inf_order{order=#order{version=OldVersion}} when OldVersion =/= Version -> throw(version);
			#inf_order{order=#order{cut_time=OldCutTime}}
				when OldCutTime >= FCutTime orelse (OldCutTime =/= null andalso Date =/= Today) -> throw(invalid_order_cut_time);
			#inf_order{courier_info=CourierInfo} when CourierInfo =/= [] -> throw(invalid_order_in_delivery);
			#inf_order{order=#order{id_status=OldStatusId}} ->
				lists:member(OldStatusId,[?DB_ORDER_STATUS_CREATED, ?DB_ORDER_STATUS_PRODUCTION]) orelse throw(invalid_order_status);
			not_found -> throw(not_found);
			_ -> throw(error)
		end,
		case update_ongoing_order(OrderId, {update_order_cut_time, OrderId, Version, FCutTime}) of
			{ok, #inf_order{order=#order{version=NewVersion}}} -> {ok, NewVersion};
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end;
change_order_delay(_RequestingUserId, _RequestingUserTypeId, _OrderId, _ChangeOrderDelay) -> {nok, invalid_parameters}.

%
% Update order
%
reallocate_order(RequestingUserId, RequestingUserTypeId, OrderId, DelayOrderTime=#reallocate_order{minutes=Minutes, version=Version})
  when is_integer(OrderId) andalso is_integer(Minutes) andalso is_integer(Version) ->
	try
		% Check ermissions
		eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse 
			eb_api_util:verify_operator_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		
		DeliveryTime = eb_cache_util:get_db_parameter(?DB_PARAM_DEFAULT_ESTIMATED_DELIVERY_TIME, ?MODULE, reallocate_order),
		
		% Get Deliveries from OrderId
		Deliveries =
			case eb_db_util:execute({get_generated_deliveries_by_id_order, OrderId}) of
				GeneratedDeliveriesByOrderId when is_list(GeneratedDeliveriesByOrderId) -> GeneratedDeliveriesByOrderId;
				_ -> throw(error)
			end,
		
		ProcessValidate =
			fun(#generated_delivery{id_delivery=IdDelivery}) ->
				case eb_api_deliveries:get_delivery_cache_entry(IdDelivery) of
					{ok, #inf_delivery{delivery=#delivery{id_courier=CourierId}}} -> 
						if
							CourierId =/= null andalso CourierId =/= undefined -> throw(courier);
							true -> noop
						end;
					_ -> throw(error)
				end
			end,
		[ProcessValidate(Delivery) || Delivery <- Deliveries],
		
		case eb_db_util:execute({get_order, OrderId}) of
			#order{version=OldVersion} when OldVersion =/= Version -> throw(version);
			#order{id_status=IdStatus} when IdStatus =/= ?DB_ORDER_STATUS_CREATED andalso 
											IdStatus =/= ?DB_ORDER_STATUS_PRODUCTION andalso
											IdStatus =/= ?DB_ORDER_STATUS_DISPATCH -> throw(invalid_order_status);
			#order{cut_time=CutTime} when CutTime =/= null andalso CutTime =/= undefined ->
				update_order_delivery_time(RequestingUserTypeId, OrderId, Deliveries, DelayOrderTime, CutTime, undefined);
			#order{cut_time=CutTime} when CutTime =:= null orelse CutTime =:= undefined ->
				update_order_delivery_time(RequestingUserTypeId, OrderId, Deliveries, DelayOrderTime, undefined, DeliveryTime);
			not_found -> throw(not_found);
			_ -> throw(error)
		end
	catch
		throw:FError -> {nok, FError}
	end;
reallocate_order(_RequestingUserId, _RequestingUserTypeId, _OrderId, _DelayOrderTime) -> {nok, invalid_parameters}.

%
% Handle scheduled orders create batch message
%
handle_message(?MESSAGE_SCHEDULED_ORDERS_CREATE) ->
	start_create_scheduled_orders_supervisor(),
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
get_waypoint_detail(DeliveryId, OrderId) ->
	case eb_db_util:execute({get_waypoint_details, DeliveryId, OrderId}) of
		WaypointDetails when is_list(WaypointDetails) -> WaypointDetails;
		_ -> nok
	end.

create_scheduled_orders_create_timer(RunSecs) ->
	eb_batch:create_timer(RunSecs * 1000, ?MODULE, ?MESSAGE_SCHEDULED_ORDERS_CREATE).

start_create_scheduled_orders_supervisor() ->
	case eb_cache_util:get_db_parameters([?DB_PARAM_SCHEDULED_ORDERS_CREATE_RUN_SECS, ?DB_PARAM_GOOGLE_DISTANCE_MATRIX_COOLDOWN],
	                                     ?MODULE, start_create_scheduled_orders_supervisor) of
		[RunSecs, Cooldown] ->
			case eb_cache_util:get_global(?GLOBAL_KEY_NOTIFY_VRP_CRASH) of
				{ok, NotifyVrpCrash} ->
					process_flag(trap_exit, true),
					Pid = spawn_link(fun() -> process_scheduled_orders_create() end),
					receive
						{'EXIT', Pid, normal} ->
							create_scheduled_orders_create_timer(RunSecs),
							?VRP_LOG_2("VRP ALGORITM EXITED NORMALLY [~p]~n", [calendar:now_to_local_time(erlang:timestamp())]),
							NotifyVrpCrash orelse eb_cache_util:put_global(?GLOBAL_KEY_NOTIFY_VRP_CRASH, true);
						{'EXIT', Pid, rerun} ->
							create_scheduled_orders_create_timer(Cooldown),
							?VRP_LOG_2("VRP ALGORITM EXITED NORMALLY (rerun) [~p]~n", [calendar:now_to_local_time(erlang:timestamp())]),
							NotifyVrpCrash orelse eb_cache_util:put_global(?GLOBAL_KEY_NOTIFY_VRP_CRASH, true);
						{'EXIT', Pid, Reason} ->
							?VRP_LOG_2("VRP ALGORITM EXITED WITH ERROR [~p]~n", [calendar:now_to_local_time(erlang:timestamp())]),
							?VRP_LOG_2("~p:start_create_scheduled_orders_supervisor(): Process process_scheduled_orders_create exited with reason: ~p~n", [?MODULE, Reason]),
							error_logger:error_msg("~p:start_create_scheduled_orders_supervisor(): Process process_scheduled_orders_create exited with reason: ~p~n", [?MODULE, Reason]),
							NotifyVrpCrash andalso eb_notification:vrp_crash(),
							NotifyVrpCrash andalso eb_cache_util:put_global(?GLOBAL_KEY_NOTIFY_VRP_CRASH, false),
							create_scheduled_orders_create_timer(RunSecs)
					end;
				_Other -> error
			end;
		_Other -> error
	end.

%
% Batch that creates deliveries from the scheduled order
%
process_scheduled_orders_create() ->
	try
		?VRP_LOG_2("DEBUG TRANSPORT ALGORITHM [~p]~n", [calendar:now_to_local_time(erlang:timestamp())]),
		case eb_cache_util:get_db_parameters([?DB_PARAM_GOOGLE_DISTANCE_MATRIX_NODE_LIMIT, ?DB_PARAM_MAX_ORDERS_PER_DELIVERY,
		                                      ?DB_PARAM_DEFAULT_ESTIMATED_DELIVERY_TIME, ?DB_PARAM_PRODUCTION_TIME, ?DB_PARAM_BUFFER_TIME,
		                                      ?DB_PARAM_SLOTTED_TW, ?DB_PARAM_VRP_COURIER_COUNT], ?MODULE, process_scheduled_orders_create) of
			[GapiNodeLimit, MaxNodes, DefaultEstimatedDeliveryTime, ProductionTime, BufferTime, SlottedTimeWindow, NumCouriers] ->
				EstimatedOrderTime = DefaultEstimatedDeliveryTime + ProductionTime + BufferTime,
				Orders =
					case eb_db_util:execute({get_orders_for_generation, EstimatedOrderTime, SlottedTimeWindow}) of
						[] -> throw(exit);
						OrderList when is_list(OrderList) -> OrderList;
						GetOrderError ->
							?VRP_LOG_2("~p:process_scheduled_orders_create(): Error in get_orders_information: ~p~n", [?MODULE, GetOrderError]),
							error_logger:error_msg("~p:process_scheduled_orders_create(): Error in get_orders_information: ~p~n", [?MODULE, GetOrderError]),
							throw(error)
					end,
				?VRP_LOG_2("Orders = ~p~n", [Orders]),
				SelectedOrders = select_orders_for_generation(Orders, GapiNodeLimit - 1),
				?VRP_LOG_2("SelectedOrders = ~p~n", [SelectedOrders]),
				TransportTypeId = get_order_transport_type_id(SelectedOrders),
				TransportCapacity =
					case eb_db_util:execute({get_transport_type, TransportTypeId}) of
						#transport_type{capacity=Capacity} -> Capacity;
						GetTransportTypeError ->
							?VRP_LOG_2("~p:process_scheduled_orders_create(): Error in get_transport_type: ~p~n", [?MODULE, GetTransportTypeError]),
							error_logger:error_msg("~p:process_scheduled_orders_create(): Error in get_transport_type: ~p~n", [?MODULE, GetTransportTypeError]),
							throw(error)
					end,
				FilteredOrders = filter_orders_for_generation(SelectedOrders, NumCouriers, MaxNodes, TransportCapacity),
				?VRP_LOG_2("FilteredOrders = ~p~n", [FilteredOrders]),
				case process_orders_recreate(FilteredOrders) of
					ok -> ok;
					error -> throw(error)
				end,
				(length(FilteredOrders) < length(Orders)) andalso exit(rerun);
			_Other -> exit(param_error)
		end
	catch
		throw:error -> exit(vrp_handled_error);
		throw:exit -> ok
	end.

%
% Creates deliveries from the scheduled order
%
process_orders_recreate(Orders) ->
	try
		?VRP_LOG_2("DEBUG TRANSPORT ALGORITHM process_orders_recreate [~p]~n", [calendar:now_to_local_time(erlang:timestamp())]),
		case eb_cache_util:get_db_parameters([?DB_PARAM_GOOGLE_API_KEY, ?DB_PARAM_GOOGLE_DISTANCE_MATRIX_ENDPOINT, ?DB_PARAM_STOP_DURATION,
		                                      ?DB_PARAM_DEPOT_STOP_DURATION, ?DB_PARAM_MAX_ORDERS_PER_DELIVERY, ?DB_PARAM_TW_BEFORE_CUT_TIME,
		                                      ?DB_PARAM_TW_AFTER_CUT_TIME, ?DB_PARAM_VRP_COURIER_COUNT], ?MODULE, process_orders_recreate) of
			[ApiKey, GapiDMEndpoint, NodeCost, DepotCost, MaxNodes, TwBeforeCutTime, TwAfterCutTime, NumCouriers] ->
				TransportTypeId = get_order_transport_type_id(Orders),
				TransportCapacity =
					case eb_db_util:execute({get_transport_type, TransportTypeId}) of
						#transport_type{capacity=Capacity} -> Capacity;
						GetTransportTypeError ->
							?VRP_LOG_2("~p:process_orders_recreate(~p): Error in get_transport_type: ~p~n", [?MODULE, Orders, GetTransportTypeError]),
							error_logger:error_msg("~p:process_orders_recreate(~p): Error in get_transport_type: ~p~n", [?MODULE, Orders, GetTransportTypeError]),
							throw(error)
					end,
				GapiWaypoints = inf_order_to_gapi_waypoint(Orders),
				[_|GapiDestinations] = GapiWaypoints,
				VrpNodes = inf_order_to_vrp_node(Orders, DepotCost, NodeCost, TwBeforeCutTime, TwAfterCutTime),
				?VRP_LOG_2("VrpNodes = ~p~n", [VrpNodes]),
				GapiDm =
					case eb_gapi_util:get_distance_matrix(GapiDMEndpoint, ApiKey, GapiWaypoints, GapiDestinations, ?GAPI_TRAVEL_MODE_DRIVING) of
						{ok, GapiDmResult} -> GapiDmResult;
						DmError ->
							?VRP_LOG_2("~p:process_orders_recreate(~p): Error in eb_gapi_util:get_distance_matrix: ~p~n", [?MODULE, Orders, DmError]),
							error_logger:error_msg("~p:process_orders_recreate(~p): Error in eb_gapi_util:get_distance_matrix: ~p~n", [?MODULE, Orders, DmError]),
							throw(error)
					end,
				VrpDm = gapi_dm_to_vrp_dm(GapiDm),
				SolutionVrpNodes = 
					case eb_vrp_util:solve(VrpNodes, VrpDm, NumCouriers, TransportCapacity, MaxNodes) of
						{ok, VrpSolution} -> VrpSolution;
						VrpError ->
							?VRP_LOG_2("~p:process_orders_recreate(~p): Error in eb_vrp_util:solve: ~p~n", [?MODULE, Orders, VrpError]),
							error_logger:error_msg("~p:process_orders_recreate(~p): Error in eb_vrp_util:solve: ~p~n", [?MODULE, Orders, VrpError]),
							throw(error)
					end,
				?VRP_LOG_2("SolutionVrpNodes = ~p~n", [SolutionVrpNodes]),
				InfVrpSolutions = [#inf_vrp_solution{orders=get_solution_route_orders(RouteVrpNodes, Orders),
				                                     gapi_waypoints=get_solution_gapi_waypoints(RouteVrpNodes, GapiWaypoints),
				                                     distance=calc_delivery_distance(RouteVrpNodes, GapiDm),
				                                     duration=calc_delivery_duration(RouteVrpNodes, GapiDm),
				                                     rel_distances=calc_rel_delivery_distances(RouteVrpNodes, GapiDm),
				                                     rel_durations=calc_rel_delivery_durations(RouteVrpNodes, GapiDm)}
				                   || RouteVrpNodes <- SolutionVrpNodes],
				DeliverySolutions = [build_new_delivery(InfVrpSolution, NodeCost, DepotCost) || InfVrpSolution <- InfVrpSolutions],
				?VRP_LOG_2("DeliverySolutions = ~p~n", [DeliverySolutions]),
				case eb_db_util:execute({create_deliveries, DeliverySolutions}) of
					{ok, CreatedDeliveryIds} ->
						?VRP_LOG_2("CreatedDeliveryIds = ~p~n", [CreatedDeliveryIds]),
						lists:foreach(fun eb_api_deliveries:update_ongoing_delivery/1, CreatedDeliveryIds),
						ok;
					CreateDeliveriesError ->
						?VRP_LOG_2("~p:process_orders_recreate(~p): Error in create_deliveries: ~p~n", [?MODULE, Orders, CreateDeliveriesError]),
						error_logger:error_msg("~p:process_orders_recreate(~p): Error in create_deliveries: ~p~n", [?MODULE, Orders, CreateDeliveriesError]),
						throw(error)
				end;
			_Other -> exit(param_error)
		end
	catch
		throw:error -> error;
		throw:exit -> ok
	end.

select_orders_for_generation([Order|Orders], NodeLimit) ->
	OrderGroupingData = get_order_grouping_data(Order),
	Selected = select_orders_for_generation(Orders, OrderGroupingData, NodeLimit - 1, []),
	[Order|Selected].

select_orders_for_generation([], _, _, Selected) ->
	lists:reverse(Selected);
select_orders_for_generation(_, _, NodeLimit, Selected) when length(Selected) >= NodeLimit ->
	lists:reverse(Selected);
select_orders_for_generation([Order|Orders], OrderGroupingData, NodeLimit, Selected) ->
	case get_order_grouping_data(Order) of
		OrderGroupingData ->
			select_orders_for_generation(Orders, OrderGroupingData, NodeLimit, [Order|Selected]);
		_ ->
			select_orders_for_generation(Orders, OrderGroupingData, NodeLimit, Selected)
	end.

filter_orders_for_generation(Orders, NumCouriers, MaxNodes, TransportCapacity) ->
	filter_orders_for_generation(Orders, NumCouriers, MaxNodes, TransportCapacity, 0, 0, []).

filter_orders_for_generation([], _, _, _, _, _, RetOrders) ->
	lists:reverse(RetOrders);
filter_orders_for_generation(_, NumCouriers, MaxNodes, _, NumNodes, _, RetOrders)
  when NumNodes =:= (NumCouriers * MaxNodes) ->
	lists:reverse(RetOrders);
filter_orders_for_generation(_, NumCouriers, _, TransportCapacity, _, TotalVolume, [_|RetOrders])
  when TotalVolume > (NumCouriers * TransportCapacity) ->
	lists:reverse(RetOrders);
filter_orders_for_generation([Order|Orders], NumCouriers, MaxNodes, TransportCapacity, NumNodes, TotalVolume, RetOrders) ->
	Volume = get_order_volume_sum(Order),
	filter_orders_for_generation(Orders, NumCouriers, MaxNodes, TransportCapacity, NumNodes + 1, TotalVolume + Volume, [Order|RetOrders]).

get_order_grouping_data(#inf_order{order=#order{id_transport_type=IdTransportType},
                                   waypoints=[#inf_order_waypoint{waypoint=#order_waypoint{latitude=Latitude, longitude=Longitude}}|_]}) ->
	{Latitude, Longitude, IdTransportType}.

build_new_delivery(InfVrpSolution=#inf_vrp_solution{orders=Orders, distance=Distance, duration=Duration, rel_distances=RelDistances,
                                                    rel_durations=RelDurations},
                   StopDuration, DepotStopDuration) ->
	TransportTypeId = get_order_transport_type_id(Orders),
	OriginInfOrderWaypoint = get_origin_inf_order_waypoint(Orders),
	DestinationInfOrderWaypoint = get_destination_inf_order_waypoint(Orders),
	MiddleInfOrderWaypoints = get_middle_inf_order_waypoints(Orders),
	#gapi_directions{route=Route} = get_directions(InfVrpSolution),
	AdjustedOrders = adjust_orders_object_ids(Orders),
	OriginObjectActions = get_origin_object_actions(AdjustedOrders),
	DestinationObjectActions = get_destination_object_actions(AdjustedOrders),
	WaypointsObjectActions = get_waypoints_object_actions(AdjustedOrders),
	OrdersObjects = get_orders_objects(AdjustedOrders),
	Origin = build_new_delivery_waypoint(OriginInfOrderWaypoint, 0, 0, DepotStopDuration, OriginObjectActions),
	Destination = build_new_delivery_waypoint(DestinationInfOrderWaypoint, lists:last(RelDistances), lists:last(RelDurations),
	                                          StopDuration, DestinationObjectActions),
	NewDeliveryWaypointInput1 = lists:zip(lists:droplast(RelDistances), lists:droplast(RelDurations)),
	NewDeliveryWaypointInput2 = lists:zip(WaypointsObjectActions, MiddleInfOrderWaypoints),
	NewDeliveryWaypointInput3 = lists:zip(NewDeliveryWaypointInput1, NewDeliveryWaypointInput2),
	Waypoints = [build_new_delivery_waypoint(MiddleInfOrderWaypoint, RelDistance, RelDuration, StopDuration, WaypointObjectActions)
	             || {{RelDistance, RelDuration}, {WaypointObjectActions, MiddleInfOrderWaypoint}} <- NewDeliveryWaypointInput3],
	Objects = [build_new_delivery_object(Object) || Object <- OrdersObjects],
	GeneratedDeliveryWaypoints = [#generated_delivery_waypoint{id_order=OrderId, id_waypoint=WaypointId}
	                              || #inf_order{order=#order{id_order=OrderId},
	                                            waypoints=[#inf_order_waypoint{waypoint=#order_waypoint{id_waypoint=WaypointId}}|_]}
	                              <- tl(Orders)],
	#new_delivery{orders=Orders, id_transport_type=TransportTypeId, distance=Distance, duration=Duration,
	              route=Route, origin=Origin, destination=Destination, waypoints=Waypoints, objects=Objects,
	              generated_delivery_waypoints=GeneratedDeliveryWaypoints}.

build_new_delivery_waypoint(#inf_order_waypoint{waypoint=#order_waypoint{id_order=OrderId, id_waypoint=OrderWaypointId}},
                            Distance, Duration, StopDuration, ObjectActions) ->
	DeliveryObjectActions = [build_new_delivery_object_action(ObjectAction) || ObjectAction <- ObjectActions],
	#new_delivery_waypoint{order_id=OrderId, order_waypoint_id=OrderWaypointId, travel_distance=Distance,
	                       travel_time_est=Duration, stop_duration_est=StopDuration,
	                       object_actions=DeliveryObjectActions}.

build_new_delivery_object_action(#order_waypoint_object_action{id_object=IdObject, id_action=IdAction}) ->
	#new_delivery_object_action{object_id=IdObject, object_action_id=IdAction}.

build_new_delivery_object(#order_object{id_order=OrderId, id_object=IdObject, reference=Reference,
                                        id_type=IdType, transport_auth=TransportAuth, original_id_object=OrderObjectId}) ->
	#new_delivery_object{order_id=OrderId, order_object_id=OrderObjectId, object_id=IdObject, reference=Reference,
	                     type_id=IdType, transport_auth=TransportAuth}.

adjust_orders_object_ids(Orders) -> adjust_orders_object_ids(Orders, 0, []).

adjust_orders_object_ids([], _, Ret) -> lists:reverse(Ret);
adjust_orders_object_ids([Order|Orders], Offset, Ret) ->
	#inf_order{waypoints=Waypoints, objects=Objects} = Order,
	NewObjects = [Object#order_object{id_object=IdObject+Offset, original_id_object=IdObject}
	              || Object=#order_object{id_object=IdObject} <- Objects],
	NewWaypoints = [adjust_waypoints_object_ids(Waypoint, Offset) || Waypoint <- Waypoints],
	NewOrder = Order#inf_order{waypoints=NewWaypoints, objects=NewObjects},
	adjust_orders_object_ids(Orders, Offset + length(Objects), [NewOrder|Ret]).
	
adjust_waypoints_object_ids(Waypoint, Offset) ->
	#inf_order_waypoint{object_actions=ObjectActions} = Waypoint,
	NewObjectActions = [Action#order_waypoint_object_action{id_object=IdObject+Offset}
	                    || Action=#order_waypoint_object_action{id_object=IdObject} <- ObjectActions],
	Waypoint#inf_order_waypoint{object_actions=NewObjectActions}.

get_origin_object_actions(Orders) ->
	ObjectActionsList = [ObjectActions || #inf_order{waypoints=[#inf_order_waypoint{object_actions=ObjectActions}|_]} <- Orders],
	lists:flatten(ObjectActionsList).

get_destination_object_actions(Orders) ->
	#inf_order{waypoints=Waypoints} = lists:last(Orders),
	#inf_order_waypoint{object_actions=ObjectActions} = lists:last(Waypoints),
	ObjectActions.

get_waypoints_object_actions(Orders) ->
	WaypointMatrix = [Waypoints || #inf_order{waypoints=[_|Waypoints]} <- lists:droplast(Orders)],
	#inf_order{waypoints=[_|LastWaypoints]} = lists:last(Orders),
	SelectedWaypoints = lists:flatten([WaypointMatrix|lists:droplast(LastWaypoints)]),
	[ObjectActions || #inf_order_waypoint{object_actions=ObjectActions} <- SelectedWaypoints].

get_orders_objects(Orders) ->
	AllObjects = [Objects || #inf_order{objects=Objects} <- Orders],
	lists:flatten(AllObjects).

get_directions(#inf_vrp_solution{orders=Orders, gapi_waypoints=GapiWaypoints}) ->
	case eb_cache_util:get_db_parameters([?DB_PARAM_GOOGLE_API_KEY, ?DB_PARAM_GOOGLE_DIRECTIONS_ENDPOINT, ?DB_PARAM_GOOGLE_DIRECTIONS_REGION],
	                                     ?MODULE, get_directions) of
		[ApiKey, Endpoint, Region] ->	
			TransportTypeId = get_order_transport_type_id(Orders),
			TravelMode = get_travel_mode(TransportTypeId),
			[Origin|RestGapiWaypoints] = GapiWaypoints,
			Destination = lists:last(RestGapiWaypoints),
			Waypoints = lists:droplast(RestGapiWaypoints),
			case eb_gapi_util:get_directions(Endpoint, ApiKey, Origin, Destination, Waypoints, TravelMode, Region, false) of
				{ok, GapiDirections} -> GapiDirections;
				{nok, Error} ->
					error_logger:info_msg("~p:get_directions(...): Error invoking Google Directions: ~p\n", [?MODULE, Error]),
					throw(error)
			end;
		ParamError ->
			error_logger:info_msg("~p:get_directions(...): Error getting parameters: ~p\n", [?MODULE, ParamError]),
			throw(error_param)
	end.

calc_delivery_distance(RouteVrpNodes, GapiDm) ->
	[#vrp_node{id=FirstNodeId}|_] = RouteVrpNodes,
	#gapi_travel_info{distance=DepotToFirstNodeDistance} = lists:nth(FirstNodeId - 1, hd(GapiDm)),
	calc_delivery_distance(RouteVrpNodes, GapiDm, DepotToFirstNodeDistance).

calc_delivery_distance([], _, Sum) -> Sum;
calc_delivery_distance([_], _, Sum) -> Sum;
calc_delivery_distance([#vrp_node{id=NodeId1}, #vrp_node{id=NodeId2}|RouteVrpNodes], GapiDm, Sum) ->
	#gapi_travel_info{distance=Distance} = lists:nth(NodeId2 - 1, lists:nth(NodeId1, GapiDm)),
	calc_delivery_distance([#vrp_node{id=NodeId2}|RouteVrpNodes], GapiDm, Sum + Distance).

calc_delivery_duration(RouteVrpNodes, GapiDm) ->
	[#vrp_node{id=FirstNodeId}|_] = RouteVrpNodes,
	#gapi_travel_info{duration=DepotToFirstNodeDuration} = lists:nth(FirstNodeId - 1, hd(GapiDm)),
	DurationSecs = calc_delivery_duration(RouteVrpNodes, GapiDm, DepotToFirstNodeDuration),
	round(DurationSecs / 60).

calc_delivery_duration([], _, Sum) -> Sum;
calc_delivery_duration([_], _, Sum) -> Sum;
calc_delivery_duration([#vrp_node{id=NodeId1}, #vrp_node{id=NodeId2}|RouteVrpNodes], GapiDm, Sum) ->
	#gapi_travel_info{duration=Duration} = lists:nth(NodeId2 - 1, lists:nth(NodeId1, GapiDm)),
	calc_delivery_duration([#vrp_node{id=NodeId2}|RouteVrpNodes], GapiDm, Sum + Duration).

calc_rel_delivery_distances(RouteVrpNodes, GapiDm) ->
	[#vrp_node{id=FirstNodeId}|_] = RouteVrpNodes,
	#gapi_travel_info{distance=DepotToFirstNodeDistance} = lists:nth(FirstNodeId - 1, hd(GapiDm)),
	calc_rel_delivery_distances(RouteVrpNodes, GapiDm, [DepotToFirstNodeDistance]).

calc_rel_delivery_distances([], _, Distances) -> lists:reverse(Distances);
calc_rel_delivery_distances([_], _, Distances) -> lists:reverse(Distances);
calc_rel_delivery_distances([#vrp_node{id=NodeId1}, #vrp_node{id=NodeId2}|RouteVrpNodes], GapiDm, Distances) ->
	#gapi_travel_info{distance=Distance} = lists:nth(NodeId2 - 1, lists:nth(NodeId1, GapiDm)),
	calc_rel_delivery_distances([#vrp_node{id=NodeId2}|RouteVrpNodes], GapiDm, [Distance|Distances]).

calc_rel_delivery_durations(RouteVrpNodes, GapiDm) ->
	[#vrp_node{id=FirstNodeId}|_] = RouteVrpNodes,
	#gapi_travel_info{duration=DepotToFirstNodeDuration} = lists:nth(FirstNodeId - 1, hd(GapiDm)),
	DurationsInSecs = calc_rel_delivery_durations(RouteVrpNodes, GapiDm, [DepotToFirstNodeDuration]),
	[round(DurationSecs / 60) || DurationSecs <- DurationsInSecs].

calc_rel_delivery_durations([], _, Durations) -> lists:reverse(Durations);
calc_rel_delivery_durations([_], _, Durations) -> lists:reverse(Durations);
calc_rel_delivery_durations([#vrp_node{id=NodeId1}, #vrp_node{id=NodeId2}|RouteVrpNodes], GapiDm, Durations) ->
	#gapi_travel_info{duration=Duration} = lists:nth(NodeId2 - 1, lists:nth(NodeId1, GapiDm)),
	calc_rel_delivery_durations([#vrp_node{id=NodeId2}|RouteVrpNodes], GapiDm, [Duration|Durations]).

get_travel_mode(?DB_TRANSPORT_TYPE_BICYCLE) -> ?GAPI_TRAVEL_MODE_WALKING; % Francisco Lino requested this change: from GAPI_TRAVEL_MODE_BICYCLING to GAPI_TRAVEL_MODE_WALKING
get_travel_mode(?DB_TRANSPORT_TYPE_PEDESTRIAN) -> ?GAPI_TRAVEL_MODE_WALKING;
get_travel_mode(_Other) -> ?GAPI_TRAVEL_MODE_DRIVING.

get_solution_route_orders(RouteVrpNodes, Orders) ->
	[lists:nth(Idx - 1, Orders) || #vrp_node{id=Idx} <- RouteVrpNodes].

get_solution_gapi_waypoints(RouteVrpNodes, GapiWaypoints) ->
	[hd(GapiWaypoints)|[lists:nth(Idx, GapiWaypoints) || #vrp_node{id=Idx} <- RouteVrpNodes]].

validate([]) -> ok;

validate([Item|Rest]) ->
	case validate(Item) of
		ok -> validate(Rest);
		Error -> Error
	end;
validate(#new_order{order_type_id=OrderTypeId, transport_type_id=TransportTypeId,
					payment_provider_id=PaymentProviderId, payment_method_id=PaymentMethodId,
					objects=Objects, account=#new_order_account{id_account=AccountId, id_slot=SlotId}}) ->
	try
		% Verify the existence of the order type
		case eb_db_util:execute({exists_order_type, OrderTypeId}) of
			true -> ok;
			false -> throw(unknown_order_type_id);
			_ -> throw(invalid_order_type_id)
		end,
		% Verify the existence of the transport type (optional)
		case TransportTypeId of
			undefined -> ok;
			_ ->
				case eb_db_util:execute({exists_transport_type, TransportTypeId}) of
					true -> ok;
					false -> throw(unknown_transport_type_id);
					_ -> throw(invalid_order_type_id)
				end
		end,
		% Verify the existence of the payment_method_id
		case eb_db_util:execute({exists_order_payment_method, PaymentMethodId}) of
			true -> ok;
			false -> throw(unknown_payment_method_id);
			_ -> throw(invalid_payment_method_id)
		end,
		case PaymentMethodId =:= ?DB_ORDER_PAYMENT_METHOD_CREDIT_CARD of
			true ->
				% Verify the existence of the payment_provider_id
				case eb_db_util:execute({exists_payment_provider, PaymentProviderId}) of
					true -> ok;
					false -> throw(unknown_payment_provider_id);
					_ -> throw(invalid_payment_provider_id)
				end;
			false -> false
		end,

		if
			SlotId =/= null andalso SlotId =/= undefined->
				% Get account information
				case eb_db_util:execute({exists_account_by_id, AccountId}) of
					true -> ok;
					false -> throw(account_not_found);
					_ -> throw(invalid_account_id)
				end,
				
				% Verify slot existance
				case eb_db_util:execute({exists_account_slot, AccountId, SlotId}) of
					true -> ok;
					false -> throw(slot_not_found);
					_ -> throw(invalid_slot_id)
				end,
				
				% Verify slot availability
				case eb_db_util:execute({available_account_slot, AccountId, SlotId}) of
					true -> ok;
					false -> throw(slot_unavailable);
					_ -> throw(invalid_slot_id)
				end;
			true -> noop
		end,

		% Validate all objects
		validate(Objects)
	catch
		throw:Error -> {nok, Error}
	end;
validate(#new_order_object{type_id=ObjectTypeId}) ->
	try
		% Verify the existence of the object type
		case eb_db_util:execute({exists_object_type, ObjectTypeId}) of
			true -> ok;
			false -> throw(unknown_object_type_id);
			_ -> throw(invalid_object_type_id)
		end
	catch
		throw:Error -> {nok, Error}
	end.


sanitize(#new_order_account{id_account=AccountId}) when not is_integer(AccountId) orelse AccountId < 1 -> nok;
sanitize(#new_order_account{id_slot=SlotId}) when not is_integer(SlotId) orelse SlotId < 1 -> nok;
sanitize(NewOrderAccount) when is_record(NewOrderAccount, new_order_account) -> {ok, NewOrderAccount};

sanitize(#new_order{order_type_id=TypeId}) when not is_integer(TypeId) -> nok; 
sanitize(#new_order{transport_type_id=TransportTypeId}) when not is_integer(TransportTypeId) -> nok; 
sanitize(#new_order{origin_id=OriginId}) when not is_integer(OriginId) -> nok; 
sanitize(#new_order{payment_method_id=PaymentMethodId}) when not is_integer(PaymentMethodId) -> nok; 
sanitize(#new_order{diner_qty=DinerQty}) when not is_integer(DinerQty) orelse DinerQty < 1 -> nok;
sanitize(#new_order{gross_total=GrossTotal}) when not is_float(GrossTotal) orelse GrossTotal < 0 -> nok;
sanitize(#new_order{objects=Objects}) when Objects =:= [] -> nok;
sanitize(Record=#new_order{reference=Reference, distribution_center=DistributionCenter, order_type_id=OrderTypeId,
						   client_info=ClientInfo, cut_time=CutTime, payment_provider_id=PaymentProviderId,
						   payment_id=PaymentId, order_prods=OrderProds, origin=Origin, waypoints=Waypoints,
						   destination=Destination, objects=Objects, account=Account}) ->
	try
		% Get Trim Pattern
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		
		% Trim binary strings
		{ok, FReference} = eb_api_util:trim_optional(Reference, TrimPattern),
		{ok, FDistributionCenter} = eb_api_util:trim_optional(DistributionCenter, TrimPattern),
		{ok, FCutTime} = eb_rest_util:get_optional_datetime(CutTime),
		{ok, FPaymentId} = eb_api_util:trim_optional(PaymentId, TrimPattern),

		% Sanitize other records
		{ok, FOrderProds} = sanitize(OrderProds, TrimPattern),
		{ok, FOrigin} = sanitize(Origin, TrimPattern),
		{ok, FWaypoints} = sanitize(Waypoints, TrimPattern),
		
		if
			Account =/= null andalso Account =/= undefined ->
				{ok, FAccount} = sanitize(Account);
			true ->
				FAccount = #new_order_account{}
		end,

		case OrderTypeId =/= ?DB_ORDER_TYPE_TAKEAWAY of
			true -> {ok, FDestination} = sanitize(Destination, TrimPattern);
			false -> FDestination = null
		end,

		{ok, FObjects} = sanitize(Objects, TrimPattern),

		case ClientInfo of
			undefined -> FClientInfo = undefined;
			_ -> {ok, FClientInfo} = sanitize_client_contact(ClientInfo, TrimPattern)
		end,

		% Size validations
		ok = eb_api_util:validate_size_optional(FReference, ?DB_FIELD_SIZE__ORDER__REFERENCE),
		ok = eb_api_util:validate_size_optional(FDistributionCenter, ?DB_FIELD_SIZE__ORDER__DISTRIBUTION_CENTER),
		ok = eb_api_util:validate_size_optional(FPaymentId, ?DB_FIELD_SIZE__ORDER__PAYMENT_ID),
		
		true = eb_api_util:is_optional_integer(PaymentProviderId),

		% Build the return record
		SanitizedRecord = Record#new_order{reference=FReference, distribution_center=FDistributionCenter,
										   client_info=FClientInfo, cut_time=FCutTime, payment_id=FPaymentId,
										   order_prods=FOrderProds, origin=FOrigin,
										   waypoints=FWaypoints, destination=FDestination, objects=FObjects,
										   account=FAccount},
		{ok, SanitizedRecord}
 	catch
 		_:_ -> nok
 	end.

sanitize([], _TrimPattern) -> {ok, []};
sanitize(Items, TrimPattern) when is_list(Items) -> sanitize(Items, [], TrimPattern);

sanitize(#new_order_prod{id_product=ProductId}, _TrimPattern) when not is_integer(ProductId) orelse ProductId < 1 -> nok; 
sanitize(#new_order_prod{quantity=Quantity}, _TrimPattern) when not is_integer(Quantity) orelse Quantity < 1 -> nok;
sanitize(Record=#new_order_prod{description=Description, name=Name, prod_options=ProdOptions}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FDescription} = eb_api_util:trim_mandatory(Description, TrimPattern),
		{ok, FName} = eb_api_util:trim_mandatory(Name, TrimPattern),

		% Size validations
		ok = eb_api_util:validate_size_optional(FDescription, ?DB_FIELD_SIZE__ORDER_PROD__DESCRIPTION),
		ok = eb_api_util:validate_size_optional(FName, ?DB_FIELD_SIZE__ORDER_PROD__NAME),

		% Sanitize other records
		{ok, FProdOptions} = sanitize(ProdOptions, TrimPattern),

		% Build the return record
		SanitizedRecord = Record#new_order_prod{description=FDescription, name=FName, prod_options=FProdOptions},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#new_order_prod_option{id_prod_option=ProdOptionId}, _TrimPattern) when not is_integer(ProdOptionId) orelse ProdOptionId < 1 -> nok;
sanitize(#new_order_prod_option{type=Type}, _TrimPattern) when not is_integer(Type) orelse Type < 1 -> nok;
sanitize(Record=#new_order_prod_option{name=Name, prod_option_entries=ProdOptionEntries}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FName} = eb_api_util:trim_mandatory(Name, TrimPattern),

		% Size validations
		ok = eb_util:validate_size(FName, ?DB_FIELD_SIZE__ORDER_PROD_OPTION__NAME),

		% Sanitize other records
		{ok, FProdOptionEntries} = sanitize(ProdOptionEntries, TrimPattern),

		% Build the return record
		SanitizedRecord = Record#new_order_prod_option{name=FName, prod_option_entries=FProdOptionEntries},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#new_order_prod_option_entry{id_prod_option_entry=ProdOptionEntryId}, _TrimPattern) when not is_integer(ProdOptionEntryId) orelse ProdOptionEntryId < 1 -> nok;
sanitize(#new_order_prod_option_entry{selected=Selected}, _TrimPattern) when not (is_boolean(Selected) orelse Selected =:= undefined) -> nok;
sanitize(Record=#new_order_prod_option_entry{name=Name}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FName} = eb_api_util:trim_mandatory(Name, TrimPattern),

		% Size validations
		ok = eb_util:validate_size(FName, ?DB_FIELD_SIZE__ORDER_PROD_OPTION__NAME),

		% Build the return record
		SanitizedRecord = Record#new_order_prod_option_entry{name=FName},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#new_order_waypoint{signature=Signature}, _TrimPattern) when not is_boolean(Signature) -> nok;
sanitize(Record=#new_order_waypoint{reference=Reference, address=Address, formatted_address=FormattedAddress, position=Position, contact_info=ContactInfo,
									notes=Notes, stop_window=StopWindow, object_actions=ObjectActions}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FReference} = eb_api_util:trim_optional(Reference, TrimPattern),
		{ok, TFormattedAddress} = eb_api_util:trim_optional(FormattedAddress, TrimPattern),
		{ok, FNotes} = eb_api_util:trim_optional(Notes, TrimPattern),
		% Size validations
		ok = eb_api_util:validate_size_optional(FReference, ?DB_FIELD_SIZE__ORDER_WAYPOINT__REFERENCE),
		ok = eb_api_util:validate_size_optional(FNotes, ?DB_FIELD_SIZE__ORDER_WAYPOINT__NOTES),
		ok = eb_api_util:validate_size_optional(TFormattedAddress, ?DB_FIELD_SIZE__ORDER_WAYPOINT__FORMATTED_ADDRESS),

		% Validate optional position and contact
		{ok, FPosition} = eb_api_util:sanitize_position(Position),

		case Address of
			null -> FAddress = undefined;
			_ -> {ok, FAddress} = eb_api_util:sanitize_address_components(Address, TrimPattern)
		end,
		case TFormattedAddress of
			null -> FFormattedAddress = undefined;
			_ -> FFormattedAddress = TFormattedAddress
		end,

		% Validate one of Address or Position or FormattedAddress
		ok = eb_api_util:check_one_of(FAddress, FPosition, FFormattedAddress),

		% Validate other records
		case ContactInfo of
			undefined -> FContactInfo = undefined;
			_ -> {ok, FContactInfo} = sanitize_contact(ContactInfo, TrimPattern)
		end,
		case StopWindow of
			undefined -> FStopWindow = undefined;
			_ -> {ok, FStopWindow} = eb_api_util:sanitize_time_window(StopWindow)
		end,
		
		% Sanitize other records
		{ok, FObjectActions} = sanitize(ObjectActions, TrimPattern),

		% Build the return record
		SanitizedRecord = Record#new_order_waypoint{reference=FReference, address=FAddress,
													formatted_address=FFormattedAddress, position=FPosition,
													contact_info=FContactInfo, notes=FNotes, stop_window=FStopWindow,
													object_actions=FObjectActions},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#new_object_action{object_action_id=ObjectActionId}, _TrimPattern) when not is_integer(ObjectActionId) -> nok;
sanitize(Record=#new_object_action{object_reference=ObjectReference, object_action=ObjectAction}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FObjectReference} = eb_api_util:trim_mandatory(ObjectReference, TrimPattern),
		{ok, FObjectAction} = eb_api_util:trim_optional(ObjectAction, TrimPattern),

		% Size validations
		ok = eb_util:validate_size(FObjectReference, ?DB_FIELD_SIZE__ORDER_OBJECT__REFERENCE),
		ok = eb_util:validate_size(FObjectAction, ?DB_FIELD_SIZE__ORDER_WAYPOINT_OBJECT_ACTION__ACTION),

		% Build the return record
		SanitizedRecord = Record#new_object_action{object_reference=FObjectReference, object_action=FObjectAction},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#new_order_object{type_id=TypeId}, _TrimPattern) when not is_integer(TypeId) -> nok;
sanitize(#new_order_object{quantity=Quantity}, _TrimPattern) when not is_integer(Quantity) orelse Quantity < 1 -> nok;
sanitize(Record=#new_order_object{reference=Reference, transport_auth=TransportAuth}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FReference} = eb_api_util:trim_mandatory(Reference, TrimPattern),
		{ok, FTransportAuth} = eb_api_util:trim_optional(TransportAuth, TrimPattern),

		% Size validations
		ok = eb_util:validate_size(FReference, ?DB_FIELD_SIZE__ORDER_OBJECT__REFERENCE),
		ok = eb_api_util:validate_size_optional(FTransportAuth, ?DB_FIELD_SIZE__ORDER_OBJECT__TRANSPORT_AUTH),

		% Build the return record
		SanitizedRecord = Record#new_order_object{reference=FReference, transport_auth=FTransportAuth},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end.

sanitize_contact(#contact_info{name=Name, phone_nr=PhoneNr, email=Email}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FName} = eb_api_util:trim_optional(Name, TrimPattern),
		{ok, FPhoneNr} = eb_api_util:trim_optional(PhoneNr, TrimPattern),
		{ok, FEmail} = eb_api_util:trim_optional(Email, TrimPattern),
		% Size validations
		ok = eb_api_util:validate_size_optional(FName, ?DB_FIELD_SIZE__ORDER__CLIENT_NAME),
		ok = eb_api_util:validate_size_optional(FPhoneNr, ?DB_FIELD_SIZE__ORDER__CLIENT_PHONE_NR),
		ok = eb_api_util:validate_size_optional(FEmail, ?DB_FIELD_SIZE__ORDER__CLIENT_EMAIL),
		% Extra validations
		case FEmail of
			FEmail when is_binary(FEmail) -> true = eb_util:is_valid_email(FEmail);
			_ -> no_validation
		end,
		% Build the return record
		SanitizedRecord = #contact_info{name=FName, phone_nr=FPhoneNr, email=FEmail},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end.

sanitize_client_contact(#client_contact_info{name=Name, phone_nr=PhoneNr, email=Email, fiscal_id=FiscalId}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FName} = eb_api_util:trim_optional(Name, TrimPattern),
		{ok, FPhoneNr} = eb_api_util:trim_optional(PhoneNr, TrimPattern),
		{ok, FEmail} = eb_api_util:trim_optional(Email, TrimPattern),
		{ok, FFiscalId} = eb_api_util:trim_optional(FiscalId, TrimPattern),
		% Size validations
		ok = eb_api_util:validate_size_optional(FName, ?DB_FIELD_SIZE__ORDER__CLIENT_NAME),
		ok = eb_api_util:validate_size_optional(FPhoneNr, ?DB_FIELD_SIZE__ORDER__CLIENT_PHONE_NR),
		ok = eb_api_util:validate_size_optional(FEmail, ?DB_FIELD_SIZE__ORDER__CLIENT_EMAIL),
		ok = eb_api_util:validate_size_optional(FFiscalId, ?DB_FIELD_SIZE__ORDER__CLIENT_FISCAL_ID),
		% Extra validations
		case FEmail of
			FEmail when is_binary(FEmail) -> true = eb_util:is_valid_email(FEmail);
			_ -> no_validation
		end,
		% Build the return record
		SanitizedRecord = #client_contact_info{name=FName, phone_nr=FPhoneNr, email=FEmail, fiscal_id=FFiscalId},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end.

sanitize([], SanitizedItems, _TrimPattern) -> {ok, lists:reverse(SanitizedItems)}; % Reverse the list to put the items in the original order
sanitize([Item|Rest], SanitizedItems, TrimPattern) ->
	case sanitize(Item, TrimPattern) of
		{ok, SanitizedItem} -> sanitize(Rest, [SanitizedItem|SanitizedItems], TrimPattern);
		_Error -> error
	end.

gapi_dm_to_vrp_dm(DistanceMatrix) -> gapi_dm_to_vrp_dm(DistanceMatrix, 1, []).

gapi_dm_to_vrp_dm([], _, DistanceMap) -> lists:reverse(DistanceMap);
gapi_dm_to_vrp_dm([Row|Rest], Orig, DistanceMap) ->
	NewDistanceMap = gapi_dm_to_vrp_dm_row(Row, Orig, 2, DistanceMap),
	gapi_dm_to_vrp_dm(Rest, Orig + 1, NewDistanceMap).

gapi_dm_to_vrp_dm_row([], _, _, DistanceMap) -> DistanceMap;
gapi_dm_to_vrp_dm_row([Elem|Rest], Orig, Dest, DistanceMap) ->
	Cost = round(Elem#gapi_travel_info.duration / 60), % Google API returns seconds
	Arc = #vrp_arc{origin=Orig, destination=Dest, cost=Cost},
	gapi_dm_to_vrp_dm_row(Rest, Orig, Dest + 1, [Arc|DistanceMap]).

inf_order_to_gapi_waypoint([]) -> [];
inf_order_to_gapi_waypoint(InfOrders) ->
	[FirstInfOrder|_] = InfOrders,
	[FirstWaypoint|_] = FirstInfOrder#inf_order.waypoints,
	#order_waypoint{latitude=Latitude, longitude=Longitude} = FirstWaypoint#inf_order_waypoint.waypoint,
	Depot = #gapi_waypoint{latitude=Latitude, longitude=Longitude},
	inf_order_to_gapi_waypoint(InfOrders, [Depot]).

inf_order_to_gapi_waypoint([], GapiWaypoints) -> lists:reverse(GapiWaypoints);
inf_order_to_gapi_waypoint([InfOrder|InfOrders], GapiWaypoints) ->
	[_,SecondWaypoint|_] = InfOrder#inf_order.waypoints,
	#order_waypoint{latitude=Latitude, longitude=Longitude} = SecondWaypoint#inf_order_waypoint.waypoint,
	GapiWaypoint = #gapi_waypoint{latitude=Latitude, longitude=Longitude},
	inf_order_to_gapi_waypoint(InfOrders, [GapiWaypoint|GapiWaypoints]).

inf_order_to_vrp_node([], _, _, _, _) -> [];
inf_order_to_vrp_node(InfOrders, DepotCost, NodeCost, TwBeforeCutTime, TwAfterCutTime) ->
	Depot = #vrp_node{id=1, capacity=0, cost=DepotCost, start_window=0, end_window=infinity},
	inf_order_to_vrp_node(InfOrders, [Depot], 2, NodeCost, TwBeforeCutTime, TwAfterCutTime).

inf_order_to_vrp_node([], VrpNodes, _, _, _, _) -> lists:reverse(VrpNodes);
inf_order_to_vrp_node([InfOrder|InfOrders], VrpNodes, NodeId, NodeCost, TwBeforeCutTime, TwAfterCutTime) ->
	CurrentTime = eb_util:get_epoch_timestamp(eb_util:get_current()),
	{StartWindow, EndWindow} =
		case InfOrder of
			#inf_order{waypoints=[_,#inf_order_waypoint{waypoint=#order_waypoint{stop_from=StopFrom, stop_to=StopTo}}|_]}
			  when StopFrom =/= null, StopTo =/= null ->
				StopFromInMilliseconds = eb_util:get_epoch_timestamp(StopFrom),
				StopFromInMinutes = round(((StopFromInMilliseconds - CurrentTime) / 1000) / 60),
				StopToInMilliseconds = eb_util:get_epoch_timestamp(StopTo),
				StopToInMinutes = round(((StopToInMilliseconds - CurrentTime) / 1000) / 60),
				{StopFromInMinutes, StopToInMinutes};
			#inf_order{waypoints=[_,#inf_order_waypoint{waypoint=#order_waypoint{stop_from=null, stop_to=StopTo}}|_]}
			  when StopTo =/= null ->
				StopToInMilliseconds = eb_util:get_epoch_timestamp(StopTo),
				StopToInMinutes = round(((StopToInMilliseconds - CurrentTime) / 1000) / 60),
				{0, StopToInMinutes};
			#inf_order{waypoints=[_,#inf_order_waypoint{waypoint=#order_waypoint{stop_from=StopFrom, stop_to=null}}|_]}
			  when StopFrom =/= null ->
				StopFromInMilliseconds = eb_util:get_epoch_timestamp(StopFrom),
				StopFromInMinutes = round(((StopFromInMilliseconds - CurrentTime) / 1000) / 60),
				{StopFromInMinutes, infinity};
			#inf_order{order=#order{cut_time=null}} ->
				{0, infinity};
			#inf_order{order=#order{cut_time=CutTime}} ->
				CutTimeInMilliseconds = eb_util:get_epoch_timestamp(CutTime),
				CutTimeInMinutes = round(((CutTimeInMilliseconds - CurrentTime) / 1000) / 60),
				{CutTimeInMinutes - TwBeforeCutTime, CutTimeInMinutes + TwAfterCutTime}
		end,
	Capacity = get_order_volume_sum(InfOrder),
	error_logger:info_msg("RJD ID = ~p, from = ~p, to = ~p~n", [InfOrder#inf_order.order#order.id_order, StartWindow, EndWindow]),
	VrpNode = #vrp_node{id=NodeId, capacity=Capacity, cost=NodeCost, start_window=StartWindow, end_window=EndWindow},
	inf_order_to_vrp_node(InfOrders, [VrpNode|VrpNodes], NodeId + 1, NodeCost, TwBeforeCutTime, TwAfterCutTime).

get_order_transport_type_id([InfOrder|_]) ->
	InfOrder#inf_order.order#order.id_transport_type.

get_order_volume_sum(#inf_order{order=#order{id_order=IdOrder}}) ->
	case eb_db_util:execute({get_order_objects_volume, IdOrder}) of
		VolumeSum when is_float(VolumeSum) -> VolumeSum;
		Error ->
			error_logger:error_msg("~p:get_order_volume_sum: Error in get_order_objects_volume: ~p~n", [?MODULE, Error]),
			throw(error)
	end.

get_origin_inf_order_waypoint([#inf_order{waypoints=[FirstWaypoint|_]}|_]) ->
	FirstWaypoint.

get_destination_inf_order_waypoint(Orders) ->
	LastOrder = lists:last(Orders),
	lists:last(LastOrder#inf_order.waypoints).

get_middle_inf_order_waypoints(Orders) ->
	OrdersMinusLast = lists:droplast(Orders),
	LastOrder = lists:last(Orders),
	Waypoints1 = [RestWaypoints || #inf_order{waypoints=[_|RestWaypoints]} <- OrdersMinusLast],
	Waypoints2 = lists:droplast(tl(LastOrder#inf_order.waypoints)),
	lists:flatten(Waypoints1, Waypoints2).

change_order_status_rule(?DB_USER_TYPE_DISPATCHER,
						 #change_order_status{status_id=StatusId, version=Version},
						 #order{id_order=OrderId, id_type=OrderTypeId, id_status=OldStatusId}) ->
	try
		validate_dispatcher_change_order_status(OldStatusId, StatusId, OrderTypeId) orelse throw(invalid_order_status_id),

		Reply =
			case update_ongoing_order(OrderId, {update_order_status, OrderId, Version, StatusId}) of
				{ok, #inf_order{order=#order{version=NewVersion}}} -> {ok, NewVersion};
				_ -> throw(error)
			end,
		Reply
	catch
		throw:Error -> {nok, Error}
	end;
change_order_status_rule(_, _, _) -> {nok, forbidden}.

update_order_delivery_time(?DB_USER_TYPE_DISPATCHER, OrderId, Deliveries, #reallocate_order{minutes=Minutes, version=Version}, 
						   CutTime, DeliveryTime) ->
	try
		{NewCutTime, NewMinutes} =
			case {CutTime, DeliveryTime} of
				{CutTime, undefined} -> {CutTime, Minutes};
				{undefined, DeliveryTime} -> {eb_util:get_current(), (Minutes+DeliveryTime)};
				{undefined, undefined} -> throw(error);
				{CutTime, DeliveryTime} -> throw(error)
			end,

		Reply =
			case eb_db_util:execute({update_order_delivery_time, OrderId, NewMinutes, NewCutTime, Version, Deliveries}) of
				{ok, NewVersion} -> 
					Process2 =
						fun(#generated_delivery{id_order=IdOrder}) ->
							case eb_db_util:execute({get_order_information, IdOrder}) of
								Order when is_record(Order, inf_order) -> Order;
								_ -> throw(error)
							end
						end,
					Process1 =
						fun(#generated_delivery{id_delivery=DeliveryId}, IdOrder) ->
							case eb_api_deliveries:get_delivery_cache_entry(DeliveryId) of
								{ok, #inf_delivery{delivery=#delivery{id_courier=CourierId}}} ->
									if 
										CourierId =/= null andalso CourierId =/= undefined ->
											% notify courier he no longer has the delivery
											eb_api_deliveries:notify_courier(CourierId, DeliveryId, ?PUSH_NOTIF_TYPE_UNASSIGN),
											% put courier available
											eb_api_session:available(CourierId);
										true -> noop
									end;
						 		_Other -> throw(error)
						 	end,

							case eb_db_util:execute({get_generated_delivery_by_id_delivery, DeliveryId}) of
								GeneratedDeliveriesByDeliveryId when is_list(GeneratedDeliveriesByDeliveryId) -> 
									SameDeliveryOtherOrders = [Delivery || Delivery <- GeneratedDeliveriesByDeliveryId, Delivery#generated_delivery.id_order =/= IdOrder],
									if
										SameDeliveryOtherOrders =/= [] ->
											[Process2(SameDeliveryOtherOrder) || SameDeliveryOtherOrder <- SameDeliveryOtherOrders];
										true -> []
									end;
								_ -> throw(error)
							end
						end,
					OrdersList = lists:flatten([Process1(Delivery, OrderId) || Delivery <- Deliveries]),	
					if
						OrdersList =/= [] ->
							% Create another Delivery with the remaining Orders
							case process_orders_recreate(OrdersList) of
								ok -> 
									[eb_api_deliveries:delete_ongoing_delivery(Delivery#generated_delivery.id_delivery) || Delivery <- Deliveries];
								error -> throw(error)
							end;
						true -> 
							[eb_api_deliveries:delete_ongoing_delivery(Delivery#generated_delivery.id_delivery) || Delivery <- Deliveries],
							noop
					end,

					{ok, NewVersion};
				Error -> throw(Error)
			end,
		Reply
	catch
		throw:FError -> {nok, FError}
	end;
update_order_delivery_time(_, _, _, _, _, _) -> {nok, forbidden}.

% Update the delivery cache if order is in a ongoing delivery
update_ongoing_order(OrderId, DBStatement) ->
	try
		% Update the DB record
		case eb_db_util:execute(DBStatement) of
			{ok, _Version} -> ok;
			Error -> throw(Error)
		end,
		% Get the deliveryId of the OrderId. If exists, update delivery cache
		Deliveries =
			case eb_db_util:execute({get_generated_deliveries_by_id_order, OrderId}) of
				GeneratedDeliveries when is_list(GeneratedDeliveries) -> GeneratedDeliveries;
				_ -> {nok, error}
			end,
		ProcessDelivery =
			fun(#generated_delivery{id_delivery=DeliveryId}) ->
				case eb_api_deliveries:get_delivery_cache_entry(DeliveryId) of
					{ok, InfDelivery} when is_record(InfDelivery, inf_delivery) ->
						% Update the delivery cache record
						eb_api_deliveries:update_ongoing_delivery(DeliveryId);
					not_found -> noop;
					_ -> throw(error)
				end
			end,
		[ProcessDelivery(Delivery) || Delivery <- Deliveries],

		% Get the newest DB record
		case eb_db_util:execute({get_order_information, OrderId}) of
			Order when is_record(Order, inf_order) -> {ok, Order};
			_ -> {nok, error}
		end
	catch
		throw:FError -> {nok, FError}
	end.

validate_dispatcher_change_order_status(?DB_ORDER_STATUS_CREATED, ?DB_ORDER_STATUS_PRODUCTION, _) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_PRODUCTION, ?DB_ORDER_STATUS_DISPATCH, _) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_DISPATCH, ?DB_ORDER_STATUS_PRODUCTION, _) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_PRODUCTION, ?DB_ORDER_STATUS_CREATED, _) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_CREATED, ?DB_ORDER_STATUS_CANCELED, ?DB_ORDER_TYPE_TAKEAWAY) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_PRODUCTION, ?DB_ORDER_STATUS_CANCELED, ?DB_ORDER_TYPE_TAKEAWAY) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_DISPATCH, ?DB_ORDER_STATUS_CANCELED, ?DB_ORDER_TYPE_TAKEAWAY) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_CREATED, ?DB_ORDER_STATUS_COMPLETED_REFUSED, ?DB_ORDER_TYPE_TAKEAWAY) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_PRODUCTION, ?DB_ORDER_STATUS_COMPLETED_REFUSED, ?DB_ORDER_TYPE_TAKEAWAY) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_DISPATCH, ?DB_ORDER_STATUS_COMPLETED_REFUSED, ?DB_ORDER_TYPE_TAKEAWAY) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_CREATED, ?DB_ORDER_STATUS_COMPLETED_SUCCESS, ?DB_ORDER_TYPE_TAKEAWAY) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_PRODUCTION, ?DB_ORDER_STATUS_COMPLETED_SUCCESS, ?DB_ORDER_TYPE_TAKEAWAY) -> true;
validate_dispatcher_change_order_status(?DB_ORDER_STATUS_DISPATCH, ?DB_ORDER_STATUS_COMPLETED_SUCCESS, ?DB_ORDER_TYPE_TAKEAWAY) -> true;
validate_dispatcher_change_order_status(_, _, _) -> false.

can_access_delivery_data(_RequestingUserId, _RequestingUserTypeId, null, _DeliveryId) -> false;
can_access_delivery_data(RequestingUserId, RequestingUserTypeId, CourierId, DeliveryId) when ?IS_CLIENT(RequestingUserTypeId) ->
	Deliveries =
		case eb_api_deliveries:get_ongoing_deliveries(RequestingUserId, CourierId) of
			{ok, OngoingDeliveries} -> OngoingDeliveries;
			_ -> []
		end,
	DeliveryIds = [NDeliveryId || #inf_delivery{delivery=#delivery{id=NDeliveryId}} <- Deliveries, NDeliveryId =:= DeliveryId],
	lists:member(DeliveryId, DeliveryIds);
can_access_delivery_data(_RequestingUserId, RequestingUserTypeId, _CourierId, _DeliveryId)
  when ?IS_DISPATCHER(RequestingUserTypeId) orelse ?IS_OPERATOR(RequestingUserTypeId) -> true;
can_access_delivery_data(_RequestingUserId, _RequestingUserTypeId, _CourierId, _DeliveryId) -> false.

get_order_tracking_info(RequestingUserId, RequestingUserTypeId, CourierId, DeliveryId) ->
	TDispatcherCourierInfo =
		case can_access_delivery_data(RequestingUserId, RequestingUserTypeId, CourierId, DeliveryId) of
			true ->
				% Get courier Data
				SessionInfo =
					case eb_api_session:get_user_session_info(CourierId) of
						{ok, SessionInfoRecord} when is_record(SessionInfoRecord, session_info) -> SessionInfoRecord;
						_ -> #session_info{}
					end,
				case ?IS_CLIENT(RequestingUserTypeId) of
					true ->
						case eb_api_deliveries:get_additional_courier_info(SessionInfo#session_info{id_user=CourierId}) of
							DispatcherCourierInfo when is_record(DispatcherCourierInfo, dispatcher_courier_info) -> DispatcherCourierInfo;
							_ -> #dispatcher_courier_info{}
						end;
					false -> #dispatcher_courier_info{}
				end;
			false -> #dispatcher_courier_info{}
		end,
	FDispatcherCourierInfo =
		if
			?IS_DISPATCHER(RequestingUserTypeId) orelse ?IS_OPERATOR(RequestingUserTypeId) -> 
				TDispatcherCourierInfo#dispatcher_courier_info{id_delivery=DeliveryId, id_courier=CourierId};
			true ->
				FEta =
					if
						TDispatcherCourierInfo#dispatcher_courier_info.id_courier == undefined -> undefined;
						true -> eb_api_deliveries:get_estimated_delivery_time(DeliveryId, RequestingUserId)
					end,
				TDispatcherCourierInfo#dispatcher_courier_info{id_delivery=undefined, eta=FEta}
		end,
	FDispatcherCourierInfo.

fill_time_window(NewOrder=#new_order{account=#new_order_account{id_account=undefined, id_slot=undefined}}, _RequestingUserId) ->
	{ok, NewOrder};
fill_time_window(NewOrder=#new_order{waypoints=[_, #new_order_waypoint{stop_window=StopWindow}|_]}, _RequestingUserId)
  when StopWindow =:= undefined ->
	{ok, NewOrder};
fill_time_window(NewOrder=#new_order{order_type_id=?DB_ORDER_TYPE_TAKEAWAY}, _RequestingUserId) ->
	{ok, NewOrder};
fill_time_window(NewOrder=#new_order{account=#new_order_account{id_account=AccountId, id_slot=SlotId},
                                     destination=Destination},
                 RequestingUserId) ->
	try
		StopWindow =
			case eb_db_util:execute({get_account_slot_information, AccountId, SlotId, RequestingUserId}) of
				#inf_account_slots{lower_slot=LowerSlot, upper_slot=UpperSlot} ->
					#time_window{from=LowerSlot, to=UpperSlot};
				_ -> throw(error)
			end,
		RetDestination = Destination#new_order_waypoint{stop_window=StopWindow},
		RetNewOrder = NewOrder#new_order{destination=RetDestination},
		{ok, RetNewOrder}
	catch
		throw:error -> {nok, fill_time_window_error}
	end.
