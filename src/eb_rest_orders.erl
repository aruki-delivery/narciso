%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_rest_orders).

-include("eb_constants.hrl").

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Create a order
%% POST /orders
handle(<<"POST">>, [], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_rest_util:get_record_parameter(Request, #new_order{}) of
		{ok, NewOrder, Request1} ->
			case eb_api_orders:create_order(RequestingUserId, RequestingUserTypeId, NewOrder) of
				{ok, OrderId} -> eb_rest_util:return_json(201, [{<<"order_id">>, OrderId}], Request1);
				{nok, ParameterError} when ParameterError =:= missing_values orelse ParameterError =:= invalid_parameters
									orelse ParameterError =:= unknown_order_type_id orelse ParameterError =:= invalid_order_type_id
									orelse ParameterError =:= unknown_transport_type_id orelse ParameterError =:= invalid_transport_type_id
									orelse ParameterError =:= unknown_object_type_id orelse ParameterError =:= invalid_object_type_id
									orelse ParameterError =:= already_used_payment_id orelse ParameterError =:= invalid_payment_id
									orelse ParameterError =:= unknown_payment_provider_id orelse ParameterError =:= invalid_payment_provider_id
									orelse ParameterError =:= payment_provider_not_found orelse ParameterError =:= invalid_payment_status_id
									orelse ParameterError =:= unknown_payment_method_id orelse ParameterError =:= invalid_payment_method_id
									orelse ParameterError =:= invalid_cut_time orelse ParameterError =:= invalid_account_id
				  					orelse ParameterError =:= invalid_slot_id
									orelse ParameterError =:= invalid_time ->
					BinParameterError = atom_to_binary(ParameterError, latin1),
					Description = <<<<"The new order record has invalid parameters: ">>/binary, BinParameterError/binary>>,
					eb_rest_util:return_error(400, BinParameterError, Description, Request1);
				{nok, version} -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong Account version">>, Request1);
				{nok, account_not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Account Id not found.">>, Request1);
				{nok, slot_not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Slot Id not found.">>, Request1);
				{nok, slot_unavailable} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Slot Id unavailable.">>, Request1);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to create an order.">>, Request1);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error creating the order.">>, Request1)
			end;
		{nok, _Other, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The new order record is invalid.">>, Request1)
	end;

%% Update order status
%% PATCH /orders
handle(<<"PATCH">>, [OrderId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	{RecordResp, ChangeOrderStatus, Request1} = eb_rest_util:get_record_parameter(Request, #change_order_status{}),
	try
		TargetOrderId =
			case eb_util:binary_to_int(OrderId) of
				{ok, IntOrderId} -> IntOrderId;
				_ -> throw(invalid_order_id)
			end,
		RecordResp =:= ok orelse throw(invalid_record),
		case eb_api_orders:change_order_status(RequestingUserId, RequestingUserTypeId, TargetOrderId, ChangeOrderStatus) of
			{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
			{nok, ParameterError} -> throw(ParameterError);
			_ -> throw(unexpected)
		end
	catch
		throw:invalid_order_id -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Order Id.">>, Request);
		throw:invalid_record -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change order status record is invalid.">>, Request1);
		throw:FParameterError when FParameterError =:= missing_values orelse
								   FParameterError =:= invalid_parameters orelse
								   FParameterError =:= invalid_order_status_id orelse
								   FParameterError =:= unknown_order_status_id ->
			BinParameterError = atom_to_binary(FParameterError, latin1),
			Description = <<<<"The change order status record has invalid parameters: ">>/binary, BinParameterError/binary>>,
			eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
		throw:invalid_order_in_delivery -> eb_rest_util:return_error(403, <<"invalid_order_in_delivery">>, <<"Order already in a delivery.">>, Request1);
		throw:forbidden -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change order.">>, Request1);
		throw:not_found -> eb_rest_util:return_error(404, <<"not_found">>, <<"Order Id not found.">>, Request1);
		throw:version -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong order version">>, Request1);
		throw:_ -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the order.">>, Request1)
	end;

%% Update order cut time
%% PATCH /orders/delay
handle(<<"PATCH">>, [OrderId, <<"delay">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	{RecordResp, ChangeOrderDelay, Request1} = eb_rest_util:get_record_parameter(Request, #change_order_delay{}),
	try
		TargetOrderId =
			case eb_util:binary_to_int(OrderId) of
				{ok, IntOrderId} -> IntOrderId;
				_ -> throw(invalid_order_id)
			end,
		RecordResp =:= ok orelse throw(invalid_record),
		case eb_api_orders:change_order_delay(RequestingUserId, RequestingUserTypeId, TargetOrderId, ChangeOrderDelay) of
			{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
			{nok, ParameterError} -> throw(ParameterError);
			_ -> throw(unexpected)
		end
	catch
		throw:invalid_order_id -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Order Id.">>, Request);
		throw:invalid_record -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change delay record is invalid.">>, Request1);
		throw:FParameterError when FParameterError =:= invalid_parameters orelse
								   FParameterError =:= invalid_order_cut_time ->
			BinParameterError = atom_to_binary(FParameterError, latin1),
			Description = <<<<"The change order status record has invalid parameters: ">>/binary, BinParameterError/binary>>,
			eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
		throw:invalid_order_in_delivery -> eb_rest_util:return_error(403, <<"invalid_order_in_delivery">>, <<"Order already in a delivery.">>, Request1);
		throw:invalid_order_status -> eb_rest_util:return_error(403, <<"invalid_order_status">>, <<"Invalid order status.">>, Request1);
		throw:invalid_parameters -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change delay record has invalid parameters.">>, Request1);
		throw:forbidden -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change order.">>, Request1);
		throw:not_found -> eb_rest_util:return_error(404, <<"not_found">>, <<"Order Id not found.">>, Request1);
		throw:version -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong order version">>, Request1);
		throw:_ -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the order.">>, Request1)
	end;

%% Reallocate order
%% PATCH /orders/<id>/reallocate
handle(<<"PATCH">>, [OrderId, <<"reallocate">>], Request) ->
		[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
		{RecordResp, DelayOrderTime, Request1} = eb_rest_util:get_record_parameter(Request, #reallocate_order{}),
	try
		TargetOrderId =
			case eb_util:binary_to_int(OrderId) of
				{ok, IntOrderId} -> IntOrderId;
				_ -> throw(invalid_order_id)
			end,
		RecordResp =:= ok orelse throw(invalid_record),
		case eb_api_orders:reallocate_order(RequestingUserId, RequestingUserTypeId, TargetOrderId, DelayOrderTime) of
			{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
			{nok, ParameterError} -> throw(ParameterError);
			_ -> throw(unexpected)
		end
	catch
		throw:invalid_order_id -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Order Id.">>, Request);
		throw:invalid_record -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The reallocate order record is invalid.">>, Request1);
		throw:invalid_parameters -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The reallocate order record is invalid.">>, Request1);
		throw:invalid_order_status -> eb_rest_util:return_error(403, <<"invalid_order_status">>, <<"The reallocate order had invalid order status.">>, Request1);
		throw:courier -> eb_rest_util:return_error(403, <<"invalid_order_status">>, <<"The delivery has courier assigned.">>, Request1);
		throw:forbidden -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to change order.">>, Request1);
		throw:not_found -> eb_rest_util:return_error(404, <<"not_found">>, <<"Order Id not found.">>, Request1);
		throw:version -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong order version">>, Request1);
		throw:_ -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the order.">>, Request1)
	end;

%% Get all orders
%% GET /orders
handle(<<"GET">>, [], Request) ->
	try
		[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	
		% Get query parameters
		{Args, Request1} = kb_action_helper:get_args(Request),
		
		StatusId =
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"status_id">>) of
				{ok, StatusIdArg} -> StatusIdArg;
				_ -> throw({invalid_status_id_param, Request1})
			end,
		TypeId =
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"type_id">>) of
				{ok, TypeIdArg} -> TypeIdArg;
				_ -> throw({invalid_type_id_param, Request1})
			end,
		PhoneNr =
			case eb_rest_util:get_optional_binary_arg_value(Args, <<"phone_nr">>) of
				{ok, PhoneNrArg} -> PhoneNrArg;
				_ -> throw({invalid_phone_nr_param, Request1})
			end,
		Navigation =
			case eb_rest_util:get_rs_navigation(Args) of
				{ok, NavigationArgs} -> NavigationArgs;
				nok -> throw({invalid_navigation_params, Request1})
			end,
		% Get deliveries information
		case eb_api_orders:get_orders(RequestingUserId, RequestingUserTypeId, StatusId, TypeId, PhoneNr, Navigation) of
			{ok, Results} -> eb_rest_util:return_success(Results, Request1);
			{nok, Error} -> throw({Error, Request1});
			_ -> throw({unexpected, Request1})
		end
	catch
		throw:{invalid_status_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid status id parameter.">>, LastRequest);
		throw:{invalid_type_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid type_id parameter.">>, LastRequest);
		throw:{invalid_phone_nr_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid phone_nr parameter.">>, LastRequest);
		throw:{invalid_navigation_params, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The navigation parameters are invalid.">>, LastRequest);
		throw:{invalid_parameters, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, LastRequest);
		throw:{forbidden, LastRequest} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get other users deliveries information.">>, LastRequest);
		throw:{_, LastRequest} -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, LastRequest)
	end;

%% Get orders statuses
%% GET /orders/statuses
handle(<<"GET">>, [<<"statuses">>], Request) ->
	case eb_api_orders:get_order_statuses() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get orders payment_methods
%% GET /orders/payment-methods
handle(<<"GET">>, [<<"payment-methods">>], Request) ->
	case eb_api_orders:get_order_payment_methods() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get orders transport types
%% GET /orders/transport-types
handle(<<"GET">>, [<<"transport-types">>], Request) ->
	case eb_api_users:get_transport_types() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get orders object types
%% GET /orders/object-types
handle(<<"GET">>, [<<"object-types">>], Request) ->
	case eb_api_deliveries:get_object_types() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get orders origins
%% GET /orders/origins
handle(<<"GET">>, [<<"origins">>], Request) ->
	case eb_api_orders:get_order_origins() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get orders types
%% GET /orders/types
handle(<<"GET">>, [<<"types">>], Request) ->
	case eb_api_orders:get_order_types() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get order information
%% GET /orders/<id>
handle(<<"GET">>, [OrderId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(OrderId) of
		{ok, TargetOrderId} ->
			case eb_api_orders:get_order(RequestingUserId, RequestingUserTypeId, TargetOrderId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Order Id not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The order has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this order information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid order ID.">>, Request)
	end;

%% Get order information
%% GET /orders/<id>/order-detail
handle(<<"GET">>, [OrderId, <<"order-detail">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(OrderId) of
		{ok, TargetOrderId} ->
			case eb_api_orders:get_order_backoffice(RequestingUserId, RequestingUserTypeId, TargetOrderId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Order Id not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The order has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this order information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid order ID.">>, Request)
	end;

%% Get today's slots' information
%% GET /orders/slots/today
handle(<<"GET">>, [<<"slots">>, <<"today">>], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_api_orders:get_todays_slots(RequestingUserTypeId) of
		{ok, Result} -> eb_rest_util:return_success(Result, Request);
		{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Order Id not found.">>, Request);
		{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this order information.">>, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Close s slot 
%% PATCH /orders/slot/<id>
handle(<<"PATCH">>, [<<"slot">>, SlotId], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(SlotId) of
		{ok, TargetSlotId} ->
			case eb_api_orders:update_order_occupancy(RequestingUserTypeId, TargetSlotId) of
				ok -> eb_rest_util:return_success(Request);
				{nok, slot_not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Slot Id not found.">>, Request);
				{nok, slot_unavailable} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Slot Id unavailable.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The order has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this order information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid slot ID.">>, Request)
	end;

%% Get order generated deliveries
%% GET /orders/<id>
handle(<<"GET">>, [OrderId, <<"generated-deliveries">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(OrderId) of
		{ok, TargetOrderId} ->
			case eb_api_orders:get_generated_deliveries(RequestingUserId, RequestingUserTypeId, TargetOrderId) of
				{ok, Results} -> eb_rest_util:return_success(Results, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"order Id not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The order has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this order information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid order Id.">>, Request)
	end;

handle(Method, Path, Request) ->
	Attributes = kb_action_helper:get_attributes(Request),
	error_logger:info_msg("\nMethod [~p]\nPath [~p]\nRequest [~p]\nAttributes [~p]\n", [Method, Path, Request, Attributes]),
	{raw, 404, [], "Request malandro...", Request}.
