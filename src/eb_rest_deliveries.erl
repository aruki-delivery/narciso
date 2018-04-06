%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_rest_deliveries).

-include("eb_constants.hrl").

-behaviour(kb_action_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/3]).

%% Get all deliveries
%% GET /deliveries
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
		CourierId =
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"courier_id">>) of
				{ok, CourierIdArg} -> CourierIdArg;
				_ -> throw({invalid_courier_id_param, Request1})
			end,
		OrderId =
			case eb_rest_util:get_optional_integer_arg_value(Args, <<"order_id">>) of
				{ok, OrderIdArg} -> OrderIdArg;
				_ -> throw({invalid_order_id_param, Request1})
			end,
		Navigation =
			case eb_rest_util:get_rs_navigation(Args) of
				{ok, NavigationArgs} -> NavigationArgs;
				nok -> throw({invalid_navigation_params, Request1})
			end,
		% Get deliveries information
		case eb_api_deliveries:get_deliveries(RequestingUserId, RequestingUserTypeId, StatusId, CourierId, OrderId, Navigation) of
			{ok, Results} -> eb_rest_util:return_success(Results, Request1);
			{nok, Error} -> throw({Error, Request1});
			_ -> throw({unexpected, Request1})
		end
	catch
		throw:{invalid_status_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid status id parameter.">>, LastRequest);
		throw:{invalid_courier_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid courier_id parameter.">>, LastRequest);
		throw:{invalid_order_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid order_id parameter.">>, LastRequest);
		throw:{invalid_navigation_params, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The navigation parameters are invalid.">>, LastRequest);
		throw:{invalid_parameters, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, LastRequest);
		throw:{forbidden, LastRequest} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get other users deliveries information.">>, LastRequest);
		throw:{_, LastRequest} -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, LastRequest)
	end;

%% Get the delivery statuses
%% GET /deliveries/statuses
handle(<<"GET">>, [<<"statuses">>], Request) ->
	case eb_api_deliveries:get_delivery_statuses() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get the waypoint statuses
%% GET /deliveries/waypoint-statuses
handle(<<"GET">>, [<<"waypoint-statuses">>], Request) ->
	case eb_api_deliveries:get_waypoint_statuses() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get the object statuses
%% GET /deliveries/object-statuses
handle(<<"GET">>, [<<"object-statuses">>], Request) ->
	case eb_api_deliveries:get_object_statuses() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get the object actions
%% GET /deliveries/object-actions
handle(<<"GET">>, [<<"object-actions">>], Request) ->
	case eb_api_deliveries:get_object_actions() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get the deliveries object types
%% GET /deliveries/object-types
handle(<<"GET">>, [<<"object-types">>], Request) ->
	case eb_api_deliveries:get_object_types() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get transport types for deliveries
%% GET /deliveries/transport-types
handle(<<"GET">>, [<<"transport-types">>], Request) ->
	case eb_api_users:get_transport_types() of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Get counters for online couriers
%% GET /deliveries/counters/ongoing
handle(<<"GET">>, [<<"counters">>, <<"ongoing">>], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_api_deliveries:get_ongoing_deliveries_counters(RequestingUserTypeId) of
		{ok, Results} -> eb_rest_util:return_success(Results, Request);
		{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
		{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get counter information.">>, Request);
		_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
	end;

%% Change Delivery Status
%% PATCH /deliveries/<id>
handle(<<"PATCH">>, [DeliveryId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	try
		TargetDeliveryId =
			case eb_util:binary_to_int(DeliveryId) of
				{ok, DeliveryIdParam} -> DeliveryIdParam;
				_Other -> throw({invalid_delivery_id_param, Request})
			end,
		{RecordResp, ChangeDelivery, Request1} = eb_rest_util:get_record_parameter(Request, #change_delivery{}),
		RecordResp =:= ok orelse throw({invalid_record, Request1}),

		% Change the delivery status
		case eb_api_deliveries:change_delivery(RequestingUserId, RequestingUserTypeId, TargetDeliveryId, ChangeDelivery) of
			{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
			{nok, Error} -> throw({Error, Request1});
			_ -> throw({unexpected, Request1})
		end
	catch
		throw:{invalid_delivery_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid delivery id parameter.">>, LastRequest);
		throw:{invalid_record, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change delivery record is invalid.">>, LastRequest);
		throw:{ParameterError, LastRequest} when ParameterError =:= missing_values orelse
												 ParameterError =:= invalid_parameters orelse
												 ParameterError =:= invalid_delivery_status_id orelse
												 ParameterError =:= missing_delivery_status_id ->
			BinParameterError = atom_to_binary(ParameterError, latin1),
			Description = <<<<"The change delivery record has invalid parameters: ">>/binary, BinParameterError/binary>>,
			eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, LastRequest);
		throw:{forbidden, LastRequest} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"Users are only allowed to change their own deliveries.">>, LastRequest);
		throw:{status, LastRequest} -> eb_rest_util:return_error(403, <<"status">>, <<"Invalid delivery status.">>, LastRequest);
		throw:{_, LastRequest} -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the delivery.">>, LastRequest)
	end;

%% Change waypoint (Modify a delivery's waypoint status (checkin, checkout), etc
%% PATCH /deliveries/<id>/waypoints/<id>
handle(<<"PATCH">>, [DeliveryId, <<"waypoints">>, WaypointId], Request) ->
	try
		[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth

		TargetDeliveryId =
			case eb_util:binary_to_int(DeliveryId) of
				{ok, DeliveryIdParam} -> DeliveryIdParam;
				_ -> throw({invalid_delivery_id_param, Request})
			end,
		TargetWaypointId =
			case eb_util:binary_to_int(WaypointId) of
				{ok, WaypointIdParam} -> WaypointIdParam;
				_ -> throw({invalid_waypoint_id_param, Request})
			end,
		{RecordResp, ChangeDeliveryWaypoint, Request1} = eb_rest_util:get_record_parameter(Request, #change_delivery_waypoint{}),
		RecordResp =:= ok orelse throw({invalid_record, Request1}),

		% Change the delivery waypoint
		case eb_api_deliveries:change_delivery_waypoint(RequestingUserId, RequestingUserTypeId, TargetDeliveryId, TargetWaypointId, ChangeDeliveryWaypoint) of
			{ok, NewVersion} -> eb_rest_util:return_json(201, [{<<"version">>, NewVersion}], Request1);
			{nok, Error} -> throw({Error, Request1});
			_ -> throw({unexpected, Request1})
		end
	catch
		throw:{invalid_delivery_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid delivery id parameter.">>, LastRequest);
		throw:{invalid_waypoint_id_param, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid waypoint id parameter.">>, LastRequest);
		throw:{invalid_record, LastRequest} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change delivery waypoint record is invalid.">>, LastRequest);
		throw:{version, LastRequest} -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong delivery version">>, LastRequest);
		throw:{NotFoundError, LastRequest} when NotFoundError =:= not_found_delivery orelse
												NotFoundError =:= not_found_waypoint ->
			BinNotFoundError = atom_to_binary(NotFoundError, latin1),
			eb_rest_util:return_error(404, BinNotFoundError, <<"The requested resource was not found.">>, LastRequest);
		throw:{ParameterError, LastRequest} when ParameterError =:= missing_values orelse
												 ParameterError =:= invalid_parameters orelse
												 ParameterError =:= missing_signature ->
			BinParameterError = atom_to_binary(ParameterError, latin1),
			Description = <<<<"The change delivery waypoint record has invalid parameters: ">>/binary, BinParameterError/binary>>,
			eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, LastRequest);
		throw:{forbidden, LastRequest} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"Users are only allowed to change their own delivery waypoints.">>, LastRequest);
		throw:{_, LastRequest} -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the delivery waypoint.">>, LastRequest)
	end;
	
%% Get delivery waypoint
%% GET /deliveries/<id>/waypoints/<id>
handle(<<"GET">>, [DeliveryId, <<"waypoints">>, WaypointId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(DeliveryId) of
		{ok, TargetDeliveryId} ->
			case eb_util:binary_to_int(WaypointId) of
				{ok, TargetWaypointId} ->
					case eb_api_deliveries:get_delivery_waypoint(RequestingUserId, RequestingUserTypeId, TargetDeliveryId, TargetWaypointId) of
						{ok, Result} -> eb_rest_util:return_success(Result, Request);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Delivery ID or Waypoint ID not found.">>, Request);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this delivery.">>, Request);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
					end;
				_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Waypoint ID.">>, Request)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid delivery ID.">>, Request)
	end;

%% Get a delivery waypoint signature
%% GET /deliveries/<id>/waypoints/<id>/signature
handle(<<"GET">>, [DeliveryId, <<"waypoints">>, WaypointId, <<"signature">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(DeliveryId) of
		{ok, TargetDeliveryId} ->
			case eb_util:binary_to_int(WaypointId) of
				{ok, TargetWaypointId} ->
					case eb_api_deliveries:get_waypoint_signature(RequestingUserId, RequestingUserTypeId, TargetDeliveryId, TargetWaypointId) of
						{ok, Result} -> eb_rest_util:return_success(Result, Request);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Delivery ID, Waypoint ID or signature not found.">>, Request);
						{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this delivery waypoint signature.">>, Request);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
					end;
				_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid Waypoint ID.">>, Request)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid delivery ID.">>, Request)
	end;

%% Get delivery
%% GET /deliveries/<id>
handle(<<"GET">>, [DeliveryId], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(DeliveryId) of
		{ok, TargetDeliveryId} ->
			case eb_api_deliveries:get_delivery(RequestingUserId, RequestingUserTypeId, TargetDeliveryId) of
				{ok, Result} -> eb_rest_util:return_success(Result, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Delivery ID not found.">>, Request);
				{nok, invalid_parameters} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The request has invalid parameters.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get this delivery information.">>, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Other -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid delivery ID.">>, Request)
	end;

%% Change Delivery Courier
%% PATCH /deliveries/<id>/courier
handle(<<"PATCH">>, [DeliveryId, <<"courier">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(DeliveryId) of
		{ok, IDeliveryId} ->
			case eb_rest_util:get_record_parameter(Request, #change_delivery_courier{}) of
				{ok, ChangeDeliveryCourier, Request1} ->
					case eb_api_deliveries:assign_delivery_to_courier(RequestingUserId, RequestingUserTypeId, IDeliveryId, ChangeDeliveryCourier) of
						{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Delivery ID not found.">>, Request1);
						{nok, version} -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong delivery version">>, Request1);
						{nok, ParameterError} when ParameterError =:= missing_values orelse ParameterError =:= invalid_parameters
											orelse ParameterError =:= invalid_delivery_status orelse ParameterError =:= missing_delivery_status
											orelse ParameterError =:= invalid_delivery_type orelse ParameterError =:= invalid_courier_status 
											orelse ParameterError =:= courier_not_found orelse ParameterError =:= invalid_order_status 
											orelse ParameterError =:= same_courier -> 
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The change delivery courier record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"Users are only allowed to change their own deliveries.">>, Request);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the delivery.">>, Request1)
					end;
				{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change delivery courier record is invalid.">>, Request1)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid delivery ID.">>, Request)
	end;

%% Change Delivery Skipping action
%% PATCH /deliveries/<id>/skip
handle(<<"PATCH">>, [DeliveryId, <<"skip">>], Request) ->
	[RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(DeliveryId) of
		{ok, IDeliveryId} ->
			case eb_rest_util:get_record_parameter(Request, #change_delivery_action{}) of
				{ok, ChangeDeliveryAction, Request1} ->
					case eb_api_deliveries:change_delivery_skip(RequestingUserId, RequestingUserTypeId, IDeliveryId, ChangeDeliveryAction) of
						{ok, NewVersion} -> eb_rest_util:return_json(200, [{<<"version">>, NewVersion}], Request1);
						{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Delivery ID not found.">>, Request1);
						{nok, version} -> eb_rest_util:return_error(409, <<"conflict">>, <<"Wrong delivery version">>, Request1);
						{nok, already_accepted} -> eb_rest_util:return_error(409, <<"already_accepted">>, <<"Delivery assigned to a courier">>, Request1);
						{nok, ParameterError} when ParameterError =:= missing_values orelse ParameterError =:= invalid_parameters
						                    orelse ParameterError =:= invalid_delivery_status orelse ParameterError =:= missing_delivery_status ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The change delivery courier record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request1);
						{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"Users are only allowed to change their own deliveries.">>, Request);
						_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error changing the delivery.">>, Request1)
					end;
				{nok, _Error, Request1} -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"The change delivery courier record is invalid.">>, Request1)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid delivery ID.">>, Request)
	end;

%% Get details for online ouriers
%% GET /deliveries/<id>/couriers/dispatcher
handle(<<"GET">>, [DeliveryId, <<"couriers">>, <<"dispatcher">>], Request) ->
	[_RequestingUserId, RequestingUserTypeId] = kb_action_helper:get_attributes(Request), % set by eb_filter_auth
	case eb_util:binary_to_int(DeliveryId) of
		{ok, IDeliveryId} ->
			case eb_api_deliveries:get_online_ctt_couriers(RequestingUserTypeId, IDeliveryId) of
				{ok, Results} -> eb_rest_util:return_success(Results, Request);
				{nok, not_found} -> eb_rest_util:return_error(404, <<"not_found">>, <<"Delivery ID not found.">>, Request);
				{nok, forbidden} -> eb_rest_util:return_error(403, <<"invalid_credentials">>, <<"User is not allowed to get detail information.">>, Request);
				{nok, ParameterError} when ParameterError =:= no_couriers orelse ParameterError =:= invalid_parameters
						                    orelse ParameterError =:= unknown_delivery ->
							BinParameterError = atom_to_binary(ParameterError, latin1),
							Description = <<<<"The change delivery courier record has invalid parameters: ">>/binary, BinParameterError/binary>>,
							eb_rest_util:return_error(400, <<"invalid_parameters">>, Description, Request);
				_Other -> eb_rest_util:return_error(500, <<"unexpected_error">>, <<"Unexpected error.">>, Request)
			end;
		_Error -> eb_rest_util:return_error(400, <<"invalid_parameters">>, <<"Invalid delivery ID.">>, Request)
	end;

%handle(_Method, _Path, Request) ->
handle(Method, Path, Request) ->
	Attributes = kb_action_helper:get_attributes(Request),
	error_logger:info_msg("\nMethod [~p]\nPath [~p]\nRequest [~p]\nAttributes [~p]\n", [Method, Path, Request, Attributes]),
	{raw, 404, [], "Request malandro...", Request}.
