%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
%% Código retirado do projecto SETools - SysVision Erlang Tools
%%
-module(eb_rest_util).

-include("eb_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_header/2, get_authorization_token/1, get_api_key/1, return_success/1, return_success/2, return_success/3, return_error/4,
         return_json/3, return_redirect/2, get_record_parameter/2, get_single_parameter/3, get_rs_navigation/1, return_image_success/2, get_arg_value/2,
         get_optional_integer_arg_value/2, get_optional_binary_arg_value/2, to_json/1, get_integer_arg_value/2, get_optional_datetime/2,
         get_optional_datetime/1, get_datetime/1, get_optional_string_arg_value/2]).
-export([set_json_parameters/2]).
get_header(Request, Header) ->
	{Headers, Request1} = kb_action_helper:get_headers(Request),
	case proplists:get_value(Header, Headers) of
		undefined -> {missing, Request1};
		Value when is_binary(Value) -> {ok, Value, Request1};
		_ -> {invalid, Request1}
	end.

get_authorization_token(Request) ->
	get_header(Request, ?HEADER_SESSION_TOKEN).

get_api_key(Request) ->
	get_header(Request, ?HEADER_API_KEY).

return_success(Request) ->
	{raw, 204, [{<<"Cache-Control">>, <<"no-store">>}, {<<"Pragma">>, <<"no-cache">>}], "", Request}.

return_success(ResponseObj, Request) ->
	return_json(200, to_json(ResponseObj), Request).

return_success(Code, ResponseObj, Request) ->
	return_json(Code, to_json(ResponseObj), Request).

return_error(Code, Value, ErrorObj, Request) when is_list(ErrorObj) ->
	return_json(Code, [{error, Value}, {error_description, to_json(ErrorObj)}], Request);

return_error(Code, Value, Description, Request) ->
	return_json(Code, [{error, Value}, {error_description, Description}], Request).

return_json(Code, JSONBody, Request) ->
	{json, Code, get_json_headers(), JSONBody, Request}.

return_image(Code, JSONBody, Request) ->
	{raw, Code, get_image_headers(), JSONBody, Request}.

return_image_success(ResponseObj, Request) ->
	return_image(200, to_json(ResponseObj), Request).

return_redirect(Url, Request) -> {redirect, Url, Request}.

get_record_parameter(Request, Record) ->
	get_record(Request, fun(Elements) -> build_record(Elements, Record) end).

get_single_parameter(Request, ParameterName, DefaultValue) ->
	get_record(Request, fun(Elements) -> build_single_parameter(Elements, ParameterName, DefaultValue) end).

get_rs_navigation(Args) ->
	get_rs_navigation(Args, #rs_navigation{}).

get_arg_value(Args, Key) ->
	case lists:keyfind(Key, 1, Args) of
		{_Key, Value} -> Value;
		_ -> undefined
	end.

get_optional_integer_arg_value(Args, Key) ->
	case get_arg_value(Args, Key) of
		undefined -> {ok, undefined};
		Value -> eb_util:binary_to_int(Value)
	end.

get_optional_binary_arg_value(Args, Key) ->
	case get_arg_value(Args, Key) of
		undefined -> {ok, undefined};
		Value -> {ok, Value}
	end.

get_integer_arg_value(Args, Key) ->
	case get_arg_value(Args, Key) of
		undefined -> undefined;
		Value -> eb_util:binary_to_int(Value)
	end.

get_optional_datetime(Args, Key) ->
	get_optional_datetime(get_arg_value(Args, Key)).

get_optional_datetime(undefined) -> {ok, undefined};
get_optional_datetime(Value) when is_integer(Value) ->
	from_timestamp(Value);
get_optional_datetime(Value) when is_binary(Value) ->
	case eb_util:binary_to_int(Value) of
		{ok, MillisecondsFromEpochUTC} -> from_timestamp(MillisecondsFromEpochUTC);
		Other -> Other
	end;
get_optional_datetime(_) -> error.

get_datetime(Value) when is_integer(Value) -> from_timestamp(Value);
get_datetime(_) -> error.

get_optional_string_arg_value(Args, Key) ->
	case get_arg_value(Args, Key) of
		undefined -> {ok, undefined};
		Value -> eb_util:binary_to_str(Value)
	end.

%
% JSON parsing functions
%
to_json([]) -> [];
to_json(List) when is_list(List) -> to_json([], List);

to_json(#user{id=UserId, id_type=UserTypeId, username=Username, id_status=UserStatusId, creation_date=CreationDate,
			  status_date=StatusDate, login_date=LoginDate, email=Email, first_name=FirstName, last_name=LastName,
			  telephone_nr=TelephoneNr, fiscal_id=FiscalId, mobileos_id=MobileosId, birth_day=BirthDay,
			  birth_month=BirthMonth, birth_year=BirthYear, national_id=NationalId, country=Country,
			  rating=Rating, version=Version}) ->
	JsonList = [{user_id, UserId}, {user_type_id, UserTypeId}, {username, Username},
				{user_status_id, UserStatusId}, {creation_date, to_json_timestamp(CreationDate)},
				{status_date, to_json_timestamp(StatusDate)}, {login_date, to_json_timestamp(LoginDate)},
				{email, Email}, {first_name, FirstName}, {last_name, LastName}, {telephone_nr, TelephoneNr},
				{fiscal_id, FiscalId}, {mobileos_id, MobileosId}, {birth_day, BirthDay}, {birth_month, BirthMonth},
				{birth_year, BirthYear}, {national_id, NationalId}, {country, Country}, {rating, Rating},
				{version, Version}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#inf_user{user=User, account=Account, account_user=AccountUser, user_address=UserAddress,
				  transports=Transports, zones=Zones}) ->
	UserJson = to_json(User),
	[VersionJson|ReversedUserJson] = lists:reverse(UserJson),
	WithZoneIds =
		case Zones of
			[] -> ReversedUserJson;
			_ -> [{zone_ids, [ZoneId || #user_zone{id_zone=ZoneId} <- Zones]}|ReversedUserJson]
		end,
	WithAddressJson =
		case UserAddress of
			[] -> WithZoneIds;
			_ -> [{address, to_json(UserAddress)}|WithZoneIds]
		end,
	WithAccountJson =
		case Account of
			#account{account_name=AccountName} ->
				#account_user{id_account=IdAccount, id_type=IdType} = AccountUser,
				AccountJson = {account, [{account_id, IdAccount}, {account_user_type_id, IdType}, {account_name, AccountName}]},
				[AccountJson|WithAddressJson];
			_ -> WithAddressJson
		end,
	WithTransportsJson =
		case Transports of
			[] -> WithAccountJson;
			_ -> [{transport_types, to_json(Transports)}|WithAccountJson]
		end,
	lists:reverse([VersionJson|WithTransportsJson]);

to_json(#user_address{id_user=_UserId, component=Component, value=Value}) -> [{component, Component}, {value, Value}];

to_json(#u_account_user{id=UserId, id_account_user_type=AccountUserTypeId, id_user_type=UserTypeId, username=Username, id_status=UserStatusId,
                        creation_date=CreationDate, status_date=StatusDate, email=Email, first_name=FirstName, last_name=LastName,
                        telephone_nr=TelephoneNr, fiscal_id=FiscalId, department_id=DepartmentId, cost_center_id=CostCenterId,
                        transport_types=TransportTypes, user_reference=UserReference, version=UserVersion}) ->
	WithVersionJson = [{user_version, UserVersion}],
	WithTransportTypesJson =
		case TransportTypes of
			[] -> WithVersionJson;
			_ -> [{transport_types, to_json(TransportTypes)}|WithVersionJson]
		end,
	[{user_id, UserId}, {account_user_type_id, AccountUserTypeId}, {user_type_id, UserTypeId}, {username, Username}, {user_status_id, UserStatusId},
	 {creation_date, to_json_timestamp(CreationDate)}, {status_date, to_json_timestamp(StatusDate)},
	 {email, Email}, {first_name, FirstName}, {last_name, LastName}, {telephone_nr, TelephoneNr}, {fiscal_id, FiscalId},
	 {department_id, DepartmentId}, {cost_center_id, CostCenterId}, {user_reference, UserReference}|WithTransportTypesJson];

to_json(#contact_request{id=ContactRequestId, email=Email, name=Name, subject=Subject, content=Content, id_status=ContactRequestStatusId,
                         creation_date=CreationDate, status_date=StatusDate, id_user=UserId, id_operator_user=OperatorId,
                         operator_notes=OperatorNotes, version=Version}) ->
	[{contact_request_id, ContactRequestId}, {email, Email}, {name, Name}, {subject, Subject}, {content, Content},
	 {contact_request_status_id, ContactRequestStatusId}, {creation_date, to_json_timestamp(CreationDate)},
	 {status_date, to_json_timestamp(StatusDate)}, {user_id, UserId}, {operator_id, OperatorId}, {operator_notes, OperatorNotes}, {version, Version}];

to_json(#parameterization{id=Id, value=Value, description=Description, version=Version}) ->
	[{id, Id}, {value, Value}, {description, Description}, {version, Version}];

to_json(#token_type{id=Id, description=Description, multiple_uses=MultipleUses, uses=Uses, expires_in_minutes=ExpiresInMinutes, version=Version}) ->
	[{id, Id}, {description, Description}, {multiple_uses, MultipleUses}, {uses, Uses}, {expires_in_minutes, ExpiresInMinutes}, {version, Version}];

to_json(#account_department{id_department=DepartmentId, description=Description}) ->
	[{department_id, DepartmentId}, {description, Description}];

to_json(#inf_account{account=#account{id=AccountId, unique_key=UniqueKey, id_status=AccountStatusId, account_name=AccountName,
                                      fiscal_id=FiscalId, telephone_nr=TelephoneNr, email=Email,
                                      creation_date=CreationDate, status_date=StatusDate, occupancy=Occupancy, version=Version},
                     account_infos=AccountInfos,
                     account_address=AccountAddress}) ->
	AdditionalInfos = to_json(AccountInfos),
	Address = to_json(AccountAddress),
	[{account_id, AccountId}, {unique_key, UniqueKey}, {account_status_id, AccountStatusId},
	 {account_name, AccountName}, {fiscal_id, FiscalId}, {telephone_nr, TelephoneNr}, {email, Email},
	 {address, Address}, {additional_infos, AdditionalInfos},
	 {creation_date, to_json_timestamp(CreationDate)}, {status_date, to_json_timestamp(StatusDate)}, {occupancy, Occupancy}, {version, Version}];

to_json(#account_info{id_account=_AccountId, property=Property, value=Value}) -> [{property, Property}, {value, Value}];

to_json(#account_address{id_account=_AccountId, component=Component, value=Value}) -> [{component, Component}, {value, Value}];

to_json(#inf_account_users{version=AccountVersion, users=AccountUsers}) ->
	Users = to_json(AccountUsers),
	[{account_version, AccountVersion}, {users, Users}];

to_json(#inf_account_slots{id=Id, lower_slot=SlotLower, upper_slot=SlotUpper, occupancy=Occupancy, max_occupancy=MaxOccupancy, availability=Availability}) ->
	[{id, Id}, {lower_slot, to_json_timestamp(SlotLower)}, {upper_slot, to_json_timestamp(SlotUpper)}, {occupancy, Occupancy}, {max_occupancy, MaxOccupancy}, {availability, Availability}];

to_json(#inf_account_slots_temp{id=Id, lower_slot=SlotLower, upper_slot=SlotUpper, occupancy=Occupancy, max_occupancy=MaxOccupancy, availability=Availability}) ->
	[{id, Id}, {lower_slot, to_json_datetime(SlotLower)}, {upper_slot, to_json_datetime(SlotUpper)}, {occupancy, Occupancy}, {max_occupancy, MaxOccupancy}, {availability, Availability}];

to_json(#notification_type{id=Id, description=Description, notify_client=_NotifyClient}) -> [{id, Id}, {description, Description}];

to_json(#courier_counters{unavailable=Unavailable, available=Available, working=Working}) ->
	[{unavailable, Unavailable}, {available, Available}, {working, Working}];

to_json(#session_info{session_token=_SessionToken, id_user=UserId, id_user_type=_UserTypeId, invalid_after=_InvalidAfter,
							 status=Status, push_info=PushInfo, position=Position, id_transport_type=_TransportTypeId,
							 notify=_Notify}) ->
	JsonWithPosition =
		case Position of
			undefined -> [];
			#position{latitude=Latitude, longitude=Longitude} ->
				PositionJSON = position_to_json(Latitude, Longitude),
				[{position, PositionJSON}]
		end,
	JsonWithPushInfo =
		case PushInfo of
			undefined -> JsonWithPosition;
			_ ->
				PushInfoJSON = to_json(PushInfo),
				[{push_info, PushInfoJSON}|JsonWithPosition]
		end,
	JsonWithStatus =
		case Status of
			undefined -> JsonWithPushInfo;
			_ -> [{status, Status}|JsonWithPushInfo]
		end,
	[{user_id, UserId}|JsonWithStatus];

to_json(#inf_online_courier{session_info=SessionInfo, user=#user{first_name=FirstName, last_name=LastName,
																					  reference=Reference},
									 photo=Photo, assigned_deliveries=AssignedDeliveries}) ->
	JsonWithTransportTypeId = [{transport_type_id, SessionInfo#session_info.id_transport_type},
							   {assigned_deliveries, to_json(AssignedDeliveries)}],
	JsonWithReference =
		case Reference of
			undefined -> JsonWithTransportTypeId;
			_ -> [{user_reference, Reference}|JsonWithTransportTypeId]
		end,
	JsonWithPhoto =
		case Photo of
			undefined -> JsonWithReference;
			_ -> [{photo, to_json(Photo)}|JsonWithReference]
		end,
	JsonWithLastName = 
		case LastName of
			undefined -> JsonWithPhoto;
			_ -> [{last_name, LastName}|JsonWithPhoto]
		end,
	JsonWithFirstName = 
		case FirstName of
			undefined -> JsonWithLastName;
			_ -> [{first_name, FirstName}|JsonWithLastName]
		end,
	to_json(SessionInfo) ++ JsonWithFirstName;

to_json(#inf_online_courier_account{id_account=AccountId, name=Name}) ->
	[{account_id, AccountId}, {name, Name}];

to_json(#push_info{id=Id, type=Type}) -> [{push_id, Id}, {push_type, Type}];

to_json(#courier_deliveries{pending_deliveries=Pending, assigned_deliveries=Assigned}) ->
	[{pending_deliveries, to_json(Pending)}, {assigned_deliveries, to_json(Assigned)}];

to_json(#inf_delivery{delivery=#delivery{id=DeliveryId, reference=DeliveryReference, id_transport_type=TransportTypeId,
										 id_transport=_TransportId, distance=Distance, duration=Duration, route=Route,
										 id_courier=CourierId, skip=Skip, cut_time=CutTime, id_status=DeliveryStatusId,
										 creation_date=CreationDate, status_date=StatusDate, version=Version},
					  waypoints=Waypoints, objects=Objects, orders=Orders}) ->
	% The first waypoint is the delivery origin. The last waypoint is the destination. The remainder waypoints are json.waypoints
	[Origin|TmpWaypoints] = Waypoints,
	{RemWaypoints, [Destination]} = lists:split(length(TmpWaypoints)-1, TmpWaypoints),

	JsonList = [{delivery_id, DeliveryId}, {delivery_reference, DeliveryReference}, {transport_type_id, TransportTypeId},
			 {distance, Distance}, {duration, Duration}, {route, Route}, {courier_id, CourierId},
			 {cut_time, to_json_timestamp(CutTime)}, {skip_action, Skip}, {delivery_status_id, DeliveryStatusId},
			 {creation_date, to_json_timestamp(CreationDate)}, {status_date, to_json_timestamp(StatusDate)},
			 {version, Version}, {origin, to_json(Origin)}, {waypoints, to_json(RemWaypoints)},
			 {destination, to_json(Destination)}, {objects, to_json(Objects)}, {orders, to_json(Orders)}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#inf_delivery_waypoint{waypoint=#u_delivery_waypoint{id_delivery=_DeliveryId, id_waypoint=WaypointId,
															 latitude=Latitude, longitude=Longitude,
															 travel_distance=TravelDistance,
															 travel_time_est=TravelTimeEst,
															 travel_time_real=TravelTimeReal,
															 stop_duration_est=StopDurationEst,
															 stop_duration_real=StopDurationReal,
															 checkin_date=CheckinDate, checkout_date=CheckoutDate,
															 checkin_lat=CheckInLat, checkin_lon=CheckInLon,
															 checkout_lat=CheckOutLat, checkout_lon=CheckOutLon,
															 id_status=StatusId, creation_date=CreationDate,
															 status_date=StatusDate, version=Version},
							   details=WaypointDetails}) ->
	JsonPositionChk =
		case lists:any(fun(X) -> X =/= undefined andalso X =/= null end, [CheckInLat, CheckInLon, CheckOutLat, CheckOutLon]) of
			true -> {position_chk, position_chk_to_json(CheckInLat, CheckInLon, CheckOutLat, CheckOutLon)};
			false -> {position_chk, null}
		end,
	JsonList = [{waypoint_id, WaypointId}, {id_status, StatusId}, {position, position_to_json(Latitude, Longitude)},
				{travel_distance, TravelDistance}, {travel_time_est, TravelTimeEst}, {travel_time_real, TravelTimeReal},
				{stop_duration_est, StopDurationEst}, {stop_duration_real, StopDurationReal},
				{checkin_date, to_json_timestamp(CheckinDate)}, {checkout_date, to_json_timestamp(CheckoutDate)},
				JsonPositionChk, {creation_date, to_json_timestamp(CreationDate)}, {status_date, to_json_timestamp(StatusDate)},
				{version, Version}, {details, to_json(WaypointDetails)}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#u_delivery_waypoint_detail{id_delivery=_DeliveryId, id_delivery_waypoint=_DeliverWaypointId,
									id_order=OrderId, id_waypoint=_WaypointId, reference=Reference,
									formatted_address=FormattedAddress, address=Address, contact_name=ContactName,
									contact_phone_nr=ContactPhoneNr, contact_email=ContactEmail, client_name=ClientName,
									client_phone_nr=ClientPhoneNr, client_email=ClientEmail,
									client_fiscal_id=ClientFiscalId, user_id=UserId, notes=Notes,
									signature=Signature, stop_from=StopFrom, stop_to=StopTo,
									stop_duration=StopDuration, rating=Rating, object_actions=ObjectActions}) ->
	JsonList = [{order_id, OrderId}, {reference, Reference}, {formatted_address, FormattedAddress}, {address, to_json(Address)},
				{contact_info, contact_to_json(ContactName, ContactPhoneNr, ContactEmail)},
				{client_info, client_contact_to_json(ClientName, ClientPhoneNr, ClientEmail, ClientFiscalId)},
				{user_id, UserId}, {notes, Notes}, {signature, Signature},
				{stop_window, time_window_to_json(StopFrom, StopTo)}, {stop_duration, StopDuration},
				{rating, Rating}, {object_actions, to_json(ObjectActions)}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#delivery_object{id_delivery=_DeliveryId, id_object=ObjectId, reference=Reference, id_type=TypeId,
						 transport_auth=TransportAuth, non_del_reason=NonDelReason}) ->
	JsonList = [{object_id, ObjectId}, {reference, Reference}, {type_id, TypeId}, {transport_auth, TransportAuth},
				{non_del_reason, NonDelReason}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#courier_transport{id=Id, id_user=_UserId, id_transport_type=TransportTypeId, current=Current, id_status=StatusId,
									creation_date=_CreationDate, status_date=_StatusDate, description=Description,
									registration_id=RegistrationId, color=Color}) ->
	[{id, Id}, {transport_type_id, TransportTypeId}, {current, Current}, {status_id, StatusId},
	 {description, Description}, {registration_id, RegistrationId}, {color, Color}];

to_json(#reference_data{id=Id, description=Description}) -> [{id, Id}, {description, Description}];

to_json(#transport_type{id=Id, description=Description, capacity=Capacity}) ->
	[{id, Id}, {description, Description}, {capacity, Capacity}];

to_json(#object_type{id=Id, description=Description, size=Size, length=Length, width=Width,
					 height=Height, weight=Weight, volume=Volume}) ->
	JsonList = [{id, Id}, {description, Description}, {size, Size}, {length, Length},
				{width, Width}, {height, Height}, {weight, Weight}, {volume, Volume}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#position{latitude=Latitude, longitude=Longitude}) -> position_to_json(Latitude, Longitude);

to_json(#counter_ongoing_deliveries{id_status=DeliveryStatusId, total=Total}) ->
	 [{delivery_status_id, DeliveryStatusId}, {total, Total}];

to_json(#order_backoffice{client_name=ClientName, client_phone=ClientPhone, client_email=ClientEmail, 
                          client_nif=ClientNif, client_address=ClientAdress, client_prods=ClientProds, 
                          waypoint_detail=WaypointDetail, deliveries=Deliveries}) ->

	[{client_name, ClientName}, {client_phone, ClientPhone}, {client_email, ClientEmail},
	 {client_nif, ClientNif}, {client_address, to_json(ClientAdress)}, {client_products, to_json(ClientProds)},
	 {waypoint_details, to_json(WaypointDetail)}, {deliveries, to_json(Deliveries)}];
	
to_json(#waypoint_detail{id=WaypointId, checkin_date=CheckinDate, checkout_date=CheckoutDate}) ->
	[{id_waypoint, WaypointId}, {checkin_date, to_json_datetime(CheckinDate)},
	 {checkout_date, to_json_datetime(CheckoutDate)}];
	
to_json(#generated_delivery{id_delivery=DeliveryId, id_order=OrderId}) ->
	case OrderId of
		undefined -> JSON1 = [];
		_ -> JSON1 = [{id_order, OrderId}]
	end,
	case DeliveryId of
		undefined -> FinalJSON = JSON1;
		_ -> FinalJSON = [{id_delivery, DeliveryId}|JSON1]
	end,
	FinalJSON;	

to_json(#order_prod{id_order=_OrderId, id_product=ProductId, description=Description,
					name=Name, quantity=Quantity, gross_amount=GrossAmount}) ->

	case GrossAmount of
		undefined -> JSON1 = [];
		_ -> JSON1 = [{gross_amount, GrossAmount}]
	end,
	case Quantity of
		undefined -> JSON2 = JSON1;
		_ -> JSON2 = [{quantity, Quantity}|JSON1]
	end,
	case Name of
		undefined -> JSON3 = JSON2;
		_ -> JSON3 = [{name, Name}|JSON2]
	end,
	case Description of
		undefined -> JSON4 = JSON3;
		_ -> JSON4 = [{description, Description}|JSON3]
	end,
	case ProductId of
		undefined -> FinalJSON = JSON4;
		_ -> FinalJSON = [{id_product, ProductId}|JSON4]
	end,
	FinalJSON;

to_json(#inf_order{order=#order{id_order=OrderId, reference=OrderReference,
								distribution_center=DistributionCenter, id_user=UserId,
								id_type=OrderTypeId, id_transport_type=TransportTypeId,
								id_origin=OriginId, client_name=ClientName, client_phone_nr=ClientPhoneNr,
								client_email=ClientEmail, client_fiscal_id=ClientFiscalId, cut_time=CutTime, id_status=StatusId,
								id_payment=PaymentId, id_payment_method=PaymentMethodId, diner_qty=DinerQty,
								gross_total=GrossTotal, tip=Tip, fee=Fee, creation_date=CreationDate,
								status_date=StatusDate, start_prod_date=StartProdDate, version=Version},
				   waypoints=Waypoints, objects=Objects, order_prods=OrderProds,
				   additional_info=AdditionalInfo, courier_info=CourierInfo}) ->
	% The first waypoint is the order origin. The last waypoint is the destination.
	% The remainder waypoints are json.waypoints
	[Origin|TmpWaypoints] = Waypoints,

	{FRemWaypoints, FDestination} =
		case OrderTypeId =/= ?DB_ORDER_TYPE_TAKEAWAY of
			true ->
				{RemWaypoints, [Destination]} = lists:split(length(TmpWaypoints)-1, TmpWaypoints),
				{RemWaypoints, Destination};
			false -> {TmpWaypoints, []}
		end,
	JsonList = [{order_id, OrderId}, {order_reference, OrderReference}, {distribution_center, DistributionCenter},
				{order_type_id, OrderTypeId}, {user_id, UserId}, {transport_type_id, TransportTypeId},
				{origin_id, OriginId},
				{client_info, client_contact_to_json(ClientName, ClientPhoneNr, ClientEmail, ClientFiscalId)},
				{cut_time, to_json_timestamp(CutTime)}, {status_id, StatusId},
				{payment_id, PaymentId}, {payment_method_id, PaymentMethodId}, {diner_qty, DinerQty},
				{gross_total, GrossTotal}, {tip, Tip}, {fee, Fee},
				{creation_date, to_json_timestamp(CreationDate)}, {status_date, to_json_timestamp(StatusDate)},
				{start_prod_date, to_json_timestamp(StartProdDate)}, {version, Version},
				{order_prods, to_json(OrderProds)}, {origin, to_json(Origin)},
				{waypoints, to_json(FRemWaypoints)}, {destination, to_json(FDestination)},
				{objects, to_json(Objects)}, {additional_info, to_json(AdditionalInfo)},
				{courier_info, to_json(CourierInfo)}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#inf_order_waypoint{waypoint=#order_waypoint{id_order=_OrderId, id_waypoint=WaypointId,
													 reference=Reference, formatted_address=FormattedAddress,
													 latitude=Latitude, longitude=Longitude,
													 contact_name=ContactName, contact_phone_nr=ContactPhoneNr,
													 contact_email=ContactEmail, notes=Notes, signature=Signature,
													 stop_from=StopFrom, stop_to=StopTo,
													 stop_duration=StopDuration},
							object_actions=ObjectActions, address=Address}) ->
	JsonList = [{waypoint_id, WaypointId}, {reference, Reference}, {address, to_json(Address)},
				{formatted_address, FormattedAddress}, {position, position_to_json(Latitude, Longitude)}, 
				{contact_info, contact_to_json(ContactName, ContactPhoneNr, ContactEmail)}, {notes, Notes},
				{signature, Signature}, {stop_window, time_window_to_json(StopFrom, StopTo)},
				{stop_duration, StopDuration}, {object_actions, to_json(ObjectActions)}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#order_waypoint_object_action{id_order=_OrderId, id_waypoint=_WaypointId, id_object=ObjectId,
									  id_action=ActionId, action=Action}) ->
	JsonList = [{object_id, ObjectId}, {object_action_id, ActionId}, {object_action, Action}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#order_waypoint_address{id_order=_OrderId, id_waypoint=_WaypointId, component=Component, value=Value}) ->
	JsonList = [{component, Component}, {value, Value}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#order_object{id_order=_OrderId, id_object=ObjectId, reference=ObjectReference, id_type=ObjectTypeId,
					  transport_auth=TransportAuth}) ->
	JsonList = [{object_id, ObjectId}, {object_reference, ObjectReference}, {object_type_id, ObjectTypeId},
				{transport_auth, TransportAuth}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#inf_order_prod{order_prod=#order_prod{id_order=_OrderId, id_product=ProductId, description=Description,
											   name=Name, quantity=Quantity, gross_amount=GrossAmount},
						order_prod_options=OrderProdOtions}) ->
	JsonList = [{id_product, ProductId}, {description, Description}, {name, Name}, {quantity, Quantity},
				{gross_amount, GrossAmount}, {order_prod_options, to_json(OrderProdOtions)}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#inf_order_prod_option{order_prod_option=#order_prod_option{id_order=_OrderId, id_product=_ProductId,
																	id_prod_option=ProdOptionId, type=Type,
																	name=Name},
							   order_prod_option_entries=OrderProdOptionEntries}) ->
	JsonList = [{id_prod_option, ProdOptionId}, {type, Type}, {name, Name},
				{order_prod_option_entries, to_json(OrderProdOptionEntries)}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#order_prod_option_entry{id_order=_OrderId, id_product=_ProductId, id_prod_option=_ProdOptionId,
								 id_prod_option_entry=ProdOptionEntryId, quantity=Quantity, name=Name,
								 selected=Selected, gross_amount=GrossAmount}) ->
	JsonList = [{id_prod_option_entry, ProdOptionEntryId}, {quantity, Quantity}, {name, Name},
				{selected, Selected}, {gross_amount, GrossAmount}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#dispatcher_info{order_id=_OrderId, start_delivery_time_est=StartDeliveryTimeEst, travel_distance=TravelDistance}) ->
	JsonList = [{start_delivery_time_est, to_json_timestamp(StartDeliveryTimeEst)},
				{travel_distance, TravelDistance}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#u_order_delivery_courier{id_delivery=DeliveryId, id_order=_OrderId, id_courier=CourierId}) ->
	JsonList = [{id_delivery, DeliveryId}, {id_courier, CourierId}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#file{name=Name, mimetype=Mimetype, base64_data=Base64Data}) -> [{name, Name}, {mimetype, Mimetype}, {base64_data, Base64Data}];

to_json(#document{id=DocumentId, id_user=_UserId, id_type=DocumentTypeId, name=Name, mimetype=Mimetype,
				  base64_data=Base64Data, id_status=DocumentStatusId, creation_date=CreationDate, status_date=StatusDate,
				  version=Version}) ->
	[{document_id, DocumentId}, {document_type_id, DocumentTypeId}, {document_status_id, DocumentStatusId}, {name, Name},
	 {mimetype, Mimetype}, {base64_data, Base64Data}, {creation_date, to_json_timestamp(CreationDate)},
	 {status_date, to_json_timestamp(StatusDate)}, {version, Version}];

to_json(#document_summary{id=DocumentId, id_user=_UserId, id_account=_AccountId, id_type=DocumentTypeId,
						  name=Name, id_status=DocumentStatusId, creation_date=CreationDate,
						  status_date=StatusDate, version=Version}) ->
	[{document_id, DocumentId}, {document_type_id, DocumentTypeId}, {document_status_id, DocumentStatusId},
	 {name, Name}, {creation_date, to_json_timestamp(CreationDate)},
	 {status_date, to_json_timestamp(StatusDate)}, {version, Version}];

to_json(#u_courier_info{id=UserId, creation_date=CreationDate, email=Email, first_name=FirstName, last_name=LastName,
						telephone_nr=TelephoneNr}) ->
	[{user_id, UserId}, {creation_date, to_json_timestamp(CreationDate)}, {email, Email}, {first_name, FirstName},
	 {last_name, LastName}, {telephone_nr, TelephoneNr}];

to_json(#user_location_component{id_location=_LocationId, component=Component, value=Value}) ->
	 [{component, Component}, {value, Value}];

to_json(#user_location_contact{id_location=_LocationId, id_contact=_ContactId, name=Name, phone_nr=PhoneNr, email=Email}) ->
	% Everything is optional
	case Name of
		undefined -> JSON1 = [];
		_ -> JSON1 = [{name, Name}]
	end,
	case PhoneNr of
		undefined -> JSON2 = JSON1;
		_ -> JSON2 = [{phone_nr, PhoneNr}|JSON1]
	end,
	case Email of
		undefined -> FinalJSON = JSON2;
		_ -> FinalJSON = [{email, Email}|JSON2]
	end,
	FinalJSON;

to_json(#component_info{component=Component, value=Value}) ->
	case Component of
		undefined -> JSON1 = [];
		_ -> JSON1 = [{component, Component}]
	end,
	case Value of
		undefined -> FinalJSON = JSON1;
		_ -> FinalJSON = [{value, Value}|JSON1]
	end,
	FinalJSON;

to_json(#object_action{object_id=IdObject, object_reference=ObjectReference, object_action_id=ObjectActionId,
					   object_action=_ObjectAction, id_status=_StatusId}) ->
	JsonList = [{object_id, IdObject}, {object_reference, ObjectReference}, {object_action_id, ObjectActionId}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#addinfo_delivery{inf_delivery=InfDelivery,
						  addinfo=#info_additional{num_waypoints=NumWaypoints, estimated_time=EstimatedTime,
												   objects_by_delivery=ObjectsByDelivery, 
												   streets_by_delivery=StreetsByDelivery}}) ->
	JsonList = to_json(InfDelivery) ++ [{waypoints_count, NumWaypoints}, {estimated_duration_mins, EstimatedTime},
				{object_types, to_json(ObjectsByDelivery)}, {streets_start_end, to_json(StreetsByDelivery)}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#push_delivery{push_notification_type=PushNotificationType, id_delivery=DeliveryId, distance=Distance, 
					   latitude=Latitude, longitude=Longitude, 
					   addinfo=#info_additional{num_waypoints=NumWaypoints, estimated_time=EstimatedTime,
												objects_by_delivery=ObjectsByDelivery, 
												streets_by_delivery=StreetsByDelivery}}) ->
	ObjectsByDeliveryJSON = to_json(ObjectsByDelivery),
	StreetsByDeliveryJSON = to_json(StreetsByDelivery),
	
	FinalJSON = [{push_notification_type, PushNotificationType},
				 {id_delivery, DeliveryId},
				 {distance, Distance},
				 {latitude, Latitude},
				 {longitude, Longitude},
				 {waypoints_count, NumWaypoints},
				 {estimated_duration_mins, EstimatedTime},
				 {object_types, ObjectsByDeliveryJSON}, 
				 {streets_start_end, StreetsByDeliveryJSON}],
	FinalJSON;

to_json(#addinfo_dispatcher{inf_delivery=InfDelivery,
							addinfo=#info_dispatcher{num_waypoints=NumWaypoints, estimated_time=EstimatedTime,
													 objects_by_delivery=ObjectsByDelivery, number_of_objects=NumberOfObjects,
													 weight_of_objects=WeightOfObjects, volume_of_objects=VolumeOfObjects}}) ->
	JsonList = to_json(InfDelivery) ++ [{waypoints_count, NumWaypoints}, {estimated_duration_mins, EstimatedTime},
				{object_types, to_json(ObjectsByDelivery)}, {number_of_objects, NumberOfObjects},
				{weight_of_objects, WeightOfObjects}, {volume_of_objects, VolumeOfObjects}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#dispatcher_courier_info{id_delivery=DeliveryId, id_courier=CourierId, first_name=FirstName, last_name=LastName,
								 telephone_nr=TelephoneNr, position=Position, %type=Type, orders=Orders,
								 transport_type=TransportType, courier_photo=CourierPhoto, eta=Eta}) ->
	JsonPosition =
		case Position of
			undefined -> {position, null};
			_ -> {position, to_json(Position)}
		end,
	JsonCourierPhoto =
		case CourierPhoto of
			undefined -> {courier_photo, null};
			_ -> {courier_photo, to_json(CourierPhoto)}
		end,
	JsonList = [{id_delivery, DeliveryId}, {id_courier, CourierId}, {first_name, FirstName},
				{last_name, LastName}, {telephone_nr, TelephoneNr}, JsonPosition,
				%{type, Type}, {orders, Orders},
				{transport_type_id, TransportType}, JsonCourierPhoto, {eta, Eta}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#objects_by_delivery{id_type=IdType, quantity=Quantity}) ->
	JsonList = [{id_type, IdType}, {quantity, Quantity}],
	lists:reverse(set_json_parameters(JsonList, []));

to_json(#streets_by_delivery{street1=Street1, street2=Street2}) ->
	case Street1 of
		undefined -> JSON1 = [];
		_ -> JSON1 = [{street_start, Street1}]
	end,
	case Street2 of
		undefined -> FinalJSON = JSON1;
		_ -> FinalJSON = [{street_end, Street2}|JSON1]
	end,
	FinalJSON;

to_json(#inf_location{location=#user_location{id=LocationId, id_user=_UserId, description=Description, latitude=Latitude,
															 longitude=Longitude},
							 components=Components, contacts=Contacts}) ->
	[{location_id, LocationId}, {description, Description}, {components, to_json(Components)},
	 {position, position_to_json(Latitude, Longitude)}, {contacts, to_json(Contacts)}];

to_json(#position_update{available=Available, working=Working}) -> [{available, Available}, {working, Working}];

to_json(#voucher{voucher_id=VoucherId, voucher_code=VoucherCode, value=Value, voucher_type_id=VoucherTypeId,
					  voucher_status_id=VoucherStatusId, value_used=ValueUsed, max_times=MaxTimes, times_used=TimesUsed,
					  expiration_date=ExpirationDate, restrict_user_id=RestrictUserId, creation_date=CreationDate,
					  status_date=StatusDate, version=Version}) ->
	[{voucher_id, VoucherId}, {voucher_code, VoucherCode}, {value, Value}, {voucher_type_id, VoucherTypeId},
	 {voucher_status_id, VoucherStatusId}, {value_used, ValueUsed}, {max_times, MaxTimes}, {times_used, TimesUsed},
	 {expiration_date, to_json_timestamp(ExpirationDate)}, {restrict_user_id, RestrictUserId},
	 {creation_date, to_json_timestamp(CreationDate)}, {status_date, to_json_timestamp(StatusDate)}, {version, Version}];

to_json(#order_voucher_info{order_price_discount=OrderPriceDiscount, version=Version}) ->
	[{order_price_discount, OrderPriceDiscount}, {version, Version}];

to_json(#inf_user_notifications{enabled=Enabled, notification_type_ids=NotificationTypeIds}) ->
	[{enabled, Enabled}, {notification_type_ids, NotificationTypeIds}];

to_json(#courier_app_config{position_update=PositionUpdate}) -> [{position_update, to_json(PositionUpdate)}];

to_json(#info_user_external{token=Token, email=Email, first_name=FirstName, last_name=LastName, telephone_nr=TelephoneNr,
									 fiscal_id=FiscalId, address=Address}) ->
	case Address of
		undefined -> JSON1 = [];
		_ -> JSON1 = [{address, to_json(Address)}]
	end,
	case FiscalId of
		undefined -> JSON2 = JSON1;
		_ -> JSON2 = [{fiscal_id, FiscalId}|JSON1]
	end,
	case TelephoneNr of
		undefined -> JSON3 = JSON2;
		_ -> JSON3 = [{telephone_nr, TelephoneNr}|JSON2]
	end,
	case LastName of
		undefined -> JSON4 = JSON3;
		_ -> JSON4 = [{last_name, LastName}|JSON3]
	end,
	case FirstName of
		undefined -> JSON5 = JSON4;
		_ -> JSON5 = [{first_name, FirstName}|JSON4]
	end,
	case Email of
		undefined -> JSON6 = JSON5;
		_ -> JSON6 = [{email, Email}|JSON5]
	end,
	FinalJSON = [{token, Token}|JSON6],
	FinalJSON;

to_json(#commercial_info{about=About}) ->
	[{about, About}];

to_json(#inf_waypoint_error{waypoint_reference=WaypointReference, formatted_address=FormattedAddress, address=Address}) ->
	[{waypoint_reference, WaypointReference}, {formatted_address, FormattedAddress}, {address, to_json(Address)}];

to_json(#inf_zone{zone=#zone{id=ZoneId, description=Description, center_latitude=Latitude, center_longitude=Longitude}, postal_codes=ZonePostalCodes}) ->
	PostalCodes = [PostalCode || #zone_postal_code{postal_code=PostalCode} <- ZonePostalCodes],
	JSONCenter = 
		if is_float(Latitude) andalso is_float(Longitude) -> [{center, [{latitude, Latitude}, {longitude, Longitude}]}]
		 ; true -> []
		end,
	[{zone_id, ZoneId}, {description, Description}, {postal_codes, PostalCodes}|JSONCenter].

%% ====================================================================
%% Internal functions
%% ====================================================================
get_json_headers() ->
	[{<<"content-type">>, <<"application/json">>}, {<<"Cache-Control">>, <<"no-store">>}, {<<"Pragma">>, <<"no-cache">>}].

get_image_headers() ->
	[{<<"content-type">>, <<"image/jpeg">>}, {<<"Cache-Control">>, <<"no-store">>}, {<<"Pragma">>, <<"no-cache">>}].

from_timestamp(MillisecondsFromEpochUTC) when is_integer(MillisecondsFromEpochUTC) ->
	{ok, eb_util:timestamp_from_epoch(MillisecondsFromEpochUTC)};
from_timestamp(_Other) -> error.

to_json_timestamp(null) -> null;
to_json_timestamp(undefined) -> null;
to_json_timestamp(Timestamp = {_Date, {_Hours, _Minutes, _Seconds}}) ->
	eb_util:get_epoch_timestamp(Timestamp).

to_json_datetime(null) -> null;
to_json_datetime(undefined) -> null;
to_json_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
	eb_util:datetime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}).

to_json(Accumulator, []) -> lists:reverse(Accumulator);
to_json(Accumulator, [Object|Rest]) ->
	NewJSON = to_json(Object),
	to_json([NewJSON|Accumulator], Rest).

% Common object "to_json's"
position_to_json(Latitude, Longitude) ->
	JsonList = [{latitude, Latitude}, {longitude, Longitude}],
	lists:reverse(set_json_parameters(JsonList, [])).

position_chk_to_json(CheckinLat, CheckinLon, CheckoutLat, CheckoutLon) -> 
	JsonList = [{checkin_lat, CheckinLat}, {checkin_lon, CheckinLon}, {checkout_lat, CheckoutLat},
				{checkout_lon, CheckoutLon}],
	lists:reverse(set_json_parameters(JsonList, [])).

contact_to_json(Name, PhoneNumber, Email) ->
	JsonList = [{name, Name}, {phone_nr, PhoneNumber}, {email, Email}],
	lists:reverse(set_json_parameters(JsonList, [])).

client_contact_to_json(Name, PhoneNumber, Email, FiscalId) ->
	JsonList = [{name, Name}, {phone_nr, PhoneNumber}, {email, Email}, {fiscal_id, FiscalId}],
	lists:reverse(set_json_parameters(JsonList, [])).

time_window_to_json(null, null) -> [];
time_window_to_json(From, To) -> [{from, to_json_timestamp(From)}, {to, to_json_timestamp(To)}].

%
% Request parameters functions
%
get_record(Request, BuildFunction) ->
	try
		{JSON, Request1} = kb_action_helper:get_json(Request),
		case build_request_record(JSON, BuildFunction) of
			{ok, Record} -> {ok, Record, Request1};
			{nok, Error} -> {nok, Error, Request1}
		end
	catch
		_:_ -> {nok, error_getting_json, Request}
	end.

build_request_record({Elements}, BuildFunction) ->
	BuildFunction(Elements);
build_request_record(_Other, _BuildFunction) ->
	{nok, invalid_parameters}.

build_record([], NewRecord) -> {ok, NewRecord};
build_record([Parameter|Rest], NewRecord) ->
	ChangedRecord = set_record_parameter(Parameter, NewRecord),
	build_record(Rest, ChangedRecord).

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_user) ->
	case Label of
		<<"username">> -> NewRecord#new_user{username=Value};
		<<"password">> -> NewRecord#new_user{password=Value};
		<<"user_type_id">> -> NewRecord#new_user{user_type_id=Value};
		<<"email">> -> NewRecord#new_user{email=Value};
		<<"first_name">> -> NewRecord#new_user{first_name=Value};
		<<"last_name">> -> NewRecord#new_user{last_name=Value};
		<<"telephone_nr">> -> NewRecord#new_user{telephone_nr=Value};
		<<"fiscal_id">> -> NewRecord#new_user{fiscal_id=Value};
		<<"reference">> -> NewRecord#new_user{reference=Value};
		<<"mobileos_id">> -> NewRecord#new_user{mobileos_id=Value};
		<<"birth_day">> -> NewRecord#new_user{birth_day=Value};
		<<"birth_month">> -> NewRecord#new_user{birth_month=Value};
		<<"birth_year">> -> NewRecord#new_user{birth_year=Value};
		<<"national_id">> -> NewRecord#new_user{national_id=Value};
		<<"country">> -> NewRecord#new_user{country=Value};
		<<"account_user_type_id">> -> NewRecord#new_user{account_user_type_id=Value};
		<<"account_version">> -> NewRecord#new_user{account_version=Value};
		<<"new_account">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #new_account{}) end) of
				{ok, NewAccount} -> NewRecord#new_user{new_account=NewAccount};
				_ -> NewRecord
			end;
		<<"department_id">> -> NewRecord#new_user{department_id=Value};
		<<"cost_center_id">> -> NewRecord#new_user{cost_center_id=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_user) ->
	case Label of
		<<"username">> -> NewRecord#change_user{username=Value};
		<<"user_status_id">> -> NewRecord#change_user{user_status_id=Value};
		<<"email">> -> NewRecord#change_user{email=Value};
		<<"first_name">> -> NewRecord#change_user{first_name=Value};
		<<"last_name">> -> NewRecord#change_user{last_name=Value};
		<<"telephone_nr">> -> NewRecord#change_user{telephone_nr=Value};
		<<"fiscal_id">> -> NewRecord#change_user{fiscal_id=Value};
		<<"mobileos_id">> -> NewRecord#change_user{mobileos_id=Value};
		<<"birth_day">> -> NewRecord#change_user{birth_day=Value};
		<<"birth_month">> -> NewRecord#change_user{birth_month=Value};
		<<"birth_year">> -> NewRecord#change_user{birth_year=Value};
		<<"national_id">> -> NewRecord#change_user{national_id=Value};
		<<"zone_ids">> -> NewRecord#change_user{zone_ids=Value};
		<<"address">> ->
			case build_request_list(Value, #component_info{}) of
				{ok, AddressComponents} -> NewRecord#change_user{address=AddressComponents};
				_ -> NewRecord
			end;
		<<"version">> -> NewRecord#change_user{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_password) ->
	case Label of
		<<"old_password">> -> NewRecord#change_password{old_password=Value};
		<<"new_password">> -> NewRecord#change_password{new_password=Value};
		<<"version">> -> NewRecord#change_password{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, additional_info) ->
	case Label of
		<<"property">> -> NewRecord#additional_info{property=Value};
		<<"value">> -> NewRecord#additional_info{value=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_account) ->
	case Label of
		<<"unique_key">> -> NewRecord#new_account{unique_key=Value};
		<<"name">> -> NewRecord#new_account{name=Value};
		<<"fiscal_id">> -> NewRecord#new_account{fiscal_id=Value};
		<<"address">> ->
			case build_request_list(Value, #component_info{}) of
				{ok, AddressComponents} -> NewRecord#new_account{address=AddressComponents};
				_ -> NewRecord
			end;
		<<"email">> -> NewRecord#new_account{email=Value};
		<<"telephone_nr">> -> NewRecord#new_account{telephone_nr=Value};
		<<"contract_nr">> -> NewRecord#new_account{contract_nr=Value};
		<<"additional_infos">> ->
			case build_request_list(Value, #additional_info{}) of
				{ok, AdditionalInfos} -> NewRecord#new_account{additional_infos=AdditionalInfos};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_account) ->
	case Label of
		<<"account_name">> -> NewRecord#change_account{account_name=Value};
		<<"fiscal_id">> -> NewRecord#change_account{fiscal_id=Value};
		<<"telephone_nr">> -> NewRecord#change_account{telephone_nr=Value};
		<<"email">> -> NewRecord#change_account{email=Value};
		<<"address">> ->
			case build_request_list(Value, #component_info{}) of
				{ok, AddressComponents} -> NewRecord#change_account{address=AddressComponents};
				_ -> NewRecord
			end;
		<<"additional_infos">> ->
			case build_request_list(Value, #additional_info{}) of
				{ok, AdditionalInfos} -> NewRecord#change_account{additional_infos=AdditionalInfos};
				_ -> NewRecord
			end;
		<<"version">> -> NewRecord#change_account{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_occupancy) ->
	case Label of
		<<"occupancy">> -> NewRecord#change_occupancy{occupancy=Value};
		<<"version">> -> NewRecord#change_occupancy{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_contact_request) ->
	case Label of
		<<"email">> -> NewRecord#new_contact_request{email=Value};
		<<"name">> -> NewRecord#new_contact_request{name=Value};
		<<"subject">> -> NewRecord#new_contact_request{subject=Value};
		<<"content">> -> NewRecord#new_contact_request{content=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, update_token_type) ->
	case Label of
		<<"multiple_uses">> -> NewRecord#update_token_type{multiple_uses=Value};
		<<"uses">> -> NewRecord#update_token_type{uses=Value};
		<<"expires_in_minutes">> -> NewRecord#update_token_type{expires_in_minutes=Value};
		<<"version">> -> NewRecord#update_token_type{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, reset_password) ->
	case Label of
		<<"user_id">> -> NewRecord#reset_password{user_id=Value};
		<<"username">> -> NewRecord#reset_password{username=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_password) ->
	case Label of
		<<"password">> -> NewRecord#new_password{password=Value};
		<<"token">> -> NewRecord#new_password{token=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_user_external) ->
	case Label of
		<<"token">> -> NewRecord#new_user_external{token=Value};
		<<"email">> -> NewRecord#new_user_external{email=Value};
		<<"first_name">> -> NewRecord#new_user_external{first_name=Value};
		<<"last_name">> -> NewRecord#new_user_external{last_name=Value};
		<<"telephone_nr">> -> NewRecord#new_user_external{telephone_nr=Value};
		<<"fiscal_id">> -> NewRecord#new_user_external{fiscal_id=Value};
		<<"address">> ->
			case build_request_list(Value, #component_info{}) of
				{ok, AddressComponents} -> NewRecord#new_user_external{address=AddressComponents};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, push_info) ->
	case Label of
		<<"push_id">> -> NewRecord#push_info{id=Value};
		<<"push_type">> -> NewRecord#push_info{type=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, position) ->
	case Label of
		<<"latitude">> -> NewRecord#position{latitude=Value};
		<<"longitude">> -> NewRecord#position{longitude=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, update_session_info) ->
	case Label of
		<<"push_info">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #push_info{}) end) of
				{ok, PushInfo} -> NewRecord#update_session_info{push_info=PushInfo};
				_ -> NewRecord
			end;
		<<"position">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #position{}) end) of
				{ok, Position} -> NewRecord#update_session_info{position=Position};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_notification) ->
	case Label of
		<<"delivery_id">> -> NewRecord#new_notification{delivery_id=Value};
		<<"waypoint_id">> -> NewRecord#new_notification{waypoint_id=Value};
		<<"notification_type_id">> -> NewRecord#new_notification{notification_type_id=Value};
		<<"message">> -> NewRecord#new_notification{message=Value};
		<<"position">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #position{}) end) of
				{ok, Position} -> NewRecord#new_notification{position=Position};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_parameterization) ->
	case Label of
		<<"value">> -> NewRecord#change_parameterization{value=Value};
		<<"version">> -> NewRecord#change_parameterization{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_account_user) ->
	case Label of
		<<"account_user_type_id">> -> NewRecord#new_account_user{account_user_type_id=Value};
		<<"version">> -> NewRecord#new_account_user{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_account_user) ->
	case Label of
		<<"username">> -> NewRecord#change_account_user{username=Value};
		<<"email">> -> NewRecord#change_account_user{email=Value};
		<<"first_name">> -> NewRecord#change_account_user{first_name=Value};
		<<"last_name">> -> NewRecord#change_account_user{last_name=Value};
		<<"telephone_nr">> -> NewRecord#change_account_user{telephone_nr=Value};
		<<"account_user_type_id">> -> NewRecord#change_account_user{account_user_type_id=Value};
		<<"department_id">> -> NewRecord#change_account_user{department_id=Value};
		<<"cost_center_id">> -> NewRecord#change_account_user{cost_center_id=Value};
		<<"account_version">> -> NewRecord#change_account_user{account_version=Value};
		<<"user_version">> -> NewRecord#change_account_user{user_version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_order) ->
	case Label of
		<<"reference">> -> NewRecord#new_order{reference=Value};
		<<"distribution_center">> -> NewRecord#new_order{distribution_center=Value};
		<<"order_type_id">> -> NewRecord#new_order{order_type_id=Value};
		<<"transport_type_id">> -> NewRecord#new_order{transport_type_id=Value};
		<<"origin_id">> -> NewRecord#new_order{origin_id=Value};
		<<"client_info">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #client_contact_info{}) end) of
				{ok, ClientInfo} -> NewRecord#new_order{client_info=ClientInfo};
				_ -> NewRecord
			end;
		<<"cut_time">> -> NewRecord#new_order{cut_time=Value};
		<<"payment_provider_id">> -> NewRecord#new_order{payment_provider_id=Value};
		<<"payment_id">> -> NewRecord#new_order{payment_id=Value};
		<<"payment_method_id">> -> NewRecord#new_order{payment_method_id=Value};
		<<"diner_qty">> -> NewRecord#new_order{diner_qty=Value};
		<<"gross_total">> -> NewRecord#new_order{gross_total=Value};
		<<"tip">> -> NewRecord#new_order{tip=Value};
		<<"fee">> -> NewRecord#new_order{fee=Value};
		<<"order_prods">> ->
			case build_request_list(Value, #new_order_prod{}) of
				{ok, OrderProds} -> NewRecord#new_order{order_prods=OrderProds};
				_ -> NewRecord
			end;
		<<"origin">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #new_order_waypoint{}) end) of
				{ok, Origin} -> NewRecord#new_order{origin=Origin};
				_ -> NewRecord
			end;
		<<"destination">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #new_order_waypoint{}) end) of
				{ok, Destination} -> NewRecord#new_order{destination=Destination};
				_ -> NewRecord
			end;
		<<"waypoints">> ->
			case build_request_list(Value, #new_order_waypoint{}) of
				{ok, Waypoints} -> NewRecord#new_order{waypoints=Waypoints};
				_ -> NewRecord
			end;
		<<"objects">> ->
			case build_request_list(Value, #new_order_object{}) of
				{ok, Objects} -> NewRecord#new_order{objects=Objects};
				_ -> NewRecord
			end;
		<<"account">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #new_order_account{}) end) of
				{ok, Account} -> NewRecord#new_order{account=Account};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_order_prod) ->
	case Label of
		<<"id_product">> -> NewRecord#new_order_prod{id_product=Value};
		<<"description">> -> NewRecord#new_order_prod{description=Value};
		<<"name">> -> NewRecord#new_order_prod{name=Value};
		<<"quantity">> -> NewRecord#new_order_prod{quantity=Value};
		<<"gross_amount">> -> NewRecord#new_order_prod{gross_amount=Value};
		<<"prod_options">> ->
			case build_request_list(Value, #new_order_prod_option{}) of
				{ok, ProdOptions} -> NewRecord#new_order_prod{prod_options=ProdOptions};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_order_prod_option) ->
	case Label of
		<<"id_prod_option">> -> NewRecord#new_order_prod_option{id_prod_option=Value};
		<<"type">> -> NewRecord#new_order_prod_option{type=Value};
		<<"name">> -> NewRecord#new_order_prod_option{name=Value};
		<<"prod_option_entries">> ->
			case build_request_list(Value, #new_order_prod_option_entry{}) of
				{ok, ProdOptionEntries} -> NewRecord#new_order_prod_option{prod_option_entries=ProdOptionEntries};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_order_prod_option_entry) ->
	case Label of
		<<"id_prod_option_entry">> -> NewRecord#new_order_prod_option_entry{id_prod_option_entry=Value};
		<<"quantity">> -> NewRecord#new_order_prod_option_entry{quantity=Value};
		<<"name">> -> NewRecord#new_order_prod_option_entry{name=Value};
		<<"selected">> -> NewRecord#new_order_prod_option_entry{selected=Value};
		<<"gross_amount">> -> NewRecord#new_order_prod_option_entry{gross_amount=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_order_waypoint) ->
	case Label of
		<<"reference">> -> NewRecord#new_order_waypoint{reference=Value};
		<<"formatted_address">> -> NewRecord#new_order_waypoint{formatted_address=Value};
		<<"address">> ->
			case build_request_list(Value, #component_info{}) of
				{ok, AddressComponents} -> NewRecord#new_order_waypoint{address=AddressComponents};
				_ -> NewRecord
			end;
		<<"position">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #position{}) end) of
				{ok, Position} -> NewRecord#new_order_waypoint{position=Position};
				_ -> NewRecord
			end;
		<<"contact_info">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #contact_info{}) end) of
				{ok, ContactInfo} -> NewRecord#new_order_waypoint{contact_info=ContactInfo};
				_ -> NewRecord
			end;
		<<"notes">> -> NewRecord#new_order_waypoint{notes=Value};
		<<"signature">> -> NewRecord#new_order_waypoint{signature=Value};
		<<"stop_window">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #time_window{}) end) of
				{ok, StopWindow} -> NewRecord#new_order_waypoint{stop_window=StopWindow};
				_ -> NewRecord
			end;
		<<"object_actions">> ->
			case build_request_list(Value, #new_object_action{}) of
				{ok, ObjectActions} -> NewRecord#new_order_waypoint{object_actions=ObjectActions};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_order_object) ->
	case Label of
		<<"reference">> -> NewRecord#new_order_object{reference=Value};
		<<"type_id">> -> NewRecord#new_order_object{type_id=Value};
		<<"transport_auth">> -> NewRecord#new_order_object{transport_auth=Value};
		<<"quantity">> -> NewRecord#new_order_object{quantity=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_order_account) ->
	case Label of
		<<"id_account">> -> NewRecord#new_order_account{id_account=Value};
		<<"id_slot">> -> NewRecord#new_order_account{id_slot=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_object_action) ->
	case Label of
		<<"object_reference">> -> NewRecord#new_object_action{object_reference=Value};
		<<"object_action_id">> -> NewRecord#new_object_action{object_action_id=Value};
		<<"object_action">> -> NewRecord#new_object_action{object_action=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_order_status) ->
	case Label of
		<<"status_id">> -> NewRecord#change_order_status{status_id=Value};
		<<"version">> -> NewRecord#change_order_status{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, reallocate_order) ->
	case Label of
		<<"minutes">> -> NewRecord#reallocate_order{minutes=Value};
		<<"version">> -> NewRecord#reallocate_order{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_order_delay) ->
	case Label of
		<<"cut_time">> -> NewRecord#change_order_delay{cut_time=Value};
		<<"version">> -> NewRecord#change_order_delay{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_delivery) ->
	case Label of
		<<"delivery_status_id">> -> NewRecord#change_delivery{delivery_status_id=Value};
		<<"version">> -> NewRecord#change_delivery{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_delivery_waypoint) ->
	case Label of
		<<"status_id">> -> NewRecord#change_delivery_waypoint{status_id=Value};
		<<"position">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #position{}) end) of
				{ok, Position} -> NewRecord#change_delivery_waypoint{position=Position};
				_ -> NewRecord
			end;
		<<"rating">> -> NewRecord#change_delivery_waypoint{rating=Value};
		<<"rating_notes">> -> NewRecord#change_delivery_waypoint{rating_notes=Value};
		<<"version">> -> NewRecord#change_delivery_waypoint{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_delivery_courier) ->
	case Label of
		<<"courier_id">>        -> NewRecord#change_delivery_courier{id_courier=Value};
		<<"version">>           -> NewRecord#change_delivery_courier{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_delivery_action) ->
	case Label of
		<<"skip_action">> -> NewRecord#change_delivery_action{skip_action=Value};
		<<"version">>     -> NewRecord#change_delivery_action{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, create_object) ->
	case Label of
		<<"object">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #new_order_object{}) end) of
				{ok, NewObject} -> NewRecord#create_object{object=NewObject};
				_ -> NewRecord
			end;
		<<"version">> -> NewRecord#create_object{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, file) ->
	case Label of
		<<"name">> -> NewRecord#file{name=Value};
		<<"mimetype">> -> NewRecord#file{mimetype=Value};
		<<"base64_data">> -> NewRecord#file{base64_data=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_document) ->
	case Label of
		<<"document_type_id">> -> NewRecord#new_document{document_type_id=Value};
		<<"document">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #file{}) end) of
				{ok, Position} -> NewRecord#new_document{document=Position};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_document) ->
	case Label of
		<<"document_status_id">> -> NewRecord#change_document{document_status_id=Value};
		<<"version">> -> NewRecord#change_document{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, component_info) ->
	case Label of
		<<"component">> -> NewRecord#component_info{component=Value};
		<<"value">> -> NewRecord#component_info{value=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, contact_info) ->
	case Label of
		<<"name">> -> NewRecord#contact_info{name=Value};
		<<"phone_nr">> -> NewRecord#contact_info{phone_nr=Value};
		<<"email">> -> NewRecord#contact_info{email=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, client_contact_info) ->
	case Label of
		<<"name">> -> NewRecord#client_contact_info{name=Value};
		<<"phone_nr">> -> NewRecord#client_contact_info{phone_nr=Value};
		<<"email">> -> NewRecord#client_contact_info{email=Value};
		<<"fiscal_id">> -> NewRecord#client_contact_info{fiscal_id=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_location) ->
	case Label of
		<<"description">> -> NewRecord#new_location{description=Value};
		<<"components">> ->
			case build_request_list(Value, #component_info{}) of
				{ok, Components} -> NewRecord#new_location{components=Components};
				_ -> NewRecord
			end;
		<<"position">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #position{}) end) of
				{ok, Position} -> NewRecord#new_location{position=Position};
				_ -> NewRecord
			end;
		<<"contacts">> ->
			case build_request_list(Value, #contact_info{}) of
				{ok, Contacts} -> NewRecord#new_location{contacts=Contacts};
				_ -> NewRecord
			end;
		<<"version">> -> NewRecord#new_location{version=Value};
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_location) ->
	case Label of
		<<"description">> -> NewRecord#change_location{description=Value};
		<<"components">> ->
			case build_request_list(Value, #component_info{}) of
				{ok, Components} -> NewRecord#change_location{components=Components};
				_ -> NewRecord
			end;
		<<"position">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #position{}) end) of
				{ok, Position} -> NewRecord#change_location{position=Position};
				_ -> NewRecord
			end;
		<<"contacts">> ->
			case build_request_list(Value, #contact_info{}) of
				{ok, Contacts} -> NewRecord#change_location{contacts=Contacts};
				_ -> NewRecord
			end;
		<<"version">> -> NewRecord#change_location{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_contact_request) ->
	case Label of
		<<"contact_request_status_id">> -> NewRecord#change_contact_request{contact_request_status_id=Value};
		<<"user_id">> -> NewRecord#change_contact_request{user_id=Value};
		<<"operator_notes">> -> NewRecord#change_contact_request{operator_notes=Value};
		<<"operator_id">> -> NewRecord#change_contact_request{operator_id=Value};
		<<"version">> -> NewRecord#change_contact_request{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_courier_transport) ->
	case Label of
		<<"transport_type_id">> -> NewRecord#new_courier_transport{transport_type_id=Value};
		<<"description">> -> NewRecord#new_courier_transport{description=Value};
		<<"registration_id">> -> NewRecord#new_courier_transport{registration_id=Value};
		<<"color">> -> NewRecord#new_courier_transport{color=Value};
		<<"version">> -> NewRecord#new_courier_transport{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_courier_transport) ->
	case Label of
		<<"transport_status_id">> -> NewRecord#change_courier_transport{transport_status_id=Value};
		<<"version">> -> NewRecord#change_courier_transport{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, new_voucher) ->
	case Label of
		<<"voucher_code">> -> NewRecord#new_voucher{voucher_code=Value};
		<<"value">> -> NewRecord#new_voucher{value=Value};
		<<"voucher_type_id">> -> NewRecord#new_voucher{voucher_type_id=Value};
		<<"max_times">> -> NewRecord#new_voucher{max_times=Value};
		<<"expiration_date">> -> NewRecord#new_voucher{expiration_date=Value};
		<<"restrict_user_id">> -> NewRecord#new_voucher{restrict_user_id=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_voucher) ->
	case Label of
		<<"value">> -> NewRecord#change_voucher{value=Value};
		<<"voucher_status_id">> -> NewRecord#change_voucher{voucher_status_id=Value};
		<<"max_times">> -> NewRecord#change_voucher{max_times=Value};
		<<"expiration_date">> -> NewRecord#change_voucher{expiration_date=Value};
		<<"restrict_user_id">> -> NewRecord#change_voucher{restrict_user_id=Value};
		<<"version">> -> NewRecord#change_voucher{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, change_commercial_info) ->
	case Label of
		<<"about">> -> NewRecord#change_commercial_info{about=Value};
		<<"version">> -> NewRecord#change_commercial_info{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, update_order_voucher) ->
	case Label of
		<<"voucher_code">> -> NewRecord#update_order_voucher{voucher_code=Value};
		<<"version">> -> NewRecord#update_order_voucher{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, authorization_info) ->
	case Label of
		<<"code">> -> NewRecord#authorization_info{code=Value};
		<<"redirect_uri">> -> NewRecord#authorization_info{redirect_uri=Value};
		<<"token">> -> NewRecord#authorization_info{token=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, update_file) ->
	case Label of
		<<"file">> ->
			case build_request_record(Value, fun(Elements) -> build_record(Elements, #file{}) end) of
				{ok, File} -> NewRecord#update_file{file=File};
				_ -> NewRecord
			end;
		<<"version">> -> NewRecord#update_file{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, update_user_notifications) ->
	case Label of
		<<"enabled">> -> NewRecord#update_user_notifications{enabled=Value};
		<<"notification_type_ids">> -> NewRecord#update_user_notifications{notification_type_ids=Value};
		<<"version">> -> NewRecord#update_user_notifications{version=Value};
		_ -> NewRecord
	end;

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, time_window) ->
	case Label of
		<<"from">> -> NewRecord#time_window{from=Value};
		<<"to">> -> NewRecord#time_window{to=Value};
		_ -> NewRecord
	end.

build_request_list(null, _Record) -> {ok, []};
build_request_list([], _Record) -> {ok, []};
build_request_list(Elements, Record) when is_list(Elements) -> build_request_list(Elements, [], Record);
build_request_list(_, _) -> nok.

build_request_list([], ProcessedElements, _Record) -> {ok, lists:reverse(ProcessedElements)};
build_request_list([{Element}|Rest], ProcessedElements, Record) ->
	case build_record(Element, Record) of
		{ok, ProcessedElement} -> build_request_list(Rest, [ProcessedElement|ProcessedElements], Record);
		_ -> nok
	end.

build_single_parameter([], _ParameterName, DefaultValue) ->
	{ok, DefaultValue};
build_single_parameter([{ParameterName, Value}|_Rest], ParameterName, _DefaultValue) ->
	{ok, Value};
build_single_parameter([{_Label, _Value}|Rest], ParameterName, DefaultValue) ->
	build_single_parameter(Rest, ParameterName, DefaultValue).

get_rs_navigation([], RSNavigation) -> {ok, RSNavigation};
get_rs_navigation([{<<"order">>, Value}|Rest], NewRecord) ->
	ChangedRecord = NewRecord#rs_navigation{order=Value},
	get_rs_navigation(Rest, ChangedRecord);
get_rs_navigation([{<<"sort">>, Value}|Rest], NewRecord) ->
	ChangedRecord = NewRecord#rs_navigation{sort=Value},
	get_rs_navigation(Rest, ChangedRecord);
get_rs_navigation([{<<"max">>, BinValue}|Rest], NewRecord) ->
	case eb_util:binary_to_int(BinValue) of
		{ok, Value} when value >= 0 ->
			ChangedRecord = NewRecord#rs_navigation{max=Value},
			get_rs_navigation(Rest, ChangedRecord);
		_ -> nok
	end;
get_rs_navigation([{<<"skip">>, BinValue}|Rest], NewRecord) ->
	case eb_util:binary_to_int(BinValue) of
		{ok, Value} when value >= 0 ->
			ChangedRecord = NewRecord#rs_navigation{skip=Value},
			get_rs_navigation(Rest, ChangedRecord);
		_ -> nok
	end;
get_rs_navigation([_Other|Rest], Record) -> get_rs_navigation(Rest, Record).

set_json_parameters([], JsonList) -> JsonList;
set_json_parameters([Element|Rest], JsonList) ->
	NewJsonList = set_json_parameter(Element, JsonList),
	set_json_parameters(Rest, NewJsonList).

set_json_parameter({_Key, null}, JsonList) -> JsonList;
set_json_parameter({_Key, undefined}, JsonList) -> JsonList;
set_json_parameter({_Key, []}, JsonList) -> JsonList;
set_json_parameter({Key, Value}, JsonList) -> [{Key, Value}|JsonList];
set_json_parameter(_Data, JsonList) -> JsonList.
