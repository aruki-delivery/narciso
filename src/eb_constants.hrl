%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%

%% ====================================================================
%% Redirection constants
%% ====================================================================
-define(REDIRECT_TOKEN_PARAMETER, <<"TOKEN">>).

%% ====================================================================
%% Application constants
%% ====================================================================
-define(HEADER_SESSION_TOKEN, <<"x-session-token">>).
-define(HEADER_API_KEY,       <<"x-api-key">>).

-define(PUSH_TYPE_ANDROID_ID,          1).
-define(PUSH_TYPE_ANDROID_DESCRIPTION, <<"Android">>).

-define(COURIER_STATUS_UNAVAILABLE, <<"unavailable">>).
-define(COURIER_STATUS_AVAILABLE,   <<"available">>).
-define(COURIER_STATUS_WORKING,     <<"working">>).

-define(NAVIGATION_SORT_ASC,  <<"asc">>).
-define(NAVIGATION_SORT_DESC, <<"desc">>).

-define(ORDER_USER_ID,            <<"user_id">>).
-define(ORDER_USER_TYPE_ID,       <<"user_type_id">>).
-define(ORDER_USER_STATUS_ID,     <<"user_status_id">>).
-define(ORDER_DELIVERY_ID,        <<"delivery_id">>).
-define(ORDER_COURIER_ID,         <<"courier_id">>).
-define(ORDER_ID,                 <<"order_id">>).
-define(ORDER_DOCUMENT_TYPE_ID,   <<"document_type_id">>).
-define(ORDER_DOCUMENT_STATUS_ID, <<"document_status_id">>).
-define(ORDER_NAME,               <<"name">>).

-define(ADDRESS_COMPONENT_01,     <<"country">>).
-define(ADDRESS_COMPONENT_02,     <<"extra_info">>).
-define(ADDRESS_COMPONENT_03,     <<"locality">>).
-define(ADDRESS_COMPONENT_04,     <<"postal_code">>).
-define(ADDRESS_COMPONENT_05,     <<"route">>).
-define(ADDRESS_COMPONENT_06,     <<"street_number">>).

-define(DISPATCHER_FILTER_01,     <<"ordem">>).
-define(DISPATCHER_FILTER_02,     <<"tipo">>).
-define(DISPATCHER_FILTER_03,     <<"corte">>).
-define(DISPATCHER_FILTER_04,     <<"skip">>).

-define(PUSH_NOTIF_TYPE_ASSIGN,   1).
-define(PUSH_NOTIF_TYPE_UNASSIGN, 2).

-define(START_ARG_MAIN_NODE, main_node).

%% ====================================================================
%% REST parameters records
%% ====================================================================
-record(new_user, {username, password, email, user_type_id, first_name, last_name, telephone_nr, fiscal_id,
				   reference, mobileos_id, birth_day, birth_month, birth_year, national_id, country,
				   account_user_type_id, new_account, account_version, department_id, cost_center_id}).
-record(new_user_external, {token, id_oauth, id_oauth_provider, user_type_id, email, first_name, last_name,
							telephone_nr, fiscal_id, address}).
-record(change_user, {username, user_status_id, email, first_name, last_name, telephone_nr, fiscal_id, mobileos_id, birth_day,
					  birth_month, birth_year, national_id, zone_ids=[], address=[], version}).
-record(new_account, {unique_key, name, fiscal_id, address=[], email, telephone_nr, contract_nr, additional_infos=[]}).
-record(authorization_info, {code, redirect_uri, token}).
-record(new_contact_request, {email, name, subject, content}).
-record(additional_info, {property, value}).
-record(update_token_type, {multiple_uses, uses, expires_in_minutes, version}).
-record(reset_password, {user_id, username}).
-record(new_password, {password, token}).
-record(change_password, {old_password, new_password, version}).
-record(new_account_user, {account_user_type_id, version}).
-record(change_account_user, {username, email, first_name, last_name, telephone_nr, account_user_type_id, department_id, cost_center_id, account_version, user_version}).
-record(push_info, {id, type}).
-record(position, {latitude, longitude}).
-record(update_session_info, {push_info, position}).
-record(rs_navigation, {order, sort, skip=0, max=0}).
-record(new_notification, {delivery_id, waypoint_id, notification_type_id, message, position}).
-record(change_parameterization, {value, version}).
-record(new_order, {reference, distribution_center, order_type_id, transport_type_id, origin_id,
					client_info, cut_time, payment_provider_id, payment_id, payment_method_id, diner_qty,
					gross_total, tip, fee, order_prods=[], origin, destination,
					waypoints=[], objects=[], account}).
-record(change_order, {new_order, version}).
-record(change_order_status, {status_id, version}).
-record(reallocate_order, {minutes, version}).
-record(change_order_delay, {cut_time, version}).
-record(new_order_waypoint, {reference, address=[], formatted_address, position, contact_info, notes,
							 signature, stop_window, stop_duration, object_actions=[]}).
-record(new_order_object, {reference, type_id, transport_auth, quantity}).
-record(new_order_account, {id_account, id_slot}).
-record(new_object_action, {object_reference, object_action_id, object_action}).
-record(update_order, {status_id, version}).
-record(new_order_prod, {id_product, description, name, quantity, gross_amount, prod_options=[]}).
-record(new_order_prod_option, {id_prod_option, type, name, prod_option_entries=[]}).
-record(new_order_prod_option_entry, {id_prod_option_entry, quantity, name, selected, gross_amount}).
-record(new_object_type, {description, size, length, width, height, weight, volume}).
-record(change_object_type, {description, size, length, width, height, weight, volume, id_status, version}).
-record(new_delivery, {orders=[], reference, id_transport_type, distance, duration, route, cut_time,
                       origin, destination, waypoints=[], objects=[], generated_delivery_waypoints=[]}).
-record(new_delivery_waypoint, {order_id, order_waypoint_id, travel_distance, travel_time_est, stop_duration_est, object_actions=[]}).
-record(new_delivery_object, {order_id, order_object_id, object_id, reference, type_id, transport_auth}).
-record(new_delivery_object_action, {object_id, object_action_id}).
-record(new_generated_delivery_info, {orders=[], order_waypoints=[], order_waypoints_objects=[]}).
-record(change_estimate, {new_delivery, version}).
-record(change_delivery, {delivery_status_id, version}).
-record(change_delivery_waypoint_rating, {rating_client, rating_notes_client, rating_courier, rating_notes_courier, version}).
-record(change_delivery_waypoint, {status_id, position, rating, rating_notes, signature_file, version}).
-record(change_delivery_object, {reference, transport_auth, type_id, action_id, status_id, position, signature, non_del_reason, version}).
-record(create_object, {object, version}).
-record(object_action, {object_id, object_reference, object_action_id, object_action, id_status}).
-record(file, {name, mimetype, base64_data}).
-record(addinfo_delivery, {inf_delivery, addinfo}).
-record(addinfo_dispatcher, {inf_delivery, addinfo}).
-record(push_delivery, {push_notification_type, id_delivery, distance, latitude, longitude, addinfo}).
-record(objects_by_delivery, {id_type, quantity}).
-record(streets_by_delivery, {street1, wp1, street2, wp2}).
-record(info_additional, {num_waypoints, estimated_time, objects_by_delivery=[], streets_by_delivery}).
-record(info_dispatcher, {num_waypoints, estimated_time, objects_by_delivery=[], number_of_objects, 
                          weight_of_objects, volume_of_objects}).
-record(time_count_by_delivery, {estimated_riding_time, estimated_waiting_time, num_waypoints}).
-record(dispatcher_extra_info, {skip_action, cut_time}).
-record(dispatcher_object_quantities, {quantity, weight, volume}).
-record(time_window, {from, to}).
-record(new_document, {document_type_id, document}).
-record(change_document, {document_status_id, version}).
-record(component_info, {component, value}).
-record(contact_info, {name, phone_nr, email}).
-record(client_contact_info, {name, phone_nr, email, fiscal_id}).
-record(new_location, {description, components, position, contacts, version}).
-record(change_location, {description, components, position, contacts, version}).
-record(change_contact_request, {contact_request_status_id, user_id, operator_notes, operator_id, version}).
-record(waypoint_info, {waypoint, component}).
-record(new_courier_transport, {transport_type_id, description, registration_id, color, version}).
-record(change_courier_transport, {transport_status_id, version}).
-record(new_voucher, {voucher_code, value, voucher_type_id, max_times, expiration_date, restrict_user_id}).
-record(update_order_voucher, {voucher_code, version}).
-record(change_voucher, {value, voucher_status_id, max_times, expiration_date, restrict_user_id, version}).
-record(order_voucher_info, {order_price_discount, version}).
-record(change_account, {account_name, fiscal_id, telephone_nr, email, address=[], additional_infos=[], version}).
-record(change_occupancy, {occupancy, version}).
-record(update_file, {file, version}).
-record(update_user_notifications, {enabled, notification_type_ids, version}).
-record(change_commercial_info, {about, version}).
-record(change_delivery_courier, {id_courier, version}).
-record(change_delivery_action, {skip_action, version}).
-record(delivery_skip, {delivery_status, skip}).

%% ====================================================================
%% Records for database tables
%% ====================================================================
-record(reference_data, {id, description}).
-record(account, {id, unique_key, id_status, account_name, fiscal_id, telephone_nr, email, creation_date, status_date, occupancy, version}).
-record(account_address, {id_account, component, value}).
-record(account_department, {id_department, id_account, description}).
-record(account_info, {id_account, property, value}).
-record(account_user, {id_account, id_user, id_type, id_department, id_cost_center}).
-record(api_client, {id, name, email, id_status, creation_date, status_date}).
-record(api_key, {api_key, id_api_client, domain, version}).
-record(commercial_info, {id_user, about}).
-record(contact_request, {id, email, name, subject, content, id_status, creation_date, status_date, id_user, id_operator_user,
						  operator_notes, version}).
-record(courier_transport, {id, id_user, id_transport_type, current, id_status, description, registration_id, color,
							creation_date, status_date}).
-record(delivery, {id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip,
				   cut_time, id_status, creation_date, status_date, version}).
-record(delivery_courier_route, {id, id_delivery, id_user, latitude, longitude, status_date}).
-record(delivery_object, {id_delivery, id_object, reference, id_type, transport_auth, non_del_reason}).
-record(delivery_object_document, {id_delivery, id_object, id_type, name, mimetype, base64_data}).
-record(delivery_waypoint, {id_delivery, id_waypoint, travel_distance, travel_time_est, travel_time_real,
							stop_duration_est, stop_duration_real, checkin_date, checkout_date, checkin_lat,
							checkin_lon, checkout_lat, checkout_lon, id_status, creation_date, status_date, version}).
-record(delivery_waypoint_object_action, {id_delivery, id_waypoint, id_object, id_action, id_status, creation_date,
										  status_date}).
-record(delivery_waypoint_rating, {id_delivery, id_delivery_waypoint, id_order, id_waypoint, rating_client,
								   rating_notes_client, rating_courier, rating_notes_courier}).
-record(delivery_waypoint_signature, {id_delivery, id_delivery_waypoint, id_order, id_waypoint, name, mimetype,
									  base64_data}).
-record(document, {id, id_user, id_account, id_type, name, mimetype, base64_data, id_status, creation_date, status_date, version}).
-record(generated_delivery, {id_delivery, id_order}).
-record(generated_delivery_waypoint, {id_delivery, id_delivery_waypoint, id_order, id_waypoint}).
-record(generated_delivery_object, {id_delivery, id_delivery_object, id_order, id_object}).
-record(notification, {id_delivery, id_order}).
-record(notification_type, {id, description, notify_client}).
-record(oauth_provider, {id, description, client_id, client_secret, authorize_endpoint, access_token_endpoint, profile_endpoint}).
-record(object_type, {id, description, size, length, width, height, weight, volume}).
-record(parameterization, {id, value, description, version}).
-record(order, {id_order, reference, distribution_center, id_user, id_type, id_transport_type, id_origin,
				client_name, client_phone_nr, client_email, client_fiscal_id, cut_time, id_status, id_payment, id_payment_method,
				diner_qty, gross_total, tip, fee, creation_date, status_date, start_prod_date, version}).
-record(order_prod, {id_order, id_product, description, name, quantity, gross_amount}).
-record(order_prod_option, {id_order, id_product, id_prod_option, type, name}).
-record(order_prod_option_entry, {id_order, id_product, id_prod_option, id_prod_option_entry, quantity, name,
								  selected, gross_amount}).
-record(order_object, {id_order, id_object, reference, id_type, transport_auth, original_id_object}).
-record(order_payment, {id, id_order, id_provider, id_status, ref_payment, authorization_only, creation_date,
						status_date}).
-record(order_voucher, {id_order, id_voucher, creation_date}).
-record(order_waypoint, {id_order, id_waypoint, reference, formatted_address, latitude, longitude,
						 contact_name, contact_phone_nr, contact_email, notes, signature, stop_from, stop_to,
						 stop_duration}).
-record(order_waypoint_address, {id_order, id_waypoint, component, value}).
-record(order_waypoint_object_action, {id_order, id_waypoint, id_object, id_action, action}).
-record(payment, {id, id_order, id_provider, id_status, ref_payment, authorization_only, creation_date, status_date}).
-record(payment_provider, {id, description, client_id, client_secret, entity_id, request_endpoint, assertion_endpoint, get_details_endpoint}).
-record(payment_request, {id, id_payment, endpoint, request_data, response_data, creation_date}).
-record(token, {id, id_type, creation_date, remaining_uses, invalid_after, version}).
-record(token_parameter, {id_token, id_type, value}).
-record(token_type, {id, description, multiple_uses, uses, expires_in_minutes, version}).
-record(transport_type, {id, description, capacity}).
-record(user, {id, id_type, username, id_status, creation_date, status_date, login_date, email, first_name,
			   last_name, telephone_nr, fiscal_id, reference, mobileos_id, birth_day, birth_month, birth_year,
			   national_id, country, rating, version}).
-record(user_address, {id_user, component, value}).
-record(user_courier, {id_user, id_courier}).
-record(user_location, {id, id_user, description, latitude, longitude}).
-record(user_location_component, {id_location, component, value}).
-record(user_location_contact, {id_contact, id_location, name, phone_nr, email}).
-record(user_notification, {id_user, id_type, enabled}).
-record(user_oauth, {id_user, id_oauth, id_oauth_provider}).
-record(user_zone, {id_user, id_zone}).
-record(voucher, {voucher_id, voucher_code, value, voucher_type_id, voucher_status_id, value_used, max_times, times_used,
				  expiration_date, restrict_user_id, creation_date, status_date, version}).
-record(zone, {id, description, center_latitude, center_longitude}).
-record(zone_postal_code, {id_zone, postal_code}).

%% ====================================================================
%% Records for database table unions/views
%% ====================================================================
-record(u_account_user, {id, id_account_user_type, id_user_type, username, id_status, creation_date, status_date, email,
		  first_name, last_name, telephone_nr, fiscal_id, department_id, cost_center_id, user_reference, transport_types=[],
		  version}).
-record(u_courier_info, {id, creation_date, email, first_name, last_name, telephone_nr}).
-record(u_order_delivery_courier, {id_delivery, id_order, id_courier}).
-record(u_delivery_waypoint, {id_delivery, id_waypoint, latitude, longitude, travel_distance, travel_time_est, travel_time_real,
							  stop_duration_est, stop_duration_real, checkin_date, checkout_date, checkin_lat,
							  checkin_lon, checkout_lat, checkout_lon, id_status, creation_date, status_date, version}).
-record(u_delivery_waypoint_detail, {id_delivery, id_delivery_waypoint, id_order, id_waypoint, reference,
									 formatted_address, address=[], contact_name, contact_phone_nr, contact_email,
									 client_name, client_phone_nr, client_email, client_fiscal_id, user_id, notes,
									 signature, stop_from, stop_to, stop_duration, rating, object_actions=[]}).

%% ====================================================================
%% Information records
%% ====================================================================
-record(session_info, {session_token, id_user, id_user_type, invalid_after, status, push_info, position,
					   id_transport_type, notify}).
-record(inf_account, {account, account_infos, account_address}).
-record(inf_account_users, {version, users}).
-record(inf_delivery, {delivery, waypoints=[], objects=[], orders=[]}).
-record(inf_delivery_waypoint, {waypoint, details}).
-record(courier_counters, {unavailable=0, available=0, working=0}).
-record(courier_deliveries, {pending_deliveries=[], assigned_deliveries=[]}).
-record(counter_ongoing_deliveries, {id_status, total}).
-record(document_summary, {id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version}).
-record(inf_order, {order, waypoints=[], objects=[], order_prods=[], additional_info, courier_info=[]}).
-record(inf_order_waypoint, {waypoint, object_actions=[], address=[]}).
-record(inf_order_prod, {order_prod, order_prod_options=[]}).
-record(inf_order_prod_option, {order_prod_option, order_prod_option_entries=[]}).
-record(inf_location, {location, components, contacts}).
-record(position_update, {available, working}).
-record(courier_app_config, {position_update}).
-record(inf_zone, {zone, postal_codes=[]}).
-record(inf_online_courier, {session_info, user, photo, assigned_deliveries=[]}).
-record(inf_online_courier_account, {id_account, name}).
-record(info_user_external, {token, email, first_name, last_name, telephone_nr, fiscal_id, address}).
-record(auth_info, {access_token, expires}).
-record(inf_user, {user, account, account_user, user_address, transports, zones=[]}).
-record(inf_waypoint_error, {waypoint_reference, formatted_address, address}).
-record(inf_user_notifications, {enabled, notification_type_ids=[]}).
-record(dispatcher_info, {order_id, start_delivery_time_est, travel_distance}).
-record(dispatcher_courier_info, {id_delivery, id_courier, first_name, last_name, telephone_nr, position, transport_type, courier_photo, eta}).
-record(delivery_estimated_times, {travel_time_est, stop_duration_est}).
-record(delivery_travel_distance, {id_delivery, travel_distance}).
-record(inf_account_slots, {id, lower_slot, upper_slot, occupancy, max_occupancy, availability}).
-record(inf_account_slots_temp, {id, lower_slot, upper_slot, occupancy, max_occupancy, availability}).
-record(order_backoffice, {client_name, client_phone, client_email, client_nif, client_address=[], client_prods=[], 
						   waypoint_detail=[], deliveries=[]}).
-record(waypoint_detail, {id, checkin_date, checkout_date}).

%% ====================================================================
%% Other records
%% ====================================================================
-record(gen_delivery, {polyline, transport_type_id, waypoints, distance, duration}).
-record(gen_waypoint, {waypoint, latitude, longitude, arrival, object_action_id, objects}).

%% ====================================================================
%% Database constants
%% ====================================================================
-define(DB_DECIMAL_PLACES_MONEY,      2).
-define(DB_DECIMAL_PLACES_COORDINATE, 8).

-define(DB_PARAM_PURGE_EXPIRED_SESSIONS_RUN_SECS,                {1, integer}).
-define(DB_PARAM_SESSION_TIMEOUT_SECS,                           {2, integer}).
-define(DB_PARAM_STATIC_CONTENT_LOCATION,                        {3, binary}).
-define(DB_PARAM_NOTIFICATION_REGISTER_USER_SENDER,              {4, binary}).
-define(DB_PARAM_NOTIFICATION_REGISTER_USER_SUBJECT,             {5, binary}).
-define(DB_PARAM_NOTIFICATION_RESET_PASSWORD_SENDER,             {6, binary}).
-define(DB_PARAM_NOTIFICATION_RESET_PASSWORD_SUBJECT,            {7, binary}).
-define(DB_PARAM_NOTIFICATION_CHANGE_EMAIL_SENDER,               {8, binary}).
-define(DB_PARAM_NOTIFICATION_CHANGE_EMAIL_SUBJECT,              {9, binary}).
-define(DB_PARAM_NOTIFICATION_CHANGE_PASSWORD_SENDER,           {10, binary}).
-define(DB_PARAM_NOTIFICATION_CHANGE_PASSWORD_SUBJECT,          {11, binary}).
-define(DB_PARAM_GOOGLE_API_KEY,                                {12, string}).
-define(DB_PARAM_GOOGLE_DIRECTIONS_ENDPOINT,                    {13, string}).
-define(DB_PARAM_GOOGLE_CLOUD_MESSAGING_ENDPOINT,               {14, string}).
-define(DB_PARAM_GOOGLE_DIRECTIONS_REGION,                      {15, string}).
-define(DB_PARAM_CALL_CENTER,                                   {16, binary}).
-define(DB_PARAM_WAIT_FOR_OTHER_COURIERS,                       {17, integer}).
-define(DB_PARAM_PENDING_RADIUS,                                {18, integer}).
-define(DB_PARAM_ROUTIFIC_API_KEY,                              {19, string}).
-define(DB_PARAM_ROUTIFIC_CREATION_ENDPOINT,                    {20, string}).
-define(DB_PARAM_ROUTIFIC_QUERY_ENDPOINT,                       {21, string}).
-define(DB_PARAM_WAIT_FOR_COURIERS,                             {22, integer}).
-define(DB_PARAM_STOP_DURATION,                                 {23, integer}).
-define(DB_PARAM_SCHEDULED_ORDERS_CREATE_RUN_SECS,              {24, integer}).
-define(DB_PARAM_DEPOT_STOP_DURATION,                           {25, integer}).
-define(DB_PARAM_SCHEDULED_ORDERS_PUSH_RUN_SECS,                {26, integer}).
-define(DB_PARAM_SCHEDULED_ORDERS_PUSH_BOUND_LOWER_MINS,        {27, integer}).
-define(DB_PARAM_SCHEDULED_ORDERS_PUSH_BOUND_UPPER_MINS,        {28, integer}).
-define(DB_PARAM_COURIER_APP_UPDATE_FREQUENCY_AVAILABLE,        {29, integer}).
-define(DB_PARAM_COURIER_APP_UPDATE_FREQUENCY_WORKING,          {30, integer}).
-define(DB_PARAM_GOOGLE_DISTANCE_MATRIX_ENDPOINT,               {31, string}).
-define(DB_PARAM_GOOGLE_SNAP_TO_ROAD_ENDPOINT,                  {32, string}).
-define(DB_PARAM_MAX_DISTANCE_DIFF_BETWEEN_WP_CHECKP,           {33, integer}).
-define(DB_PARAM_PASSWORD_REDIRECT_URL,                         {34, binary}).
-define(DB_PARAM_EMAIL_REDIRECT_URL,                            {35, binary}).
-define(DB_PARAM_USER_REDIRECT_URL,                             {36, binary}).
-define(DB_PARAM_PRODUCTION_TIME,                               {37, integer}).
-define(DB_PARAM_BUFFER_TIME,                                   {38, integer}).
-define(DB_PARAM_MAX_ORDERS_PER_DELIVERY,                       {39, integer}).
-define(DB_PARAM_GOOGLE_DISTANCE_MATRIX_NODE_LIMIT,             {40, integer}).
-define(DB_PARAM_ORDER_CREATED_STATUS_ACCEPTABLE_TIME,          {41, integer}).
-define(DB_PARAM_PRODUCTION_TIME_ALERT_LEVEL_1,                 {42, integer}).
-define(DB_PARAM_PRODUCTION_TIME_ALERT_LEVEL_2,                 {43, integer}).
-define(DB_PARAM_PRODUCTION_TIME_ALERT_LEVEL_3,                 {44, integer}).
-define(DB_PARAM_DEFAULT_ESTIMATED_DELIVERY_TIME,               {45, integer}).
-define(DB_PARAM_GOOGLE_DISTANCE_MATRIX_COOLDOWN,               {46, integer}).
-define(DB_PARAM_NOTIFICATION_VRP_CRASH_SENDER,                 {47, binary}).
-define(DB_PARAM_NOTIFICATION_VRP_CRASH_SUBJECT,                {48, binary}).
-define(DB_PARAM_NOTIFICATION_VRP_CRASH_EMAIL,                  {49, binary}).
-define(DB_PARAM_VRP_COURIER_COUNT,                             {50, integer}).
-define(DB_PARAM_TW_BEFORE_CUT_TIME,                            {51, integer}).
-define(DB_PARAM_TW_AFTER_CUT_TIME,                             {52, integer}).
-define(DB_PARAM_SLOTTED_TW,                                    {53, integer}).

-define(DB_USER_TYPE_ADMINISTRATOR,            1). % REST session type operator
-define(DB_USER_TYPE_OPERATOR,                 2). % REST session type operator
-define(DB_USER_TYPE_DOCUMENT_VALIDATOR,       3). % REST session type operator
-define(DB_USER_TYPE_COURIER,                  4). % REST session type courier
-define(DB_USER_TYPE_CLIENT_PRIVATE,           5). % REST session type client
-define(DB_USER_TYPE_CLIENT_BUSINESS,          6). % REST session type client
-define(DB_USER_TYPE_CLIENT_ECOMMERCE,         7). % REST session type client
-define(DB_USER_TYPE_EXTERNAL_APP,             8). % REST session type external apps
-define(DB_USER_TYPE_DISPATCHER,               9). % REST session type client
-define(DB_USER_TYPE_CLIENT_COURIER_BUSINESS, 10). % REST session type client
-define(DB_USER_TYPE_EXTERNAL_COURIER,        11). % REST session type client
-define(DB_USER_TYPE_CALL_CENTER,             12). % REST session type call center

-define(DB_USER_STATUS_PENDING_CONFIRMATION, 1).
-define(DB_USER_STATUS_PENDING_VALIDATION,   2).
-define(DB_USER_STATUS_ACTIVE,               3).
-define(DB_USER_STATUS_INACTIVE,             4).

-define(DB_ACCOUNT_STATUS_PENDING,  1).
-define(DB_ACCOUNT_STATUS_ACTIVE,   2).
-define(DB_ACCOUNT_STATUS_INACTIVE, 3).

-define(DB_ACCOUNT_USER_TYPE_ADMINISTRATOR, 1).
-define(DB_ACCOUNT_USER_TYPE_USER,          2).

-define(DB_CONTACT_REQUEST_STATUS_PENDING,    1).
-define(DB_CONTACT_REQUEST_STATUS_PROCESSING, 2).
-define(DB_CONTACT_REQUEST_STATUS_DONE,       3).

-define(DB_TOKEN_TYPE_CREATE_USER,          1).
-define(DB_TOKEN_TYPE_RESET_PASSWORD,       2).
-define(DB_TOKEN_TYPE_TRACKING,             3).
-define(DB_TOKEN_TYPE_LOGIN_EXTERNAL_USER,  4).

-define(DB_TOKEN_PARAMETER_TYPE_USER_ID,           1).
-define(DB_TOKEN_PARAMETER_TYPE_ORDER_ID,          2).
-define(DB_TOKEN_PARAMETER_TYPE_VERSION,           3).
-define(DB_TOKEN_PARAMETER_TYPE_USER_TYPE_ID,      4).
-define(DB_TOKEN_PARAMETER_TYPE_OAUTH_PROVIDER_ID, 5).

-define(DB_PAYMENT_METHOD_TYPE_OWNER_ACCOUNT, 1).
-define(DB_PAYMENT_METHOD_TYPE_OWNER_USER,    2).
-define(DB_PAYMENT_METHOD_TYPE_OWNER_BOTH,    3).

-define(DB_DELIVERY_TYPE_SUPEREXPRESS_NOW, 1).
-define(DB_DELIVERY_TYPE_SUPEREXPRESS_4H,  2).
-define(DB_DELIVERY_TYPE_DISTRIBUTION,     3).
-define(DB_DELIVERY_TYPE_COLLECTION,       4).
-define(DB_DELIVERY_TYPE_TODAY_1,          5).
-define(DB_DELIVERY_TYPE_TODAY_2,          6).

-define(DB_TRANSPORT_TYPE_MOTORCYCLE, 1).
-define(DB_TRANSPORT_TYPE_BICYCLE,    2).
-define(DB_TRANSPORT_TYPE_PEDESTRIAN, 3).
-define(DB_TRANSPORT_TYPE_VAN,        4).

-define(DB_MAX_DELIVERY_PER_COURIER, 3).

-define(DB_DELIVERY_STATUS_CREATED,                       1).
-define(DB_DELIVERY_STATUS_ACCEPTED,                      3).
-define(DB_DELIVERY_STATUS_EXECUTING,                     4).
-define(DB_DELIVERY_STATUS_WAITING_CONFIRMATION,          5).
-define(DB_DELIVERY_STATUS_COMPLETED_SUCCESS,             6).
-define(DB_DELIVERY_STATUS_COMPLETED_UNPAID,              7).
-define(DB_DELIVERY_STATUS_COMPLETED_REFUSED,             8).
-define(DB_DELIVERY_STATUS_SCHEDULED,                     9).
-define(DB_DELIVERY_STATUS_COMPLETED_CANCELED,           10).
-define(DB_DELIVERY_STATUS_CANCELED_CUT_TIME,            11).
-define(DB_DELIVERY_STATUS_EXECUTING_PAYMENT,            12).
-define(DB_DELIVERY_STATUS_COMPLETED_COLLECTION_ERROR,   13).
-define(DB_DELIVERY_STATUS_COMPLETED_DISTRIBUTION_ERROR, 14).
-define(DB_DELIVERY_STATUS_WAITING_EXECUTION,            15).

-define(DB_DELIVERY_SKIP_TRUE,  true).
-define(DB_DELIVERY_SKIP_FALSE, false).

-define(DB_WAYPOINT_STATUS_WAITING, 1).
-define(DB_WAYPOINT_STATUS_ARRIVED, 2).
-define(DB_WAYPOINT_STATUS_LEFT,    3).

-define(DB_WAYPOINT_TRAVEL_TIME_EST, 120).

-define(DB_NOTIFICATION_TYPE_DELAYED,           1).
-define(DB_NOTIFICATION_TYPE_NOT_FOUND_ADDRESS, 2).
-define(DB_NOTIFICATION_TYPE_NOT_FOUND_CONTACT, 3).
-define(DB_NOTIFICATION_TYPE_PROBLEM,           4).

-define(DB_API_CLIENT_STATUS_ACTIVE,    1).
-define(DB_API_CLIENT_STATUS_SUSPENDED, 2).
-define(DB_API_CLIENT_STATUS_INACTIVE,  3).

-define(DB_OBJECT_ACTION_PICKUP,  1).
-define(DB_OBJECT_ACTION_DROPOFF, 2).
-define(DB_OBJECT_ACTION_OTHER,   3).

-define(DB_OBJECT_STATUS_PENDING, 1).
-define(DB_OBJECT_STATUS_DONE,    2).

-define(DB_OBJECT_TYPE_STATUS_ACTIVE,   1).
-define(DB_OBJECT_TYPE_STATUS_INACTIVE, 2).

-define(DB_NOTIFICATION_STATUS_PENDING, 1).
-define(DB_NOTIFICATION_STATUS_DONE,    2).

-define(DB_ORDER_STATUS_CREATED,           1).
-define(DB_ORDER_STATUS_PRODUCTION,        2).
-define(DB_ORDER_STATUS_DISPATCH,          4).
-define(DB_ORDER_STATUS_EXECUTING,         5).
-define(DB_ORDER_STATUS_COMPLETED_SUCCESS, 6).
-define(DB_ORDER_STATUS_COMPLETED_REFUSED, 7).
-define(DB_ORDER_STATUS_CANCELED,          8).

-define(DB_ORDER_TYPE_DELIVERY,  1).
-define(DB_ORDER_TYPE_TAKEAWAY,  2).

-define(DB_ORDER_ORIGIN_APP,         1).
-define(DB_ORDER_ORIGIN_WEB,         2).
-define(DB_ORDER_ORIGIN_CALL_CENTER, 3).

-define(DB_ORDER_PAYMENT_METHOD_CREDIT_CARD, 1).
-define(DB_ORDER_PAYMENT_METHOD_MONEY,       2).
-define(DB_ORDER_PAYMENT_METHOD_TPA,         3).

-define(DB_PAYMENT_PROVIDER_SIBS, 1).

-define(DB_PAYMENT_STATUS_CREATED,        1).
-define(DB_PAYMENT_STATUS_AUTHORIZED,     2).
-define(DB_PAYMENT_STATUS_FINISHED_OK,    3).
-define(DB_PAYMENT_STATUS_FINISHED_ERROR, 4).

-define(DB_DOCUMENT_TYPE_PHOTO,           1).
-define(DB_DOCUMENT_TYPE_DRIVING_LICENSE, 2).
-define(DB_DOCUMENT_TYPE_ID_CARD,         3).

-define(DB_DOCUMENT_STATUS_SUBMITED, 1).
-define(DB_DOCUMENT_STATUS_VALID,    2).
-define(DB_DOCUMENT_STATUS_INVALID,  3).

-define(DB_COURIER_TRANSPORT_STATUS_PENDING,  1).
-define(DB_COURIER_TRANSPORT_STATUS_ACTIVE,   2).
-define(DB_COURIER_TRANSPORT_STATUS_INACTIVE, 3).

-define(DB_COURIER_TRANSPORT_CURRENT_YES, true).
-define(DB_COURIER_TRANSPORT_CURRENT_NO,  false).

-define(DB_VOUCHER_TYPE_ONE_TIME,             1).
-define(DB_VOUCHER_TYPE_MULTI_TIME_MAX_VALUE, 2).
-define(DB_VOUCHER_TYPE_MULTI_TIME_N_TIMES,   3).

-define(DB_VOUCHER_STATUS_UNUSED,         1).
-define(DB_VOUCHER_STATUS_PARTIALLY_USED, 2).
-define(DB_VOUCHER_STATUS_USED,           3).
-define(DB_VOUCHER_STATUS_CANCELLED,      4).

-define(DB_OAUTH_PROVIDER_FB, 1).
-define(DB_OAUTH_PROVIDER_GOOGLE, 1).

-define(DB_OBJECT_PHOTO_OBJECT,     1).
-define(DB_OBJECT_PHOTO_TRANS_AUTH, 2).

% DB Column sizes
-define(DB_FIELD_SIZE__USER__EMAIL,        100).
-define(DB_FIELD_SIZE__USER__FIRST_NAME,    50).
-define(DB_FIELD_SIZE__USER__LAST_NAME,     50).
-define(DB_FIELD_SIZE__USER__TELEPHONE_NR,  15).
-define(DB_FIELD_SIZE__USER__FISCAL_ID,     20).
-define(DB_FIELD_SIZE__USER__REFERENCE,     30).
-define(DB_FIELD_SIZE__USER__NATIONAL_ID,   20).
-define(DB_FIELD_SIZE__USER__COUNTRY,       50).

-define(DB_FIELD_SIZE__USER_AUTH__USERNAME, 100).
-define(DB_FIELD_SIZE__USER_AUTH__PASSWORD, 100).

-define(DB_FIELD_SIZE__USER_OAUTH_ID_OAUTH, 100).

-define(DB_FIELD_SIZE__CONTACT_REQUEST__EMAIL,           100).
-define(DB_FIELD_SIZE__CONTACT_REQUEST__NAME,            100).
-define(DB_FIELD_SIZE__CONTACT_REQUEST__SUBJECT,         100).
-define(DB_FIELD_SIZE__CONTACT_REQUEST__CONTENT,        1000).
-define(DB_FIELD_SIZE__CONTACT_REQUEST__OPERATOR_NOTES, 1000).

-define(DB_FIELD_SIZE__TOKEN__ID, 50).

-define(DB_FIELD_SIZE__TOKEN_PARAMETER__ID_TOKEN,  50).
-define(DB_FIELD_SIZE__TOKEN_PARAMETER__VALUE,    100).

-define(DB_FIELD_SIZE__ACCOUNT__UNIQUE_KEY,   30).
-define(DB_FIELD_SIZE__ACCOUNT__ACCOUNT_NAME, 100).
-define(DB_FIELD_SIZE__ACCOUNT__FISCAL_ID,    20).
-define(DB_FIELD_SIZE__ACCOUNT__TELEPHONE_NR, 15).
-define(DB_FIELD_SIZE__ACCOUNT__EMAIL,        100).
-define(DB_FIELD_SIZE__ACCOUNT__CONTRACT_NR,  30).

-define(DB_FIELD_SIZE__ACCOUNT_INFO__PROPERTY,  50).
-define(DB_FIELD_SIZE__ACCOUNT_INFO__VALUE,    100).

-define(DB_FIELD_SIZE__ACCOUNT_ADDRESS__COMPONENT, 50).
-define(DB_FIELD_SIZE__ACCOUNT_ADDRESS__VALUE,     100).

-define(DB_FIELD_SIZE__COST_CENTER__NAME, 30).
-define(DB_FIELD_SIZE__COST_CENTER__DESCRIPTION, 30).

-define(DB_FIELD_SIZE__DELIVERY__REFERENCE,              30).
-define(DB_FIELD_SIZE__DELIVERY__ROUTE,                1500).

-define(DB_FIELD_SIZE__DELIVERY__DISTRIBUTION_CENTER,    50).
-define(DB_FIELD_SIZE__DELIVERY__CLIENT_NAME,           100).
-define(DB_FIELD_SIZE__DELIVERY__CLIENT_PHONE_NR,        15).
-define(DB_FIELD_SIZE__DELIVERY__CLIENT_EMAIL,          100).

-define(DB_FIELD_SIZE__DELIVERY_WAYPOINT_RATING__RATING_NOTES_CLIENT,  200).
-define(DB_FIELD_SIZE__DELIVERY_WAYPOINT_RATING__RATING_NOTES_COURIER, 200).

-define(DB_FIELD_SIZE__DELIVERY_WAYPOINT_ADDRESS__COMPONENT,     50).
-define(DB_FIELD_SIZE__DELIVERY_WAYPOINT_ADDRESS__VALUE,        100).

-define(DB_FIELD_SIZE__DELIVERY_WAYPOINT_OBJECT_ACTION__ACTION,  50).

-define(DB_FIELD_SIZE__DELIVERY_WAYPOINT_SIGNATURE__NAME,        50).
-define(DB_FIELD_SIZE__DELIVERY_WAYPOINT_SIGNATURE__MIMETYPE,    50).

-define(DB_FIELD_SIZE__ORDER__REFERENCE,            50).
-define(DB_FIELD_SIZE__ORDER__DISTRIBUTION_CENTER,  50).
-define(DB_FIELD_SIZE__ORDER__CLIENT_NAME,         100).
-define(DB_FIELD_SIZE__ORDER__CLIENT_PHONE_NR,      20).
-define(DB_FIELD_SIZE__ORDER__CLIENT_EMAIL,        100).
-define(DB_FIELD_SIZE__ORDER__CLIENT_FISCAL_ID,     20).
-define(DB_FIELD_SIZE__ORDER__PAYMENT_ID,           32).

-define(DB_FIELD_SIZE__ORDER_PROD__DESCRIPTION, 500).
-define(DB_FIELD_SIZE__ORDER_PROD__NAME,        1000).

-define(DB_FIELD_SIZE__ORDER_PROD_OPTION__NAME, 1000).

-define(DB_FIELD_SIZE__ORDER_PROD_OPTION_ENTRY__NAME, 1000).

-define(DB_FIELD_SIZE__ORDER_WAYPOINT__REFERENCE,          30).
-define(DB_FIELD_SIZE__ORDER_WAYPOINT__FORMATTED_ADDRESS, 100).
-define(DB_FIELD_SIZE__ORDER_WAYPOINT__CONTACT_NAME,      100).
-define(DB_FIELD_SIZE__ORDER_WAYPOINT__CONTACT_PHONE_NR,   20).
-define(DB_FIELD_SIZE__ORDER_WAYPOINT__CONTACT_EMAIL,     100).
-define(DB_FIELD_SIZE__ORDER_WAYPOINT__NOTES,             200).
-define(DB_FIELD_SIZE__ORDER_WAYPOINT__STOP_FROM,           5).
-define(DB_FIELD_SIZE__ORDER_WAYPOINT__STOP_TO,             5).

-define(DB_FIELD_SIZE__ORDER_OBJECT__REFERENCE,      30).
-define(DB_FIELD_SIZE__ORDER_OBJECT__TRANSPORT_AUTH, 30).
-define(DB_FIELD_SIZE__ORDER_OBJECT__DESCRIPTION,    30).

-define(DB_FIELD_SIZE__ORDER_WAYPOINT_OBJECT_ACTION__ACTION, 50).

-define(DB_FIELD_SIZE__OBJECT_TYPE__DESCRIPTION, 30).

-define(DB_FIELD_SIZE__OBJECT__REFERENCE,      30).
-define(DB_FIELD_SIZE__OBJECT__TRANSPORT_AUTH, 30).
-define(DB_FIELD_SIZE__OBJECT__DESCRIPTION,    30).
-define(DB_FIELD_SIZE__OBJECT__NON_DEL_REASON, 50).

-define(DB_FIELD_SIZE__NOTIFICATION__MESSAGE, 300).
-define(DB_FIELD_SIZE__NOTIFICATION__NOTES,   200).

-define(DB_FIELD_SIZE__PARAMETRIZATION__VALUE,       200).
-define(DB_FIELD_SIZE__PARAMETRIZATION__DESCRIPTION, 100).

-define(DB_FIELD_SIZE__TOKEN_TYPE__DESCRIPTION, 30).

-define(DB_FIELD_SIZE__USER_LOCATION__DESCRIPTION, 50).

-define(DB_FIELD_SIZE__USER_LOCATION_COMPONENT__COMPONENT, 50).
-define(DB_FIELD_SIZE__USER_LOCATION_COMPONENT__VALUE,     100).

-define(DB_FIELD_SIZE__USER_LOCATION_CONTACT_NAME,     50).
-define(DB_FIELD_SIZE__USER_LOCATION_CONTACT_PHONE_NR, 15).
-define(DB_FIELD_SIZE__USER_LOCATION_CONTACT_EMAIL,    100).

-define(DB_FIELD_SIZE__COURIER_TRANSPORT_TYPE__DESCRIPTION,     30).
-define(DB_FIELD_SIZE__COURIER_TRANSPORT_TYPE__REGISTRATION_ID, 20).

-define(DB_FIELD_SIZE__VOUCHER__VOUCHER_CODE, 10).

-define(DB_FIELD_SIZE__FILE__NAME,     50).
-define(DB_FIELD_SIZE__FILE__MIMETYPE, 50).

%% ====================================================================
%% Guard functions
%% ====================================================================
-define(IS_OPERATOR(UserTypeId), (UserTypeId =:= ?DB_USER_TYPE_ADMINISTRATOR orelse UserTypeId =:= ?DB_USER_TYPE_OPERATOR orelse UserTypeId =:= ?DB_USER_TYPE_CALL_CENTER)).
-define(IS_CLIENT(UserTypeId), (UserTypeId =:= ?DB_USER_TYPE_CLIENT_PRIVATE orelse UserTypeId =:= ?DB_USER_TYPE_CLIENT_BUSINESS orelse UserTypeId =:= ?DB_USER_TYPE_CLIENT_ECOMMERCE)).
-define(IS_DISPATCHER(UserTypeId), (UserTypeId =:= ?DB_USER_TYPE_DISPATCHER)).
-define(IS_COURIER(UserTypeId), (UserTypeId =:= ?DB_USER_TYPE_COURIER orelse UserTypeId =:= ?DB_USER_TYPE_EXTERNAL_COURIER)).
-define(IS_EXTERNAL_APP(UserTypeId), (UserTypeId =:= ?DB_USER_TYPE_EXTERNAL_APP)).
-define(IS_PRIVATE_CLIENT(UserTypeId), (UserTypeId =:= ?DB_USER_TYPE_CLIENT_PRIVATE)).
-define(IS_BUSINESS_CLIENT(UserTypeId), (UserTypeId =:= ?DB_USER_TYPE_CLIENT_BUSINESS orelse UserTypeId =:= ?DB_USER_TYPE_CLIENT_COURIER_BUSINESS)).
