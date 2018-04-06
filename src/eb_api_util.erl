%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_api_util).

-include("eb_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([verify_same_user/2, verify_user_permissions/3, verify_account_permissions_admin/3, verify_account_permissions/3, verify_operator_permissions/1, verify_client_permissions/1,
         verify_courier_permissions/1, verify_client_operator_permissions/1, verify_client_courier_permissions/1, verify_client_operator_app_permissions/1, 
         verify_user_or_account_permissions/3, verify_client_app_permissions/1, verify_operator_or_dispatcher_permissions/1, verify_account_or_dispatcher_permissions/3, 
         sanitize_navigation/2, sanitize_position/1, trim_optional/1,
         trim_optional/2, trim_mandatory/1, trim_mandatory/2, is_optional_integer/1, is_nullable_integer/1, is_positive_nullable_integer/1,
         sanitize_additional_infos/2, check_one_of/2, check_one_of/3, validate_optional_id/2, validate_id/2, sanitize_file/1, sanitize_file/2, is_optional_float/1,
         get_update_value/2, verify_ecommerce_permissions/1, sanitize_waypoint_contact/2, sanitize_time_window/1, is_optional_hour_minute/2, validate_size_optional/2, 
         validate_component/1, mandatory_component/1, verify_app_permissions/1, verify_dispatcher_permissions/1, sanitize_address_components/2, is_optional_color/1]).

verify_same_user(UserId, UserId) -> ok; % Same user
verify_same_user(_RequestingUserId, _UserId) -> nok.

verify_operator_permissions(?DB_USER_TYPE_ADMINISTRATOR) -> ok; % BO user
verify_operator_permissions(?DB_USER_TYPE_OPERATOR) -> ok; % BO user
verify_operator_permissions(?DB_USER_TYPE_CALL_CENTER) -> ok; % Call Center user
verify_operator_permissions(_RequestingUserTypeId) -> nok.

verify_client_permissions(?DB_USER_TYPE_CLIENT_PRIVATE) -> ok; % Client
verify_client_permissions(?DB_USER_TYPE_CLIENT_BUSINESS) -> ok; % Client
verify_client_permissions(?DB_USER_TYPE_CLIENT_ECOMMERCE) -> ok; % Client
verify_client_permissions(?DB_USER_TYPE_CLIENT_COURIER_BUSINESS) -> ok; % Client
verify_client_permissions(_RequestingUserTypeId) -> nok.

verify_courier_permissions(?DB_USER_TYPE_COURIER) -> ok; % Courier
verify_courier_permissions(?DB_USER_TYPE_EXTERNAL_COURIER) -> ok; % Courier
verify_courier_permissions(_RequestingUserTypeId) -> nok.

verify_app_permissions(?DB_USER_TYPE_EXTERNAL_APP) -> ok; % External App
verify_app_permissions(_RequestingUserTypeId) -> nok.

verify_dispatcher_permissions(?DB_USER_TYPE_DISPATCHER) -> ok;  %Dispatcher
verify_dispatcher_permissions(_RequestingUserTypeId) -> nok.

verify_ecommerce_permissions(?DB_USER_TYPE_CLIENT_ECOMMERCE) -> ok; % ECommerce
verify_ecommerce_permissions(_RequestingUserTypeId) -> nok.

verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) ->
	case verify_same_user(RequestingUserId, TargetUserId) of
		ok -> ok;
		_ -> verify_operator_permissions(RequestingUserTypeId)
	end.

verify_client_operator_permissions(RequestingUserTypeId) ->
	case verify_client_permissions(RequestingUserTypeId) of
		ok -> ok;
		_ -> verify_operator_permissions(RequestingUserTypeId)
	end.

verify_client_courier_permissions(RequestingUserTypeId) ->
	case verify_client_permissions(RequestingUserTypeId) of
		ok -> ok;
		_ -> verify_courier_permissions(RequestingUserTypeId)
	end.

verify_client_operator_app_permissions(RequestingUserTypeId) ->
	case verify_client_permissions(RequestingUserTypeId) of
		ok -> ok;
		_ ->
			case verify_operator_permissions(RequestingUserTypeId) of
				ok -> ok;
				_ -> verify_app_permissions(RequestingUserTypeId)
			end
	end.

verify_client_app_permissions(RequestingUserTypeId) ->
	case verify_client_permissions(RequestingUserTypeId) of
		ok -> ok;
		_ -> verify_app_permissions(RequestingUserTypeId)
	end.


verify_operator_or_dispatcher_permissions(RequestingUserTypeId) ->
	case verify_operator_permissions(RequestingUserTypeId) of
		ok -> ok;
		nok -> verify_dispatcher_permissions(RequestingUserTypeId)
	end.

verify_account_permissions(RequestingUserId, RequestingUserTypeId, AccountId) -> 
	case verify_operator_permissions(RequestingUserTypeId) of
		ok -> ok;
		_ ->
			case get_account_user(RequestingUserId, AccountId) of
				{ok, _Result} -> ok;
				not_found -> nok;
				Other -> Other
			end
	end.

verify_account_or_dispatcher_permissions(RequestingUserId, RequestingUserTypeId, AccountId) ->
	case verify_account_permissions(RequestingUserId, RequestingUserTypeId, AccountId) of
		ok -> ok;
		nok -> verify_dispatcher_permissions(RequestingUserTypeId)
	end.

verify_account_permissions_admin(RequestingUserId, RequestingUserTypeId, AccountId) -> 
	case verify_operator_permissions(RequestingUserTypeId) of
		ok -> ok;
		_ ->
			case get_account_user(RequestingUserId, AccountId) of
				{ok, #account_user{id_type=?DB_USER_TYPE_ADMINISTRATOR}} -> ok;
				{ok, _Result} -> nok;
				not_found -> nok;
				Other -> Other
			end
	end.

verify_user_or_account_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) ->
	try
		% Validate same user or operator
		verify_user_permissions(RequestingUserId, RequestingUserTypeId, TargetUserId) =:= ok andalso throw(ok),
		% Validate requesting user is account admin
		RequestingAccountId =
			case get_account_id(RequestingUserId) of
				{ok, AccountId1} -> AccountId1;
				_ -> throw(nok)
			end,
		verify_account_permissions_admin(RequestingUserId, RequestingUserTypeId, RequestingAccountId) =/= ok andalso throw(nok),
		% Validate that the target user belongs to the same account
		TargetAccountId =
			case get_account_id(TargetUserId) of
				{ok, AccountId2} -> AccountId2;
				_ -> throw(nok)
			end,
		if RequestingAccountId =:= TargetAccountId -> ok
		 ; RequestingAccountId =/= TargetAccountId -> nok
		end
	catch
		throw:Ret -> Ret
	end.

	

sanitize_navigation(#rs_navigation{skip=Value}, _ValidOrderColumns) when not is_integer(Value) orelse Value < 0 -> nok;
sanitize_navigation(#rs_navigation{max=Value}, _ValidOrderColumns) when not is_integer(Value) orelse Value < 0 -> nok;
sanitize_navigation(#rs_navigation{order=Order, sort=Sort, skip=Skip, max=Max}, ValidOrderColumns) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FOrder} = trim_optional(Order, TrimPattern),
		{ok, FSort} = trim_optional(Sort, TrimPattern),
		% Check binary strings
		true = check_optional_in_list(FOrder, ValidOrderColumns),
		true = check_optional_in_list(FSort, [?NAVIGATION_SORT_ASC, ?NAVIGATION_SORT_DESC]),
		% Build the return record
		SanitizedRecord = #rs_navigation{order=FOrder, sort=FSort, skip=Skip,  max=Max},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end.

sanitize_position(#position{latitude=Value}) when not is_float(Value) orelse Value < -90.000000 orelse Value > 90.000000 orelse Value == 0 -> nok;
sanitize_position(#position{longitude=Value}) when not is_float(Value) orelse Value < -180.000000 orelse Value > 180.000000 orelse Value == 0 -> nok;

sanitize_position(Record) when is_record(Record, position) -> {ok, Record}.

sanitize_file(File) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		sanitize_file(File, TrimPattern)
	catch
		_:_ -> nok
	end.

sanitize_file(#file{name=Name, mimetype=Mimetype, base64_data=Base64Data}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FName} = trim_mandatory(Name, TrimPattern),
		{ok, FMimetype} = trim_mandatory(Mimetype, TrimPattern),
		{ok, FBase64Data} = trim_mandatory(Base64Data, TrimPattern),
		% Size validations
		ok = eb_util:validate_size(FName, ?DB_FIELD_SIZE__FILE__NAME),
		ok = eb_util:validate_size(FMimetype, ?DB_FIELD_SIZE__FILE__MIMETYPE),
		% Build the return record
		SanitizedRecord = #file{name=FName, mimetype=FMimetype, base64_data=FBase64Data},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end.

sanitize_waypoint_contact(#contact_info{name=Name, phone_nr=PhoneNr, email=Email}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FName} = trim_mandatory(Name, TrimPattern),
		{ok, FPhoneNr} = trim_optional(PhoneNr, TrimPattern),
		{ok, FEmail} = trim_optional(Email, TrimPattern),
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

sanitize_time_window(#time_window{from=From, to=To}) ->
	try
		% Trim binary strings
		{ok, FFrom} = eb_rest_util:get_optional_datetime(From),
		{ok, FTo} = eb_rest_util:get_optional_datetime(To),
		ok = eb_util:verify_dates(FFrom, FTo),

		% Build the return record
		SanitizedRecord = #time_window{from=FFrom, to=FTo},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end.

trim_optional(Value) ->
	case eb_util:get_trim_pattern() of
		{ok, TrimPattern} -> trim_optional(Value, TrimPattern);
		Error -> Error
	end.

trim_optional(undefined, _) -> {ok, undefined};
trim_optional(null, _) -> {ok, null};
trim_optional(Value, TrimPattern) ->
	trim_mandatory(Value, TrimPattern).

trim_mandatory(Value) ->
	case eb_util:get_trim_pattern() of
		{ok, TrimPattern} -> trim_mandatory(Value, TrimPattern);
		Error -> Error
	end.

trim_mandatory(Value, TrimPattern) ->
	case eb_util:trim(Value, TrimPattern) of
		{ok, <<>>} -> missing_value; % We do not want empty values on not null columns
		{ok, Trimmed} -> {ok, Trimmed};
		Error -> Error
	end.

validate_size_optional(null, _Size) -> ok;
validate_size_optional(undefined, _Size) -> ok;
validate_size_optional(Str, Size) -> eb_util:validate_size(Str, Size).

is_optional_integer(undefined) -> true;
is_optional_integer(Integer) -> is_nullable_integer(Integer).

is_optional_float(undefined) -> true;
is_optional_float(Float) -> is_float(Float).

is_optional_hour_minute(undefined, _Pattern) -> true;
is_optional_hour_minute(HHMM, Pattern) -> eb_util:is_valid_hour_minute(HHMM, Pattern).

is_optional_color(undefined) -> true;
is_optional_color(BinColor) when is_binary(BinColor) -> is_optional_color(erlang:binary_to_list(BinColor));
is_optional_color([$#, R1, R2, G1, G2, B1, B2]) 
  when (R1 >= $0 andalso R1 =< $9) orelse (R1 >= $a andalso R1 =< $f) orelse (R1 >= $A andalso R1 =< $F),
       (R2 >= $0 andalso R2 =< $9) orelse (R2 >= $a andalso R2 =< $f) orelse (R2 >= $A andalso R2 =< $F),
       (G1 >= $0 andalso G1 =< $9) orelse (G1 >= $a andalso G1 =< $f) orelse (G1 >= $A andalso G1 =< $F),
       (G2 >= $0 andalso G2 =< $9) orelse (G2 >= $a andalso G2 =< $f) orelse (G2 >= $A andalso G2 =< $F),
       (B1 >= $0 andalso B1 =< $9) orelse (B1 >= $a andalso B1 =< $f) orelse (B1 >= $A andalso B1 =< $F),
       (B2 >= $0 andalso B2 =< $9) orelse (B2 >= $a andalso B2 =< $f) orelse (B2 >= $A andalso B2 =< $F) ->
	true;
is_optional_color(_Other) -> false.

is_nullable_integer(null) -> true;
is_nullable_integer(Integer) -> is_integer(Integer).

is_positive_nullable_integer(null) -> true;
is_positive_nullable_integer(Integer) when is_integer(Integer) andalso Integer > 0 -> true;
is_positive_nullable_integer(_Other) -> false.

sanitize_additional_infos([], _TrimPattern) -> {ok, []};
sanitize_additional_infos(AdditionalInfos, TrimPattern) when is_list(AdditionalInfos) -> sanitize_additional_infos(AdditionalInfos, [], TrimPattern);
sanitize_additional_infos(_Other, _TrimPattern) -> error.

sanitize_address_components([], _TrimPattern) -> {ok, []};
sanitize_address_components(AddressComponents, TrimPattern) when is_list(AddressComponents) ->
	sanitize_address_components(AddressComponents, [], TrimPattern);
sanitize_address_components(_Other, _TrimPattern) -> error.

check_one_of(undefined, undefined) -> nok;
check_one_of([], undefined) -> nok;
check_one_of(_, _) -> ok.

check_one_of(undefined, undefined, undefined) -> nok;
check_one_of([], undefined, undefined) -> nok;
check_one_of(_, _, _) -> ok.

get_update_value(undefined, OldValue) -> OldValue;
get_update_value(NewValue, _OldValue) -> NewValue.

% Use only for exists_* sql statements
validate_optional_id(undefined, _Statement) -> ok;
validate_optional_id(Id, Statement) ->
	validate_id(Id, Statement).

validate_id(undefined, _Statement) -> missing;
validate_id(Id, Statement) when is_integer(Id) ->
	case eb_db_util:execute({Statement, Id}) of
		true -> ok;
		false -> nok;
		_ -> error
	end;
validate_id(_Id, _Statement) -> invalid.

%% ====================================================================
%% Internal functions
%% ====================================================================
sanitize_additional_infos([], SanitizedAdditionalInfos, _TrimPattern) -> {ok, SanitizedAdditionalInfos};
sanitize_additional_infos([AdditionalInfo|Rest], SanitizedAdditionalInfos, TrimPattern) ->
	case sanitize_additional_info(AdditionalInfo, TrimPattern) of
		{ok, SanitizedAdditionalInfo} -> sanitize_additional_infos(Rest, [SanitizedAdditionalInfo|SanitizedAdditionalInfos], TrimPattern);
		_Error -> error
	end.

sanitize_additional_info(#additional_info{value=undefined}, _TrimPattern) -> nok;
sanitize_additional_info(#additional_info{property=Property, value=Value}, TrimPattern) ->
	try
		% Trim binary strings
		{ok, FProperty} = trim_mandatory(Property, TrimPattern),
		{ok, FValue} = trim_optional(Value, TrimPattern),
		% Build the return record
		SanitizedRecord = #additional_info{property=FProperty, value=FValue},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;
sanitize_additional_info(_Other, _TrimPattern) -> error.

sanitize_address_components([], SanitizedAddressComponents, _TrimPattern) -> {ok, SanitizedAddressComponents};
sanitize_address_components([Component|Rest], SanitizedAddressComponents, TrimPattern) ->
	case sanitize_address_component(Component, TrimPattern) of
		{ok, SanitizedAddressComponent} -> sanitize_address_components(Rest, [SanitizedAddressComponent|SanitizedAddressComponents], TrimPattern);
		_Error -> error
	end.

sanitize_address_component(#component_info{value=undefined}, _TrimPattern) -> nok;
sanitize_address_component(#component_info{component=Component, value=Value}, TrimPattern) ->
	try
		io:format("component: ~p~p~n", [Component, Value]),
		% Trim binary strings
		{ok, FComponent} = trim_mandatory(Component, TrimPattern),
		{ok, FValue} = trim_optional(Value, TrimPattern),
		% Build the return record
		SanitizedRecord = #component_info{component=FComponent, value=FValue},
		{ok, SanitizedRecord}
	catch
		_:_ -> io:format("erro component\n"),nok
	end;
sanitize_address_component(_Other, _TrimPattern) -> error.

check_optional_in_list(undefined, _List) -> true;
check_optional_in_list(null, _List) -> true;
check_optional_in_list(Element, List) -> lists:member(Element, List).

get_account_user(RequestingUserId, AccountId) ->
	case eb_db_util:execute({get_account_user, RequestingUserId, AccountId}) of
		Result when is_record(Result, account_user) -> {ok, Result};
		Other -> Other
	end.

get_account_id(UserId) ->
	case eb_db_util:execute({get_account_user_by_id_user, UserId}) of
		#account_user{id_account=AccountId} -> {ok, AccountId};
		Other -> Other
	end.

validate_component(Component) ->
	if 
		Component =:= ?ADDRESS_COMPONENT_01 orelse Component =:= ?ADDRESS_COMPONENT_02 orelse
		Component =:= ?ADDRESS_COMPONENT_03 orelse Component =:= ?ADDRESS_COMPONENT_04 orelse
		Component =:= ?ADDRESS_COMPONENT_05 ->
			Reply = ok;
		true -> 
			Reply = nok
	end,
	Reply.

mandatory_component(Component) ->
	if 
		Component =:= ?ADDRESS_COMPONENT_01 orelse Component =:= ?ADDRESS_COMPONENT_02 orelse
		Component =:= ?ADDRESS_COMPONENT_03 orelse Component =:= ?ADDRESS_COMPONENT_04 orelse
		Component =:= ?ADDRESS_COMPONENT_05 ->
			Reply = ok;
		true -> 
			Reply = nok
	end,
	Reply.
