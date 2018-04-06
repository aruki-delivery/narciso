%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_api_accounts).

-include("eb_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_accounts/1, get_account/3, change_occupancy/3, get_account_slots/4]).

%
% Get all accounts
%
get_accounts(RequestingUserTypeId) when is_integer(RequestingUserTypeId) ->
	case eb_api_util:verify_operator_or_dispatcher_permissions(RequestingUserTypeId) of
		ok ->
			case eb_db_util:execute({get_accounts}) of
				Results when is_list(Results) -> {ok, Results};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
get_accounts(_InvalidRequestingUserTypeId) -> {nok, invalid_parameters}.

%
% Get account information
%
get_account(RequestingUserId, RequestingUserTypeId, TargetAccountId)
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(TargetAccountId) ->
	try
 		% Verify permissions
		eb_api_util:verify_account_permissions(RequestingUserId, RequestingUserTypeId, TargetAccountId) =:= ok orelse
			eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
  
		% Get account information
		case eb_db_util:execute({get_account_information, TargetAccountId}) of
			InfAccount when is_record(InfAccount, inf_account) -> {ok, InfAccount};
			not_found -> throw(not_found);
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
get_account(_RequestingUserId, _RequestingUserTypeId, _TargetAccountId) -> {nok, invalid_parameters}.

%
% Change occupancy
%
change_occupancy(RequestingUserTypeId, AccountId, #change_occupancy{occupancy=Occupancy, version=Version})
  when is_integer(RequestingUserTypeId) andalso is_integer(AccountId) ->
	try
		% Verify permissions
		case eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) of
			ok -> noop;
			nok -> throw(forbidden);
			_ -> throw(verify_dispatcher_permissions)
		end,
		
		% Verify account existence and version
		case eb_db_util:execute({get_account_information, AccountId}) of
			#inf_account{account=#account{version=Version}} -> ok;
			InfAccount when is_record(InfAccount, inf_account) -> throw(version);
			not_found -> throw(not_found);
			_ -> throw(error)
		end,

		% Check ocuppancy type
		case eb_db_util:execute({exists_occupancy_type, Occupancy}) of
			true -> noop;
			false -> throw(invalid_occupancy_type);
			_ -> throw(exists_occupancy_type)
		end,

		% Change occupancy
		case eb_db_util:execute({change_occupancy, AccountId, Occupancy, Version}) of
			{ok, NewVersion} -> {ok, NewVersion};
			_ -> throw(change_occupancy)
		end
	catch
		throw:Error -> {nok, Error}
	end;
change_occupancy(_RequestingUserTypeId, _AccountId, _ChangeOccupancy) -> {nok, invalid_parameters}.

%
% Get account slot information
%
get_account_slots(RequestingUserId, RequestingUserTypeId, AccountId, IdType) 			
  when is_integer(RequestingUserId) andalso is_integer(RequestingUserTypeId) andalso is_integer(AccountId)  andalso is_integer(IdType) ->
	try
		% Verify permissions
		eb_api_util:verify_account_permissions(RequestingUserId, RequestingUserTypeId, AccountId) =:= ok orelse
			eb_api_util:verify_dispatcher_permissions(RequestingUserTypeId) =:= ok orelse 
			eb_api_util:verify_client_permissions(RequestingUserTypeId) =:= ok orelse throw(forbidden),
		% Get the parameters from cache
		BufferTime = eb_cache_util:get_db_parameter(?DB_PARAM_BUFFER_TIME, ?MODULE, get_account_slots),
		ProductionTime = eb_cache_util:get_db_parameter(?DB_PARAM_PRODUCTION_TIME, ?MODULE, get_account_slots),
		EstimatedDeliveryTime = eb_cache_util:get_db_parameter(?DB_PARAM_DEFAULT_ESTIMATED_DELIVERY_TIME, ?MODULE, get_account_slots),
		if
			IdType =:= ?DB_ORDER_TYPE_DELIVERY ->
				MinutesToAdd = BufferTime + ProductionTime + EstimatedDeliveryTime;
			true ->
				MinutesToAdd = BufferTime + ProductionTime
		end,
		case eb_db_util:execute({get_account_slots_information, AccountId, RequestingUserTypeId, MinutesToAdd}) of
			Results when is_list(Results) -> {ok, Results};
			_ -> throw(error)
		end
	catch
		throw:Error -> {nok, Error}
	end;
get_account_slots(_RequestingUserId, _RequestingUserTypeId, _AccountId, _IdType) -> {nok, invalid_parameters}.
