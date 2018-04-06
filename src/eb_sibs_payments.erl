%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
-module(eb_sibs_payments).

-include("eb_sibs_payments.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_payment_details/6]).

get_payment_details(Endpoint, UserId, Password, EntityId, PaymentId, Amount) ->
	try
		RequestUrl = binary_to_list(Endpoint) ++ binary_to_list(PaymentId) ++ "?authentication.userId=" ++
						 binary_to_list(UserId) ++ "&authentication.password=" ++ binary_to_list(Password) ++
						 "&authentication.entityId=" ++ binary_to_list(EntityId),
		HTTPOptions = [{timeout, 15000}, {connect_timeout, 15000}],
		Options = [{sync, true}, {full_result, false}],
		% Execute
		FBody =
			case httpc:request(get, {RequestUrl, []}, HTTPOptions, Options) of
				{ok, {_Status, ResponseBody}} -> ResponseBody;
				{error, Reason} ->
					error_logger:error_msg("~p:get_payment_details(~p, ~p}: Unexpected error getting payment details: ~p~n", [?MODULE, RequestUrl, HTTPOptions, Options, Reason]),
					throw(Reason)
			end,
		FResp =
			case build_get_payment_details_response(FBody) of
				{ok, #get_payment_details_response{id=PaymentId, amount=BinAmount, result=#result{code=Code}}} ->
					{FAmount, _Rst} = string:to_float(binary_to_list(BinAmount)),
					case lists:member(Code, ?SUCCESS_CODES) andalso FAmount =:= Amount of
						true -> {ok, RequestUrl, FBody};
						false -> {nok, RequestUrl, FBody}
					end;
				{nok, Error} -> throw(Error)
			end,
		FResp
	catch
		throw:FError -> {nok, FError};
		_:_ -> {nok, error}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
build_get_payment_details_response(ResponseBody) ->
	JSON = jsondoc:decode(ResponseBody),
	parse_get_payment_details_response(JSON).

parse_get_payment_details_response({Data}) when is_list(Data) ->
	build_request_record(Data, fun(Elements) -> build_record(Elements, #get_payment_details_response{}) end).

parse_result_details({Data}) when is_list(Data) ->
	build_request_record(Data, fun(Elements) -> build_record(Elements, #result{}) end).

build_request_record(Elements, BuildFunction) when is_list(Elements) -> BuildFunction(Elements);
build_request_record(_Other, _BuildFunction) -> {nok, invalid_parameters}.

build_record([], NewRecord) -> {ok, NewRecord};
build_record([Parameter|Rest], NewRecord) ->
	ChangedRecord = set_record_parameter(Parameter, NewRecord),
	build_record(Rest, ChangedRecord).

set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, get_payment_details_response) ->
	case Label of
		<<"id">> -> NewRecord#get_payment_details_response{id=Value};
		<<"amount">> -> NewRecord#get_payment_details_response{amount=Value};
		<<"result">> ->
			case parse_result_details(Value) of
				{ok, NewResult} -> NewRecord#get_payment_details_response{result=NewResult};
				_ -> NewRecord
			end;
		_ -> NewRecord
	end;
set_record_parameter({Label, Value}, NewRecord) when is_record(NewRecord, result) ->
	case Label of
		<<"code">> -> NewRecord#result{code=Value};
		<<"description">> -> NewRecord#result{description=Value};
		_ -> NewRecord
	end.
