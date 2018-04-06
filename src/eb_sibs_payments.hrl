%%
%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-record(get_payment_details, {user_id, password, entity_id}).
-record(result, {code, description}).
-record(get_payment_details_response, {id, amount, result=#result{}}).
-define(SUCCESS_CODES, [<<"000.000.000">>, <<"000.100.110">>]).