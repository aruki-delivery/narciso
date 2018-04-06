%% Copyright 2015-16 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_db_worker).

-include("eb_constants.hrl").

-define(PROPERTY_DB_HOSTNAME, db_hostname).
-define(PROPERTY_DB_PORT,     db_port).
-define(PROPERTY_DB_DATABASE, db_database).
-define(PROPERTY_DB_USERNAME, db_username).
-define(PROPERTY_DB_PASSWORD, db_password).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {connection, statements, properties}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init(Properties) ->
	process_flag(trap_exit, true),
	case init_db(Properties) of
		{ok, Connection, Statements} -> {ok, #state{connection=Connection, statements=Statements, properties=Properties}};
		{error, Reason} -> {stop, Reason}
	end.


%
% User operations
%
handle_call({get_authenticated_user, Username, Password}, _From, State) when is_binary(Username) ->
	return_single_result(get_user_by_username_password, [Username, Password], fun(Row) -> build_user(Row) end, State);
handle_call({get_authenticated_user, UserId, Password}, _From, State) when is_integer(UserId) ->
	return_single_result(get_user_by_id_password, [UserId, Password], fun(Row) -> build_user(Row) end, State);
handle_call({get_user_external, UserId}, _From, State) ->
	return_single_result(get_user_oauth, [UserId], fun(Row) -> build_user(Row) end, State);

handle_call({count_users, UserTypeId, UserStatusId}, _From, State) ->
	% Decide which query to execute
	case UserTypeId of
		undefined ->
			% No user type filter
			case UserStatusId of
				undefined ->
					% No user status filter
					Query = count_users,
					Params = [];
				_ ->
					% User status filter
					Query = count_users_by_id_status,
					Params = [UserStatusId]
			end;
		_ ->
			% User type filter
			case UserStatusId of
				undefined ->
					% No user status filter
					Query = count_users_by_id_type,
					Params = [UserTypeId];
				_ ->
					% User status filter
					Query = count_users_by_id_type_and_id_status,
					Params = [UserTypeId, UserStatusId]
			end
	end,
	return_integer_value(Query, Params, State);

handle_call({get_users_information, UserTypeId, UserStatusId, #rs_navigation{order=Order, sort=Sort, skip=Skip, max=Max}}, _From, State) ->
	% Decide which query to execute
	case UserTypeId of
		undefined ->
			% No user type filter
			case UserStatusId of
				undefined ->
					% No user status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_USER_ID -> [get_users_id_asc, get_users_id_desc];
						?ORDER_USER_TYPE_ID -> [get_users_id_type_asc, get_users_id_type_desc];
						?ORDER_USER_STATUS_ID -> [get_users_id_status_asc, get_users_id_status_desc];
						_ -> [get_users, get_users]
					end,
					Params = [];
				_ ->
					% User status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_USER_ID -> [get_users_by_id_status_id_asc, get_users_by_id_status_id_desc];
						?ORDER_USER_TYPE_ID -> [get_users_by_id_status_id_type_asc, get_users_by_id_status_id_type_desc];
						_ -> [get_users_by_id_status, get_users_by_id_status]
					end,
					Params = [UserStatusId]
			end;
		_ ->
			% User type filter
			case UserStatusId of
				undefined ->
					% No user status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_USER_ID -> [get_users_by_id_type_id_asc, get_users_by_id_type_id_desc];
						?ORDER_USER_STATUS_ID -> [get_users_by_id_type_id_status_asc, get_users_by_id_type_id_status_desc];
						_ -> [get_users_by_id_type, get_users_by_id_type]
					end,
					Params = [UserTypeId];
				_ ->
					% User status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_USER_ID -> [get_users_by_id_type_and_id_status_id_asc, get_users_by_id_type_and_id_status_id_desc];
						_ -> [get_users_by_id_type_and_id_status, get_users_by_id_type_and_id_status]
					end,
					Params = [UserTypeId, UserStatusId]
			end
	end,
	% Get sort asc/desc query
	Query = get_sort_query(Sort, Queries),
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, Params, MaxResults, fun(Row) -> build_user(Row) end, State) of
		Users when is_list(Users) ->
			TrimmedResults = trim_list(Users, Skip, Max),
			Results = [build_user_information(TrimmedResult, State) || TrimmedResult <- TrimmedResults];
		Other -> Results = Other
	end,
	{reply, Results, State};

handle_call({get_user, UserId}, _From, State) when is_integer(UserId) ->
	return_single_result(get_user, [UserId], fun(Row) -> build_user(Row) end, State);
handle_call({get_user, Username}, _From, State) when is_binary(Username) ->
	return_single_result(get_user_by_username, [Username], fun(Row) -> build_user(Row) end, State);

handle_call({get_user_information, UserId}, _From, State) ->
	case get_single_result(get_user, [UserId], fun(Row) -> build_user(Row) end, State) of
		User when is_record(User, user) -> Result = build_user_information(User, State);
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({exists_user, Username}, _From, State) ->
	return_exists(count_user_auth, [Username], State);

handle_call({exists_user_external, OauthId, OauthProviderId}, _From, State) ->
	return_exists(count_user_oauth, [OauthId, OauthProviderId], State);

handle_call({create_user, ExistingAccountId,
			 #new_user{username=Username, password=Password, email=Email, user_type_id=UserTypeId,
					   first_name=RFirstName, last_name=RLastName, telephone_nr=RTelephoneNr,
					   fiscal_id=RFiscalID, reference=RReference, mobileos_id=RMobileOsId, birth_day=RBirthDay,
					   birth_month=RBirthMonth, birth_year=RBirthYear, national_id=RNationalId,
					   country=RCountry, account_user_type_id=AccountUserTypeId, new_account=NewAccount,
					   account_version=AccountVersion, department_id=DepartmentId, cost_center_id=CostCenterId}},
			_From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		FirstName = get_optional(RFirstName),
		LastName = get_optional(RLastName),
		TelephoneNr = get_optional(RTelephoneNr),
		FiscalID = get_optional(RFiscalID),
		Reference = get_optional(RReference),
		MobileOsId = get_optional(RMobileOsId),
		BirthDay = get_optional(RBirthDay),
		BirthMonth = get_optional(RBirthMonth),
		BirthYear = get_optional(RBirthYear),
		NationalId = get_optional(RNationalId),
		Country = get_optional(RCountry),

		% t_user
		{ok, _} = execute_statement(create_user, [?DB_USER_STATUS_ACTIVE, Email, UserTypeId,
												  FirstName, LastName, TelephoneNr, FiscalID, Reference,
												  MobileOsId, BirthDay, BirthMonth, BirthYear, NationalId,
												  Country, 0, OperationTimestamp], State),
		{ok, NewUserId} = get_integer_value(get_last_id, [], State),

		% t_user_auth
		{ok, _} = execute_statement(create_user_auth, [NewUserId, Username, Password], State),
		case UserTypeId of
			UserTypeId when ?IS_BUSINESS_CLIENT(UserTypeId) ->
				case ExistingAccountId of
					ExistingAccountId when is_integer(ExistingAccountId) ->
						% t_account
						{ok, 1} = execute_statement(increment_account_version, [ExistingAccountId, AccountVersion], State),
						AccountId = ExistingAccountId,
						NewAccountVersion = AccountVersion + 1;
					_Undefined ->
						#new_account{unique_key=AUniqueKey, name=AName, fiscal_id=AFiscalId, email=AEmail, telephone_nr=ATelephoneNr, contract_nr=AContractNr} = NewAccount,
						% t_account
						{ok, _} = execute_statement(create_account, [AUniqueKey, ?DB_ACCOUNT_STATUS_PENDING, AName, AFiscalId, ATelephoneNr, AEmail, AContractNr, OperationTimestamp], State),
						{ok, NewAccountId} = get_integer_value(get_last_id, [], State),
						% t_account_info
						CreateAccountInfo = fun F(_AccountId, [], _FState) -> ok;
						                        F(AccountId, [#additional_info{property=Property, value=Value}|Rest], FState) ->
						                             {ok, _} = execute_statement(create_account_info, [AccountId, Property, Value], FState),
						                             F(AccountId, Rest, FState)
						end,
						ok = CreateAccountInfo(NewAccountId, NewAccount#new_account.additional_infos, State),
						% t_account_address
						CreateAccountAddress = fun F(_AccountId, [], _FState) -> ok;
						                           F(AccountId, [#component_info{component=Component, value=Value}|Rest], FState) ->
						                             {ok, _} = execute_statement(create_account_address, [AccountId, Component, Value], FState),
						                             F(AccountId, Rest, FState)
						end,
						ok = CreateAccountAddress(NewAccountId, NewAccount#new_account.address, State),
						AccountId = NewAccountId,
						NewAccountVersion = undefined
				end,
				% t_account_user
				{ok, _} = execute_statement(create_account_user, [AccountId, NewUserId, AccountUserTypeId, DepartmentId, CostCenterId], State);
			UserTypeId when UserTypeId =:= ?DB_USER_TYPE_COURIER ->
				case ExistingAccountId of
					ExistingAccountId when is_integer(ExistingAccountId) ->
						% t_account
						{ok, 1} = execute_statement(increment_account_version, [ExistingAccountId, AccountVersion], State),
						NewAccountVersion = AccountVersion + 1,
						% t_account_user
						{ok, _} = execute_statement(create_account_user, [ExistingAccountId, NewUserId, AccountUserTypeId, DepartmentId, CostCenterId], State);
					_Undefined -> NewAccountVersion = undefined % do_nothing
				end;
			_ -> NewAccountVersion = undefined % do_nothing
		end,
		{ok, NewUserId, NewAccountVersion}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({create_user_external, #new_user_external{id_oauth=OauthId, id_oauth_provider=OauthProviderId,
																		user_type_id=UserTypeId, email=Email, first_name=RFirstName,
																		last_name=RLastName, telephone_nr=RTelephoneNr, fiscal_id=RFiscalID,
																		address=Address}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		FirstName = get_optional(RFirstName),
		LastName = get_optional(RLastName),
		TelephoneNr = get_optional(RTelephoneNr),
		FiscalID = get_optional(RFiscalID),

		% t_user
		{ok, _} = execute_statement(create_user, [?DB_USER_STATUS_PENDING_CONFIRMATION, Email, UserTypeId, FirstName, LastName,
		                                          TelephoneNr, FiscalID, 0, OperationTimestamp], State),
		{ok, NewUserId} = get_integer_value(get_last_id, [], State),

		% t_user_address
		case Address of
			undefined -> noop;
			[] -> noop;
			_ -> ok = create_user_address_components_rule(NewUserId, Address, State)
		end,

		% t_user_oauth
		{ok, _} = execute_statement(create_user_oauth, [NewUserId, OauthId, OauthProviderId], State),

		TokenParameters = [
			#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_ID, value=erlang:integer_to_binary(NewUserId)}
		],
		{ok, TokenId} = create_token_rule(NewUserId, ?DB_TOKEN_TYPE_CREATE_USER, TokenParameters, OperationTimestamp, State),
		{ok, TokenId}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({confirm_user_email, UserId}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		case get_single_result(get_user, [UserId], fun(Row) -> build_user(Row) end, State) of
			#user{id_type=_UserTypeId, version=Version} ->
				UserStatusId = ?DB_USER_STATUS_ACTIVE,
				OperationTimestamp = eb_util:get_current(),
				{ok, _Updated} = execute_statement(update_user_status, [UserId, Version, UserStatusId, OperationTimestamp], State),
				ok;
			Error -> Error
		end
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({set_password, UserId, Version, NewPassword}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_user_auth
		{ok, 1} = execute_statement(set_password, [UserId, NewPassword], State),
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		ok
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({set_password_and_activate_user, UserId, Version, NewPassword}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_user_auth
		{ok, 1} = execute_statement(set_password, [UserId, NewPassword], State),
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),

		case get_single_result(get_user, [UserId], fun(Row) -> build_user(Row) end, State) of
			#user{id_type=_UserTypeId, version=UserVersion} ->
				UserStatusId = ?DB_USER_STATUS_ACTIVE,
				OperationTimestamp = eb_util:get_current(),
				{ok, 1} = execute_statement(update_user_status, [UserId, UserVersion, UserStatusId, OperationTimestamp], State),
				ok;
			Error -> Error
		end
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({change_user,
             #user{id=UserId, username=OldUsername, id_status=OldStatusId, status_date=OldStatusDate, email=OldEmail, first_name=OldFirstName, last_name=OldLastName, telephone_nr=OldTelephoneNr, fiscal_id=OldFiscalId, mobileos_id=OldMobileId, birth_day=OldBirthDay, birth_month=OldBirthMonth, birth_year=OldBirthYear, national_id=OldNationalId},
             #change_user{username=Username, user_status_id=StatusId, email=Email, first_name=FirstName, last_name=LastName, telephone_nr=TelephoneNr, fiscal_id=FiscalId, address=Address, mobileos_id=MobileId, birth_day=BirthDay, birth_month=BirthMonth, birth_year=BirthYear, national_id=NationalId, zone_ids=ZoneIds, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		NewUsername = eb_api_util:get_update_value(Username, OldUsername),
		NewFirstName = eb_api_util:get_update_value(FirstName, OldFirstName),
		NewLastName = eb_api_util:get_update_value(LastName, OldLastName),
		NewTelephoneNr = eb_api_util:get_update_value(TelephoneNr, OldTelephoneNr),
		NewFiscalId = eb_api_util:get_update_value(FiscalId, OldFiscalId),
		NewMobileId = eb_api_util:get_update_value(MobileId, OldMobileId),
		NewBirthDay = eb_api_util:get_update_value(BirthDay, OldBirthDay),
		NewBirthMonth = eb_api_util:get_update_value(BirthMonth, OldBirthMonth),
		NewBirthYear = eb_api_util:get_update_value(BirthYear, OldBirthYear),
		NewNationalId = eb_api_util:get_update_value(NationalId, OldNationalId),
		% Email setting - if email changes, user status changes to pending 
		case Email of
			Email when Email =:= undefined orelse Email =:= OldEmail ->
				NewEmail = OldEmail,
				TmpStatusId = undefined,
				TokenId = none;
			_ ->
				NewEmail = Email,
				TmpStatusId = ?DB_USER_STATUS_PENDING_CONFIRMATION,
				% Token creation
				TokenParameters = [#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_ID, value=erlang:integer_to_binary(UserId)}],
				{ok, TokenId} = create_token_rule(UserId, ?DB_TOKEN_TYPE_CREATE_USER, TokenParameters, OperationTimestamp, State)
		end,
		% Status setting
		case TmpStatusId of
			undefined ->
				case StatusId of
					undefined ->
						NewStatusId = OldStatusId,
						NewStatusDate = OldStatusDate;
					_ ->
						NewStatusId = StatusId,
						NewStatusDate = OperationTimestamp
				end;
			_ ->
				NewStatusId = TmpStatusId,
				NewStatusDate = OperationTimestamp
		end,
		% t_user
		{ok, 1} = execute_statement(update_user, [UserId, Version, NewEmail, NewStatusId, NewStatusDate, NewFirstName, NewLastName, NewTelephoneNr, NewFiscalId, NewMobileId, NewBirthDay, NewBirthMonth, NewBirthYear, NewNationalId], State),
		NewVersion = Version + 1,
		% t_user_auth
		{ok, 1} = execute_statement(set_username, [UserId, NewUsername], State),
		% t_user_address
		case Address of
			undefined -> noop;
			[] -> noop;
			_ ->
				{ok, _} = execute_statement(delete_user_address, [UserId], State),
				ok = create_user_address_components_rule(UserId, Address, State)
		end,
		% t_user_zone
		case ZoneIds of
			undefined -> noop;
			[] -> noop;
			_ ->
				{ok, _} = execute_statement(delete_user_zones, [UserId], State),
				ok = create_user_zone_rule(UserId, ZoneIds, State)
		end,
		case TokenId of
			none -> {ok, NewVersion};
			_ -> {ok, TokenId, NewVersion}
		end
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({change_account_user,
             #user{username=OldUsername, id_status=OldUserStatusId, status_date=OldStatusDate, email=OldEmail, first_name=OldFirstName, last_name=OldLastName, telephone_nr=OldTelephoneNr, fiscal_id=OldFiscalId, mobileos_id=OldMobileId, birth_day=OldBirthDay, birth_month=OldBirthMonth, birth_year=OldBirthYear, national_id=OldNationalId},
             #account_user{id_account=AccountId, id_user=UserId, id_type=OldAccountUserTypeId, id_department=OldDepartmentId, id_cost_center=OldCostCenterId},
             #change_account_user{username=Username, email=Email, first_name=FirstName, last_name=LastName, telephone_nr=TelephoneNr, account_user_type_id=AccountUserTypeId, department_id=DepartmentId, cost_center_id=CostCenterId, account_version=AccountVersion, user_version=UserVersion}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		NewUsername = eb_api_util:get_update_value(Username, OldUsername),
		NewFirstName = eb_api_util:get_update_value(FirstName, OldFirstName),
		NewLastName = eb_api_util:get_update_value(LastName, OldLastName),
		NewTelephoneNr = eb_api_util:get_update_value(TelephoneNr, OldTelephoneNr),
		NewAccountUserTypeId = eb_api_util:get_update_value(AccountUserTypeId, OldAccountUserTypeId),
		NewDepartmentId = eb_api_util:get_update_value(DepartmentId, OldDepartmentId),
		NewCostCenterId = eb_api_util:get_update_value(CostCenterId, OldCostCenterId),
		% Email setting - if email changes, user status changes to pending
		{NewEmail, NewUserStatusId, NewStatusDate, TokenId} =
			case Email of
				Email when Email =:= undefined orelse Email =:= OldEmail ->
					{OldEmail, OldUserStatusId, OldStatusDate, none};
				_ ->
					% Token creation
					TokenParameters = [#token_parameter{id_type=?DB_TOKEN_PARAMETER_TYPE_USER_ID, value=erlang:integer_to_binary(UserId)}],
					{ok, CreatedTokenId} = create_token_rule(UserId, ?DB_TOKEN_TYPE_CREATE_USER, TokenParameters, OperationTimestamp, State),
					{Email, ?DB_USER_STATUS_PENDING_CONFIRMATION, OperationTimestamp, CreatedTokenId}
			end,
		% t_user
		{ok, 1} = execute_statement(update_user, [UserId, UserVersion, NewEmail, NewUserStatusId, NewStatusDate, NewFirstName, NewLastName, NewTelephoneNr, OldFiscalId, OldMobileId, OldBirthDay, OldBirthMonth, OldBirthYear, OldNationalId], State),
		% t_user_auth
		{ok, 1} = execute_statement(set_username, [UserId, NewUsername], State),
		% t_account_user
		{ok, 1} = execute_statement(update_account_user, [AccountId, UserId, NewAccountUserTypeId, NewDepartmentId, NewCostCenterId], State),
		% t_account
		{ok, 1} = execute_statement(increment_account_version, [AccountId, AccountVersion], State),
		case TokenId of
			none -> {ok, AccountVersion + 1, UserVersion + 1};
			_ -> {ok, TokenId, AccountVersion + 1, UserVersion + 1}
		end
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({exists_account_user, UserId, AccountId}, _From, State) ->
	return_exists_result(exists_account_user, [UserId, AccountId], State);

handle_call({get_user_photo, UserId}, _From, State) ->
	case get_multiple_results(get_user_documents_by_id_user_and_id_type_and_id_status, [UserId, ?DB_DOCUMENT_TYPE_PHOTO, ?DB_DOCUMENT_STATUS_VALID],
	                          0, fun(Row) -> build_document(Row) end, State) of
		[] -> Result = not_found;
		[Document|_Rest] -> Result = Document;
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({get_account_photo, AccountId}, _From, State) ->
	case get_multiple_results(get_account_documents_by_id_account_and_id_type_and_id_status, [AccountId, ?DB_DOCUMENT_TYPE_PHOTO, ?DB_DOCUMENT_STATUS_VALID],
	                          0, fun(Row) -> build_document(Row) end, State) of
		[] -> Result = not_found;
		[Document|_Rest] -> Result = Document;
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({get_user_documents_summary, UserId, DocumentTypeId, DocumentStatusId, #rs_navigation{order=Order, sort=Sort, skip=Skip, max=Max}},
            _From, State) ->
	% Decide which query to execute
	case DocumentTypeId of
		undefined ->
			% No document type filter
			case DocumentStatusId of
				undefined ->
					% No document status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_DOCUMENT_TYPE_ID -> [get_user_documents_summary_by_id_user_id_type_asc, get_user_documents_summary_by_id_user_id_type_desc];
						?ORDER_DOCUMENT_STATUS_ID -> [get_user_documents_summary_by_id_user_id_status_asc, get_user_documents_summary_by_id_user_id_status_desc];
						_ -> [get_user_documents_summary_by_id_user, get_user_documents_summary_by_id_user]
					end,
					Params = [UserId];
				_ ->
					% Document status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_DOCUMENT_TYPE_ID -> [get_user_documents_summary_by_id_user_and_id_status_id_type_asc, get_user_documents_summary_by_id_user_and_id_status_id_type_desc];
						_ -> [get_user_documents_summary_by_id_user_and_id_status, get_user_documents_summary_by_id_user_and_id_status]
					end,
					Params = [UserId, DocumentStatusId]
			end;
		_ ->
			% Document type filter
			case DocumentStatusId of
				undefined ->
					% No document status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_DOCUMENT_STATUS_ID -> [get_user_documents_summary_by_id_user_and_id_type_id_status_asc, get_user_documents_summary_by_id_user_and_id_type_id_status_desc];
						_ -> [get_user_documents_summary_by_id_user_and_id_type, get_user_documents_summary_by_id_user_and_id_type]
					end,
					Params = [UserId, DocumentTypeId];
				_ ->
					% Document status filter
					Queries = [get_user_documents_summary_by_id_user_and_id_type_and_id_status, get_user_documents_summary_by_id_user_and_id_type_and_id_status],
					Params = [UserId, DocumentTypeId, DocumentStatusId]
			end
	end,
	% Get sort asc/desc query
	Query = get_sort_query(Sort, Queries),
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, Params, MaxResults, fun(Row) -> build_document_summary(Row) end, State) of
		Users when is_list(Users) -> Results = trim_list(Users, Skip, Max);
		Other -> Results = Other
	end,
	{reply, Results, State};

handle_call({get_user_document, UserId, DocumentId}, _From, State) when is_integer(UserId) ->
	return_single_result(get_user_document, [UserId, DocumentId], fun(Row) -> build_document(Row) end, State);

handle_call({get_user_document_summary, UserId, DocumentId}, _From, State) when is_integer(UserId) ->
	return_single_result(get_user_document_summary, [UserId, DocumentId], fun(Row) -> build_document_summary(Row) end, State);

handle_call({create_user_document, UserId, #new_document{document_type_id=DocumentTypeId,
                                                         document=#file{name=Name, mimetype=Mimetype, base64_data=Base64Data}}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		DocumentStatusId =
			case DocumentTypeId of
				?DB_DOCUMENT_TYPE_PHOTO -> ?DB_DOCUMENT_STATUS_VALID;
				_ -> ?DB_DOCUMENT_STATUS_SUBMITED
			end,
		% t_document
		{ok, _} = execute_statement(create_document, [UserId, undefined, DocumentTypeId, Name, Mimetype, Base64Data, DocumentStatusId,
		                                              OperationTimestamp], State),
		{ok, NewDocumentId} = get_integer_value(get_last_id, [], State),
		{ok, NewDocumentId}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({change_user_document, UserId, DocumentId, #change_document{document_status_id=DocumentStatusId, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_document
		{ok, _} = execute_statement(update_user_document_status, [DocumentId, UserId, Version, DocumentStatusId, OperationTimestamp], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({update_user_photo, UserId, #update_file{file=#file{name=Name, mimetype=Mimetype, base64_data=Base64Data},
                                                     version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_document
		{ok, _} = execute_statement(delete_document_by_id_user_and_id_type, [UserId, ?DB_DOCUMENT_TYPE_PHOTO], State),
		{ok, 1} = execute_statement(create_document, [UserId, undefined, ?DB_DOCUMENT_TYPE_PHOTO, Name, Mimetype, Base64Data,
		                                              ?DB_DOCUMENT_STATUS_VALID, OperationTimestamp], State),
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		{ok, Version + 1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_user_notifications, UserId}, _From, State) ->
	case get_multiple_results(get_user_notifications, [UserId], 0, fun(Row) -> build_user_notification(Row) end, State) of
		Notifications when is_list(Notifications) -> Results = build_user_notifications_information(Notifications);
		Other -> Results = Other
	end,
	{reply, Results, State};

handle_call({set_user_notifications, UserId, #update_user_notifications{enabled=Enabled, notification_type_ids=NotificationTypeIds, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_user_notification
		{ok, _} = execute_statement(delete_user_notifications, [UserId], State),
		ok = create_user_notification_rule(UserId, NotificationTypeIds, Enabled, State),
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		{ok, Version + 1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_commercial_info, UserId}, _From, State) ->
	Result =
		case get_single_result(get_commercial_info, [UserId], fun(Row) -> build_commercial_info(Row) end, State) of
			CommercialInfo when is_record(CommercialInfo, commercial_info) -> CommercialInfo;
			Other -> Other
		end,
	{reply, Result, State};

handle_call({change_commercial_info, UserId, #change_commercial_info{about=About, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_commercial_info
		{ok, 1} = execute_statement(update_commercial_info, [UserId, About], State),
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		{ok, Version + 1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({create_commercial_info, UserId, #change_commercial_info{about=About, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_commercial_info
		{ok, 1} = execute_statement(create_commercial_info, [UserId, About], State),
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		{ok, Version + 1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({set_login_date, UserId},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_user
		{ok, 1} = execute_statement(set_user_login_date, [UserId, OperationTimestamp], State),
		ok
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

%
% User couriers operations
%
handle_call({get_user_couriers, UserId, #rs_navigation{order=Order, sort=Sort, skip=Skip, max=Max}}, _From, State) ->
	% Determine order queries
	Queries = case Order of
		?ORDER_NAME -> [get_user_couriers_name_asc, get_user_couriers_name_desc];
		_ -> [get_user_couriers, get_user_couriers]
	end,
	% Get sort asc/desc query
	Query = get_sort_query(Sort, Queries),
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, [UserId], MaxResults, fun(Row) -> build_u_courier_info(Row) end, State) of
		Couriers when is_list(Couriers) -> Results = trim_list(Couriers, Skip, Max);
		Other -> Results = Other
	end,
	{reply, Results, State};

handle_call({get_user_courier, UserId, CourierId}, _From, State) when is_integer(UserId) ->
	return_single_result(get_user_courier, [UserId, CourierId], fun(Row) -> build_user_courier(Row) end, State);

handle_call({create_user_courier, UserId, CourierId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		% t_user_courier
		{ok, _} = execute_statement(create_user_courier, [UserId, CourierId], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({delete_user_courier, UserId, CourierId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		% t_user_courier
		{ok, _} = execute_statement(delete_user_courier, [UserId, CourierId], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_user_used_couriers, UserId, DateFrom, DateTo, #rs_navigation{order=Order, sort=Sort, skip=Skip, max=Max}}, _From, State) ->
	% Decide which query to execute
	case DateFrom of
		undefined ->
			% No dates
			% Determine order queries
			Queries = case Order of
				?ORDER_NAME -> [get_user_used_couriers_name_asc, get_user_used_couriers_name_desc];
				_ -> [get_user_used_couriers, get_user_used_couriers]
			end,
			Params = [UserId, ?DB_DELIVERY_STATUS_COMPLETED_SUCCESS];
		_ ->
			% Use dates
			% Determine order queries
			Queries = case Order of
				?ORDER_NAME -> [get_user_used_couriers_by_date_interval_name_asc, get_user_used_couriers_by_date_interval_name_desc];
				_ -> [get_user_used_couriers_by_date_interval, get_user_used_couriers_by_date_interval]
			end,
			Params = [UserId, ?DB_DELIVERY_STATUS_COMPLETED_SUCCESS, DateFrom, DateTo]
	end,
	% Get sort asc/desc query
	Query = get_sort_query(Sort, Queries),
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, Params, MaxResults, fun(Row) -> build_u_courier_info(Row) end, State) of
		Couriers when is_list(Couriers) -> Results = trim_list(Couriers, Skip, Max);
		Other -> Results = Other
	end,
	{reply, Results, State};

%
% User location operations
%
handle_call({get_user_locations_information, UserId, Name, PhoneNr, #rs_navigation{skip=Skip, max=Max}}, _From, State) ->
% Decide which query to execute
	case Name of
		undefined ->
			% No name filter
			case PhoneNr of
				undefined ->
					% No phone_nr filter
					% Determine location contact queries
					Query = get_user_locations,
					Params = [UserId];
				_ ->
					% Phone_nr filter
					% Determine order queries
					Query = get_user_locations_by_phone_nr,
					Params = [UserId, PhoneNr]
			end;
		_ ->
			% Phone_nr filter
			case PhoneNr of
				undefined ->
					% No phone_nr filter
					% Determine order queries
					Query = get_user_locations_by_name,
					Params = [UserId, Name];
				_ ->
					% Phone_nr filter
					% Determine order queries
					Query = get_user_locations_by_name_and_phone_nr,
					Params = [UserId, Name, PhoneNr]
			end
	end,
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, Params, MaxResults, fun(Row) -> build_user_location(Row) end, State) of
		Locations when is_list(Locations) ->
			TrimmedLocations = trim_list(Locations, Skip, Max),
			Results = [build_user_location_information(Location, State) || Location <- TrimmedLocations];
		Other -> Results = Other
	end,
	{reply, Results, State};

handle_call({get_user_location, UserId, LocationId}, _From, State) ->
	return_single_result(get_user_location, [UserId, LocationId], fun(Row) -> build_user_location(Row) end, State);

handle_call({get_user_location_information, UserId, LocationId}, _From, State) ->
	return_single_result(get_user_location, [UserId, LocationId], fun(Row) -> build_user_location_information(Row, State) end, State);

handle_call({create_user_location, UserId, #new_location{description=Description, components=Components, position=#position{latitude=Latitude, longitude=Longitude}, contacts=Contacts, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		% t_user_location
		{ok, _} = execute_statement(create_user_location, [UserId, Description, Latitude, Longitude], State),
		{ok, NewLocationId} = get_integer_value(get_last_id, [], State),
		% t_user_location_component
		case Components of
			undefined -> do_nothing;
			_ -> ok = create_user_location_components_rule(NewLocationId, Components, State)
		end,
		% t_user_location_contact
		case Contacts of
			undefined -> do_nothing;
			_ -> ok = create_user_location_contacts_rule(NewLocationId, Contacts, State)
		end,
		{ok, NewLocationId, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({delete_user_location, UserId, LocationId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		% t_user_location_component
		{ok, _} = execute_statement(delete_user_location_components, [LocationId], State),
		% t_user_location_contact
		{ok, _} = execute_statement(delete_user_location_contacts, [LocationId], State),
		% t_user_location
		{ok, _} = execute_statement(delete_user_location, [UserId, LocationId], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({change_user_location,
             #user_location{id_user=UserId, id=LocationId, description=OldDescription, latitude=OldLatitude,
                            longitude=OldLongitude},
             #change_location{description=Description, components=Components, position=Position, contacts=Contacts, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		NewDescription = eb_api_util:get_update_value(Description, OldDescription),
		case Position of
			undefined ->
				NewLatitude = OldLatitude,
				NewLongitude = OldLongitude;
			#position{latitude=Latitude, longitude=Longitude} ->
				NewLatitude = Latitude,
				NewLongitude = Longitude
		end,
		% t_user_location
		{ok, 1} = execute_statement(update_user_location, [UserId, LocationId, NewDescription, NewLatitude, NewLongitude], State),
		% t_user_location_component
		case Components of
			undefined -> do_nothing;
			_ ->
				{ok, _} = execute_statement(delete_user_location_components, [LocationId], State),
				ok = create_user_location_components_rule(LocationId, Components, State)
		end,
		% t_user_location_contact
		case Contacts of
			undefined -> do_nothing;
			_ ->
				{ok, _} = execute_statement(delete_user_location_contacts, [LocationId], State),
				ok = create_user_location_contacts_rule(LocationId, Contacts, State)
		end,
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_online_courier_information, SessionInfos}, _From, State) ->
	Result = [build_online_courier_info(SessionInfo, State) || SessionInfo <- SessionInfos],
	{reply, Result, State};

handle_call({get_online_courier_accounts, SessionInfos}, _From, State) ->
	UserIds = [UserId || #session_info{id_user=UserId} <- SessionInfos],
	Results =
		case get_multiple_results(get_user_accounts, [UserIds], 0, fun(Row) -> build_account(Row) end, State) of
			Accounts when is_list(Accounts) -> [build_online_courier_account_info(Account) || Account <- Accounts];
			Other -> Other
		end,
	{reply, Results, State};

%
% Account operations
%
handle_call({get_accounts}, _From, State) ->
	case get_multiple_results(get_accounts, [], 0, fun(Row) -> build_account(Row) end, State) of
		Accounts when is_list(Accounts) -> Results = [build_account_information(Account, State) || Account <- Accounts];
		Other -> Results = Other
	end,
	{reply, Results, State};

handle_call({get_account_information, AccountId}, _From, State) ->
	case get_single_result(get_account, [AccountId], fun(Row) -> build_account(Row) end, State) of
		Account when is_record(Account, account) -> Result = build_account_information(Account, State);
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({exists_account, UniqueKey}, _From, State) when is_binary(UniqueKey) ->
	return_exists(count_accounts_by_unique_key, [UniqueKey], State);

handle_call({exists_account_by_id, IdAccount}, _From, State) ->
	return_exists(count_account_by_id, [IdAccount], State);

handle_call({get_account_users, AccountId, UserTypeId}, _From, State) ->
	case get_single_result(get_account, [AccountId], fun(Row) -> build_account(Row) end, State) of
		#account{version=Version} ->
			{Query, Params} =
				case UserTypeId of
					undefined -> {get_account_users, [AccountId]};
					_ -> {get_account_users_by_user_id_type, [AccountId, UserTypeId]}
				end,
			case get_multiple_results(Query, Params, 0, fun(Row) -> build_u_account_user(Row, State) end, State) of
				Users when is_list(Users) -> Result = #inf_account_users{version=Version, users=Users};
				Other -> Result = Other
			end;
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({add_account_user, AccountId, UserId, AccountUserTypeId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_account
		{ok, _} = execute_statement(increment_account_version, [AccountId, Version], State),
		% t_account_user
		{ok, _} = execute_statement(create_account_user, [AccountId, UserId, AccountUserTypeId, undefined, undefined], State),
		ok
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({remove_account_user, AccountId, UserId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_account
		{ok, _} = execute_statement(increment_account_version, [AccountId, Version], State),
		% t_account_user
		{ok, _} = execute_statement(delete_account_user, [AccountId, UserId], State),
		ok
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_account_user, UserId, AccountId}, _From, State) ->
	case get_single_result(get_account_user, [UserId, AccountId], fun(Row) -> build_account_user(Row) end, State) of
		AccountUser when is_record(AccountUser, account_user) -> Result = AccountUser;
		Other -> Result = Other
	end,
	{reply, Result, State};
	
handle_call({get_account_user_by_id_user, UserId}, _From, State) ->
	case get_single_result(get_account_user_by_id_user, [UserId], fun(Row) -> build_account_user(Row) end, State) of
		AccountUser when is_record(AccountUser, account_user) -> Result = AccountUser;
		Other -> Result = Other
	end,
	{reply, Result, State};
	
handle_call({change_account,
             #account{id=AccountId, account_name=OldAccountName, fiscal_id=OldFiscalId, telephone_nr=OldTelephoneNr, email=OldEmail},
             #change_account{account_name=NewAccountName, fiscal_id=NewFiscalId, telephone_nr=NewTelephoneNr, email=NewEmail, address=NewAddress, additional_infos=NewAdditionalInfos, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		AccountName = eb_api_util:get_update_value(NewAccountName, OldAccountName),
		FiscalId = eb_api_util:get_update_value(NewFiscalId, OldFiscalId),
		TelephoneNr = eb_api_util:get_update_value(NewTelephoneNr, OldTelephoneNr),
		Email = eb_api_util:get_update_value(NewEmail, OldEmail),
		% t_account
		{ok, 1} = execute_statement(update_account, [AccountId, Version, AccountName, FiscalId, TelephoneNr, Email], State),
		% t_account_info
		case NewAdditionalInfos of
			undefined -> noop;
			[] -> noop;
			_ ->
				{ok, _} = execute_statement(delete_account_info, [AccountId], State),
				ok = create_account_additional_info(AccountId, NewAdditionalInfos, State)
		end,
		% t_account_address
		case NewAddress of
			undefined -> noop;
			[] -> noop;
			_ ->
				{ok, _} = execute_statement(delete_account_address, [AccountId], State),
				ok = create_account_address_components_rule(AccountId, NewAddress, State)
		end,
		{ok, Version + 1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({exists_occupancy_type, Id}, _From, State) ->
	return_exists(count_ocupancy_type, [Id], State);

handle_call({change_occupancy, AccountId, Occupancy, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_account
		{ok, 1} = execute_statement(update_account_occupancy, [AccountId, Version, Occupancy, OperationTimestamp], State),
		{ok, Version + 1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_account_slots_information, AccountId, RequestingUserTypeId, MinutesToAdd}, _From, State) ->
	case get_multiple_results(get_account_slots_information, [AccountId, MinutesToAdd], 0, fun(Row) -> build_account_slots_information(Row, RequestingUserTypeId) end, State) of
		AccountSlots when is_list(AccountSlots) -> Result = AccountSlots;
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({get_account_slot_information, AccountId, SlotId, RequestingUserTypeId}, _From, State) ->
	case get_single_result(get_account_slot_information, [AccountId, SlotId], fun(Row) -> build_account_slots_information(Row, RequestingUserTypeId) end, State) of
		AccountSlot when is_record(AccountSlot, inf_account_slots) -> Result = AccountSlot;
		Other -> Result = Other
	end,
	{reply, Result, State};

%
% Object types operations
%
handle_call({get_object_types}, _From, State) ->
	return_multiple_results(get_object_types, [], 0, fun(Row) -> build_object_type(Row) end, State);

handle_call({get_object_type, Id}, _From, State) ->
	return_single_result(get_object_type, [Id], fun(Row) -> build_object_type(Row) end, State);

handle_call({exists_object_type, Id}, _From, State) ->
	return_exists(count_object_types, [Id], State);

handle_call({create_account_document, UserId, AccountId,
             #new_document{document_type_id=DocumentTypeId,
                           document=#file{name=Name, mimetype=Mimetype, base64_data=Base64Data}}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_document
		{ok, _} = execute_statement(create_document, [UserId, AccountId, DocumentTypeId, Name, Mimetype, Base64Data,
		                                                      ?DB_DOCUMENT_STATUS_SUBMITED, OperationTimestamp], State),
		{ok, NewDocumentId} = get_integer_value(get_last_id, [], State),
		{ok, NewDocumentId}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_account_document, AccountId, DocumentId}, _From, State) when is_integer(AccountId) ->
	return_single_result(get_account_document, [AccountId, DocumentId], fun(Row) -> build_document(Row) end, State);

handle_call({get_account_document_summary, AccountId, DocumentId}, _From, State) when is_integer(AccountId) ->
	return_single_result(get_account_document_summary, [AccountId, DocumentId], fun(Row) -> build_document_summary(Row) end, State);

handle_call({change_account_document, AccountId, DocumentId, #change_document{document_status_id=DocumentStatusId, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_document
		{ok, _} = execute_statement(update_account_document_status, [DocumentId, AccountId, Version, DocumentStatusId, OperationTimestamp], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_account_documents_summary, AccountId, DocumentTypeId, DocumentStatusId, #rs_navigation{order=Order, sort=Sort, skip=Skip, max=Max}},
            _From, State) ->
	% Decide which query to execute
	case DocumentTypeId of
		undefined ->
			% No document type filter
			case DocumentStatusId of
				undefined ->
					% No document status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_DOCUMENT_TYPE_ID -> [get_account_documents_summary_by_id_account_id_type_asc, get_account_documents_summary_by_id_account_id_type_desc];
						?ORDER_DOCUMENT_STATUS_ID -> [get_account_documents_summary_by_id_account_id_status_asc, get_user_documents_summary_by_id_account_id_status_desc];
						_ -> [get_account_documents_summary_by_id_account, get_account_documents_summary_by_id_account]
					end,
					Params = [AccountId];
				_ ->
					% Document status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_DOCUMENT_TYPE_ID -> [get_account_documents_summary_by_id_account_and_id_status_id_type_asc, get_account_documents_summary_by_id_account_and_id_status_id_type_desc];
						_ -> [get_account_documents_summary_by_id_account_and_id_status, get_account_documents_summary_by_id_account_and_id_status]
					end,
					Params = [AccountId, DocumentStatusId]
			end;
		_ ->
			% Document type filter
			case DocumentStatusId of
				undefined ->
					% No document status filter
					% Determine order queries
					Queries = case Order of
						?ORDER_DOCUMENT_STATUS_ID -> [get_account_documents_summary_by_id_account_and_id_type_id_status_asc, get_account_documents_summary_by_id_account_and_id_type_id_status_desc];
						_ -> [get_account_documents_summary_by_id_account_and_id_type, get_account_documents_summary_by_id_account_and_id_type]
					end,
					Params = [AccountId, DocumentTypeId];
				_ ->
					% Document status filter
					Queries = [get_account_documents_summary_by_id_account_and_id_type_and_id_status, get_account_documents_summary_by_id_account_and_id_type_and_id_status],
					Params = [AccountId, DocumentTypeId, DocumentStatusId]
			end
	end,
	% Get sort asc/desc query
	Query = get_sort_query(Sort, Queries),
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, Params, MaxResults, fun(Row) -> build_document_summary(Row) end, State) of
		Documents when is_list(Documents) -> Results = trim_list(Documents, Skip, Max);
		Other -> Results = Other
	end,
	{reply, Results, State};

handle_call({delete_account_document, AccountId, DocumentId}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		% t_document
		{ok, _} = execute_statement(delete_account_document, [AccountId, DocumentId], State),
		ok
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};


%
% Parameterization operations
%
handle_call({get_parameterizations}, _From, State) ->
	return_multiple_results(get_parameterizations, [], 0, fun(Row) -> build_parameterization(Row) end, State);

handle_call({get_parameterization, Id}, _From, State) ->
	return_single_result(get_parameterization, [Id], fun(Row) -> build_parameterization(Row) end, State);

handle_call({update_parameterization, Id, Version, Value}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		case get_single_result(get_parameterization, [Id], fun(Row) -> build_parameterization(Row) end, State) of
			#parameterization{version=Version} ->
				% Same version: update
				{ok, 1} = execute_statement(update_parameterization, [Id, Version, Value], State),
				{ok, Version+1};
			Parameterization when is_record(Parameterization, parameterization) -> wrong_version;
			Error -> Error
		end
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

%
% Token type operations
%
handle_call({get_token_types}, _From, State) ->
	return_multiple_results(get_token_types, [], 0, fun(Row) -> build_token_type(Row) end, State);

handle_call({get_token_type, Id}, _From, State) ->
	return_single_result(get_token_type, [Id], fun(Row) -> build_token_type(Row) end, State);

handle_call({update_token_type, Id, Version, MultipleUses, Uses, ExpiresInMinutes}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		case get_single_result(get_token_type, [Id], fun(Row) -> build_token_type(Row) end, State) of
			#token_type{version=Version} ->
				% Same version: update
				{ok, 1} = execute_statement(update_token_type, [Id, Version, MultipleUses, Uses, ExpiresInMinutes], State),
				{ok, Version+1};
			TokenType when is_record(TokenType, token_type) -> wrong_version;
			Error -> Error
		end
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

%
% User types operations
%
handle_call({get_user_types}, _From, State) ->
	return_reference_data(get_user_types, State);

handle_call({exists_user_type, Id}, _From, State) ->
	return_exists(count_user_types, [Id], State);

%
% User statuses operations
%
handle_call({get_user_statuses}, _From, State) ->
	return_reference_data(get_user_statuses, State);

handle_call({exists_user_status, Id}, _From, State) ->
	return_exists(count_user_statuses, [Id], State);

%
% Document types operations
%
handle_call({get_document_types}, _From, State) ->
	return_reference_data(get_document_types, State);

handle_call({exists_document_type, Id}, _From, State) ->
	return_exists(count_document_types, [Id], State);

%
% Document statuses operations
%
handle_call({get_document_statuses}, _From, State) ->
	return_reference_data(get_document_statuses, State);

handle_call({exists_document_status, Id}, _From, State) ->
	return_exists(count_document_statuses, [Id], State);

%
% Transport type operations
%
handle_call({get_transport_types}, _From, State) ->
	return_multiple_results(get_transport_types, [], 0, fun(Row) -> build_transport_type(Row) end, State);

handle_call({get_transport_type, Id}, _From, State) ->
	return_single_result(get_transport_type, [Id], fun(Row) -> build_transport_type(Row) end, State);

handle_call({exists_transport_type, Id}, _From, State) ->
	return_exists(count_transport_types, [Id], State);

%
% Courier transport operations
%
handle_call({get_courier_transport_status}, _From, State) ->
	return_reference_data(get_courier_transport_status, State);

handle_call({get_courier_transports, UserId}, _From, State) ->
	return_multiple_results(get_courier_transports, [UserId], 0, fun(Row) -> build_courier_transport(Row) end, State);

handle_call({get_courier_transport, TransportId}, _From, State) ->
	return_single_result(get_courier_transport, [TransportId], fun(Row) -> build_courier_transport(Row) end, State);

handle_call({get_current_courier_transport, UserId}, _From, State) ->
	return_single_result(get_current_courier_transport, [UserId], fun(Row) -> build_courier_transport(Row) end, State);

handle_call({get_active_courier_transports, UserId}, _From, State) ->
	return_multiple_results(get_active_courier_transports, [UserId, ?DB_COURIER_TRANSPORT_STATUS_ACTIVE], 0,
							fun(Row) -> build_courier_transport(Row) end, State);

handle_call({create_courier_transport, UserId, #new_courier_transport{transport_type_id=TransportTypeId, description=Description, registration_id=RegistrationId, color=Color}, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		% t_courier_transport_type
		{ok, _} = execute_statement(create_courier_transport, [UserId, TransportTypeId, ?DB_COURIER_TRANSPORT_CURRENT_NO, ?DB_COURIER_TRANSPORT_STATUS_PENDING, OperationTimestamp, OperationTimestamp, Description, RegistrationId, Color], State),
		{ok, NewCourierTransportId} = get_integer_value(get_last_id, [], State),
		{ok, NewCourierTransportId, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({update_courier_transport, UserId, StatusId, TransportId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		% t_user_courier
		{ok, _} = execute_statement(update_courier_transport, [StatusId, OperationTimestamp, TransportId], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({update_current_courier_transport, OldTransportId, NewTransportId, UserId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_user
		{ok, 1} = execute_statement(update_user_version, [UserId, Version], State),
		% t_user_courier
		{ok, _} = execute_statement(update_current_courier_transport, [?DB_COURIER_TRANSPORT_CURRENT_NO, OperationTimestamp, OldTransportId, ?DB_COURIER_TRANSPORT_CURRENT_YES], State),
		{ok, _} = execute_statement(update_current_courier_transport, [?DB_COURIER_TRANSPORT_CURRENT_YES, OperationTimestamp, NewTransportId, ?DB_COURIER_TRANSPORT_CURRENT_NO], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

%
% Courier Transport Status operations
%
handle_call({exists_courier_transport_status, Id}, _From, State) ->
	return_exists(count_courier_transport_status, [Id], State);

%
% User notification types operations
%
handle_call({get_user_notification_types}, _From, State) ->
	return_reference_data(get_user_notification_types, State);

handle_call({exists_user_notification_type, Id}, _From, State) ->
	return_exists(count_user_notification_types, [Id], State);

%
% User zone operations
%
handle_call({get_zones}, _From, State) ->
	case get_multiple_results(get_zones, [], 0, fun(Row) -> build_zone(Row) end, State) of
		Zones when is_list(Zones) -> Results = [build_zone_information(Zone, State) || Zone <- Zones];
		Other -> Results = Other
	end,
	{reply, Results, State};

handle_call({exists_zone, Id}, _From, State) ->
	return_exists(count_zones, [Id], State);

handle_call({exists_user_zone, UserId, ZoneId}, _From, State) ->
	return_exists_result(exists_user_zone, [UserId, ZoneId], State);

%
% Mobile OS operations
%
handle_call({get_mobileos}, _From, State) ->
	return_reference_data(get_mobileos, State);

handle_call({exists_mobileos, Id}, _From, State) ->
	return_exists_result(exists_mobileos, [Id], State);

%
% Contact request operations
%
handle_call({get_contact_request_statuses}, _From, State) ->
	return_reference_data(get_contact_request_statuses, State);

handle_call({exists_contact_request_status, Id}, _From, State) ->
	return_exists(count_contact_request_statuses, [Id], State);

handle_call({get_contact_requests}, _From, State) ->
	return_multiple_results(get_contact_requests, [], 0, fun(Row) -> build_contact_request(Row) end, State);

handle_call({get_contact_request, Id}, _From, State) ->
	return_single_result(get_contact_request, [Id], fun(Row) -> build_contact_request(Row) end, State);

handle_call({create_contact_request, #new_contact_request{email=Email, name=Name, subject=Subject, content=Content}, UserId}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		case UserId of
			undefined -> InsertUserId = null;
			_ -> InsertUserId = UserId
		end,
		{ok, _Inserted} = execute_statement(create_contact_request, [Email, Name, Subject, Content, ?DB_CONTACT_REQUEST_STATUS_PENDING, InsertUserId, OperationTimestamp], State),
		{ok, NewContactRequestId} = get_integer_value(get_last_id, [], State),
		{ok, NewContactRequestId}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({update_contact_request, 
			 #contact_request{id=ContactRequestId, id_status=OldIdStatus, id_user=OldIdUser, operator_notes=OldOperatorNotes, id_operator_user=OldIdOperator, status_date=OldStatusDate}, 
			 #change_contact_request{contact_request_status_id=IdStatus, user_id=IdUser, operator_notes=OperatorNotes, operator_id=IdOperator, version=Version}}, 
			_From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		NewIdStatus      = eb_api_util:get_update_value(IdStatus, OldIdStatus),
		NewIdUser        = eb_api_util:get_update_value(IdUser, OldIdUser),
		NewOperatorNotes = eb_api_util:get_update_value(OperatorNotes, OldOperatorNotes),
		NewIdOperator    = eb_api_util:get_update_value(IdOperator, OldIdOperator),
		case NewIdStatus of
			OldIdStatus ->
				NewStatusDate = OldStatusDate;
			_ ->
				NewStatusDate = eb_util:get_current()
		end,
		% t_contact_request
		{ok, 1} = execute_statement(update_contact_request, [NewIdStatus, NewIdUser, NewOperatorNotes, NewIdOperator, NewStatusDate, ContactRequestId, Version], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({delete_contact_request, ContactRequestId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		{ok, 1} = execute_statement(delete_contact_request, [ContactRequestId, Version], State),
		ok
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({exists_contact_status, IdStatus}, _From, State) ->
	return_exists(count_contact_status, [IdStatus], State);

%
% Create contact request
%
handle_call({create_notification, #new_notification{delivery_id=DeliveryId, waypoint_id=WaypointId, notification_type_id=NotificationTypeId, message=Message, position=#position{latitude=RLatitude, longitude=RLongitude}}}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		Latitude = eb_util:round_float(RLatitude, ?DB_DECIMAL_PLACES_COORDINATE),
		Longitude = eb_util:round_float(RLongitude, ?DB_DECIMAL_PLACES_COORDINATE),
		
		{ok, _Inserted} = execute_statement(create_notification, [DeliveryId, WaypointId, NotificationTypeId, Message, Latitude, Longitude, OperationTimestamp, ?DB_NOTIFICATION_STATUS_PENDING, OperationTimestamp],State),
		{ok, NewContactRequestId} = get_integer_value(get_last_id, [], State),
		{ok, NewContactRequestId}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

%
% Courier route operations
%
handle_call({create_courier_route, UserId, DeliveryId, Latitude, Longitude}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
 		NewLatitude = eb_util:round_float(Latitude, ?DB_DECIMAL_PLACES_COORDINATE),
 		NewLongitude = eb_util:round_float(Longitude, ?DB_DECIMAL_PLACES_COORDINATE),
		OperationTimestamp = eb_util:get_current(),
 		{ok, _Inserted} = execute_statement(create_courier_route, [UserId, DeliveryId, NewLatitude, NewLongitude, OperationTimestamp], State),
		ok
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

%
% Token operations
%
handle_call({get_token_info, TokenId}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		{Secs, MicroSecs} = eb_util:get_timestamp_as_secs_microsecs(OperationTimestamp),
		Current = Secs + MicroSecs,
		case get_single_result(get_token, [TokenId], fun(Row) -> build_token(Row) end, State) of
			Token = #token{id_type=TokenTypeId, remaining_uses=RemainingUses, invalid_after=InvalidAfer, version=Version} ->
				case InvalidAfer =/= null of
					true ->
						{InvalidAferSecs, InvalidAferMicroSecs} = eb_util:get_timestamp_as_secs_microsecs(InvalidAfer),
						InvalidAfterTimeout = InvalidAferSecs + InvalidAferMicroSecs,
						case Current > InvalidAfterTimeout of
							true -> TSOperation = delete;
							false -> TSOperation = do_nothing
						end;
					false -> TSOperation = do_nothing
				end,
				case TSOperation =:= do_nothing of
					true ->
						TokenParameters = get_multiple_results(get_token_parameters, [TokenId], 0, fun(Row) -> build_token_parameter(Row) end, State),
						case get_single_result(get_token_type, [TokenTypeId], fun(Row) -> build_token_type(Row) end, State) of
							#token_type{multiple_uses=false} -> MUOperation = delete;
							TokenType when is_record(TokenType, token_type) ->
								case RemainingUses of
									RemainingUses when RemainingUses =/= null andalso RemainingUses > 1 -> MUOperation = update;
									RemainingUses when RemainingUses =/= null -> MUOperation = delete;
									_ -> MUOperation = do_nothing
								end
						end,
						FinalOperation = MUOperation,
						FinalResponse = {ok, Token, TokenParameters};
					false ->
						FinalOperation = TSOperation,
						FinalResponse = {nok, invalid}
				end,
				case FinalOperation of
					delete ->
						{ok, _} = execute_statement(remove_token_parameter, [TokenId], State),
						{ok, _} = execute_statement(remove_token, [TokenId], State);
					update -> {ok, _} = execute_statement(decrease_token_uses, [TokenId, Version], State);
					_ -> do_nothing
				end,
				FinalResponse;
			Error -> Error
		end
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({create_token, Id, TokenTypeId, TokenParameters}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		{ok, TokenId} = create_token_rule(Id, TokenTypeId, TokenParameters, OperationTimestamp, State),
		{ok, TokenId}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

%
% Account status operations
%
handle_call({get_account_statuses}, _From, State) ->
	return_reference_data(get_account_statuses, State);

%
% Account user types operations
%
handle_call({get_account_user_types}, _From, State) ->
	return_reference_data(get_account_user_types, State);

handle_call({exists_account_user_type, Id}, _From, State) ->
	return_exists(count_account_user_types, [Id], State);

%
% Account department operations
%
handle_call({exists_account_department, AccountId, DepartmentId}, _From, State) ->
	return_exists_result(exists_account_department, [AccountId, DepartmentId], State);

%
% Order operations
%
handle_call({get_order_types}, _From, State) ->
	return_reference_data(get_order_types, State);

handle_call({exists_order_type, Id}, _From, State) ->
	return_exists(count_order_types, [Id], State);

handle_call({exists_order_status, Id}, _From, State) ->
	return_exists_result(exists_order_status, [Id], State);

handle_call({get_last_user_order, UserId}, _From, State) ->
	return_single_result(get_last_user_order, [UserId], fun(Row) -> build_order(Row) end, State);
%
% Delivery operations
%
handle_call({get_delivery_statuses}, _From, State) ->
	return_reference_data(get_delivery_statuses, State);

handle_call({exists_delivery_status, Id}, _From, State) ->
	return_exists(count_delivery_statuses, [Id], State);

handle_call({get_waypoint_statuses}, _From, State) ->
	return_reference_data(get_waypoint_statuses, State);

handle_call({exists_waypoint_status, Id}, _From, State) ->
	return_exists(count_waypoint_statuses, [Id], State);

handle_call({get_object_actions}, _From, State) ->
	return_reference_data(get_object_actions, State);

handle_call({exists_object_action, Id}, _From, State) ->
	return_exists(count_object_actions, [Id], State);

handle_call({get_object_statuses}, _From, State) ->
	return_reference_data(get_object_statuses, State);

handle_call({exists_object_status, Id}, _From, State) ->
	return_exists(count_object_statuses, [Id], State);

handle_call({get_delivery, DeliveryId}, _From, State) ->
	return_single_result(get_delivery, [DeliveryId], fun(Row) -> build_delivery(Row) end, State);

handle_call({get_delivery_information, DeliveryId}, _From, State) ->
	Result =
		case get_single_result(get_delivery, [DeliveryId], fun(Row) -> build_delivery(Row) end, State) of
			Delivery when is_record(Delivery, delivery) -> build_delivery_information(Delivery, State);
			Other -> Other
		end,
	{reply, Result, State};

handle_call({get_delivery_waypoint_information, DeliveryId, WaypointId}, _From, State) ->
	return_single_result(get_delivery_waypoint, [DeliveryId, WaypointId], fun(Row) -> build_u_delivery_waypoint_information(Row, State) end, State);

handle_call({create_deliveries, Deliveries}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		CreateDelivery = 
			fun(#new_delivery{orders=Orders, reference=Reference, id_transport_type=IdTransportType,
			                  distance=Distance, duration=Duration, route=Route, cut_time=CutTime,
			                  origin=Origin, destination=Destination, waypoints=Waypoints, objects=Objects,
			                  generated_delivery_waypoints=GeneratedDeliveryWaypoints}) ->
				OrderIds = [IdOrder || #inf_order{order=#order{id_order=IdOrder}} <- Orders],
				% validar versoes das orders
				ok = validate_orders_versions(Orders, State),
				% limpar deliveries das orders
				ok = clean_orders_deliveries(OrderIds, State),
				% t_delivery
				{ok, _} = execute_statement(create_delivery, [Reference, IdTransportType, Distance, Duration, Route,
				                                              CutTime, ?DB_DELIVERY_STATUS_CREATED, OperationTimestamp],
				                            State),
				{ok, NewDeliveryId} = get_integer_value(get_last_id, [], State),
				% t_delivery_object
				ok = create_delivery_objects_rule(NewDeliveryId, Objects, State),
				% t_delivery_waypoint, t_delivery_waypoint_object_action, t_generated_delivery_waypoint
				AllWaypoints = [Origin|(Waypoints ++ [Destination])],
				ok = create_delivery_waypoints_rule(NewDeliveryId, 1, AllWaypoints, OperationTimestamp, State),
				% t_generated_delivery
				ok = create_generated_deliveries_rule(OrderIds, NewDeliveryId, State),
				% t_generated_delivery_waypoint
				ok = create_generated_delivery_waypoints_rule(GeneratedDeliveryWaypoints, NewDeliveryId, State),
				NewDeliveryId
			end,
		{ok, [CreateDelivery(NewDelivery) || NewDelivery <- Deliveries]}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_object_qty_by_type_by_delivery, DeliveryId}, _From, State) ->
	return_multiple_results(get_object_qty_by_type_by_delivery, [DeliveryId], 0, fun(Row) -> build_object_by_delivery(Row) end, State);

handle_call({get_address_component_by_delivery, DeliveryId, Component}, _From, State) ->
	return_single_result(get_address_component_by_delivery, [DeliveryId, Component], fun(Row) -> build_component_by_delivery(Row) end, State);
 
handle_call({get_estimated_delivery_time, DeliveryId}, _From, State) ->
	return_single_result(get_estimated_delivery_time, [DeliveryId], fun(Row) -> build_time_count_by_delivery(Row) end, State);

handle_call({get_generated_deliveries_by_id_order, OrderId}, _From, State) ->
	return_multiple_results(get_generated_deliveries_by_id_order, [OrderId], 0, fun(Row) -> build_generated_delivery(Row) end, State);
handle_call({get_generated_delivery_by_id_delivery, DeliveryId}, _From, State) ->
	return_multiple_results(get_generated_delivery_by_id_delivery, [DeliveryId], 0, fun(Row) -> build_generated_delivery(Row) end, State);
	   
handle_call({get_deliveries_information, StatusId, CourierId, Navigation=#rs_navigation{order=Order}}, _From, State) ->
	case StatusId of
		undefined ->
			% No Status filter
			case CourierId of
				undefined ->
					% No CourierId filter
					% Determine order queries
					Params = [],
					Queries = case Order of
						?ORDER_DELIVERY_ID -> [get_deliveries_id_asc, get_deliveries_id_desc];
						?ORDER_COURIER_ID -> [get_deliveries_id_courier_asc, get_deliveries_id_courier_desc];
						_ -> [get_deliveries, get_deliveries]
					end;
				_ ->
					% CourierId filter
					% Determine order queries
					Params = [CourierId],
					Queries = case Order of
						?ORDER_DELIVERY_ID -> [get_deliveries_by_id_courier_id_asc, get_deliveries_by_id_courier_id_desc];
						?ORDER_COURIER_ID -> [get_deliveries_by_id_courier_id_courier_asc, get_deliveries_by_id_courier_id_courier_desc];
						_ -> [get_deliveries_by_id_courier, get_deliveries_by_id_courier]
					end
			end;
		_ ->
			% Status filter
			case CourierId of
				undefined ->
					% No CourierId filter
					% Determine order queries
					Params = [StatusId],
					Queries = case Order of
						?ORDER_DELIVERY_ID -> [get_deliveries_by_id_status_id_asc, get_deliveries_by_id_status_id_desc];
						?ORDER_COURIER_ID -> [get_deliveries_by_id_status_id_courier_asc, get_deliveries_by_id_status_id_courier_desc];
						_ -> [get_deliveries_by_id_status, get_deliveries_by_id_status]
					end;
				_ ->
					% CourierId filter
					% Determine order queries
					Params = [StatusId, CourierId],
					Queries = case Order of
						?ORDER_DELIVERY_ID -> [get_deliveries_by_id_status_id_courier_id_asc, get_deliveries_by_id_status_id_courier_id_desc];
						?ORDER_COURIER_ID -> [get_deliveries_by_id_status_id_courier_id_courier_asc, get_deliveries_by_id_status_id_courier_id_courier_desc];
						_ -> [get_deliveries_by_id_status_id_courier, get_deliveries_by_id_status_id_courier]
					end
			end
	end,
	Results = execute_deliveries_information_query(Queries, Params, Navigation, State),
	{reply, Results, State};

handle_call({get_dispatcher_deliveries_information, StatusId, CourierId, ProductionTime, BufferTime, Navigation=#rs_navigation{order=Order}}, _From, State) ->
	case StatusId of
		undefined ->
			% No Status filter
			case CourierId of
				undefined ->
					% No CourierId filter
					% Determine order queries
					Params = [],
					Queries = case Order of
						?ORDER_DELIVERY_ID -> [get_deliveries_id_asc, get_deliveries_id_desc];
						?ORDER_COURIER_ID -> [get_deliveries_id_courier_asc, get_deliveries_id_courier_desc];
						_ -> [get_deliveries, get_deliveries]
					end;
				_ ->
					% CourierId filter
					% Determine order queries
					Params = [CourierId],
					Queries = case Order of
						?ORDER_DELIVERY_ID -> [get_deliveries_by_id_courier_id_asc, get_deliveries_by_id_courier_id_desc];
						?ORDER_COURIER_ID -> [get_deliveries_by_id_courier_id_courier_asc, get_deliveries_by_id_courier_id_courier_desc];
						_ -> [get_deliveries_by_id_courier, get_deliveries_by_id_courier]
					end
			end;
		_ ->
			% Status filter
			case CourierId of
				undefined ->
					% No CourierId filter
					% Determine order queries
					Params = [StatusId],
					Queries = case Order of
						?ORDER_DELIVERY_ID -> [get_deliveries_by_id_status_id_asc, get_deliveries_by_id_status_id_desc];
						?ORDER_COURIER_ID -> [get_deliveries_by_id_status_id_courier_asc, get_deliveries_by_id_status_id_courier_desc];
						_ -> [get_deliveries_by_id_status, get_deliveries_by_id_status]
					end;
				_ ->
					% CourierId filter
					% Determine order queries
					Params = [StatusId, CourierId],
					Queries = case Order of
						?ORDER_DELIVERY_ID -> [get_deliveries_by_id_status_id_courier_id_asc, get_deliveries_by_id_status_id_courier_id_desc];
						?ORDER_COURIER_ID -> [get_deliveries_by_id_status_id_courier_id_courier_asc, get_deliveries_by_id_status_id_courier_id_courier_desc];
						_ -> [get_deliveries_by_id_status_id_courier, get_deliveries_by_id_status_id_courier]
					end
			end
	end,
	Results = execute_dispatcher_deliveries_information_query(Queries, Params, ProductionTime, BufferTime, Navigation, State),
	{reply, Results, State};

handle_call({get_courier_deliveries_information, CourierId, StatusId, Navigation=#rs_navigation{order=Order}}, _From, State) ->
	case StatusId of
		undefined ->
			% No Status filter
			% Determine order queries
			Params = [CourierId],
			Queries = case Order of
				?ORDER_DELIVERY_ID -> [get_deliveries_by_id_courier_id_asc, get_deliveries_by_id_courier_id_desc];
				_ -> [get_deliveries_by_id_courier, get_deliveries_by_id_courier]
			end;
		_ ->
			% Status filter
			% Determine order queries
			Params = [CourierId, StatusId],
			Queries = case Order of
				?ORDER_DELIVERY_ID -> [get_deliveries_by_id_courier_and_id_status_id_asc, get_deliveries_by_id_courier_and_id_status_id_desc];
				_ -> [get_deliveries_by_id_courier_and_id_status, get_deliveries_id_courier_and_id_by_status]
			end
	end,
	Results = execute_deliveries_information_query(Queries, Params, Navigation, State),
	{reply, Results, State};

handle_call({get_ongoing_deliveries_information}, _From, State) ->
	Results = execute_deliveries_information_query([get_ongoing_deliveries, get_ongoing_deliveries], [], #rs_navigation{}, State),
	{reply, Results, State};

handle_call({get_courier_delivery_information, CourierId, DeliveryId}, _From, State) ->
	case get_single_result(get_delivery_by_id_id_courier, [DeliveryId, CourierId], fun(Row) -> build_delivery(Row) end, State) of
		Delivery when is_record(Delivery, delivery) -> Result = build_delivery_information(Delivery, State);
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({update_delivery_status, Id, Version, StatusId}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		case StatusId of
			?DB_DELIVERY_STATUS_EXECUTING -> OrderStatusId = ?DB_ORDER_STATUS_EXECUTING;
			?DB_DELIVERY_STATUS_COMPLETED_SUCCESS -> OrderStatusId = ?DB_ORDER_STATUS_COMPLETED_SUCCESS;
			?DB_DELIVERY_STATUS_COMPLETED_COLLECTION_ERROR -> OrderStatusId = ?DB_ORDER_STATUS_COMPLETED_REFUSED;
			?DB_DELIVERY_STATUS_COMPLETED_DISTRIBUTION_ERROR -> OrderStatusId = ?DB_ORDER_STATUS_COMPLETED_REFUSED;
			?DB_DELIVERY_STATUS_CANCELED_CUT_TIME -> OrderStatusId = ?DB_ORDER_STATUS_COMPLETED_REFUSED;
			?DB_DELIVERY_STATUS_COMPLETED_CANCELED -> OrderStatusId = ?DB_ORDER_STATUS_CANCELED;
			_ -> OrderStatusId = null
		end,
		{ok, 1} = execute_statement(update_delivery_status, [Id, Version, StatusId, OperationTimestamp], State),
		case OrderStatusId of
			null -> do_nothing;
			_ ->
				Orders = get_multiple_results(get_orders_by_id_delivery, [Id], 0, fun(Row) -> build_order(Row) end, State),
				io:format("Lista de Orders para actualizar: ~p~n", [Orders]),
				%Update orders list status id
				[{ok, 1} = execute_statement(update_order_status, [OrderId, OrderVersion, OrderStatusId, OperationTimestamp], State) || #order{id_order=OrderId, version=OrderVersion} <- Orders]
		end,
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({update_delivery_status_skip, Id, Version, StatusId, SkipValue}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		{ok, 1} = execute_statement(update_delivery_status_skip, [Id, Version, StatusId, SkipValue, OperationTimestamp], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({assign_delivery, Id, CourierId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		{ok, 1} = execute_statement(assign_delivery, [Id, Version, ?DB_DELIVERY_STATUS_ACCEPTED, OperationTimestamp, CourierId], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({assign_delivery_dispatcher, DeliveryId, CourierId, StatusId, Version}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% Assign delivery to the courier
		{ok, 1} = execute_statement(assign_delivery_dispatcher, [DeliveryId, Version, StatusId, OperationTimestamp, CourierId], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({update_delivery_waypoint, DeliveryId, WaypointId, WaypointStatusId, StopDuration, TravelTime,
			 DeliveryStatusId, _Rating, _RatingNotes, #position{latitude=Latitude, longitude=Longitude},
			 _SignatureFile, DeliveryVersion, Orders}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		case DeliveryStatusId of
			null ->
				{ok, 1} = execute_statement(update_delivery_version, [DeliveryId, DeliveryVersion], State);
			_ ->
				{ok, 1} = execute_statement(update_delivery_status, [DeliveryId, DeliveryVersion, DeliveryStatusId, OperationTimestamp], State)
		end,
		OrderStatusId =
			if
				DeliveryStatusId =:= ?DB_DELIVERY_STATUS_EXECUTING -> ?DB_ORDER_STATUS_EXECUTING;
				WaypointId =:= 1 -> null;
				WaypointStatusId =:= ?DB_WAYPOINT_STATUS_LEFT -> ?DB_ORDER_STATUS_COMPLETED_SUCCESS;
				true -> null
			end,
		case OrderStatusId of
			null -> do_nothing;
			_ ->
				%Update orders list status id
				[{ok, 1} = execute_statement(update_order_status, [OrderId, OrderVersion, OrderStatusId, OperationTimestamp], State) || #inf_order{order=#order{id_order=OrderId, version=OrderVersion}} <- Orders]
		end,
		case WaypointStatusId of
			?DB_WAYPOINT_STATUS_LEFT ->
				{ok, 1} = execute_statement(update_delivery_waypoint_status_stop, [DeliveryId, WaypointId, WaypointStatusId, StopDuration, Latitude, Longitude, OperationTimestamp], State);
			?DB_WAYPOINT_STATUS_ARRIVED ->
				{ok, 1} = execute_statement(update_delivery_waypoint_status_travel_arrived, [DeliveryId, WaypointId, WaypointStatusId, TravelTime, Latitude, Longitude, OperationTimestamp], State);
			_ ->
				{ok, 1} = execute_statement(update_delivery_waypoint_status_travel, [DeliveryId, WaypointId, WaypointStatusId, TravelTime, OperationTimestamp], State)
		end,
		{ok, DeliveryVersion+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_delivery_by_client_courier, UserId, DeliveryId}, _From, State) when is_integer(UserId) ->
	return_single_result(get_delivery_by_id_client_courier, [DeliveryId, UserId, ?DB_ACCOUNT_USER_TYPE_ADMINISTRATOR], fun(Row) -> build_delivery(Row) end, State);

handle_call({verify_account_admin, RequestingUserId, TargetUserId}, _From, State) when is_integer(RequestingUserId) andalso is_integer(TargetUserId) ->
	return_exists(verify_account_admin, [RequestingUserId, TargetUserId], State);

%
% Orders operations
%
handle_call({create_order, UserId, #new_order{reference=Reference, distribution_center=DistributionCenter,
											  order_type_id=OrderTypeId, transport_type_id=TransportTypeId,
											  origin_id=OriginId, client_info=ClientInfo, cut_time=CutTime,
											  payment_provider_id=PaymentProviderId, payment_id=PaymentId,
											  payment_method_id=PaymentMethodId, diner_qty=DinerQty,
											  gross_total=GrossTotal, tip=Tip, fee=Fee, order_prods=OrderProds,
											  origin=Origin, destination=Destination, waypoints=Waypoints,
											  objects=Objects, 
											  account=#new_order_account{id_account=AccountId, id_slot=SlotId}}, 
			 CheckPayment, PaymentStatusId, AuthorizationOnly,
			 PaymentRequestEndpoint, PaymentRequest, PaymentRequestResponse}, _From,
			State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),

		case ClientInfo of
			#client_contact_info{name=Name, phone_nr=PhoneNr, email=Email, fiscal_id=FiscalId} ->
				ClientName = get_optional(Name),
				ClientPhoneNr = get_optional(PhoneNr),
				ClientEmail = get_optional(Email),
				ClientFiscalId = get_optional(FiscalId);
			_ ->
				ClientName = null,
				ClientPhoneNr = null,
				ClientEmail = null,
				ClientFiscalId = null
		end,
		% t_order
		{ok, _} = execute_statement(create_order, [Reference, DistributionCenter, UserId,
												   OrderTypeId, TransportTypeId, OriginId,
												   ClientName, ClientPhoneNr, ClientEmail, ClientFiscalId, CutTime,
												   ?DB_ORDER_STATUS_CREATED, PaymentId, PaymentMethodId,
												   DinerQty, GrossTotal, Tip, Fee, OperationTimestamp], State),
		{ok, NewOrderId} = get_integer_value(get_last_id, [], State),

		% Create Order Details
		ok = create_order_prods_rule(NewOrderId, OrderProds, State),

		% Create Order Objects
		ok = create_order_objects_rule(NewOrderId, 0, Objects, State),
		NewObjects = get_multiple_results(get_order_objects, [NewOrderId], 0, fun(Row) -> build_order_object(Row) end, State),
	
		% Create Order Waypoints
		ok = create_order_waypoint_rule(NewOrderId, 1, Origin, NewObjects, State),
		ok = create_order_waypoints_rule(NewOrderId, 2, Waypoints, NewObjects, State),
		
		case OrderTypeId =/= ?DB_ORDER_TYPE_TAKEAWAY of
			true ->
				ok = create_order_waypoint_rule(NewOrderId, (2 + length(Waypoints)), Destination, NewObjects, State);
			false -> noop
		end,
		case CheckPayment of
			true ->
				FPaymentRequest = list_to_binary(PaymentRequest),
				FPaymentRequestResponse = list_to_binary(PaymentRequestResponse),
				% t_order_payment
				{ok, NewPaymentId} = execute_statement(create_order_payment, [NewOrderId, PaymentProviderId, PaymentStatusId, PaymentId, AuthorizationOnly, OperationTimestamp], State),
				% t_payment_request
				{ok, _} = execute_statement(create_payment_request, [NewPaymentId, PaymentRequestEndpoint, FPaymentRequest, FPaymentRequestResponse, OperationTimestamp], State);
			false -> do_nothing
		end,
		
		if
			SlotId =/= null andalso SlotId =/= undefined ->
				{ok, 1} = execute_statement(update_account_slots_information, [AccountId, SlotId], State);
			true -> noop
		end,

		{ok, NewOrderId}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_order_statuses}, _From, State) ->
	return_reference_data(get_order_statuses, State);

handle_call({get_order_origins}, _From, State) ->
	return_reference_data(get_order_origins, State);

handle_call({get_order_payment_methods}, _From, State) ->
	return_reference_data(get_order_payment_methods, State);

handle_call({exists_order_payment_method, Id}, _From, State) ->
	return_exists_result(exists_order_payment_method, [Id], State);

handle_call({exists_order_payment, Id}, _From, State) ->
	return_exists_result(exists_order_payment, [Id], State);

handle_call({exists_account_slot, IdAccount, IdSlot}, _From, State) ->
	return_exists(count_slot_existance, [IdAccount, IdSlot], State);

handle_call({available_account_slot, IdAccount, IdSlot}, _From, State) ->
	return_exists(count_slot_availability, [IdAccount, IdSlot], State);

handle_call({get_orders_information, StatusId, TypeId, PhoneNr, Navigation=#rs_navigation{order=Order}}, _From, State) ->
	{Queries, Params} =
		case {StatusId, TypeId, PhoneNr, Order} of
			{undefined, undefined, undefined, ?ORDER_ID} -> {[get_orders_asc, get_orders_desc], []};
			{undefined, undefined, undefined, _Order} -> {[get_orders, get_orders], []} ;
			{StatusId, undefined, undefined, ?ORDER_ID} -> {[get_orders_by_id_status_id_asc, get_orders_by_id_status_id_desc], [StatusId]};
			{StatusId, undefined, undefined, _Order} -> {[get_orders_by_id_status, get_orders_by_id_status], [StatusId]};
			{undefined, TypeId, undefined, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_type_asc, get_dispatcher_orders_by_id_type_desc], [TypeId]};
			{undefined, TypeId, undefined, _Order} -> {[get_orders_by_id_type, get_orders_by_id_type], [TypeId]};
			{StatusId, TypeId, undefined, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_status_by_id_type_asc, get_dispatcher_orders_by_id_status_by_id_type_desc], [StatusId, TypeId]};
			{StatusId, TypeId, undefined, _Order} -> {[get_dispatcher_orders_by_id_status_by_id_type, get_dispatcher_orders_by_id_status_by_id_type], [StatusId, TypeId]};
			{undefined, undefined, PhoneNr, ?ORDER_ID} -> {[get_orders_by_phone_nr_asc, get_orders_by_phone_nr_desc], [PhoneNr]};
			{undefined, undefined, PhoneNr, _Order} -> {[get_orders_by_phone_nr, get_orders_by_phone_nr], [PhoneNr]} ;
			{StatusId, undefined, PhoneNr, ?ORDER_ID} -> {[get_orders_by_id_status_by_phone_nr_asc, get_orders_by_id_status_by_phone_nr_desc], [StatusId, PhoneNr]};
			{StatusId, undefined, PhoneNr, _Order} -> {[get_orders_by_id_status_by_phone_nr, get_orders_by_id_status_by_phone_nr], [StatusId, PhoneNr]};
			{undefined, TypeId, PhoneNr, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_type_by_phone_nr_asc, get_dispatcher_orders_by_id_type_by_phone_nr_desc], [TypeId, PhoneNr]};
			{undefined, TypeId, PhoneNr, _Order} -> {[get_orders_by_id_type_by_phone_nr, get_orders_by_id_type_by_phone_nr], [TypeId, PhoneNr]};
			{StatusId, TypeId, PhoneNr, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr_asc, get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr_desc], [StatusId, TypeId, PhoneNr]};
			{StatusId, TypeId, PhoneNr, _Order} -> {[get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr, get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr], [StatusId, TypeId, PhoneNr]}
		end,
	Results = execute_orders_information_query(Queries, Params, Navigation, State),
	{reply, Results, State};

handle_call({get_orders_for_generation, EstimatedOrderTime, SlottedTimeWindow}, _From, State) ->
	Queries = [get_orders_for_generation, get_orders_for_generation],
	Statuses = [?DB_ORDER_STATUS_CREATED, ?DB_ORDER_STATUS_PRODUCTION, ?DB_ORDER_STATUS_DISPATCH],
	StartWindowTimestamp = add_minutes_to_db_timestamp(EstimatedOrderTime, eb_util:get_current()),
	EndWindowTimestamp = add_minutes_to_db_timestamp(SlottedTimeWindow, StartWindowTimestamp),
	Type = ?DB_ORDER_TYPE_DELIVERY,
	Params = [Statuses, StartWindowTimestamp, EndWindowTimestamp, Type],
	Results = execute_orders_information_query(Queries, Params, #rs_navigation{}, State),
	{reply, Results, State};

handle_call({get_dispatcher_orders_information, StatusId, TypeId, PhoneNr, ProductionTime, BufferTime, Navigation=#rs_navigation{order=Order}}, _From, State) ->
	{Queries, Params} =
		case {StatusId, TypeId, PhoneNr, Order} of
			{undefined, undefined, undefined, ?ORDER_ID} -> {[get_dispatcher_orders_asc, get_dispatcher_orders_desc], []};
			{undefined, undefined, undefined, _Order} -> {[get_dispatcher_orders, get_dispatcher_orders], []} ;
			{StatusId, undefined, undefined, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_status_asc, get_dispatcher_orders_by_id_status_desc], [StatusId]};
			{StatusId, undefined, undefined, _Order} -> {[get_dispatcher_orders_by_id_status, get_dispatcher_orders_by_id_status], [StatusId]};
			{undefined, TypeId, undefined, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_type_asc, get_dispatcher_orders_by_id_type_desc], [TypeId]};
			{undefined, TypeId, undefined, _Order} -> {[get_dispatcher_orders_by_id_type, get_dispatcher_orders_by_id_type], [TypeId]};
			{StatusId, TypeId, undefined, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_status_by_id_type_asc, get_dispatcher_orders_by_id_status_by_id_type_desc], [StatusId, TypeId]};
			{StatusId, TypeId, undefined, _Order} -> {[get_dispatcher_orders_by_id_status_by_id_type, get_dispatcher_orders_by_id_status_by_id_type], [StatusId, TypeId]};
			{undefined, undefined, PhoneNr, ?ORDER_ID} -> {[get_dispatcher_orders_by_phone_nr_asc, get_dispatcher_orders_by_phone_nr_desc], [PhoneNr]};
			{undefined, undefined, PhoneNr, _Order} -> {[get_dispatcher_orders_by_phone_nr, get_dispatcher_orders_by_phone_nr], [PhoneNr]} ;
			{StatusId, undefined, PhoneNr, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_status_by_phone_nr_asc, get_dispatcher_orders_by_id_status_by_phone_nr_desc], [StatusId, PhoneNr]};
			{StatusId, undefined, PhoneNr, _Order} -> {[get_dispatcher_orders_by_id_status_by_phone_nr, get_dispatcher_orders_by_id_status_by_phone_nr], [StatusId, PhoneNr]};
			{undefined, TypeId, PhoneNr, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_type_by_phone_nr_asc, get_dispatcher_orders_by_id_type_by_phone_nr_desc], [TypeId, PhoneNr]};
			{undefined, TypeId, PhoneNr, _Order} -> {[get_dispatcher_orders_by_id_type_by_phone_nr, get_dispatcher_orders_by_id_type_by_phone_nr], [TypeId, PhoneNr]};
			{StatusId, TypeId, PhoneNr, ?ORDER_ID} -> {[get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr_asc, get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr_desc], [StatusId, TypeId, PhoneNr]};
			{StatusId, TypeId, PhoneNr, _Order} -> {[get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr, get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr], [StatusId, TypeId, PhoneNr]}
		end,
	Results = execute_orders_information_dispatcher_query(Queries, Params, ProductionTime, BufferTime, Navigation, State),
	{reply, Results, State};

handle_call({get_user_orders_information, UserId, StatusId, TypeId, PhoneNr, Navigation=#rs_navigation{order=Order}}, _From, State) ->
	{Queries, Params} =
		case {StatusId, TypeId, PhoneNr, Order} of
			{undefined, undefined, undefined, ?ORDER_ID} -> {[get_orders_by_id_user_id_asc, get_orders_by_id_user_id_desc], [UserId]};
			{undefined, undefined, undefined, _Order} -> {[get_orders_by_id_user, get_orders_by_id_user], [UserId]};
			{StatusId, undefined, undefined, ?ORDER_ID} -> {[get_orders_by_id_user_and_id_status_id_asc, get_orders_by_id_user_and_id_status_id_desc], [UserId, StatusId]};
			{StatusId, undefined, undefined, _OrderId} -> {[get_orders_by_id_user_and_id_status, get_orders_by_id_user_and_id_status], [UserId, StatusId]};			
			{undefined, TypeId, undefined, ?ORDER_ID} -> {[get_orders_by_id_user_by_id_type_asc, get_orders_by_id_user_by_id_type_desc], [UserId, TypeId]};
			{undefined, TypeId, undefined, _Order} -> {[get_orders_by_id_user_by_id_type, get_orders_by_id_user_by_id_type], [UserId, TypeId]};
			{StatusId, TypeId, undefined, ?ORDER_ID} -> {[get_orders_by_id_user_id_status_id_type_asc, get_orders_by_id_user_id_status_id_type_desc], [UserId, StatusId, TypeId]};
			{StatusId, TypeId, undefined, _OrderId} -> {[get_orders_by_id_user_id_status_id_type, get_orders_by_id_user_id_status_id_type], [UserId, StatusId, TypeId]};
			{undefined, undefined, PhoneNr, ?ORDER_ID} -> {[get_orders_by_id_user_phone_nr_asc, get_orders_by_id_user_phone_nr_desc], [UserId, PhoneNr]};
			{undefined, undefined, PhoneNr, _Order} -> {[get_orders_by_id_user_phone_nr, get_orders_by_id_user_phone_nr], [UserId, PhoneNr]};
			{StatusId, undefined, PhoneNr, ?ORDER_ID} -> {[get_orders_by_id_user_and_id_status_phone_nr_asc, get_orders_by_id_user_and_id_status_phone_nr_desc], [UserId, StatusId, PhoneNr]};
			{StatusId, undefined, PhoneNr, _OrderId} -> {[get_orders_by_id_user_and_id_status_phone_nr, get_orders_by_id_user_and_id_status_phone_nr], [UserId, StatusId, PhoneNr]};			
			{undefined, TypeId, PhoneNr, ?ORDER_ID} -> {[get_orders_by_id_user_by_id_type_by_phone_nr_asc, get_orders_by_id_user_by_id_type_by_phone_nr_desc], [UserId, TypeId, PhoneNr]};
			{undefined, TypeId, PhoneNr, _Order} -> {[get_orders_by_id_user_by_id_type_by_phone_nr, get_orders_by_id_user_by_id_type_by_phone_nr], [UserId, TypeId, PhoneNr]};
			{StatusId, TypeId, PhoneNr, ?ORDER_ID} -> {[get_orders_by_id_user_id_status_by_id_type_by_phone_nr_asc, get_orders_by_id_user_id_status_by_id_type_by_phone_nr_desc], [UserId, StatusId, TypeId, PhoneNr]};
			{StatusId, TypeId, PhoneNr, _OrderId} -> {[get_orders_by_id_user_id_status_by_id_type_by_phone_nr, get_orders_by_id_user_id_status_by_id_type_by_phone_nr], [UserId, StatusId, TypeId, PhoneNr]}		
		end,
	Results = execute_orders_information_query(Queries, Params, Navigation, State),
	{reply, Results, State};

handle_call({get_order, OrderId}, _From, State) ->
	return_single_result(get_order, [OrderId], fun(Row) -> build_order(Row) end, State);

handle_call({get_order_information, OrderId}, _From, State) ->
	case get_single_result(get_order, [OrderId], fun(Row) -> build_order(Row) end, State) of
		Order when is_record(Order, order) -> Result = build_order_information(Order, State);
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({get_todays_slots, RequestingUserTypeId}, _From, State) -> 
	case get_multiple_results(get_todays_slots_information, [], 0, fun(Row) -> build_account_slots_information_temp(Row, RequestingUserTypeId) end, State) of
		AccountSlots when is_list(AccountSlots) -> Result = AccountSlots;
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({update_slot_occupancy, SlotId}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		{ok, _} = execute_statement(update_slot_max_occupancy_by_id, [SlotId], State)
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({verify_todays_slot, IdSlot}, _From, State) ->
	return_exists(verify_todays_slot_existance, [IdSlot], State);

handle_call({get_order_user_information, OrderId, UserId}, _From, State) ->
	case get_single_result(get_order_by_id_id_user, [OrderId, UserId], fun(Row) -> build_order(Row) end, State) of
		Order when is_record(Order, order) -> Result = build_order_information(Order, State);
		Other -> Result = Other
	end,
	{reply, Result, State};

handle_call({update_order_status, Id, Version, StatusId}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		if 
			StatusId =:= ?DB_ORDER_STATUS_PRODUCTION ->
				{ok, 1} = execute_statement(update_order_status_prod, [Id, Version, StatusId, OperationTimestamp], State);
			true ->
				{ok, 1} = execute_statement(update_order_status, [Id, Version, StatusId, OperationTimestamp], State)
		end,
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({update_order_delivery_time, OrderId, NewMinutes, NewCutTime, Version, Deliveries}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		CutTimeToInsert = add_minutes_to_db_timestamp(NewMinutes, NewCutTime),
		
		% Update t_order
		{ok, 1} = execute_statement(update_order_cut_time_production_time, [OrderId, Version, ?DB_ORDER_STATUS_PRODUCTION, CutTimeToInsert, OperationTimestamp], State),

		ProcessDelivery =
			fun(#generated_delivery{id_delivery=DeliveryId}, IdOrder) ->				

				GeneratedDeliveries = get_multiple_results(get_generated_delivery_by_id_delivery, [DeliveryId], 0, fun(Row) -> 
																		   build_generated_delivery(Row) end, State),
				OtherDeliveries = [Delivery || Delivery <- GeneratedDeliveries, Delivery#generated_delivery.id_order =/= IdOrder],

				% Delete delivery
				{ok, _} = execute_statement(delete_generated_delivery_object, [DeliveryId, IdOrder], State),
				{ok, _} = execute_statement(delete_generated_delivery_waypoint, [DeliveryId, IdOrder], State),
				{ok, _} = execute_statement(delete_generated_delivery, [DeliveryId, IdOrder], State),
				
				if
					OtherDeliveries =:= [] ->   % Unica order desta delivery
						{ok, _} = execute_statement(delete_delivery_courier_route, [DeliveryId], State),
						{ok, _} = execute_statement(delete_delivery_object_action, [DeliveryId], State),
						{ok, _} = execute_statement(delete_delivery_object, [DeliveryId], State),
						{ok, _} = execute_statement(delete_delivery_waypoint, [DeliveryId], State),
						{ok, _} = execute_statement(delete_delivery, [DeliveryId], State);
					true ->	noop
				end
			end,
		[ProcessDelivery(Delivery, OrderId) || Delivery <- Deliveries],	
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({update_order_cut_time, Id, Version, CutTime}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		Statuses = [?DB_ORDER_STATUS_CREATED, ?DB_ORDER_STATUS_PRODUCTION],
		{ok, 1} = execute_statement(update_order_cut_time, [Id, Version, Statuses, CutTime, OperationTimestamp], State),
		{ok, Version+1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_request_orders_for_route_generation}, _From, State) ->
	Results = [],
	{reply, Results, State};

%
% Oauth Operations
%
handle_call({get_oauth_provider, Id}, _From, State) ->
	return_single_result(get_oauth_provider, [Id], fun(Row) -> build_oauth_provider(Row) end, State);

%
% Payment Provider operations
%
handle_call({get_payment_provider, Id}, _From, State) ->
	return_single_result(get_payment_provider, [Id], fun(Row) -> build_payment_provider(Row) end, State);

handle_call({exists_payment_provider, Id}, _From, State) ->
	return_exists_result(exists_payment_provider, [Id], State);

%
%Department operations
%
handle_call({get_departments, AccountId}, _From, State) ->
	return_multiple_results(get_departments, [AccountId], 0, fun(Row) -> build_department(Row) end, State);

%
% API Client operations
%
handle_call({get_active_api_key, ApiKey}, _From, State) ->
	return_single_result(get_api_key_by_status, [ApiKey, ?DB_API_CLIENT_STATUS_ACTIVE], fun(Row) -> build_api_key(Row) end, State);

%
% Voucher operations
%
handle_call({create_voucher, #new_voucher{voucher_code=VoucherCode, value=Value, voucher_type_id=VoucherTypeId, max_times=MaxTimes, expiration_date=ExpirationDate, restrict_user_id=RestrictUserId}}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		RoundedValue = eb_util:round_float(Value, ?DB_DECIMAL_PLACES_MONEY),
		% t_voucher
		{ok, _} = execute_statement(create_voucher, [VoucherCode, RoundedValue, VoucherTypeId,
													 ?DB_VOUCHER_STATUS_UNUSED, MaxTimes, ExpirationDate,
													 RestrictUserId, OperationTimestamp], State),
		{ok, _NewDeliveryId} = get_integer_value(get_last_id, [], State)
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({exists_voucher, VoucherCode}, _From, State) ->
	return_exists(count_voucher, [VoucherCode], State);

handle_call({exists_voucher_type, VoucherTypeId}, _From, State) ->
	return_exists(count_voucher_type, [VoucherTypeId], State);

handle_call({exists_voucher_status, VoucherStatusId}, _From, State) ->
	return_exists(count_voucher_status, [VoucherStatusId], State);

handle_call({get_voucher, VoucherId}, _From, State) when is_integer(VoucherId) ->
	return_single_result(get_voucher, [VoucherId], fun(Row) -> build_voucher(Row) end, State);

handle_call({get_voucher_by_code, VoucherCode}, _From, State) ->
	return_single_result(get_voucher_by_code, [VoucherCode], fun(Row) -> build_voucher(Row) end, State);

handle_call({get_voucher_types}, _From, State) ->
	return_reference_data(get_voucher_types, State);

handle_call({get_voucher_statuses}, _From, State) ->
	return_reference_data(get_voucher_statuses, State);

handle_call({get_vouchers, VoucherCode, VoucherStatusId, UserId}, _From, State) ->
	% Decide which query to execute
	{Query, Params} =
		case {VoucherCode, VoucherStatusId, UserId} of
			{undefined, undefined, undefined} -> {get_vouchers, []};
			{VoucherCode, undefined, undefined} -> {get_vouchers_code, [VoucherCode]};
			{undefined, VoucherStatusId, undefined} -> {get_vouchers_id_status, [VoucherStatusId]};
			{undefined, undefined, UserId} -> {get_vouchers_restrict_user_id, [UserId]};
			{VoucherCode, VoucherStatusId, undefined} -> {get_vouchers_code_id_status, [VoucherCode, VoucherStatusId]};
			{VoucherCode, undefined, UserId} -> {get_vouchers_code_restrict_user_id, [VoucherCode, UserId]};
			{undefined, VoucherStatusId, UserId} -> {get_vouchers_id_status_restrict_user_id, [VoucherStatusId, UserId]};
			{VoucherCode, VoucherStatusId, UserId} ->
				{get_vouchers_code_id_status_restrict_user_id, [VoucherCode, VoucherStatusId, UserId]}
		end,
	% Execute query
	return_multiple_results(Query, Params, 0, fun(Row) -> build_voucher(Row) end, State);

handle_call({change_voucher,
             #voucher{voucher_id=VoucherId, value=OldValue, voucher_status_id=OldVoucherStatusId, max_times=OldMaxTimes, expiration_date=OldExpirationDate, restrict_user_id=OldRestrictUserId, status_date=OldStatusDate},
             #change_voucher{value=NewValue, voucher_status_id=NewVoucherStatusId, max_times=NewMaxTimes, expiration_date=NewExpirationDate, restrict_user_id=NewRestrictUserId, version=Version}},
            _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		Value = eb_api_util:get_update_value(NewValue, OldValue),
		VoucherStatusId = eb_api_util:get_update_value(NewVoucherStatusId, OldVoucherStatusId),
		MaxTimes = eb_api_util:get_update_value(NewMaxTimes, OldMaxTimes),
		ExpirationDate = eb_api_util:get_update_value(NewExpirationDate, OldExpirationDate),
		RestrictUserId = eb_api_util:get_update_value(NewRestrictUserId, OldRestrictUserId),
		% Status setting
		StatusDate = 
			if VoucherStatusId =:= OldVoucherStatusId -> OldStatusDate
			 ; VoucherStatusId =/= OldVoucherStatusId -> OperationTimestamp
			end,
		% t_voucher
		{ok, 1} = execute_statement(update_voucher, [VoucherId, Version, Value, VoucherStatusId, MaxTimes, ExpirationDate, RestrictUserId, StatusDate], State),
		{ok, Version + 1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({validate_voucher, VoucherCode, RequestingUserId}, _From, State) ->
	OperationTimestamp = eb_util:get_current(),
	return_exists(count_valid_vouchers, [VoucherCode, RequestingUserId, OperationTimestamp,
										 ?DB_VOUCHER_STATUS_UNUSED, ?DB_VOUCHER_STATUS_PARTIALLY_USED,
										 ?DB_VOUCHER_TYPE_MULTI_TIME_N_TIMES], State);

handle_call({apply_voucher, OrderId, VoucherId, OrderVersion}, _From, State=#state{connection=Connection}) ->
	TransactionFunction = fun() ->
		OperationTimestamp = eb_util:get_current(),
		% t_delivery
		{ok, 1} = execute_statement(increment_order_version, [OrderId, OrderVersion], State),
		% t_order_voucher
		{ok, _} = execute_statement(delete_order_voucher, [OrderId], State),
		{ok, 1} = execute_statement(create_order_voucher, [OrderId, VoucherId, OperationTimestamp], State),
		{ok, OrderVersion + 1}
	end,
	Result = with_transaction(Connection, TransactionFunction),
	{reply, Result, State};

handle_call({get_waypoint_details, DeliveryId, OrderId}, _From, State) ->
	return_multiple_results(get_waypoint_detail_by_delivery_id_order_id, [DeliveryId, OrderId], 0, fun(Row) -> build_waypoint_details(Row) end, State);

%
% Other operations
%
handle_call({get_notification_types}, _From, State) ->
	return_multiple_results(get_notification_types, [], 0, fun(Row) -> build_notification_type(Row) end, State);

handle_call({exists_notification_type, Id}, _From, State) ->
	return_exists(exists_notification_type, [Id], State);

handle_call({exists_client_courier_relation, ClientUserId, CourierUserId}, _From, State) ->
	return_exists(count_client_courier_deliveries, [ClientUserId, CourierUserId], State);

handle_call({get_order_objects_volume, OrderId}, _From, State) ->
	return_single_result(get_order_objects_volume, [OrderId], fun({Sum}) -> get_binary_to_float(Sum) end, State);

handle_call(Request, _From, State) ->
	error_logger:error_msg("~p: Unknown request ~p~n", [?MODULE, Request]),
	{reply, error, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{connection=Connection}) ->
	ok = epgsql:close(Connection),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
init_db(Properties) ->
	try
		Connection = connect(Properties),
		% t_user
		PSCountUsers = prepare_statement(Connection, "count_users", "SELECT COUNT(*) FROM t_user"),
		PSCountUsersByIdType = prepare_statement(Connection, "count_users_by_id_type", "SELECT COUNT(*) FROM t_user WHERE id_type = $1"),
		PSCountUsersByIdStatus = prepare_statement(Connection, "count_users_by_id_status", "SELECT COUNT(*) FROM t_user WHERE id_status = $1"),
		PSCountUsersByIdTypeAndIdStatus = prepare_statement(Connection, "count_users_by_id_type_and_id_status", "SELECT COUNT(*) FROM t_user WHERE id_type = $1 AND id_status = $2"),
		PSGetUsers = prepare_statement(Connection, "get_users", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user"),
		PSGetUsersIdAsc = prepare_statement(Connection, "get_users_id_asc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user ORDER BY t.id ASC"),
		PSGetUsersByIdTypeIdAsc = prepare_statement(Connection, "get_users_by_id_type_id_asc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_type = ANY($1) ORDER BY t.id ASC"),
		PSGetUsersByIdStatusIdAsc = prepare_statement(Connection, "get_users_by_id_status_id_asc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_status = $1 ORDER BY t.id ASC"),
		PSGetUsersByIdTypeAndIdStatusIdAsc = prepare_statement(Connection, "get_users_by_id_type_and_id_status_id_asc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_type = ANY($1) AND t.id_status = $2 ORDER BY t.id ASC"),
		PSGetUsersIdDesc = prepare_statement(Connection, "get_users_id_desc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user ORDER BY t.id DESC"),
		PSGetUsersByIdTypeIdDesc = prepare_statement(Connection, "get_users_by_id_type_id_desc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_type = ANY($1) ORDER BY t.id DESC"),
		PSGetUsersByIdStatusIdDesc = prepare_statement(Connection, "get_users_by_id_status_id_desc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_status = $1 ORDER BY t.id DESC"),
		PSGetUsersByIdTypeAndIdStatusIdDesc = prepare_statement(Connection, "get_users_by_id_type_and_id_status_id_desc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_type = ANY($1) AND t.id_status = $2 ORDER BY t.id DESC"),
		PSGetUsersByIdType = prepare_statement(Connection, "get_users_by_id_type", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_type = ANY($1)"),
		PSGetUsersByIdTypeAndIdStatus = prepare_statement(Connection, "get_users_by_id_type_and_id_status", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_type = ANY($1) AND t.id_status = $2"),
		PSGetUsersIdTypeAsc = prepare_statement(Connection, "get_users_id_type_asc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user ORDER BY t.id_type ASC"),
		PSGetUsersIdTypeDesc = prepare_statement(Connection, "get_users_id_type_desc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user ORDER BY t.id_type DESC"),
		PSGetUsersByIdStatusIdTypeAsc = prepare_statement(Connection, "get_users_by_id_status_id_type_asc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_status = $1 ORDER BY t.id_type ASC"),		
		PSGetUsersByIdStatusIdTypeDesc = prepare_statement(Connection, "get_users_by_id_status_id_type_desc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_status = $1 ORDER BY t.id_type DESC"),
		PSGetUsersByIdStatus = prepare_statement(Connection, "get_users_by_id_status", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id_status = $1"),
		PSGetUsersIdStatusAsc = prepare_statement(Connection, "get_users_id_status_asc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user ORDER BY id_status ASC"),
		PSGetUsersByIdTypeIdStatusAsc = prepare_statement(Connection, "get_users_by_id_type_id_status_asc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND id_type = ANY($1) ORDER BY id_status ASC"),
		PSGetUsersIdStatusDesc = prepare_statement(Connection, "get_users_id_status_desc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user ORDER BY id_status DESC"),
		PSGetUsersByIdTypeIdStatusDesc = prepare_statement(Connection, "get_users_by_id_type_id_status_desc", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND id_type = ANY($1) ORDER BY id_status DESC"),
		PSGetUser = prepare_statement(Connection, "get_user", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND id = $1"),
		PSCreateUser = prepare_statement(Connection, "create_user", "INSERT INTO t_user (id_type, id_status, creation_date, status_date, email, first_name, last_name, telephone_nr, fiscal_id, reference, mobileos_id, birth_day, birth_month, birth_year, national_id, country, rating, version) VALUES ($3, $1, $16, $16, $2, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, 1)"),
		PSUpdateUserStatus = prepare_statement(Connection, "update_user_status", "UPDATE t_user SET id_status = $3, status_date = $4, version = version + 1 WHERE id = $1 AND version = $2"),
		PSUpdateUser = prepare_statement(Connection, "update_user", "UPDATE t_user SET id_status = $4, status_date = $5, email = $3, first_name = $6, last_name = $7, telephone_nr = $8, fiscal_id = $9, mobileos_id = $10, birth_day = $11, birth_month = $12, birth_year = $13, national_id = $14, version = version + 1 WHERE id = $1 AND version = $2"),
		PSUpdateUserVersion = prepare_statement(Connection, "update_user_version", "UPDATE t_user SET version = version + 1 WHERE id = $1 AND version = $2"),
		PSSetUserLoginDate = prepare_statement(Connection, "set_user_login_date", "UPDATE t_user SET login_date = $2, version = version + 1 WHERE id = $1"),
		% t_user_oauth
		PSCountUserOauth = prepare_statement(Connection, "count_user_oauth", "SELECT COUNT(*) FROM t_user_oauth WHERE id_oauth = $1 AND id_oauth_provider = $2"),
		PSCreateUserOauth = prepare_statement(Connection, "create_user_oauth", "INSERT INTO t_user_oauth (id_user, id_oauth, id_oauth_provider) VALUES ($1, $2, $3)"),
		% t_user_auth
		PSCountUserAuth = prepare_statement(Connection, "count_user_auth", "SELECT COUNT(*) FROM t_user_auth WHERE username = $1"),
		PSCreateUserAuth = prepare_statement(Connection, "create_user_auth", "INSERT INTO t_user_auth (id_user, username, password) VALUES ($1, $2, get_password_hash($3))"),
		PSSetPassword = prepare_statement(Connection, "set_password", "UPDATE t_user_auth SET password = get_password_hash($2) WHERE id_user = $1"),
		PSSetUsername = prepare_statement(Connection, "set_username", "UPDATE t_user_auth SET username = $2 WHERE id_user = $1"),
		% t_user_address
		PSGetUserAddress = prepare_statement(Connection, "get_user_address", "SELECT id_user, component, value FROM t_user_address WHERE id_user = $1"),
		PSCreateUserAddress = prepare_statement(Connection, "create_user_address", "INSERT INTO t_user_address (id_user, component, value) VALUES ($1, $2, $3)"),
		PSDeleteUserAddress = prepare_statement(Connection, "delete_user_address", "DELETE FROM t_user_address WHERE id_user = $1"),
		% t_document
		PSGetUserDocument = prepare_statement(Connection, "get_user_document", "SELECT id, id_user, id_account, id_type, name, mimetype, base64_data, id_status, creation_date, status_date, version FROM t_document WHERE id = $2 AND id_user = $1 AND id_account IS NULL"),
		PSGetUserDocumentSummary = prepare_statement(Connection, "get_user_document_summary", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id = $2 AND id_user = $1 AND id_account IS NULL"),
		PSCreateDocument = prepare_statement(Connection, "create_document", "INSERT INTO t_document (id_user, id_account, id_type, name, mimetype, base64_data, id_status, creation_date, status_date, version) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $8, 1)"),
		PSUpdateUserDocumentStatus = prepare_statement(Connection, "update_user_document_status", "UPDATE t_document SET id_status = $4, status_date = $5, version = version + 1 WHERE id = $1 AND id_user = $2 AND version = $3"),
		PSGetUserDocumentsByIdUserAndIdTypeAndIdStatus = prepare_statement(Connection, "get_user_documents_by_id_user_and_id_type_and_id_status", "SELECT id, id_user, id_account, id_type, name, mimetype, base64_data, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_type = $2 AND id_status = $3 AND id_account IS NULL ORDER BY creation_date DESC"),
		PSGetUserDocumentsSummaryByIdUser = prepare_statement(Connection, "get_user_documents_summary_by_id_user", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_account IS NULL"),
		PSGetUserDocumentsSummaryByIdUserIdTypeAsc = prepare_statement(Connection, "get_user_documents_summary_by_id_user_id_type_asc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_account IS NULL ORDER BY id_type ASC"),
		PSGetUserDocumentsSummaryByIdUserIdTypeDesc = prepare_statement(Connection, "get_user_documents_summary_by_id_user_id_type_desc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_account IS NULL ORDER BY id_type DESC"),
		PSGetUserDocumentsSummaryByIdUserIdStatusAsc = prepare_statement(Connection, "get_user_documents_summary_by_id_user_id_status_asc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_account IS NULL ORDER BY id_status ASC"),
		PSGetUserDocumentsSummaryByIdUserIdStatusDesc = prepare_statement(Connection, "get_user_documents_summary_by_id_user_id_status_desc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_account IS NULL ORDER BY id_status DESC"),
		PSGetUserDocumentsSummaryByIdUserAndIdStatusIdTypeAsc = prepare_statement(Connection, "get_user_documents_summary_by_id_user_and_id_status_id_type_asc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_status = $2 AND id_account IS NULL ORDER BY id_type ASC"),
		PSGetUserDocumentsSummaryByIdUserAndIdStatusIdTypeDesc = prepare_statement(Connection, "get_user_documents_summary_by_id_user_and_id_status_id_type_desc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_status = $2 AND id_account IS NULL ORDER BY id_type DESC"),
		PSGetUserDocumentsSummaryByIdUserAndIdStatus = prepare_statement(Connection, "get_user_documents_summary_by_id_user_and_id_status", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_status = $2 AND id_account IS NULL"),
		PSGetUserDocumentsSummaryByIdUserAndIdTypeIdStatusAsc = prepare_statement(Connection, "get_user_documents_summary_by_id_user_and_id_type_id_status_asc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_type = $2 AND id_account IS NULL ORDER BY id_status ASC"),
		PSGetUserDocumentsSummaryByIdUserAndIdTypeIdStatusDesc = prepare_statement(Connection, "get_user_documents_summary_by_id_user_and_id_type_id_status_desc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_type = $2 AND id_account IS NULL ORDER BY id_status DESC"),
		PSGetUserDocumentsSummaryByIdUserAndIdType = prepare_statement(Connection, "get_user_documents_summary_by_id_user_and_id_type", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_type = $2 AND id_account IS NULL"),
		PSGetUserDocumentsSummaryByIdUserAndIdTypeAndIdStatus = prepare_statement(Connection, "get_user_documents_summary_by_id_user_and_id_type_and_id_status", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_user = $1 AND id_type = $2 AND id_status = $3 AND id_account IS NULL"),
		PSGetAccountDocument = prepare_statement(Connection, "get_account_document", "SELECT id, id_user, id_account, id_type, name, mimetype, base64_data, id_status, creation_date, status_date, version FROM t_document WHERE id = $2 AND id_account = $1"),
		PSGetAccountDocumentSummary = prepare_statement(Connection, "get_account_document_summary", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id = $2 AND id_account = $1"),
		PSUpdateAccountDocumentStatus = prepare_statement(Connection, "update_account_document_status", "UPDATE t_document SET id_status = $4, status_date = $5, version = version + 1 WHERE id = $1 AND id_account = $2 AND version = $3"),
		PSGetAccountDocumentsByIdAccountAndIdTypeAndIdStatus = prepare_statement(Connection, "get_account_documents_by_id_account_and_id_type_and_id_status", "SELECT id, id_user, id_account, id_type, name, mimetype, base64_data, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 AND id_type = $2 AND id_status = $3"),
		PSGetAccountDocumentsSummaryByIdAccount = prepare_statement(Connection, "get_account_documents_summary_by_id_account", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1"),
		PSGetAccountDocumentsSummaryByIdAccountIdTypeAsc = prepare_statement(Connection, "get_account_documents_summary_by_id_account_id_type_asc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 ORDER BY id_type ASC"),
		PSGetAccountDocumentsSummaryByIdAccountIdTypeDesc = prepare_statement(Connection, "get_account_documents_summary_by_id_account_id_type_desc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 ORDER BY id_type DESC"),
		PSGetAccountDocumentsSummaryByIdAccountIdStatusAsc = prepare_statement(Connection, "get_account_documents_summary_by_id_account_id_status_asc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 ORDER BY id_status ASC"),
		PSGetAccountDocumentsSummaryByIdAccountIdStatusDesc = prepare_statement(Connection, "get_account_documents_summary_by_id_account_id_status_desc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 ORDER BY id_status DESC"),
		PSGetAccountDocumentsSummaryByIdAccountAndIdStatusIdTypeAsc = prepare_statement(Connection, "get_acc_documents_summary_by_id_account_and_id_status_id_type_asc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 AND id_status = $2 ORDER BY id_type ASC"),
		PSGetAccountDocumentsSummaryByIdAccountAndIdStatusIdTypeDesc = prepare_statement(Connection, "get_acc_documents_summary_by_id_account_and_id_status_id_type_desc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 AND id_status = $2 ORDER BY id_type DESC"),
		PSGetAccountDocumentsSummaryByIdAccountAndIdStatus = prepare_statement(Connection, "get_account_documents_summary_by_id_account_and_id_status", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 AND id_status = $2"),
		PSGetAccountDocumentsSummaryByIdAccountAndIdTypeIdStatusAsc = prepare_statement(Connection, "get_acc_documents_summary_by_id_account_and_id_type_id_status_asc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 AND id_type = $2 ORDER BY id_status ASC"),
		PSGetAccountDocumentsSummaryByIdAccountAndIdTypeIdStatusDesc = prepare_statement(Connection, "get_acc_documents_summary_by_id_account_and_id_type_id_status_desc", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 AND id_type = $2 ORDER BY id_status DESC"),
		PSGetAccountDocumentsSummaryByIdAccountAndIdType = prepare_statement(Connection, "get_account_documents_summary_by_id_account_and_id_type", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 AND id_type = $2"),
		PSGetAccountDocumentsSummaryByIdAccountAndIdTypeAndIdStatus = prepare_statement(Connection, "get_account_documents_summary_by_id_account_and_id_type_and_id_status", "SELECT id, id_user, id_account, id_type, name, id_status, creation_date, status_date, version FROM t_document WHERE id_account = $1 AND id_type = $2 AND id_status = $3"),
		PSDeleteAccountDocument = prepare_statement(Connection, "delete_account_document", "DELETE FROM t_document WHERE id = $2 AND id_account = $1"),
		PSDeleteDocumentByIdUserAndIdType = prepare_statement(Connection, "delete_document_by_id_user_and_id_type", "DELETE FROM t_document WHERE id_user = $1 AND id_type = $2"),
		% t_parameterization
		PSGetParameterizations = prepare_statement(Connection, "get_parameterizations", "SELECT id, value, description, version FROM t_parameterization"),
		PSGetParameterization = prepare_statement(Connection, "get_parameterization", "SELECT id, value, description, version FROM t_parameterization WHERE id = $1"),
		PSUpdateParameterization = prepare_statement(Connection, "update_parameterization", "UPDATE t_parameterization SET value = $3, version = version + 1 WHERE id = $1 AND version = $2"),
		% t_user_type
		PSGetUserTypes = prepare_statement(Connection, "get_user_types", "SELECT id, description FROM t_user_type"),
		PSCountUserTypes = prepare_statement(Connection, "count_user_types", "SELECT COUNT(*) FROM t_user_type WHERE id = $1"),
		% t_user_status
		PSGetUserStatuses = prepare_statement(Connection, "get_user_statuses", "SELECT id, description FROM t_user_status"),
		PSCountUserStatuses = prepare_statement(Connection, "count_user_statuses", "SELECT COUNT(*) FROM t_user_status WHERE id = $1"),
		% t_document_type
		PSGetDocumentTypes = prepare_statement(Connection, "get_document_types", "SELECT id, description FROM t_document_type"),
		PSCountDocumentTypes = prepare_statement(Connection, "count_document_types", "SELECT COUNT(*) FROM t_document_type WHERE id = $1"),
		% t_document_status
		PSGetDocumentStatuses = prepare_statement(Connection, "get_document_statuses", "SELECT id, description FROM t_document_status"),
		PSCountDocumentStatuses = prepare_statement(Connection, "count_document_statuses", "SELECT COUNT(*) FROM t_document_status WHERE id = $1"),
		% t_transport_type
		PSGetTransportTypes = prepare_statement(Connection, "get_transport_types", "SELECT id, description, capacity FROM t_transport_type"),
		PSGetTransportType = prepare_statement(Connection, "get_transport_type", "SELECT id, description, capacity FROM t_transport_type WHERE id = $1"),
		PSCountTransportTypes = prepare_statement(Connection, "count_transport_types", "SELECT COUNT(*) FROM t_transport_type WHERE id = $1"),
		% t_courier_transport_status
		PSCountCourierTransportStatus = prepare_statement(Connection, "count_courier_transport_status", "SELECT COUNT(*) FROM t_courier_transport_status WHERE id = $1"),
		% t_user_notification_type
		PSGetUserNotificationTypes = prepare_statement(Connection, "get_user_notification_types", "SELECT id, description FROM t_user_notification_type"),
		PSCountUserNotificationTypes = prepare_statement(Connection, "count_user_notification_types", "SELECT COUNT(*) FROM t_user_notification_type WHERE id = $1"),
		% t_order_payment_method
		PSGetOrderPaymentMethods = prepare_statement(Connection, "get_order_payment_methods", "SELECT id, description FROM t_order_payment_method"),
		PSExistsOrderPaymentMethod = prepare_statement(Connection, "exists_order_payment_method", "SELECT id FROM t_order_payment_method WHERE id = $1"),
		% t_mobileos
		PSGetMobileos = prepare_statement(Connection, "get_mobileos", "SELECT id, description FROM t_mobileos"),
		PSExistsMobileos = prepare_statement(Connection, "exists_mobileos", "SELECT id FROM t_mobileos WHERE id = $1"),
		% t_courier_transport_status
		PSGetCourierTransportStatuses = prepare_statement(Connection, "get_courier_transport_status", "SELECT id, description FROM t_courier_transport_status"),
		% t_courier_transport
		PSGetCourierTransports = prepare_statement(Connection, "get_courier_transports", "SELECT id, id_user, id_transport_type, current, id_status, description, registration_id, color, creation_date, status_date FROM t_courier_transport WHERE id_user = $1"),
		PSGetCourierTransport = prepare_statement(Connection, "get_courier_transport", "SELECT id, id_user, id_transport_type, current, id_status, description, registration_id, color, creation_date, status_date FROM t_courier_transport WHERE id = $1"),
		PSGetActiveCourierTransports = prepare_statement(Connection, "get_active_courier_transports", "SELECT id, id_user, id_transport_type, current, id_status, description, registration_id, color, creation_date, status_date FROM t_courier_transport WHERE id_user = $1"),
		PSGetCurrentCourierTransport = prepare_statement(Connection, "get_current_courier_transport", "SELECT id, id_user, id_transport_type, current, id_status, description, registration_id, color, creation_date, status_date FROM t_courier_transport WHERE id_user = $1 and current = true"),
		PSCreateCourierTransport = prepare_statement(Connection, "create_courier_transport", "INSERT INTO t_courier_transport (id_user, id_transport_type, current, id_status, creation_date, status_date, description, registration_id, color) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)"),
		PSUpdateCourierTransport = prepare_statement(Connection, "update_courier_transport", "UPDATE t_courier_transport SET id_status = $1, status_date = $2 WHERE id = $3"),
		PSUpdateCurrentCourierTransport = prepare_statement(Connection, "update_current_courier_transport", "UPDATE t_courier_transport SET current = $1, status_date = $2 WHERE id = $3 AND current = $4"),
		% t_contact_request_status
		PSGetContactRequestStatuses = prepare_statement(Connection, "get_contact_request_statuses", "SELECT id, description FROM t_contact_request_status"),
		PSCountContactRequestStatuses = prepare_statement(Connection, "count_contact_request_statuses", "SELECT COUNT(*) FROM t_contact_request_status WHERE id = $1"),
		% t_contact_request
		PSGetContactRequests = prepare_statement(Connection, "get_contact_requests", "SELECT id, email, name, subject, content, id_status, creation_date, status_date, id_user, id_operator_user, operator_notes, version FROM t_contact_request"),
		PSGetContactRequest = prepare_statement(Connection, "get_contact_request", "SELECT id, email, name, subject, content, id_status, creation_date, status_date, id_user, id_operator_user, operator_notes, version FROM t_contact_request WHERE id = $1"),
		PSCreateContactRequest = prepare_statement(Connection, "create_contact_request", "INSERT INTO t_contact_request (email, name, subject, content, id_status, creation_date, status_date, id_user, version) VALUES ($1, $2, $3, $4, $5, $7, $7, $6, 1)"),
		PSUpdateContactRequest = prepare_statement(Connection, "update_contact_request", "UPDATE t_contact_request SET id_status = $1, id_user = $2, operator_notes = $3, id_operator_user = $4, status_date = $5, version = version + 1 WHERE id = $6 AND version = $7"),
		PSDeleteContactRequest = prepare_statement(Connection, "delete_contact_request", "DELETE FROM t_contact_request WHERE id = $1 and version = $2"),
		PSCountContactStatuses = prepare_statement(Connection, "count_contact_status", "SELECT COUNT(*) FROM t_contact_request_status WHERE id = $1"),
		% t_token_type
		PSGetTokenTypes = prepare_statement(Connection, "get_token_types", "SELECT id, description, multiple_uses, uses, expires_in_minutes, version FROM t_token_type"),
		PSGetTokenType = prepare_statement(Connection, "get_token_type", "SELECT id, description, multiple_uses, uses, expires_in_minutes, version FROM t_token_type WHERE id = $1"),
		PSUpdateTokenType = prepare_statement(Connection, "update_token_type", "UPDATE t_token_type SET multiple_uses = $3, uses = $4, expires_in_minutes = $5, version = version + 1 WHERE id = $1 AND version = $2"),
		% t_token
		PSCreateToken = prepare_statement(Connection, "create_token", "INSERT INTO t_token (id, id_type, creation_date, remaining_uses, invalid_after, version) VALUES ($1, $2, $5, $3, $4, 1)"),
		PSGetToken = prepare_statement(Connection, "get_token", "SELECT id, id_type, creation_date, remaining_uses, invalid_after, version FROM t_token WHERE id = $1"),
		PSRemoveToken = prepare_statement(Connection, "remove_token", "DELETE FROM t_token WHERE id = $1"),
		PSDecreaseTokenUses = prepare_statement(Connection, "decrease_token_uses", "UPDATE t_token SET remaining_uses = remaining_uses - 1, version = version + 1 WHERE id = $1 AND version = $2"),
		% t_token_parameter
		PSGetTokenParameters = prepare_statement(Connection, "get_token_parameters", "SELECT id_token, id_type, value FROM t_token_parameter WHERE id_token = $1 ORDER BY id_type"),
		PSCreateTokenParameter = prepare_statement(Connection, "create_token_parameter", "INSERT INTO t_token_parameter (id_token, id_type, value) VALUES ($1, $2, $3)"),
		PSRemoveTokenParameter = prepare_statement(Connection, "remove_token_parameter", "DELETE FROM t_token_parameter WHERE id_token = $1"),
		% t_account_status
		PSGetAccountStatuses = prepare_statement(Connection, "get_account_statuses", "SELECT id, description FROM t_account_status"),
		% t_account_user_type
		PSGetAccountUserTypes = prepare_statement(Connection, "get_account_user_types", "SELECT id, description FROM t_account_user_type"),
		PSCountAccountUserTypes = prepare_statement(Connection, "count_account_user_types", "SELECT COUNT(*) FROM t_account_user_type WHERE id = $1"),
		% t_account
		PSGetAccounts = prepare_statement(Connection, "get_accounts", "SELECT id, unique_key, id_status, account_name, fiscal_id, telephone_nr, email, creation_date, status_date, occupancy, version FROM t_account"),
		PSGetAccount = prepare_statement(Connection, "get_account", "SELECT id, unique_key, id_status, account_name, fiscal_id, telephone_nr, email, creation_date, status_date, occupancy, version FROM t_account WHERE id = $1"),
		PSCountAccountsByUniqueKey = prepare_statement(Connection, "count_accounts_by_unique_key", "SELECT COUNT(*) FROM t_account WHERE unique_key = $1"),
		PSCountAccountById = prepare_statement(Connection, "count_account_by_id", "SELECT COUNT(*) FROM t_account WHERE id = $1"),
		PSCreateAccount = prepare_statement(Connection, "create_account", "INSERT INTO t_account (unique_key, id_status, account_name, fiscal_id, telephone_nr, email, contract_nr, creation_date, status_date, version) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $8, 1)"),
		PSIncrementAccountVersion = prepare_statement(Connection, "increment_account_version", "UPDATE t_account SET version = version + 1 WHERE id = $1 AND version = $2"),
		PSVerifyAccountAdmin = prepare_statement(Connection, "verify_account_admin", "SELECT COUNT(*) FROM t_account_user au, t_account_user au2 WHERE au.id_account = au2.id_account AND au.id_user = $1 AND au.id_type = 1 AND au2.id_user = $2"),
		PSUpdateAccount = prepare_statement(Connection, "update_account", "UPDATE t_account SET account_name = $3, fiscal_id = $4, telephone_nr = $5, email = $6, version = version + 1 WHERE id = $1 AND version = $2"),
		PSUpdateAccountOccupancy = prepare_statement(Connection, "update_account_occupancy", "UPDATE t_account SET occupancy = $3, status_date = $4, version = version + 1 WHERE id = $1 AND version = $2"),
		% t_account_occupancy_type
		PSCountOccupancyType = prepare_statement(Connection, "count_ocupancy_type", "SELECT COUNT(*) FROM t_account_occupancy_type WHERE id = $1"),		
		% t_account_info
		PSGetAccountInfos = prepare_statement(Connection, "get_account_infos", "SELECT id_account, property, value FROM t_account_info WHERE id_account = $1"),
		PSCreateAccountInfo = prepare_statement(Connection, "create_account_info", "INSERT INTO t_account_info(id_account, property, value) VALUES ($1, $2, $3)"),
		PSDeleteAccountInfo = prepare_statement(Connection, "delete_account_info", "DELETE FROM t_account_info WHERE id_account = $1"),
		% t_account_address
		PSGetAccountAddress = prepare_statement(Connection, "get_account_address", "SELECT id_account, component, value FROM t_account_address WHERE id_account = $1"),
		PSCreateAccountAddress = prepare_statement(Connection, "create_account_address", "INSERT INTO t_account_address (id_account, component, value) VALUES ($1, $2, $3)"),
		PSDeleteAccountAddress = prepare_statement(Connection, "delete_account_address", "DELETE FROM t_account_address WHERE id_account = $1"),
		% t_account_user
		PSCreateAccountUser = prepare_statement(Connection, "create_account_user", "INSERT INTO t_account_user (id_account, id_user, id_type, id_department, id_cost_center) VALUES ($1, $2, $3, $4, $5)"),
		PSDeleteAccountUser = prepare_statement(Connection, "delete_account_user", "DELETE FROM t_account_user WHERE id_account = $1 AND id_user = $2"),
		PSGetAccountUser = prepare_statement(Connection, "get_account_user", "SELECT id_account, id_user, id_type, id_department, id_cost_center FROM t_account_user WHERE id_user = $1 AND id_account = $2"), 
		PSGetAccountUserByIdUser = prepare_statement(Connection, "get_account_user_by_id_user", "SELECT id_account, id_user, id_type, id_department, id_cost_center FROM t_account_user WHERE id_user = $1"), 
		PSUpdateAccountUser = prepare_statement(Connection, "update_account_user", "UPDATE t_account_user SET id_type = $3, id_department = $4, id_cost_center = $5 WHERE id_account = $1 AND id_user = $2"), 
		PSExistsAccountUser = prepare_statement(Connection, "exists_account_user", "SELECT id_account, id_user FROM t_account_user WHERE id_user = $1 AND id_account = $2"), 
		% t_account_department
		PSGetDepartments = prepare_statement(Connection, "get_departments", "SELECT id_department, id_account, description FROM t_account_department WHERE id_account = $1"),
		PSExistsAccountDepartment = prepare_statement(Connection, "exists_account_department", "SELECT id_department, id_account FROM t_account_department WHERE id_account = $1 AND id_department = $2"),
		% t_delivery_status
		PSGetDeliveryStatuses = prepare_statement(Connection, "get_delivery_statuses", "SELECT id, description FROM t_delivery_status"),
		PSCountDeliveryStatuses = prepare_statement(Connection, "count_delivery_statuses", "SELECT COUNT(*) FROM t_delivery_status WHERE id = $1"),
 		% t_delivery
		PSGetDeliveries = prepare_statement(Connection, "get_deliveries", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery"),
		PSGetOngoingDeliveries = prepare_statement(Connection, "get_ongoing_deliveries", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status IN (1, 2, 3, 4, 5, 9, 15) ORDER BY id ASC"),
		PSGetDeliveriesIdAsc = prepare_statement(Connection, "get_deliveries_id_asc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery ORDER BY id ASC"),
		PSGetDeliveriesIdDesc = prepare_statement(Connection, "get_deliveries_id_desc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery ORDER BY id DESC"),
		PSGetDeliveriesIdCourierAsc = prepare_statement(Connection, "get_deliveries_id_courier_asc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery ORDER BY id_courier ASC"),
		PSGetDeliveriesIdCourierDesc = prepare_statement(Connection, "get_deliveries_id_courier_desc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery ORDER BY id_courier DESC"),
		PSGetDeliveriesByIdStatusIdAsc = prepare_statement(Connection, "get_deliveries_by_id_status_id_asc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1 ORDER BY id ASC"),
		PSGetDeliveriesByIdStatusIdDesc = prepare_statement(Connection, "get_deliveries_by_id_status_id_desc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1 ORDER BY id DESC"),
		PSGetDeliveriesByIdStatusIdCourierAsc = prepare_statement(Connection, "get_deliveries_by_id_status_id_courier_asc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1 ORDER BY id_courier ASC"),
		PSGetDeliveriesByIdStatusIdCourierDesc = prepare_statement(Connection, "get_deliveries_by_id_status_id_courier_desc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1 ORDER BY id_courier DESC"),
		PSGetDeliveriesByIdStatus = prepare_statement(Connection, "get_deliveries_by_id_status", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1"),
		PSGetDeliveriesByIdCourierIdAsc = prepare_statement(Connection, "get_deliveries_by_id_courier_id_asc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_courier = $1 ORDER BY id ASC"),
		PSGetDeliveriesByIdCourierIdDesc = prepare_statement(Connection, "get_deliveries_by_id_courier_id_desc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_courier = $1 ORDER BY id DESC"),
		PSGetDeliveriesByIdCourier = prepare_statement(Connection, "get_deliveries_by_id_courier", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_courier = $1"),
		PSGetDeliveriesByIdCourierAndIdStatusIdAsc = prepare_statement(Connection, "get_deliveries_by_id_courier_and_id_status_id_asc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_courier = $1 AND id_status = $2 ORDER BY id ASC"),
		PSGetDeliveriesByIdCourierAndIdStatusIdDesc = prepare_statement(Connection, "get_deliveries_by_id_courier_and_id_status_id_desc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_courier = $1 AND id_status = $2 ORDER BY id DESC"),
		PSGetDeliveriesByIdCourierAndIdStatus = prepare_statement(Connection, "get_deliveries_by_id_courier_and_id_status", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_courier = $1 AND id_status = $2"),
		PSGetDeliveriesByIdCourierIdCourierAsc = prepare_statement(Connection, "get_deliveries_by_id_courier_id_courier_asc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_courier = $1 ORDER BY id_courier ASC"),
		PSGetDeliveriesByIdCourierIdCourierDesc = prepare_statement(Connection, "get_deliveries_by_id_courier_id_courier_desc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_courier = $1 ORDER BY id_courier DESC"),
		PSGetDeliveriesByIdStatusIdCourierIdAsc = prepare_statement(Connection, "get_deliveries_by_id_status_id_courier_id_asc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1 AND id_courier = $2 ORDER BY id ASC"),
		PSGetDeliveriesByIdStatusIdCourierIdDesc = prepare_statement(Connection, "get_deliveries_by_id_status_id_courier_id_desc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1 AND id_courier = $2 ORDER BY id DESC"),
		PSGetDeliveriesByIdStatusIdCourierIdCourierAsc = prepare_statement(Connection, "get_deliveries_by_id_status_id_courier_id_courier_asc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1 AND id_courier = $2 ORDER BY id_courier ASC"),
		PSGetDeliveriesByIdStatusIdCourierIdCourierDesc = prepare_statement(Connection, "get_deliveries_by_id_status_id_courier_id_courier_desc", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1 AND id_courier = $2 ORDER BY id_courier DESC"),
		PSGetDeliveriesByIdStatusIdCourier = prepare_statement(Connection, "get_deliveries_by_id_status_id_courier", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id_status = $1 AND id_courier = $2"),		PSGetDeliveryByIdIdCourier = prepare_statement(Connection, "get_delivery_by_id_id_courier", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id = $1 AND id_courier = $2"),
		PSGetDelivery = prepare_statement(Connection, "get_delivery", "SELECT id, reference, id_transport_type, id_transport, distance, duration, route, id_courier, skip, cut_time, id_status, creation_date, status_date, version FROM t_delivery WHERE id = $1"),
		PSCreateDelivery = prepare_statement(Connection, "create_delivery", "INSERT INTO t_delivery (reference, id_transport_type, distance, duration, route, cut_time, id_status, creation_date, status_date, version) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $8, 1)"),
		PSUpdateDeliveryVersion = prepare_statement(Connection, "update_delivery_version", "UPDATE t_delivery SET version = version + 1 WHERE id = $1 AND version = $2"),
		PSUpdateDeliveryStatus = prepare_statement(Connection, "update_delivery_status", "UPDATE t_delivery SET id_status = $3, status_date = $4, version = version + 1 WHERE id = $1 AND version = $2"),
		PSAssignDeliveryDispatcher = prepare_statement(Connection, "assign_delivery_dispatcher", "UPDATE t_delivery SET id_status = $3, status_date = $4, id_courier = $5, version = version + 1 WHERE id = $1 AND version = $2"),
		PSDeleteDelivery = prepare_statement(Connection, "delete_delivery", "DELETE FROM t_delivery WHERE id = $1"),
		PSDeleteDeliveryWaypoint = prepare_statement(Connection, "delete_delivery_waypoint", "DELETE FROM t_delivery_waypoint WHERE id_delivery = $1"),
		PSDeleteDeliveryObject = prepare_statement(Connection, "delete_delivery_object", "DELETE FROM t_delivery_object WHERE id_delivery = $1"),
		PSDeleteDeliveryObjectAction = prepare_statement(Connection, "delete_delivery_object_action", "DELETE FROM t_delivery_waypoint_object_action WHERE id_delivery = $1"),
		PSDeleteDeliveryCourierRoute = prepare_statement(Connection, "delete_delivery_courier_route", "DELETE FROM t_delivery_courier_route WHERE id_delivery = $1"),
		% t_generated_delivery
		PSGetGeneratedDeliveryByIdDelivery = prepare_statement(Connection, "get_generated_delivery_by_id_delivery", "SELECT id_delivery, id_order FROM t_generated_delivery WHERE id_delivery = $1"),
		PSGetGeneratedDeliveriesByIdOrder = prepare_statement(Connection, "get_generated_deliveries_by_id_order", "SELECT id_delivery, id_order FROM t_generated_delivery WHERE id_order = $1"),
		PSCreateGeneratedDelivery = prepare_statement(Connection, "create_generated_delivery", "INSERT INTO t_generated_delivery (id_delivery, id_order) VALUES ($1, $2)"),
		PSDeleteGeneratedDelivery = prepare_statement(Connection, "delete_generated_delivery", "DELETE FROM t_generated_delivery WHERE id_delivery = $1  AND id_order = $2"),
		PSDeleteGeneratedDeliveryByIdDelivery = prepare_statement(Connection, "delete_generated_delivery_by_id_delivery", "DELETE FROM t_generated_delivery WHERE id_delivery = $1"),
		% t_generated_delivery_waypoint
		PSGetGeneratedDeliveryWaypoints = prepare_statement(Connection, "get_generated_delivery_waypoints", "SELECT id_delivery, id_delivery_waypoint, id_order, id_waypoint FROM t_generated_delivery_waypoint WHERE id_delivery = $1"),
		PSCreateGeneratedDeliveryWaypoint = prepare_statement(Connection, "create_generated_delivery_waypoint", "INSERT INTO t_generated_delivery_waypoint (id_delivery, id_delivery_waypoint, id_order, id_waypoint) VALUES ($1, $2, $3, $4)"),
		PSDeleteGeneratedDeliveryWaypoint = prepare_statement(Connection, "delete_generated_delivery_waypoint", "DELETE FROM t_generated_delivery_waypoint WHERE id_delivery = $1 AND id_order = $2"),
		PSDeleteGeneratedDeliveryWaypointByIdDelivery = prepare_statement(Connection, "delete_generated_delivery_waypoint_by_id_delivery", "DELETE FROM t_generated_delivery_waypoint WHERE id_delivery = $1"),
		% t_generated_delivery_object
		PSGetGeneratedDeliveryObjects = prepare_statement(Connection, "get_generated_delivery_objects", "SELECT id_delivery, id_delivery_object, id_order, id_object FROM t_generated_delivery_object WHERE id_delivery = $1"),
		PSCreateGeneratedDeliveryObject = prepare_statement(Connection, "create_generated_delivery_object", "INSERT INTO t_generated_delivery_object (id_delivery, id_delivery_object, id_order, id_object) VALUES ($1, $2, $3, $4)"),
		PSDeleteGeneratedDeliveryObject = prepare_statement(Connection, "delete_generated_delivery_object", "DELETE FROM t_generated_delivery_object WHERE id_delivery = $1 AND id_order = $2"),
		PSDeleteGeneratedDeliveryObjectByIdDelivery = prepare_statement(Connection, "delete_generated_delivery_object_by_id_delivery", "DELETE FROM t_generated_delivery_object WHERE id_delivery = $1"),
		% t_order_status
		PSGetOrderStatuses = prepare_statement(Connection, "get_order_statuses", "SELECT id, description FROM t_order_status"),
		PSExistsOrderStatus = prepare_statement(Connection, "exists_order_status", "SELECT id FROM t_order_status WHERE id = $1"),
		% t_order_origin
		PSGetOrderOrigins = prepare_statement(Connection, "get_order_origins", "SELECT id, description FROM t_order_origin"),
		% t_order_type
		PSGetOrderTypes = prepare_statement(Connection, "get_order_types", "SELECT id, description FROM t_order_type"),
		% t_order
		GetOrderFields = "id, reference, distribution_center, id_user, id_type, id_transport_type, id_origin, client_name, client_phone_nr, client_email, client_fiscal_id, cut_time, id_status, id_payment, id_payment_method, diner_qty, gross_total, tip, fee, creation_date, status_date, start_prod_date, version",
		GetOrderJoinFields = "o.id, o.reference, o.distribution_center, o.id_user, o.id_type, o.id_transport_type, o.id_origin, o.client_name, o.client_phone_nr, o.client_email, o.client_fiscal_id, o.cut_time, o.id_status, o.id_payment, o.id_payment_method, o.diner_qty, o.gross_total, o.tip, o.fee, o.creation_date, o.status_date, o.start_prod_date, o.version",
		PSGetLastUserOrder = prepare_statement(Connection, "get_last_user_order", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 ORDER BY creation_date DESC LIMIT 1"),
		PSCreateOrder = prepare_statement(Connection, "create_order", "INSERT INTO t_order (reference, distribution_center, id_user, id_type, id_transport_type, id_origin, client_name, client_phone_nr, client_email, client_fiscal_id, cut_time, id_status, id_payment, id_payment_method, diner_qty, gross_total, tip, fee, creation_date, status_date, version) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $19, 1)"),
		PSUpdateOrderCutTime = prepare_statement(Connection, "update_order_cut_time", "UPDATE t_order SET cut_time = $4, status_date = $5, version = version + 1 WHERE id = $1 AND version = $2 AND id_status = ANY($3) AND NOT EXISTS (SELECT * FROM t_generated_delivery gd WHERE gd.id_order = id)"),
		PSUpdateOrderStatus = prepare_statement(Connection, "update_order_status", "UPDATE t_order SET id_status = $3, status_date = $4, version = version + 1 WHERE id = $1 AND version = $2"),
		PSUpdateOrderStatusProd = prepare_statement(Connection, "update_order_status_prod", "UPDATE t_order SET id_status = $3, status_date = $4, start_prod_date = $4, version = version + 1 WHERE id = $1 AND version = $2"),
		PSUpdateOrderCutTimeProductionTime = prepare_statement(Connection, "update_order_cut_time_production_time", "UPDATE t_order SET id_status = $3, cut_time = $4, status_date = $5, start_prod_date = $5, version = version + 1 WHERE id = $1 AND version = $2"),
		PSGetOrder = prepare_statement(Connection, "get_order", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id = $1"),
		PSGetOrderByIdIdUser = prepare_statement(Connection, "get_order_by_id_id_user", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id = $1 AND id_user = $2"),
		PSGetOrdersByIdDelivery = prepare_statement(Connection, "get_orders_by_id_delivery", "SELECT " ++ GetOrderFields ++ " FROM t_order o, t_generated_delivery tgd WHERE tgd.id_delivery = $1 AND tgd.id_order = o.id"),
		PSGetOrders = prepare_statement(Connection, "get_orders", "SELECT " ++ GetOrderFields ++ " FROM t_order"),
		PSGetOrdersAsc = prepare_statement(Connection, "get_orders_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order ORDER BY id ASC"),
		PSGetOrdersDesc = prepare_statement(Connection, "get_orders_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order ORDER BY id DESC"),
		PSGetOrdersByIdAsc = prepare_statement(Connection, "get_orders_by_id_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id = $1 ORDER BY id ASC"),
		PSGetOrdersByIdDesc = prepare_statement(Connection, "get_orders_by_id_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id = $1 ORDER BY id DESC"),
		PSGetOrdersByIdStatusIdAsc = prepare_statement(Connection, "get_orders_by_id_status_id_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 ORDER BY id ASC"),
		PSGetOrdersByIdStatusIdDesc = prepare_statement(Connection, "get_orders_by_id_status_id_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 ORDER BY id DESC"),
		PSGetOrdersByIdStatus = prepare_statement(Connection, "get_orders_by_id_status", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1"),
		PSGetOrdersByIdType = prepare_statement(Connection, "get_orders_by_id_type", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_type = $1"),
		PSGetOrdersByIdUserIdAsc = prepare_statement(Connection, "get_orders_by_id_user_id_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 ORDER BY id ASC"),
		PSGetOrdersByIdUserIdDesc = prepare_statement(Connection, "get_orders_by_id_user_id_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 ORDER BY id DESC"),
		PSGetOrdersByIdUser = prepare_statement(Connection, "get_orders_by_id_user", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1"),
		PSGetOrdersByIdUserAndIdStatusIdAsc = prepare_statement(Connection, "get_orders_by_id_user_and_id_status_id_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 ORDER BY id ASC"),
		PSGetOrdersByIdUserAndIdStatusIdDesc = prepare_statement(Connection, "get_orders_by_id_user_and_id_status_id_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 ORDER BY id DESC"),
		PSGetOrdersByIdUserAndIdStatus = prepare_statement(Connection, "get_orders_by_id_user_and_id_status", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2"),
		PSGetOrdersForGeneration = prepare_statement(Connection, "get_orders_for_generation", "SELECT " ++ GetOrderJoinFields ++ " FROM t_order o WHERE id_status = ANY($1) AND NOT EXISTS (SELECT * FROM t_generated_delivery gd WHERE gd.id_order = o.id) AND (cut_time IS NULL OR cut_time < $2 OR (cut_time < $3 AND EXISTS (SELECT * FROM t_order o2 WHERE o2.id_status = ANY($1) AND NOT EXISTS (SELECT * FROM t_generated_delivery gd2 WHERE gd2.id_order = o2.id) AND o2.cut_time < $2 AND o2.id_type = $4))) AND o.id_type = $4 ORDER BY o.id"),
		PSGetDispatcherOrdersByIdStatusAndIdType = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_by_id_type", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND id_type = $2"),
		PSGetDispatcherOrdersByIdStatusAndIdTypeAsc = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_by_id_type_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND id_type = $2 ORDER BY id ASC"),
		PSGetDispatcherOrdersByIdStatusAndIdTypeDesc = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_by_id_type_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND id_type = $2 ORDER BY id DESC"),
		PSGetDispatcherOrdersByIdStatusAsc = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 ORDER BY id ASC"),
		PSGetDispatcherOrdersByIdStatusDesc = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 ORDER BY id DESC"),
		PSGetDispatcherOrdersByIdStatus = prepare_statement(Connection, "get_dispatcher_orders_by_id_status", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1"),
		PSGetDispatcherOrdersByIdTypeAsc = prepare_statement(Connection, "get_dispatcher_orders_by_id_type_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_type = $1 AND id_status IN (1, 2, 3, 4) ORDER BY id ASC"),
		PSGetDispatcherOrdersByIdTypeDesc = prepare_statement(Connection, "get_dispatcher_orders_by_id_type_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_type = $1 AND id_status IN (1, 2, 3, 4) ORDER BY id DESC"),
		PSGetDispatcherOrdersByIdType = prepare_statement(Connection, "get_dispatcher_orders_by_id_type", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_type = $1 AND id_status IN (1, 2, 3, 4)"),
		PSGetDispatcherOrders = prepare_statement(Connection, "get_dispatcher_orders", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status IN (1, 2, 3, 4)"),
		PSGetDispatcherOrdersAsc = prepare_statement(Connection, "get_dispatcher_orders_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status IN (1, 2, 3, 4) ORDER BY id ASC"),
		PSGetDispatcherOrdersDesc = prepare_statement(Connection, "get_dispatcher_orders_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status IN (1, 2, 3, 4) ORDER BY id DESC"),
		PSExistsOrderPayment = prepare_statement(Connection, "exists_order_payment", "SELECT id FROM t_order WHERE id_payment = $1"),
		PSGetOrdersByPhoneNrAsc = prepare_statement(Connection, "get_orders_by_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE client_phone_nr = $1 ORDER BY id ASC"),
		PSGetOrdersByPhoneNrDesc = prepare_statement(Connection, "get_orders_by_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE client_phone_nr = $1 ORDER BY id DESC"),
		PSGetOrdersByPhoneNr = prepare_statement(Connection, "get_orders_by_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE client_phone_nr = $1"),
		PSGetOrdersByIdStatusPhoneNrAsc = prepare_statement(Connection, "get_orders_by_id_status_by_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND client_phone_nr = $2 ORDER BY id ASC"),
		PSGetOrdersByIdStatusPhoneNrDesc = prepare_statement(Connection, "get_orders_by_id_status_by_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND client_phone_nr = $2 ORDER BY id DESC"),
		PSGetOrdersByIdStatusPhoneNr = prepare_statement(Connection, "get_orders_by_id_status_by_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND client_phone_nr = $2"),
		PSGetOrdersByIdTypePhoneNr = prepare_statement(Connection, "get_orders_by_id_type_by_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_type = $1 AND client_phone_nr = $2"),
		PSGetDispatcherOrdersByPhoneAsc = prepare_statement(Connection, "get_dispatcher_orders_by_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status IN (1, 2, 3, 4) AND client_phone_nr = $1 ORDER BY id ASC"),
		PSGetDispatcherOrdersByPhoneDesc = prepare_statement(Connection, "get_dispatcher_orders_by_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status IN (1, 2, 3, 4) AND client_phone_nr = $1 ORDER BY id DESC"),
		PSGetDispatcherOrdersByPhone = prepare_statement(Connection, "get_dispatcher_orders_by_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status IN (1, 2, 3, 4) AND client_phone_nr = $1"),
		PSGetDispatcherOrdersByIdStatusByPhoneAsc = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_by_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND client_phone_nr = $2 ORDER BY id ASC"),
		PSGetDispatcherOrdersByIdStatusByPhoneDesc = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_by_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND client_phone_nr = $2 ORDER BY id DESC"),
		PSGetDispatcherOrdersByIdStatusByPhone = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_by_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND client_phone_nr = $2"),
		PSGetDispatcherOrdersByIdTypeByPhoneAsc = prepare_statement(Connection, "get_dispatcher_orders_by_id_type_by_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_type = $1 AND client_phone_nr = $2 AND id_status IN (1, 2, 3, 4) ORDER BY id ASC"),
		PSGetDispatcherOrdersByIdTypeByPhoneDesc = prepare_statement(Connection, "get_dispatcher_orders_by_id_type_by_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_type = $1 AND client_phone_nr = $2 AND id_status IN (1, 2, 3, 4) ORDER BY id DESC"),
		PSGetDispatcherOrdersByIdTypeByPhone = prepare_statement(Connection, "get_dispatcher_orders_by_id_type_by_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_type = $1 AND client_phone_nr = $2 AND id_status IN (1, 2, 3, 4)"),
		PSGetDispatcherOrdersByIdStatusAndIdTypeByPhoneAsc = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND id_type = $2 AND client_phone_nr = $3 ORDER BY id ASC"),
		PSGetDispatcherOrdersByIdStatusAndIdTypeByPhoneDesc = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND id_type = $2 AND client_phone_nr = $3 ORDER BY id DESC"),
		PSGetDispatcherOrdersByIdStatusAndIdTypeByPhone = prepare_statement(Connection, "get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_status = $1 AND id_type = $2 AND client_phone_nr = $3"),
		PSGetOrdersByIdUserIdTypeAsc = prepare_statement(Connection, "get_orders_by_id_user_by_id_type_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_type = $2 ORDER BY id ASC"),
		PSGetOrdersByIdUserIdTypeDesc = prepare_statement(Connection, "get_orders_by_id_user_by_id_type_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_type = $2 ORDER BY id DESC"),
		PSGetOrdersByIdUserIdType = prepare_statement(Connection, "get_orders_by_id_user_by_id_type", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_type = $2"),
		PSGetOrdersByIdUserIdStatusIdTypeAsc = prepare_statement(Connection, "get_orders_by_id_user_id_status_id_type_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 AND id_type = $3 ORDER BY id ASC"),
		PSGetOrdersByIdUserIdStatusIdTypeDesc = prepare_statement(Connection, "get_orders_by_id_user_id_status_id_type_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 AND id_type = $3 ORDER BY id DESC"),
		PSGetOrdersByIdUserIdStatusIdType = prepare_statement(Connection, "get_orders_by_id_user_id_status_id_type", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 AND id_type = $3"),
		PSGetOrdersByIdUserPhoneNrAsc = prepare_statement(Connection, "get_orders_by_id_user_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND client_phone_nr = $2 ORDER BY id ASC"),
		PSGetOrdersByIdUserPhoneNrDesc = prepare_statement(Connection, "get_orders_by_id_user_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND client_phone_nr = $2 ORDER BY id DESC"),
		PSGetOrdersByIdUserPhoneNr = prepare_statement(Connection, "get_orders_by_id_user_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND client_phone_nr = $2"),
		PSGetOrdersByIdUserIdStatusPhoneNrAsc = prepare_statement(Connection, "get_orders_by_id_user_and_id_status_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 AND client_phone_nr = $3 ORDER BY id ASC"),
		PSGetOrdersByIdUserIdStatusPhoneNrDesc = prepare_statement(Connection, "get_orders_by_id_user_and_id_status_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 AND client_phone_nr = $3 ORDER BY id DESC"),
		PSGetOrdersByIdUserIdStatusPhoneNr = prepare_statement(Connection, "get_orders_by_id_user_and_id_status_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 AND client_phone_nr = $3"),
		PSGetOrdersByIdUserIdTypePhoneNrAsc = prepare_statement(Connection, "get_orders_by_id_user_by_id_type_by_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_type = $2 AND client_phone_nr = $3 ORDER BY id ASC"),
		PSGetOrdersByIdUserIdTypePhoneNrDesc = prepare_statement(Connection, "get_orders_by_id_user_by_id_type_by_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_type = $2 AND client_phone_nr = $3 ORDER BY id DESC"),
		PSGetOrdersByIdUserIdTypePhoneNr = prepare_statement(Connection, "get_orders_by_id_user_by_id_type_by_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_type = $2 AND client_phone_nr = $3"),
		PSGetOrdersByIdUserIdStatusIdTypePhoneNrAsc = prepare_statement(Connection, "get_orders_by_id_user_id_status_by_id_type_by_phone_nr_asc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 AND id_type = $3 AND client_phone_nr = $4 ORDER BY id ASC"),
		PSGetOrdersByIdUserIdStatusIdTypePhoneNrDesc = prepare_statement(Connection, "get_orders_by_id_user_id_status_by_id_type_by_phone_nr_desc", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 AND id_type = $3 AND client_phone_nr = $4 ORDER BY id DESC"),
		PSGetOrdersByIdUserIdStatusIdTypePhoneNr = prepare_statement(Connection, "get_orders_by_id_user_id_status_by_id_type_by_phone_nr", "SELECT " ++ GetOrderFields ++ " FROM t_order WHERE id_user = $1 AND id_status = $2 AND id_type = $3 AND client_phone_nr = $4"),		
		% t_order_prod
		PSCreateOrderProd = prepare_statement(Connection, "create_order_prod", "INSERT INTO t_order_prod (id_order, id_product, description, name, quantity, gross_amount) VALUES ($1, $2, $3, $4, $5, $6)"),
		PSGetOrderProds = prepare_statement(Connection, "get_order_prods", "SELECT id_order, id_product, description, name, quantity, gross_amount FROM t_order_prod WHERE id_order = $1"),
		% t_order_prod_option
		PSCreateOrderProdOption = prepare_statement(Connection, "create_order_prod_option", "INSERT INTO t_order_prod_option (id_order, id_product, id_prod_option, type, name) VALUES ($1, $2, $3, $4, $5)"),
		PSGetOrderProdOptions = prepare_statement(Connection, "get_order_prod_options", "SELECT id_order, id_product, id_prod_option, type, name FROM t_order_prod_option WHERE id_order = $1 AND id_product = $2"),
		% t_order_prod_option_entry
		PSCreateOrderProdOptionEntry = prepare_statement(Connection, "create_order_prod_option_entry", "INSERT INTO t_order_prod_option_entry (id_order, id_product, id_prod_option, id_prod_option_entry, quantity, name, selected, gross_amount) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"),
		PSGetOrderProdOptionEntries = prepare_statement(Connection, "get_order_prod_option_entries", "SELECT id_order, id_product, id_prod_option, id_prod_option_entry, quantity, name, selected, gross_amount FROM t_order_prod_option_entry WHERE id_order = $1 AND id_product = $2 AND id_prod_option = $3"),
		% t_order_waypoint
		PSCreateOrderWaypoint = prepare_statement(Connection, "create_order_waypoint", "INSERT INTO t_order_waypoint (id_order, id_waypoint, reference, latitude, longitude, formatted_address, contact_name, contact_phone_nr, contact_email, notes, signature, stop_from, stop_to, stop_duration) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)"),
		PSGetOrderWaypoints = prepare_statement(Connection, "get_order_waypoints", "SELECT id_order, id_waypoint, reference, formatted_address, latitude, longitude, contact_name, contact_phone_nr, contact_email, notes, signature, stop_from, stop_to, stop_duration FROM t_order_waypoint WHERE id_order = $1 ORDER BY id_waypoint ASC"),
		% t_order_object
		PSCreateOrderObject = prepare_statement(Connection, "create_order_object", "INSERT INTO t_order_object (id_order, id_object, reference, id_type, transport_auth) VALUES ($1, $2, $3, $4, $5)"),
		PSGetOrderObjects = prepare_statement(Connection, "get_order_objects", "SELECT id_order, id_object, reference, id_type, transport_auth FROM t_order_object WHERE id_order = $1"),
		% t_order_type
		PSCountOrderTypes = prepare_statement(Connection, "count_order_types", "SELECT COUNT(*) FROM t_order_type WHERE id = $1"),
		% t_order_waypoint_object_action
		PSCreateOrderWaypoinObjectAction = prepare_statement(Connection, "create_order_waypoint_object_action", "INSERT INTO t_order_waypoint_object_action (id_order, id_waypoint, id_object, id_action, action) VALUES ($1, $2, $3, $4, $5)"),
		PSGetOrderWaypointObjectActions = prepare_statement(Connection, "get_order_waypoint_object_actions", "SELECT id_order, id_waypoint, id_object, id_action, action FROM t_order_waypoint_object_action WHERE id_order = $1 AND id_waypoint = $2"),
		% t_waypoint_status
		PSGetWaypointStatuses = prepare_statement(Connection, "get_waypoint_statuses", "SELECT id, description FROM t_waypoint_status"),
		PSCountWaypointStatuses = prepare_statement(Connection, "count_waypoint_statuses", "SELECT COUNT(*) FROM t_waypoint_status WHERE id = $1"),
		PSGetDeliveryWaypoints = prepare_statement(Connection, "get_delivery_waypoints",
			"SELECT DISTINCT tdw.id_delivery, tdw.id_waypoint, tow.latitude, tow.longitude, tdw.travel_distance, tdw.travel_time_est, tdw.travel_time_real, tdw.stop_duration_est, tdw.stop_duration_real, tdw.checkin_date, tdw.checkout_date, tdw.checkin_lat, tdw.checkin_lon, tdw.checkout_lat, tdw.checkout_lon, tdw.id_status, tdw.creation_date, tdw.status_date, tdw.version
			   FROM t_delivery_waypoint tdw, t_generated_delivery_waypoint tgdw, t_order_waypoint tow
			  WHERE tdw.id_delivery = $1
				AND tgdw.id_delivery = tdw.id_delivery
				AND tgdw.id_delivery_waypoint = tdw.id_waypoint
				AND tgdw.id_order = tow.id_order
				AND tgdw.id_waypoint = tow.id_waypoint
		   ORDER BY id_waypoint"),
		PSGetDeliveryWaypoint = prepare_statement(Connection, "get_delivery_waypoint",
			"SELECT DISTINCT tdw.id_delivery, tdw.id_waypoint, tow.latitude, tow.longitude, tdw.travel_distance, tdw.travel_time_est, tdw.travel_time_real, tdw.stop_duration_est, tdw.stop_duration_real, tdw.checkin_date, tdw.checkout_date, tdw.checkin_lat, tdw.checkin_lon, tdw.checkout_lat, tdw.checkout_lon, tdw.id_status, tdw.creation_date, tdw.status_date, tdw.version
			   FROM t_delivery_waypoint tdw, t_generated_delivery_waypoint tgdw, t_order_waypoint tow
			  WHERE tdw.id_delivery = $1
				AND tdw.id_waypoint = $2
				AND tgdw.id_delivery = tdw.id_delivery
				AND tgdw.id_delivery_waypoint = tdw.id_waypoint
				AND tgdw.id_order = tow.id_order
				AND tgdw.id_waypoint = tow.id_waypoint"),
		% t_delivery_waypoint
		PSGetDeliveryWaypointByOrder = prepare_statement(Connection, "get_delivery_waypoint_by_id_order", "SELECT dw.id_delivery, dw.id_waypoint, dw.travel_distance, dw.travel_time_est, dw.travel_time_real, dw.stop_duration_est, dw.stop_duration_real, dw.checkin_date, dw.checkout_date, dw.checkin_lat, dw.checkin_lon, dw.checkout_lat, dw.checkout_lon, dw.id_status, dw.creation_date, dw.status_date, dw.version FROM t_delivery_waypoint dw, t_generated_delivery_waypoint gdw, t_order_waypoint ow WHERE ow.id_order = $1 AND ow.id_waypoint = (SELECT MAX(ow2.id_waypoint) FROM t_order_waypoint ow2 WHERE ow2.id_order = ow.id_order) AND gdw.id_order = ow.id_order AND gdw.id_waypoint = ow.id_waypoint AND gdw.id_delivery = dw.id_delivery AND gdw.id_delivery_waypoint = dw.id_waypoint"),
		PSCreateDeliveryWaypoint = prepare_statement(Connection, "create_delivery_waypoint", "INSERT INTO t_delivery_waypoint (id_delivery, id_waypoint, travel_distance, travel_time_est, stop_duration_est, id_status, creation_date, status_date, version) VALUES ($1, $2, $3, $4, $5, $6, $7, $7, 1)"),
		PSUpdateDeliveryWaypointStatusTravel = prepare_statement(Connection, "update_delivery_waypoint_status_travel", "UPDATE t_delivery_waypoint SET id_status = $3, travel_time_real = $4, status_date = $5 WHERE id_delivery = $1 AND id_waypoint = $2"),
		PSUpdateDeliveryWaypointStatusTravelArrived = prepare_statement(Connection, "update_delivery_waypoint_status_travel_arrived", "UPDATE t_delivery_waypoint SET id_status = $3, travel_time_real = $4, checkin_lat = $5, checkin_lon = $6, checkin_date = $7, status_date = $7 WHERE id_delivery = $1 AND id_waypoint = $2"),
		PSUpdateDeliveryWaypointStatusStop = prepare_statement(Connection, "update_delivery_waypoint_status_stop", "UPDATE t_delivery_waypoint SET id_status = $3, stop_duration_real = $4, checkout_lat = $5, checkout_lon = $6, checkout_date = $7, status_date = $7 WHERE id_delivery = $1 AND id_waypoint = $2"),
		% t_object_action
		PSGetObjectActions = prepare_statement(Connection, "get_object_actions", "SELECT id, description FROM t_object_action"),
		PSCountObjectActions = prepare_statement(Connection, "count_object_actions", "SELECT COUNT(*) FROM t_object_action WHERE id = $1"),
		% t_object_status
		PSGetObjectStatuses = prepare_statement(Connection, "get_object_statuses", "SELECT id, description FROM t_object_status"),
		PSCountObjectStatuses = prepare_statement(Connection, "count_object_statuses", "SELECT COUNT(*) FROM t_object_status WHERE id = $1"),
		% t_object_type
		PSGetObjectTypes = prepare_statement(Connection, "get_object_types", "SELECT id, description, size, length, width, height, weight, volume FROM t_object_type"),
		PSGetObjectType = prepare_statement(Connection, "get_object_type", "SELECT id, description, size, length, width, height, weight, volume FROM t_object_type WHERE id = $1"),
		PSCountObjectTypes = prepare_statement(Connection, "count_object_types", "SELECT COUNT(*) FROM t_object_type WHERE id = $1"),
		% t_delivery_object
		PSGetDeliveryObjects = prepare_statement(Connection, "get_delivery_objects",
			"SELECT tdo.id_delivery, tdo.id_object, too.reference, too.id_type, too.transport_auth, tdo.non_del_reason
			   FROM t_generated_delivery_object tgdo, t_delivery_object tdo, t_order_object too
			  WHERE tgdo.id_delivery = tdo.id_delivery
				AND tgdo.id_delivery_object = tdo.id_object
				AND tgdo.id_order = too.id_order
				AND tgdo.id_object = too.id_object
				AND tgdo.id_delivery = $1"),
		PSCreateDeliveryObject = prepare_statement(Connection, "create_delivery_object", "INSERT INTO t_delivery_object (id_delivery, id_object, reference, id_type, transport_auth) VALUES ($1, $2, $3, $4, $5)"),
		PSCreateDeliveryWaypointObjectAction = prepare_statement(Connection, "create_delivery_waypoint_object_action", "INSERT INTO t_delivery_waypoint_object_action (id_delivery, id_waypoint, id_object, id_action, id_status, creation_date, status_date) VALUES ($1, $2, $3, $4, $5, $6, $6)"),
		PSGetDeliveryWaypointObjectActions = prepare_statement(Connection, "get_delivery_waypoint_object_actions",
			"SELECT tgdo.id_delivery_object, too.reference, tdwoa.id_action, tdwoa.id_status, towoa.action
			   FROM t_generated_delivery_waypoint tgdw, t_generated_delivery_object tgdo, t_delivery_waypoint_object_action tdwoa, t_order_object too, t_order_waypoint_object_action towoa
			  WHERE tgdw.id_delivery = tdwoa.id_delivery
				AND tgdw.id_delivery_waypoint = tdwoa.id_waypoint
				AND tgdo.id_delivery_object = tdwoa.id_object
				AND tgdo.id_order = too.id_order
				AND tgdo.id_object = too.id_object
				AND tgdw.id_order = towoa.id_order
				AND tgdw.id_waypoint = towoa.id_waypoint
				AND tgdo.id_object = towoa.id_object
				AND tgdw.id_delivery = tgdo.id_delivery
				AND tgdw.id_order = tgdo.id_order
				AND tgdw.id_delivery = $1
				AND tgdw.id_delivery_waypoint = $2
				AND tgdw.id_order = $3
				AND tgdw.id_waypoint = $4"),
		PSGetDeliveryWaypointDetails = prepare_statement(Connection, "get_delivery_waypoint_details",
			"SELECT tgdw.id_delivery, tgdw.id_delivery_waypoint, tow.id_order, tow.id_waypoint, tow.reference, tow.formatted_address, tow.contact_name, tow.contact_phone_nr, tow.contact_email, torder.client_name, torder.client_phone_nr, torder.client_email, torder.client_fiscal_id, torder.id_user, tow.notes, tow.signature, tow.stop_from, tow.stop_to, tow.stop_duration
			   FROM t_generated_delivery_waypoint tgdw, t_order_waypoint tow, t_order torder
			  WHERE tgdw.id_order    = tow.id_order
				AND tgdw.id_waypoint = tow.id_waypoint
				AND tgdw.id_order    = torder.id
				AND tgdw.id_delivery = $1
				AND tgdw.id_delivery_waypoint = $2"),
		PSGetDeliveryWaypointRating = prepare_statement(Connection, "get_delivery_waypoint_rating", "SELECT id_delivery, id_delivery_waypoint, id_order, id_waypoint, rating_client, rating_notes_client, rating_courier, rating_notes_courier FROM t_delivery_waypoint_rating WHERE id_delivery = $1 AND id_delivery_waypoint = $2 AND id_order = $3 AND id_waypoint = $4"), 
		% t_order_waypoint_address
		PSCreateOrderWaypointAddress = prepare_statement(Connection, "create_order_waypoint_address", "INSERT INTO t_order_waypoint_address (id_order, id_waypoint, component, value) VALUES ($1, $2, $3, $4)"),
		PSGetOrderWaypointAddress = prepare_statement(Connection, "get_order_waypoint_address", "SELECT id_order, id_waypoint, component, value FROM t_order_waypoint_address WHERE id_order = $1 AND id_waypoint = $2"),
		PSGetOrderLastWaypointAddress = prepare_statement(Connection, "get_order_last_waypoint_address", "SELECT owa1.id_order, owa1.id_waypoint, owa1.component, owa1.value FROM t_order_waypoint_address owa1 WHERE owa1.id_order = $1 AND owa1.id_waypoint = (SELECT MAX (owa2.id_waypoint) FROM t_order_waypoint_address owa2 WHERE owa2.id_order = owa1.id_order)"),
		% t_delivery_waypoint_signature
		PSGetDeliveryWaypointSignature = prepare_statement(Connection, "get_delivery_waypoint_signature", "SELECT id_delivery, id_delivery_waypoint, id_order, id_waypoint, name, mimetype, base64_data FROM t_delivery_waypoint_signature WHERE id_delivery = $1 AND id_delivery_waypoint = $2 AND id_order = $3 AND id_waypoint = $4"),
		% t_notification_type
		PSGetNotificationTypes = prepare_statement(Connection, "get_notification_types", "SELECT id, description, notify_client FROM t_notification_type"),
		PSCountNotificationTypes = prepare_statement(Connection, "count_notification_types", "SELECT COUNT(*) FROM t_notification_type WHERE id = $1"),
		% t_api_key
		PSGetApiKeyByStatus = prepare_statement(Connection, "get_api_key_by_status", "SELECT ak.api_key, ak.id_api_client, ak.domain, ak.version FROM t_api_key ak, t_api_client ac WHERE ak.api_key = $1 AND ac.id = ak.id_api_client AND ac.id_status = $2"),
		% t_user_courier
		PSGetUserCourier = prepare_statement(Connection, "get_user_courier", "SELECT id_user, id_courier FROM t_user_courier WHERE id_user = $1 AND id_courier = $2"),
		PSCreateUserCourier = prepare_statement(Connection, "create_user_courier", "INSERT INTO t_user_courier (id_user, id_courier) VALUES ($1, $2)"),
		PSDeleteUserCourier = prepare_statement(Connection, "delete_user_courier", "DELETE FROM t_user_courier WHERE id_user = $1 AND id_courier = $2"),
		% t_user_location
		PSGetUserLocations = prepare_statement(Connection, "get_user_locations", "SELECT id, id_user, description, latitude, longitude FROM t_user_location WHERE id_user = $1"),
		PSGetUserLocation = prepare_statement(Connection, "get_user_location", "SELECT id, id_user, description, latitude, longitude FROM t_user_location WHERE id_user = $1 AND id = $2"),
		PSCreateUserLocation = prepare_statement(Connection, "create_user_location", "INSERT INTO t_user_location (id_user, description, latitude, longitude) VALUES ($1, $2, $3, $4)"),
		PSUpdateUserLocation = prepare_statement(Connection, "update_user_location", "UPDATE t_user_location SET description = $3, latitude = $4, longitude = $5 WHERE id_user = $1 AND id = $2"),
		PSDeleteUserLocation = prepare_statement(Connection, "delete_user_location", "DELETE FROM t_user_location WHERE id_user = $1 AND id = $2"),
		% t_user_location_component
		PSGetUserLocationComponents = prepare_statement(Connection, "get_user_location_components", "SELECT id_location, component, value FROM t_user_location_component WHERE id_location = $1"),
		PSGetUserLocationComponent = prepare_statement(Connection, "get_user_location_component", "SELECT id_location, component, value FROM t_user_location_component WHERE id_location = $1 AND component = $2"),
		PSCreateUserLocationComponent = prepare_statement(Connection, "create_user_location_component", "INSERT INTO t_user_location_component (id_location, component, value) VALUES ($1, $2, $3)"),
		PSDeleteUserLocationComponents = prepare_statement(Connection, "delete_user_location_components", "DELETE FROM t_user_location_component WHERE id_location = $1"),
		% t_user_location_contact
		PSGetUserLocationContacts = prepare_statement(Connection, "get_user_location_contacts", "SELECT id_contact, id_location, name, phone_nr, email FROM t_user_location_contact WHERE id_location = $1"),
		PSGetUserLocationContact = prepare_statement(Connection, "get_user_location_contact", "SELECT id_contact, id_location, name, phone_nr, email FROM t_user_location_contact WHERE id_contact = $1 AND id_location = $2"),
		PSCreateUserLocationContact = prepare_statement(Connection, "create_user_location_contact", "INSERT INTO t_user_location_contact (id_location, name, phone_nr, email) VALUES ($1, $2, $3, $4)"),
		PSDeleteUserLocationContacts = prepare_statement(Connection, "delete_user_location_contacts", "DELETE FROM t_user_location_contact WHERE id_location = $1"),
		% t_delivery_courier_route
		PSCreateDeliveryCourierRoute = prepare_statement(Connection, "create_courier_route", "INSERT INTO t_delivery_courier_route (id_user, id_delivery, latitude, longitude, status_date) VALUES ($1, $2, $3, $4, $5)"),
		% t_oauth_provider
		PSGetOauthProvider = prepare_statement(Connection, "get_oauth_provider", "SELECT id, description, client_id, client_secret, authorize_endpoint, access_token_endpoint, profile_endpoint FROM t_oauth_provider WHERE id = $1"),
		% t_payment_provider
		PSGetPaymentProvider = prepare_statement(Connection, "get_payment_provider", "SELECT id, description, client_id, client_secret, entity_id, request_endpoint, assertion_endpoint, get_details_endpoint FROM t_payment_provider WHERE id = $1"),
		PSExistsPaymentProvider = prepare_statement(Connection, "exists_payment_provider", "SELECT id FROM t_payment_provider WHERE id = $1"),
		% t_order_payment
		PSCreateOrderPayment = prepare_statement(Connection, "create_order_payment", "INSERT INTO t_order_payment (id_order, id_provider, id_status, ref_payment, authorization_only, creation_date, status_date) VALUES ($1, $2, $3, $4, $5, $6, $6)"),
		% t_payment_status
		PSCountPaymentStatus = prepare_statement(Connection, "count_payment_status", "SELECT COUNT(*) FROM t_payment_status WHERE id = $1"),
		% t_payment_request
		PSCreatePaymentRequest = prepare_statement(Connection, "create_payment_request", "INSERT INTO t_payment_request (id_payment, endpoint, request_data, response_data, creation_date) VALUES ($1, $2, $3, $4, $5)"),
		% t_voucher
		PSCreateVoucher = prepare_statement(Connection, "create_voucher", "INSERT INTO t_voucher (code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version) VALUES ($1, $2, $3, $4, 0.0, $5, 0, $6, $7, $8, $8, 1)"),
		PSCountVoucher = prepare_statement(Connection, "count_voucher", "SELECT COUNT(*) FROM t_voucher WHERE UPPER(code) = UPPER($1)"),
		PSGetVoucher = prepare_statement(Connection, "get_voucher", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher WHERE id = $1"),
		PSGetVoucherByCode = prepare_statement(Connection, "get_voucher_by_code", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher WHERE code = $1"),
		PSGetVouchers = prepare_statement(Connection, "get_vouchers", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher"),
		PSGetVouchersCode = prepare_statement(Connection, "get_vouchers_code", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher WHERE UPPER(code) = UPPER($1)"),
		PSGetVouchersIdStatus = prepare_statement(Connection, "get_vouchers_id_status", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher WHERE id_status = $1"),
		PSGetVouchersRestrictUserId = prepare_statement(Connection, "get_vouchers_restrict_user_id", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher WHERE restrict_user_id = $1"),
		PSGetVouchersCodeIdStatus = prepare_statement(Connection, "get_vouchers_code_id_status", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher WHERE UPPER(code) = UPPER($1) AND id_status = $2"),
		PSGetVouchersCodeRestrictUserId = prepare_statement(Connection, "get_vouchers_code_restrict_user_id", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher WHERE UPPER(code) = UPPER($1) AND restrict_user_id = $2"),
		PSGetVouchersIdStatusRestrictUserId = prepare_statement(Connection, "get_vouchers_id_status_restrict_user_id", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher WHERE id_status = $1 AND restrict_user_id = $2"),
		PSGetVouchersCodeIdStatusRestrictUserId = prepare_statement(Connection, "get_vouchers_code_id_status_restrict_user_id", "SELECT id, code, value, id_type, id_status, used_value, max_times, times_used, expiration_date, restrict_user_id, creation_date, status_date, version FROM t_voucher WHERE UPPER(code) = UPPER($1) AND id_status = $2 AND restrict_user_id = $3"),
		PSUpdateVoucher = prepare_statement(Connection, "update_voucher", "UPDATE t_voucher SET value = $3, id_status = $4, max_times = $5, expiration_date = $6, restrict_user_id = $7, status_date = $8, version = version + 1 WHERE id = $1 AND version = $2"),
		PSCountValidVouchers = prepare_statement(Connection, "count_valid_vouchers", "SELECT COUNT(*) FROM t_voucher WHERE UPPER(code) = UPPER($1) AND (used_value IS NULL OR used_value < value) AND (id_type <> $6 OR times_used < max_times) AND (id_status IN ($4, $5)) AND (expiration_date IS NULL OR expiration_date < $3)AND (restrict_user_id IS NULL OR restrict_user_id = $2)"),
		% t_order_voucher
		PSCreateOrderVoucher = prepare_statement(Connection, "create_order_voucher", "INSERT INTO t_order_voucher (id_order, id_voucher, creation_date) VALUES ($1, $2, $3)"),
		PSDeleteOrderVoucher = prepare_statement(Connection, "delete_order_voucher", "DELETE FROM t_order_voucher WHERE id_order = $1"),
		% t_voucher_type
		PSCountVoucherType = prepare_statement(Connection, "count_voucher_type", "SELECT COUNT(*) FROM t_voucher_type WHERE id = $1"),
		PSGetVoucherTypes = prepare_statement(Connection, "get_voucher_types", "SELECT id, description FROM t_voucher_type"),
		% t_voucher_status
		PSCountVoucherStatus = prepare_statement(Connection, "count_voucher_status", "SELECT COUNT(*) FROM t_voucher_status WHERE id = $1"),
		PSGetVoucherStatuses = prepare_statement(Connection, "get_voucher_statuses", "SELECT id, description FROM t_voucher_status"),
		% t_user_notification
		PSGetUserNotifications = prepare_statement(Connection, "get_user_notifications", "SELECT id_user, id_type, enabled FROM t_user_notification WHERE id_user = $1"),
		PSDeleteUserNotifications = prepare_statement(Connection, "delete_user_notifications", "DELETE FROM t_user_notification WHERE id_user = $1"),
		PSCreateUserNotification = prepare_statement(Connection, "create_user_notification", "INSERT INTO t_user_notification (id_user, id_type, enabled) VALUES ($1, $2, $3)"),
		% t_user_notification_type
		PSCountUserNotificationType = prepare_statement(Connection, "count_user_notification_type", "SELECT COUNT(*) FROM t_user_notification_type WHERE id = $1"),
		% t_commercial_info
		PSGetCommercialInfo = prepare_statement(Connection, "get_commercial_info", "SELECT id_user, about FROM t_commercial_info WHERE id_user = $1"),
		PSUpdateCommercialInfo = prepare_statement(Connection, "update_commercial_info", "UPDATE t_commercial_info SET about = $2 WHERE id_user = $1"),
		PSCreateCommercialInfo = prepare_statement(Connection, "create_commercial_info", "INSERT INTO t_commercial_info (id_user, about) VALUES ($1, $2)"),
		% t_zone
		PSGetZones = prepare_statement(Connection, "get_zones", "SELECT id, description, center_latitude, center_longitude FROM t_zone"),
		PSCountZones = prepare_statement(Connection, "count_zones", "SELECT COUNT(*) FROM t_zone WHERE id = $1"),
		% t_zone_postal_code
		PSGetZonePostalCodes = prepare_statement(Connection, "get_zone_postal_codes", "SELECT id_zone, postal_code FROM t_zone_postal_code WHERE id_zone = $1"),
		% t_user_zone
		PSGetUserZones = prepare_statement(Connection, "get_user_zones", "SELECT id_user, id_zone FROM t_user_zone WHERE id_user = $1"),
		PSExistsUserZone = prepare_statement(Connection, "exists_user_zone", "SELECT id_user, id_zone FROM t_user_zone WHERE id_user = $1 AND id_zone = $2"),
		PSDeleteUserZones = prepare_statement(Connection, "delete_user_zones", "DELETE FROM t_user_zone WHERE id_user = $1"),
		PSCreateUserZone = prepare_statement(Connection, "create_user_zone", "INSERT INTO t_user_zone (id_user, id_zone) VALUES ($1, $2)"),
		% Other
		PSGetUserByUsernamePassword = prepare_statement(Connection, "get_user_by_username_password", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND UPPER(a.username) = UPPER($1) AND a.password = get_password_hash ($2, a.password)"),
		PSGetUserByIdPassword = prepare_statement(Connection, "get_user_by_id_password", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND t.id = $1 AND a.password = get_password_hash ($2, a.password)"),
		PSGetUserByUsername = prepare_statement(Connection, "get_user_by_username", "SELECT t.id, t.id_type, a.username, t.id_status, t.creation_date, t.status_date, t.login_date, t.email, t.first_name, t.last_name, t.telephone_nr, t.fiscal_id, t.reference, t.mobileos_id, t.birth_day, t.birth_month, t.birth_year, t.national_id, t.country, t.rating, t.version FROM t_user t, t_user_auth a WHERE t.id = a.id_user AND a.username = $1"),
		PSGetUserOauth = prepare_statement(Connection, "get_user_oauth", "SELECT u.id, u.id_type, o.id_oauth, u.id_status, u.creation_date, u.status_date, u.login_date, u.email, u.first_name, u.last_name, u.telephone_nr, u.fiscal_id, u.reference, u.mobileos_id, u.birth_day, u.birth_month, u.birth_year, u.national_id, u.country, u.rating, u.version FROM t_user u, t_user_oauth o WHERE u.id = o.id_user AND o.id_oauth = $1"),
		PSGetAccountUsers = prepare_statement(Connection, "get_account_users", "SELECT u.id, au.id_type as id_account_user_type, u.id_type as id_user_type, ua.username, u.id_status, u.creation_date, u.status_date, u.email, u.first_name, u.last_name, u.telephone_nr, u.fiscal_id, au.id_department, au.id_cost_center, u.reference, u.rating, u.version FROM t_user u, t_account_user au, t_user_auth ua WHERE au.id_account = $1 AND au.id_user = u.id AND u.id = ua.id_user"),
		PSGetAccountUsersByUserIdType = prepare_statement(Connection, "get_account_users_by_user_id_type", "SELECT u.id, au.id_type as id_account_user_type, u.id_type as id_user_type, ua.username, u.id_status, u.creation_date, u.status_date, u.email, u.first_name, u.last_name, u.telephone_nr, u.fiscal_id, au.id_department, au.id_cost_center, u.reference, u.rating, u.version FROM t_user u, t_account_user au, t_user_auth ua WHERE au.id_account = $1 AND u.id_type = $2 AND au.id_user = u.id AND u.id = ua.id_user"),
		PSGetUserAccounts = prepare_statement(Connection, "get_user_accounts", "SELECT a.id, a.unique_key, a.id_status, a.account_name, a.fiscal_id, a.telephone_nr, a.email, a.creation_date, a.status_date, a.occupancy, a.version FROM t_account a, t_account_user au WHERE a.id = au.id_account AND au.id_user = ANY($1)"),
		PSCreatenotification = prepare_statement(Connection, "create_notification", "INSERT INTO t_notification (id_delivery, id_waypoint, id_type, message, latitude, longitude, status_date, id_status, creation_date) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)"),
		PSCountClientCourierDeliveries = prepare_statement(Connection, "count_client_courier_deliveries",
			"SELECT COUNT(tgd.*)
			   FROM t_delivery td, t_generated_delivery tgd, t_order o
			  WHERE tgd.id_delivery = td.id
				AND tgd.id_order = o.id
				AND o.id_user = $1
				AND td.id_courier = $2"),
		PSGetUserCouriers = prepare_statement(Connection, "get_user_couriers", "SELECT u.id, u.creation_date, u.email, u.first_name, u.last_name, u.telephone_nr FROM t_user u, t_user_courier uc WHERE uc.id_user = $1 AND u.id = uc.id_courier"),
		PSGetUserCouriersNameAsc = prepare_statement(Connection, "get_user_couriers_name_asc", "SELECT u.id, u.creation_date, u.email, u.first_name, u.last_name, u.telephone_nr FROM t_user u, t_user_courier uc WHERE uc.id_user = $1 AND u.id = uc.id_courier ORDER BY u.first_name ASC, u.last_name ASC"),
		PSGetUserCouriersNameDesc = prepare_statement(Connection, "get_user_couriers_name_desc", "SELECT u.id, u.creation_date, u.email, u.first_name, u.last_name, u.telephone_nr FROM t_user u, t_user_courier uc WHERE uc.id_user = $1 AND u.id = uc.id_courier ORDER BY u.first_name DESC, u.last_name DESC"),
		PSGetUserLocationsByName = prepare_statement(Connection, "get_user_locations_by_name", "SELECT u.id, u.id_user, u.description, u.latitude, u.longitude FROM t_user_location u, t_user_location_contact c WHERE u.id = c.id_location AND u.id_user = $1 AND c.name = $2"),
		PSGetUserLocationsByPhoneNr = prepare_statement(Connection, "get_user_locations_by_phone_nr", "SELECT u.id, u.id_user, u.description, u.latitude, u.longitude FROM t_user_location u, t_user_location_contact c WHERE u.id = c.id_location AND u.id_user = $1 AND c.phone_nr = $2"),
		PSGetUserLocationsByNameAndPhoneNr = prepare_statement(Connection, "get_user_locations_by_name_and_phone_nr", "SELECT u.id, u.id_user, u.description, u.latitude, u.longitude FROM t_user_location u, t_user_location_contact c WHERE u.id = c.id_location AND u.id_user = $1 AND c.name = $2 AND c.phone_nr = $3"),
		PSGetOrderDeliveriesCouriers = prepare_statement(Connection, "get_order_deliveries_couriers",
			"SELECT tgd.id_delivery, tgd.id_order, td.id_courier
			   FROM t_generated_delivery tgd, t_order tor, t_delivery td
			  WHERE tgd.id_delivery = td.id
				AND tgd.id_order = tor.id
				AND tgd.id_order = $1"),
		PSGetObjectQtyByTypeByDelivery = prepare_statement(Connection, "get_object_qty_by_type_by_delivery", "SELECT id_type, COUNT(*) FROM t_delivery_object WHERE id_delivery = $1 GROUP BY id_type"),
		PSGetAddressComponentByDelivery = prepare_statement(Connection, "get_address_component_by_delivery", "SELECT wa1.value, wa1.id_waypoint, wa2.value, wa2.id_waypoint FROM t_order_waypoint_address wa1, t_order_waypoint_address wa2 WHERE wa1.component = wa2.component AND wa1.id_order = (SELECT MIN(gd3.id_order) FROM t_generated_delivery_waypoint gd3 WHERE gd3.id_delivery = $1 AND gd3.id_delivery_waypoint = (SELECT MIN(gd5.id_delivery_waypoint) FROM t_generated_delivery_waypoint gd5 WHERE gd5.id_delivery = gd3.id_delivery)) AND wa2.id_order = (SELECT gd4.id_order FROM t_generated_delivery_waypoint gd4 WHERE gd4.id_delivery = $1 AND gd4.id_delivery_waypoint = (SELECT MAX(gd6.id_delivery_waypoint) FROM t_generated_delivery_waypoint gd6 WHERE gd6.id_delivery = gd4.id_delivery)) AND wa1.id_waypoint = (SELECT MIN(wa3.id_waypoint) FROM t_order_waypoint_address wa3 WHERE wa3.id_order = wa1.id_order) AND wa2.id_waypoint = (SELECT MAX(wa4.id_waypoint) FROM t_order_waypoint_address wa4 WHERE wa4.id_order = wa2.id_order) AND wa1.component = $2"),
 		PSGetEstimatedDeliveryTime = prepare_statement(Connection, "get_estimated_delivery_time", "SELECT SUM(travel_time_est), SUM(stop_duration_est), COUNT(*) FROM t_delivery_waypoint WHERE id_delivery = $1"),
		PSGetWaypointDetailByDeliveryId = prepare_statement(Connection, "get_waypoint_detail_by_delivery_id_order_id", "SELECT dw.id_waypoint, dw.checkin_date, dw.checkout_date FROM t_delivery_waypoint dw, t_generated_delivery_waypoint gdw WHERE dw.id_delivery = gdw.id_delivery AND dw.id_waypoint = gdw.id_delivery_waypoint AND dw.id_delivery = $1 AND gdw.id_order = $2 AND (gdw.id_delivery_waypoint = (SELECT MAX(gdw1.id_delivery_waypoint) FROM t_generated_delivery_waypoint gdw1 WHERE gdw1.id_order = $2) OR   gdw.id_delivery_waypoint = (SELECT MIN(gdw2.id_delivery_waypoint) FROM t_generated_delivery_waypoint gdw2 WHERE gdw2.id_order = $2)) ORDER BY 1"),
		PSGetLastId = prepare_statement(Connection, "get_last_id", "SELECT lastval()"),
		PSGetOrderObjectsVolume = prepare_statement(Connection, "get_order_objects_volume", "SELECT SUM(ot.volume) FROM t_order_object oo, t_object_type ot WHERE oo.id_order = $1 AND oo.id_type = ot.id"),
		PSGetDeliveryEstimatedTimes = prepare_statement(Connection, "get_delivery_estimated_times_by_order_id", "SELECT dw.travel_time_est, dw.stop_duration_est FROM t_delivery_waypoint dw WHERE dw.id_waypoint = 2 AND dw.id_delivery = (SELECT MIN(gdw3.id_delivery) FROM t_generated_delivery_waypoint gdw3 WHERE gdw3.id_order = $1)"),
		PSGetDeliveryTravelDistance = prepare_statement(Connection, "get_delivery_travel_distance_by_order_id", "SELECT dw.id_delivery, SUM(dw.travel_distance) FROM t_delivery_waypoint dw, t_generated_delivery_waypoint gdw WHERE dw.id_delivery = gdw.id_delivery AND dw.id_waypoint = gdw.id_delivery_waypoint AND gdw.id_delivery_waypoint > 1 AND gdw.id_delivery_waypoint <= (SELECT MAX(gdw2.id_delivery_waypoint) FROM t_generated_delivery_waypoint gdw2 WHERE gdw2.id_order = $1) AND gdw.id_delivery = (SELECT MIN(gdw3.id_delivery) FROM t_generated_delivery_waypoint gdw3 WHERE gdw3.id_order = $1) GROUP BY dw.id_delivery"),
		PSCreateBookingSchedulePeriodToday = prepare_statement(Connection, "create_booking_schedule_period_today", "INSERT INTO t_account_booking (id_account, slot, occupancy, max_occupancy) SELECT 1, tsrange((now()::date + lower(tbsp.slot)), (now()::date + upper(tbsp.slot))), 0, tbsp.max_occupancy FROM t_booking_schedule_period tbsp, t_account_booking_schedule tabs, t_booking_schedule_dow tbsd WHERE tbsp.id_schedule = tbsd.id_schedule AND tbsd.id_schedule = tabs.id AND tabs.id_account = 1 AND tbsd.day_of_week = EXTRACT(dow FROM now())"),
		PSCreateBookingSchedulePeriodTomorrow = prepare_statement(Connection, "create_booking_schedule_period_tomorrow", "INSERT INTO t_account_booking (id_account, slot, occupancy, max_occupancy) SELECT 1, tsrange(((now() + interval '1 day')::date + lower(tbsp.slot)), ((now() + interval '1 day')::date + upper(tbsp.slot))), 0, tbsp.max_occupancy FROM t_booking_schedule_period tbsp, t_account_booking_schedule tabs, t_booking_schedule_dow tbsd WHERE tbsp.id_schedule = tbsd.id_schedule AND tbsd.id_schedule = tabs.id AND tabs.id_account = 1 AND tbsd.day_of_week = EXTRACT(dow FROM (now() + interval '1 day'))"),
		PSGetAccountSlotsInformation = prepare_statement(Connection, "get_account_slots_information", "SELECT id, lower(slot), upper(slot), occupancy, max_occupancy FROM t_account_booking WHERE id_account = $1 AND ((upper(slot) >= (now() + ($2 ||' minutes')::interval) AND lower(slot)::date = (now()::date)) OR lower(slot)::date = ((now()::date) + 1)) ORDER BY slot"),
		PSGetAccountSlotInformation = prepare_statement(Connection, "get_account_slot_information", "SELECT id, lower(slot), upper(slot), occupancy, max_occupancy FROM t_account_booking WHERE id_account = $1 AND id = $2"),
		PSCountAccountSlotExistance = prepare_statement(Connection, "count_slot_existance", "SELECT COUNT(*) FROM t_account_booking WHERE id_account = $1 AND id = $2"),
		PSCountAccountSlotAvailability = prepare_statement(Connection, "count_slot_availability", "SELECT COUNT(*) FROM t_account_booking WHERE id_account = $1 AND id = $2 AND occupancy < max_occupancy"),
		PSUpdateAccountSlotsInformation = prepare_statement(Connection, "update_account_slots_information", "UPDATE t_account_booking SET occupancy = occupancy + 1 WHERE id_account = $1 AND id = $2 AND occupancy < max_occupancy"),
		PSGetTodaysSlotsInformation = prepare_statement(Connection, "get_todays_slots_information", "SELECT id, lower(slot), upper(slot), occupancy, max_occupancy FROM t_account_booking WHERE lower(slot)::date = (now()::date) ORDER BY lower(slot)"),
		PSUpdateSlotMaxOccupancyById = prepare_statement(Connection, "update_slot_max_occupancy_by_id", "UPDATE t_account_booking set max_occupancy = occupancy  WHERE id = $1 AND lower(slot)::date = (now()::date)"),
		PSVerifyTodaySlotExistance = prepare_statement(Connection, "verify_todays_slot_existance", "SELECT COUNT(*) FROM t_account_booking WHERE id = $1 AND lower(slot)::date = (now()::date)"),
		Statements = [
					  {count_users, PSCountUsers},
					  {count_users_by_id_type, PSCountUsersByIdType},
		              {count_users_by_id_status, PSCountUsersByIdStatus},
		              {count_users_by_id_type_and_id_status, PSCountUsersByIdTypeAndIdStatus},
		              {get_users, PSGetUsers},
		              {get_users_id_asc, PSGetUsersIdAsc},
		              {get_users_by_id_type_id_asc, PSGetUsersByIdTypeIdAsc},
		              {get_users_by_id_status_id_asc, PSGetUsersByIdStatusIdAsc},
		              {get_users_by_id_type_and_id_status_id_asc, PSGetUsersByIdTypeAndIdStatusIdAsc},
		              {get_users_id_desc, PSGetUsersIdDesc},
		              {get_users_by_id_type_id_desc, PSGetUsersByIdTypeIdDesc},
		              {get_users_by_id_status_id_desc, PSGetUsersByIdStatusIdDesc},
		              {get_users_by_id_type_and_id_status_id_desc, PSGetUsersByIdTypeAndIdStatusIdDesc},
		              {get_users_by_id_type, PSGetUsersByIdType},
		              {get_users_by_id_type_and_id_status, PSGetUsersByIdTypeAndIdStatus},
		              {get_users_id_type_asc, PSGetUsersIdTypeAsc},
		              {get_users_id_type_desc, PSGetUsersIdTypeDesc},
		              {get_users_by_id_status_id_type_asc, PSGetUsersByIdStatusIdTypeAsc},
		              {get_users_by_id_status_id_type_desc, PSGetUsersByIdStatusIdTypeDesc},
		              {get_users_by_id_status, PSGetUsersByIdStatus},
		              {get_users_id_status_asc, PSGetUsersIdStatusAsc},
		              {get_users_by_id_type_id_status_asc, PSGetUsersByIdTypeIdStatusAsc},
		              {get_users_id_status_desc, PSGetUsersIdStatusDesc},
		              {get_users_by_id_type_id_status_desc, PSGetUsersByIdTypeIdStatusDesc},
		              {get_user, PSGetUser},
		              {create_user, PSCreateUser},
		              {update_user_status, PSUpdateUserStatus},
		              {update_user, PSUpdateUser},
		              {update_user_version, PSUpdateUserVersion},
		              {set_user_login_date, PSSetUserLoginDate},
		              {count_user_oauth, PSCountUserOauth},
		              {create_user_oauth, PSCreateUserOauth},
		              {count_user_auth, PSCountUserAuth},
		              {create_user_auth, PSCreateUserAuth},
		              {set_password, PSSetPassword},
		              {set_username, PSSetUsername},
		              {get_user_address, PSGetUserAddress},
		              {create_user_address, PSCreateUserAddress},
		              {delete_user_address, PSDeleteUserAddress},
		              {get_user_document, PSGetUserDocument},
		              {get_user_document_summary, PSGetUserDocumentSummary},
		              {create_document, PSCreateDocument},
		              {update_user_document_status, PSUpdateUserDocumentStatus},
		              {get_user_documents_by_id_user_and_id_type_and_id_status, PSGetUserDocumentsByIdUserAndIdTypeAndIdStatus},
		              {get_user_documents_summary_by_id_user, PSGetUserDocumentsSummaryByIdUser},
		              {get_user_documents_summary_by_id_user_id_type_asc, PSGetUserDocumentsSummaryByIdUserIdTypeAsc},
		              {get_user_documents_summary_by_id_user_id_type_desc, PSGetUserDocumentsSummaryByIdUserIdTypeDesc},
		              {get_user_documents_summary_by_id_user_id_status_asc, PSGetUserDocumentsSummaryByIdUserIdStatusAsc},
		              {get_user_documents_summary_by_id_user_id_status_desc, PSGetUserDocumentsSummaryByIdUserIdStatusDesc},
		              {get_user_documents_summary_by_id_user_and_id_status_id_type_asc, PSGetUserDocumentsSummaryByIdUserAndIdStatusIdTypeAsc},
		              {get_user_documents_summary_by_id_user_and_id_status_id_type_desc, PSGetUserDocumentsSummaryByIdUserAndIdStatusIdTypeDesc},
		              {get_user_documents_summary_by_id_user_and_id_status, PSGetUserDocumentsSummaryByIdUserAndIdStatus},
		              {get_user_documents_summary_by_id_user_and_id_type_id_status_asc, PSGetUserDocumentsSummaryByIdUserAndIdTypeIdStatusAsc},
		              {get_user_documents_summary_by_id_user_and_id_type_id_status_desc, PSGetUserDocumentsSummaryByIdUserAndIdTypeIdStatusDesc},
		              {get_user_documents_summary_by_id_user_and_id_type, PSGetUserDocumentsSummaryByIdUserAndIdType},
		              {get_user_documents_summary_by_id_user_and_id_type_and_id_status, PSGetUserDocumentsSummaryByIdUserAndIdTypeAndIdStatus},
		              {get_account_document, PSGetAccountDocument},
		              {get_account_document_summary, PSGetAccountDocumentSummary},
		              {update_account_document_status, PSUpdateAccountDocumentStatus},
		              {get_account_documents_by_id_account_and_id_type_and_id_status, PSGetAccountDocumentsByIdAccountAndIdTypeAndIdStatus},
		              {get_account_documents_summary_by_id_account, PSGetAccountDocumentsSummaryByIdAccount},
		              {get_account_documents_summary_by_id_account_id_type_asc, PSGetAccountDocumentsSummaryByIdAccountIdTypeAsc},
		              {get_account_documents_summary_by_id_account_id_type_desc, PSGetAccountDocumentsSummaryByIdAccountIdTypeDesc},
		              {get_account_documents_summary_by_id_account_id_status_asc, PSGetAccountDocumentsSummaryByIdAccountIdStatusAsc},
		              {get_account_documents_summary_by_id_account_id_status_desc, PSGetAccountDocumentsSummaryByIdAccountIdStatusDesc},
		              {get_acc_documents_summary_by_id_account_and_id_status_id_type_asc, PSGetAccountDocumentsSummaryByIdAccountAndIdStatusIdTypeAsc},
		              {get_acc_documents_summary_by_id_account_and_id_status_id_type_desc, PSGetAccountDocumentsSummaryByIdAccountAndIdStatusIdTypeDesc},
		              {get_account_documents_summary_by_id_account_and_id_status, PSGetAccountDocumentsSummaryByIdAccountAndIdStatus},
		              {get_acc_documents_summary_by_id_account_and_id_type_id_status_asc, PSGetAccountDocumentsSummaryByIdAccountAndIdTypeIdStatusAsc},
		              {get_acc_documents_summary_by_id_account_and_id_type_id_status_desc, PSGetAccountDocumentsSummaryByIdAccountAndIdTypeIdStatusDesc},
		              {get_account_documents_summary_by_id_account_and_id_type, PSGetAccountDocumentsSummaryByIdAccountAndIdType},
		              {get_account_documents_summary_by_id_account_and_id_type_and_id_status, PSGetAccountDocumentsSummaryByIdAccountAndIdTypeAndIdStatus},
		              {delete_account_document, PSDeleteAccountDocument},
		              {delete_document_by_id_user_and_id_type, PSDeleteDocumentByIdUserAndIdType},
		              {get_parameterizations, PSGetParameterizations},
		              {get_parameterization, PSGetParameterization},
		              {update_parameterization, PSUpdateParameterization},
		              {get_user_types, PSGetUserTypes},
		              {count_user_types, PSCountUserTypes},
		              {get_user_statuses, PSGetUserStatuses},
		              {count_user_statuses, PSCountUserStatuses},
		              {get_document_types, PSGetDocumentTypes},
		              {count_document_types, PSCountDocumentTypes},
		              {get_document_statuses, PSGetDocumentStatuses},
		              {count_document_statuses, PSCountDocumentStatuses},
		              {get_transport_types, PSGetTransportTypes},
		              {get_transport_type, PSGetTransportType},
		              {count_transport_types, PSCountTransportTypes},
		              {count_courier_transport_status, PSCountCourierTransportStatus},
		              {get_user_notification_types, PSGetUserNotificationTypes},
		              {count_user_notification_types, PSCountUserNotificationTypes},
		              {get_order_payment_methods, PSGetOrderPaymentMethods},
		              {exists_order_payment_method, PSExistsOrderPaymentMethod},
		              {get_mobileos, PSGetMobileos},
		              {exists_mobileos, PSExistsMobileos},
		              {get_courier_transport_status, PSGetCourierTransportStatuses},
		              {get_courier_transports, PSGetCourierTransports},
		              {get_courier_transport, PSGetCourierTransport},
		              {get_active_courier_transports, PSGetActiveCourierTransports},
		              {get_current_courier_transport, PSGetCurrentCourierTransport},
		              {create_courier_transport, PSCreateCourierTransport},
		              {update_courier_transport, PSUpdateCourierTransport},
		              {update_current_courier_transport, PSUpdateCurrentCourierTransport},
		              {get_contact_request_statuses, PSGetContactRequestStatuses},
		              {count_contact_request_statuses, PSCountContactRequestStatuses},
		              {get_contact_requests, PSGetContactRequests},
		              {get_contact_request, PSGetContactRequest},
		              {create_contact_request, PSCreateContactRequest},
		              {update_contact_request, PSUpdateContactRequest},
		              {delete_contact_request, PSDeleteContactRequest},
		              {count_contact_status, PSCountContactStatuses},
		              {get_token_types, PSGetTokenTypes},
		              {get_token_type, PSGetTokenType},
		              {update_token_type, PSUpdateTokenType},
		              {create_token, PSCreateToken},
		              {get_token, PSGetToken},
		              {remove_token, PSRemoveToken},
		              {decrease_token_uses, PSDecreaseTokenUses},
		              {get_token_parameters, PSGetTokenParameters},
		              {create_token_parameter, PSCreateTokenParameter},
		              {remove_token_parameter, PSRemoveTokenParameter},
		              {get_account_statuses, PSGetAccountStatuses},
		              {get_account_user_types, PSGetAccountUserTypes},
		              {count_account_user_types, PSCountAccountUserTypes},
		              {get_accounts, PSGetAccounts},
		              {get_account, PSGetAccount},
		              {count_accounts_by_unique_key, PSCountAccountsByUniqueKey},
					  {count_account_by_id, PSCountAccountById},
		              {create_account, PSCreateAccount},
		              {increment_account_version, PSIncrementAccountVersion},
		              {verify_account_admin, PSVerifyAccountAdmin},
		              {update_account, PSUpdateAccount},
					  {update_account_occupancy, PSUpdateAccountOccupancy},
					  {count_ocupancy_type, PSCountOccupancyType},
		              {get_account_infos, PSGetAccountInfos},
		              {create_account_info, PSCreateAccountInfo},
		              {delete_account_info, PSDeleteAccountInfo},
		              {get_account_address, PSGetAccountAddress},
		              {create_account_address, PSCreateAccountAddress},
		              {delete_account_address, PSDeleteAccountAddress},
		              {create_account_user, PSCreateAccountUser},
		              {delete_account_user, PSDeleteAccountUser},
		              {get_account_user, PSGetAccountUser},
		              {get_account_user_by_id_user, PSGetAccountUserByIdUser},
		              {update_account_user, PSUpdateAccountUser}, 
		              {exists_account_user, PSExistsAccountUser}, 
		              {get_departments, PSGetDepartments},
		              {get_delivery_statuses, PSGetDeliveryStatuses},
		              {count_delivery_statuses, PSCountDeliveryStatuses},
		              {exists_account_department, PSExistsAccountDepartment},
		              {get_deliveries, PSGetDeliveries},
		              {get_ongoing_deliveries, PSGetOngoingDeliveries},
		              {get_deliveries_id_asc, PSGetDeliveriesIdAsc},
		              {get_deliveries_id_desc, PSGetDeliveriesIdDesc},
		              {get_deliveries_id_courier_asc, PSGetDeliveriesIdCourierAsc},
		              {get_deliveries_id_courier_desc, PSGetDeliveriesIdCourierDesc},
		              {get_deliveries_by_id_status_id_asc, PSGetDeliveriesByIdStatusIdAsc},
		              {get_deliveries_by_id_status_id_desc, PSGetDeliveriesByIdStatusIdDesc},
		              {get_deliveries_by_id_status_id_courier_asc, PSGetDeliveriesByIdStatusIdCourierAsc},
		              {get_deliveries_by_id_status_id_courier_desc, PSGetDeliveriesByIdStatusIdCourierDesc},
		              {get_deliveries_by_id_status, PSGetDeliveriesByIdStatus},
		              {get_deliveries_by_id_courier_id_asc, PSGetDeliveriesByIdCourierIdAsc},
		              {get_deliveries_by_id_courier_id_desc, PSGetDeliveriesByIdCourierIdDesc},
		              {get_deliveries_by_id_courier, PSGetDeliveriesByIdCourier},
		              {get_deliveries_by_id_courier_and_id_status_id_asc, PSGetDeliveriesByIdCourierAndIdStatusIdAsc},
		              {get_deliveries_by_id_courier_and_id_status_id_desc, PSGetDeliveriesByIdCourierAndIdStatusIdDesc},
		              {get_deliveries_by_id_courier_and_id_status, PSGetDeliveriesByIdCourierAndIdStatus},
		              {get_deliveries_by_id_status_id_courier_id_asc, PSGetDeliveriesByIdStatusIdCourierIdAsc},
		              {get_deliveries_by_id_status_id_courier_id_desc, PSGetDeliveriesByIdStatusIdCourierIdDesc},
		              {get_deliveries_by_id_status_id_courier_id_courier_asc, PSGetDeliveriesByIdStatusIdCourierIdCourierAsc},
		              {get_deliveries_by_id_status_id_courier_id_courier_desc, PSGetDeliveriesByIdStatusIdCourierIdCourierDesc},
		              {get_deliveries_by_id_status_id_courier, PSGetDeliveriesByIdStatusIdCourier},
		              {get_deliveries_by_id_courier_id_courier_asc, PSGetDeliveriesByIdCourierIdCourierAsc},
		              {get_deliveries_by_id_courier_id_courier_desc, PSGetDeliveriesByIdCourierIdCourierDesc},
					  {get_delivery_by_id_id_courier, PSGetDeliveryByIdIdCourier},
		              {get_delivery, PSGetDelivery},
		              {get_delivery_objects, PSGetDeliveryObjects},
		              {create_delivery, PSCreateDelivery},
		              {update_delivery_version, PSUpdateDeliveryVersion},
		              {update_delivery_status, PSUpdateDeliveryStatus},
		              {assign_delivery_dispatcher, PSAssignDeliveryDispatcher},
		              {delete_delivery, PSDeleteDelivery},
		              {delete_delivery_waypoint, PSDeleteDeliveryWaypoint},
		              {delete_delivery_object, PSDeleteDeliveryObject},
		              {delete_delivery_object_action, PSDeleteDeliveryObjectAction},
		              {delete_delivery_courier_route, PSDeleteDeliveryCourierRoute},					 
		              {get_generated_delivery_by_id_delivery, PSGetGeneratedDeliveryByIdDelivery},
		              {get_generated_deliveries_by_id_order, PSGetGeneratedDeliveriesByIdOrder},
		              {get_generated_delivery_waypoints, PSGetGeneratedDeliveryWaypoints},
		              {get_generated_delivery_objects, PSGetGeneratedDeliveryObjects},
		              {create_generated_delivery, PSCreateGeneratedDelivery},
		              {delete_generated_delivery, PSDeleteGeneratedDelivery},
		              {delete_generated_delivery_by_id_delivery, PSDeleteGeneratedDeliveryByIdDelivery},
		              {create_generated_delivery_waypoint, PSCreateGeneratedDeliveryWaypoint},
		              {delete_generated_delivery_waypoint, PSDeleteGeneratedDeliveryWaypoint},
		              {delete_generated_delivery_waypoint_by_id_delivery, PSDeleteGeneratedDeliveryWaypointByIdDelivery},
		              {create_generated_delivery_object, PSCreateGeneratedDeliveryObject},
		              {delete_generated_delivery_object, PSDeleteGeneratedDeliveryObject},
		              {delete_generated_delivery_object_by_id_delivery, PSDeleteGeneratedDeliveryObjectByIdDelivery},
		              {get_order_statuses, PSGetOrderStatuses},
		              {exists_order_status, PSExistsOrderStatus},
		              {get_last_user_order, PSGetLastUserOrder},
		              {get_order_origins, PSGetOrderOrigins},
		              {get_order_types, PSGetOrderTypes},
		              {create_order, PSCreateOrder},
		              {get_orders, PSGetOrders},
		              {get_orders_asc, PSGetOrdersAsc},
		              {get_orders_desc, PSGetOrdersDesc},
		              {get_orders_by_id_asc, PSGetOrdersByIdAsc},
		              {get_orders_by_id_desc, PSGetOrdersByIdDesc},
		              {get_orders_by_id_status_id_asc, PSGetOrdersByIdStatusIdAsc},
		              {get_orders_by_id_status_id_desc, PSGetOrdersByIdStatusIdDesc},
		              {get_orders_by_id_status, PSGetOrdersByIdStatus},
		              {get_orders_by_id_type, PSGetOrdersByIdType},
		              {get_orders_by_id_user_id_asc, PSGetOrdersByIdUserIdAsc},
		              {get_orders_by_id_user_id_desc, PSGetOrdersByIdUserIdDesc},
		              {get_orders_by_id_user, PSGetOrdersByIdUser},
		              {get_orders_by_id_user_and_id_status_id_asc, PSGetOrdersByIdUserAndIdStatusIdAsc},
		              {get_orders_by_id_user_and_id_status_id_desc, PSGetOrdersByIdUserAndIdStatusIdDesc},
		              {get_orders_by_id_user_and_id_status, PSGetOrdersByIdUserAndIdStatus},
		              {get_orders_for_generation, PSGetOrdersForGeneration},
					  {update_order_status, PSUpdateOrderStatus},
					  {update_order_status_prod, PSUpdateOrderStatusProd},
					  {update_order_cut_time, PSUpdateOrderCutTime},
					  {update_order_cut_time_production_time, PSUpdateOrderCutTimeProductionTime},
		              {get_order, PSGetOrder},
		              {get_order_by_id_id_user, PSGetOrderByIdIdUser},
					  {get_orders_by_id_delivery, PSGetOrdersByIdDelivery},
					  {get_dispatcher_orders_by_id_status_by_id_type, PSGetDispatcherOrdersByIdStatusAndIdType},
					  {get_dispatcher_orders_by_id_status_by_id_type_asc, PSGetDispatcherOrdersByIdStatusAndIdTypeAsc},
					  {get_dispatcher_orders_by_id_status_by_id_type_desc, PSGetDispatcherOrdersByIdStatusAndIdTypeDesc},
					  {get_dispatcher_orders_by_id_status_asc, PSGetDispatcherOrdersByIdStatusAsc},
					  {get_dispatcher_orders_by_id_status_desc, PSGetDispatcherOrdersByIdStatusDesc},
					  {get_dispatcher_orders_by_id_status, PSGetDispatcherOrdersByIdStatus},
					  {get_dispatcher_orders_by_id_type_asc, PSGetDispatcherOrdersByIdTypeAsc},
					  {get_dispatcher_orders_by_id_type_desc, PSGetDispatcherOrdersByIdTypeDesc},
					  {get_dispatcher_orders_by_id_type, PSGetDispatcherOrdersByIdType},
					  {get_dispatcher_orders, PSGetDispatcherOrders},
					  {get_dispatcher_orders_asc, PSGetDispatcherOrdersAsc},
					  {get_dispatcher_orders_desc, PSGetDispatcherOrdersDesc},
		              {exists_order_payment, PSExistsOrderPayment},
					  {get_orders_by_phone_nr_asc, PSGetOrdersByPhoneNrAsc}, 
					  {get_orders_by_phone_nr_desc, PSGetOrdersByPhoneNrDesc},
					  {get_orders_by_phone_nr, PSGetOrdersByPhoneNr}, 
					  {get_orders_by_id_status_by_phone_nr_asc, PSGetOrdersByIdStatusPhoneNrAsc},
					  {get_orders_by_id_status_by_phone_nr_desc, PSGetOrdersByIdStatusPhoneNrDesc},
					  {get_orders_by_id_status_by_phone_nr, PSGetOrdersByIdStatusPhoneNr} ,
					  {get_orders_by_id_type_by_phone_nr, PSGetOrdersByIdTypePhoneNr},
					  {get_dispatcher_orders_by_phone_nr_asc, PSGetDispatcherOrdersByPhoneAsc},
					  {get_dispatcher_orders_by_phone_nr_desc, PSGetDispatcherOrdersByPhoneDesc},
					  {get_dispatcher_orders_by_phone_nr, PSGetDispatcherOrdersByPhone} ,
					  {get_dispatcher_orders_by_id_status_by_phone_nr_asc, PSGetDispatcherOrdersByIdStatusByPhoneAsc},
					  {get_dispatcher_orders_by_id_status_by_phone_nr_desc, PSGetDispatcherOrdersByIdStatusByPhoneDesc},
				      {get_dispatcher_orders_by_id_status_by_phone_nr, PSGetDispatcherOrdersByIdStatusByPhone}, 
					  {get_dispatcher_orders_by_id_type_by_phone_nr_asc, PSGetDispatcherOrdersByIdTypeByPhoneAsc} ,
					  {get_dispatcher_orders_by_id_type_by_phone_nr_desc, PSGetDispatcherOrdersByIdTypeByPhoneDesc},
					  {get_dispatcher_orders_by_id_type_by_phone_nr, PSGetDispatcherOrdersByIdTypeByPhone},
					  {get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr_asc, PSGetDispatcherOrdersByIdStatusAndIdTypeByPhoneAsc},
					  {get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr_desc, PSGetDispatcherOrdersByIdStatusAndIdTypeByPhoneDesc},
					  {get_dispatcher_orders_by_id_status_by_id_type_by_phone_nr, PSGetDispatcherOrdersByIdStatusAndIdTypeByPhone},
					  {get_orders_by_id_user_by_id_type_asc, PSGetOrdersByIdUserIdTypeAsc}, 
					  {get_orders_by_id_user_by_id_type_desc, PSGetOrdersByIdUserIdTypeDesc},
					  {get_orders_by_id_user_by_id_type, PSGetOrdersByIdUserIdType}, 
					  {get_orders_by_id_user_id_status_id_type_asc, PSGetOrdersByIdUserIdStatusIdTypeAsc}, 
					  {get_orders_by_id_user_id_status_id_type_desc, PSGetOrdersByIdUserIdStatusIdTypeDesc},
					  {get_orders_by_id_user_id_status_id_type, PSGetOrdersByIdUserIdStatusIdType}, 
					  {get_orders_by_id_user_phone_nr_asc, PSGetOrdersByIdUserPhoneNrAsc},
					  {get_orders_by_id_user_phone_nr_desc, PSGetOrdersByIdUserPhoneNrDesc},
					  {get_orders_by_id_user_phone_nr, PSGetOrdersByIdUserPhoneNr},
					  {get_orders_by_id_user_and_id_status_phone_nr_asc, PSGetOrdersByIdUserIdStatusPhoneNrAsc},
					  {get_orders_by_id_user_and_id_status_phone_nr_desc, PSGetOrdersByIdUserIdStatusPhoneNrDesc},
					  {get_orders_by_id_user_and_id_status_phone_nr, PSGetOrdersByIdUserIdStatusPhoneNr},
					  {get_orders_by_id_user_by_id_type_by_phone_nr_asc, PSGetOrdersByIdUserIdTypePhoneNrAsc},
					  {get_orders_by_id_user_by_id_type_by_phone_nr_desc, PSGetOrdersByIdUserIdTypePhoneNrDesc},
					  {get_orders_by_id_user_by_id_type_by_phone_nr, PSGetOrdersByIdUserIdTypePhoneNr},
					  {get_orders_by_id_user_id_status_by_id_type_by_phone_nr_asc, PSGetOrdersByIdUserIdStatusIdTypePhoneNrAsc},
					  {get_orders_by_id_user_id_status_by_id_type_by_phone_nr_desc, PSGetOrdersByIdUserIdStatusIdTypePhoneNrDesc},
					  {get_orders_by_id_user_id_status_by_id_type_by_phone_nr, PSGetOrdersByIdUserIdStatusIdTypePhoneNr},
		              {create_order, PSCreateOrder},
		              {get_orders, PSGetOrders},
		              {create_order_prod, PSCreateOrderProd},
		              {get_order_prods, PSGetOrderProds},
		              {create_order_prod_option, PSCreateOrderProdOption},
		              {get_order_prod_options, PSGetOrderProdOptions},
		              {create_order_prod_option_entry, PSCreateOrderProdOptionEntry},
		              {get_order_prod_option_entries, PSGetOrderProdOptionEntries},
		              {create_order_waypoint, PSCreateOrderWaypoint},
		              {get_order_waypoints, PSGetOrderWaypoints},
		              {create_order_object, PSCreateOrderObject},
		              {get_order_objects, PSGetOrderObjects},
		              {count_order_types, PSCountOrderTypes},
		              {create_order_waypoint_object_action, PSCreateOrderWaypoinObjectAction},
		              {get_order_waypoint_object_actions, PSGetOrderWaypointObjectActions},
		              {get_waypoint_statuses, PSGetWaypointStatuses},
		              {count_waypoint_statuses, PSCountWaypointStatuses},
		              {get_delivery_waypoints, PSGetDeliveryWaypoints},
					  {get_delivery_waypoint, PSGetDeliveryWaypoint},
					  {get_delivery_waypoint_by_id_order, PSGetDeliveryWaypointByOrder},
		              {create_delivery_waypoint, PSCreateDeliveryWaypoint},
		              {update_delivery_waypoint_status_travel, PSUpdateDeliveryWaypointStatusTravel},
		              {update_delivery_waypoint_status_travel_arrived, PSUpdateDeliveryWaypointStatusTravelArrived},
		              {update_delivery_waypoint_status_stop, PSUpdateDeliveryWaypointStatusStop},
		              {get_object_actions, PSGetObjectActions},
		              {count_object_actions, PSCountObjectActions},
		              {get_object_statuses, PSGetObjectStatuses},
		              {count_object_statuses, PSCountObjectStatuses},
		              {get_object_types, PSGetObjectTypes},
		              {get_object_type, PSGetObjectType},
		              {count_object_types, PSCountObjectTypes},
		              {create_delivery_object, PSCreateDeliveryObject},
		              {create_delivery_waypoint_object_action, PSCreateDeliveryWaypointObjectAction},
		              {get_delivery_waypoint_object_actions, PSGetDeliveryWaypointObjectActions},
		              {get_delivery_waypoint_details, PSGetDeliveryWaypointDetails},
		              {get_delivery_waypoint_rating, PSGetDeliveryWaypointRating},
		              {create_order_waypoint_address, PSCreateOrderWaypointAddress},
		              {get_order_waypoint_address, PSGetOrderWaypointAddress},
					  {get_order_last_waypoint_address, PSGetOrderLastWaypointAddress},
					  {get_delivery_waypoint_signature, PSGetDeliveryWaypointSignature},
		              {get_notification_types, PSGetNotificationTypes},
		              {count_notification_types, PSCountNotificationTypes},
		              {get_api_key_by_status, PSGetApiKeyByStatus},
		              {get_user_courier, PSGetUserCourier},
		              {create_user_courier, PSCreateUserCourier},
		              {delete_user_courier, PSDeleteUserCourier},
		              {get_user_locations, PSGetUserLocations},
		              {get_user_location, PSGetUserLocation},
		              {create_user_location, PSCreateUserLocation},
		              {update_user_location, PSUpdateUserLocation},
		              {delete_user_location, PSDeleteUserLocation},
		              {get_user_location_components, PSGetUserLocationComponents},
		              {get_user_location_component, PSGetUserLocationComponent},
		              {create_user_location_component, PSCreateUserLocationComponent},
		              {delete_user_location_components, PSDeleteUserLocationComponents},
		              {get_user_location_contacts, PSGetUserLocationContacts},
		              {get_user_location_contact, PSGetUserLocationContact},
		              {create_user_location_contact, PSCreateUserLocationContact},
		              {delete_user_location_contacts, PSDeleteUserLocationContacts},
		              {get_order_deliveries_couriers, PSGetOrderDeliveriesCouriers},
		              {create_courier_route, PSCreateDeliveryCourierRoute},
		              {get_oauth_provider, PSGetOauthProvider},
		              {get_payment_provider, PSGetPaymentProvider},
		              {exists_payment_provider, PSExistsPaymentProvider},
		              {create_order_payment, PSCreateOrderPayment},
		              {count_payment_status, PSCountPaymentStatus},
		              {create_payment_request, PSCreatePaymentRequest},
		              {create_voucher, PSCreateVoucher},
		              {count_voucher, PSCountVoucher},
		              {get_voucher, PSGetVoucher},
		              {get_voucher_by_code, PSGetVoucherByCode},
		              {get_vouchers, PSGetVouchers},
		              {get_vouchers_code, PSGetVouchersCode},
		              {get_vouchers_id_status, PSGetVouchersIdStatus},
		              {get_vouchers_restrict_user_id, PSGetVouchersRestrictUserId},
		              {get_vouchers_code_id_status, PSGetVouchersCodeIdStatus},
		              {get_vouchers_code_restrict_user_id, PSGetVouchersCodeRestrictUserId},
		              {get_vouchers_id_status_restrict_user_id, PSGetVouchersIdStatusRestrictUserId},
		              {get_vouchers_code_id_status_restrict_user_id, PSGetVouchersCodeIdStatusRestrictUserId},
		              {update_voucher, PSUpdateVoucher},
		              {count_valid_vouchers, PSCountValidVouchers},
		              {create_order_voucher, PSCreateOrderVoucher},
		              {delete_order_voucher, PSDeleteOrderVoucher},
		              {count_voucher_type, PSCountVoucherType},
		              {get_voucher_types, PSGetVoucherTypes},
		              {count_voucher_status, PSCountVoucherStatus},
		              {get_voucher_statuses, PSGetVoucherStatuses},
		              {get_user_notifications, PSGetUserNotifications},
		              {delete_user_notifications, PSDeleteUserNotifications},
		              {create_user_notification, PSCreateUserNotification},
		              {count_user_notification_type, PSCountUserNotificationType},
		              {get_commercial_info, PSGetCommercialInfo},
		              {update_commercial_info, PSUpdateCommercialInfo},
		              {create_commercial_info, PSCreateCommercialInfo},
		              {get_zones, PSGetZones},
		              {count_zones, PSCountZones},
		              {get_zone_postal_codes, PSGetZonePostalCodes},
		              {get_user_zones, PSGetUserZones},
		              {exists_user_zone, PSExistsUserZone},
		              {delete_user_zones, PSDeleteUserZones},
		              {create_user_zone, PSCreateUserZone},
		              {get_user_by_username_password, PSGetUserByUsernamePassword},
		              {get_user_by_id_password, PSGetUserByIdPassword},
		              {get_user_by_username, PSGetUserByUsername},
		              {get_user_oauth, PSGetUserOauth},
		              {get_account_users, PSGetAccountUsers},
		              {get_account_users_by_user_id_type, PSGetAccountUsersByUserIdType},
		              {get_user_accounts, PSGetUserAccounts},
		              {create_notification, PSCreatenotification},
					  {count_client_courier_deliveries, PSCountClientCourierDeliveries},
		              {get_user_couriers, PSGetUserCouriers},
		              {get_user_couriers_name_asc, PSGetUserCouriersNameAsc},
		              {get_user_couriers_name_desc, PSGetUserCouriersNameDesc},
		              {get_user_locations_by_name, PSGetUserLocationsByName},
		              {get_user_locations_by_phone_nr, PSGetUserLocationsByPhoneNr},
		              {get_user_locations_by_name_and_phone_nr, PSGetUserLocationsByNameAndPhoneNr},
					  {get_object_qty_by_type_by_delivery, PSGetObjectQtyByTypeByDelivery},
 		              {get_address_component_by_delivery, PSGetAddressComponentByDelivery},
 		              {get_estimated_delivery_time, PSGetEstimatedDeliveryTime},
					  {get_waypoint_detail_by_delivery_id_order_id, PSGetWaypointDetailByDeliveryId},
		              {get_last_id, PSGetLastId},
		              {get_order_objects_volume, PSGetOrderObjectsVolume},
					  {get_delivery_estimated_times_by_order_id, PSGetDeliveryEstimatedTimes},
					  {get_delivery_travel_distance_by_order_id, PSGetDeliveryTravelDistance},
					  {create_booking_schedule_period_today, PSCreateBookingSchedulePeriodToday},
					  {create_booking_schedule_period_tomorrow, PSCreateBookingSchedulePeriodTomorrow},
					  {get_account_slots_information, PSGetAccountSlotsInformation},
					  {get_account_slot_information, PSGetAccountSlotInformation},
					  {count_slot_existance, PSCountAccountSlotExistance},
					  {count_slot_availability, PSCountAccountSlotAvailability},
					  {update_account_slots_information, PSUpdateAccountSlotsInformation},
					  {get_todays_slots_information, PSGetTodaysSlotsInformation},
					  {update_slot_max_occupancy_by_id, PSUpdateSlotMaxOccupancyById},
					  {verify_todays_slot_existance, PSVerifyTodaySlotExistance}
		             ],
		{ok, Connection, Statements}
	catch
		_:Reason -> {error, Reason}
	end.

connect(Properties) ->
	Hostname = proplists:get_value(?PROPERTY_DB_HOSTNAME, Properties),
	Port = proplists:get_value(?PROPERTY_DB_PORT, Properties),
	Database = proplists:get_value(?PROPERTY_DB_DATABASE, Properties),
	Username = proplists:get_value(?PROPERTY_DB_USERNAME, Properties),
	Password = proplists:get_value(?PROPERTY_DB_PASSWORD, Properties),
	case epgsql:connect(Hostname, Username, Password, [{port, Port}, {database, Database}]) of
		{ok, Connection} -> Connection;
		Error ->
			error_logger:error_msg("~p:connect(~p): Error connecting to the DB: ~p", [?MODULE, Properties, Error]),
			throw(unable_to_connect_to_db)
	end.

prepare_statement(Connection, Name, SQL) ->
	case epgsql:parse(Connection, Name, SQL, []) of
		{ok, Statement} -> Statement;
		Error ->
			error_logger:error_msg("~p:prepare_statement(..., ~p, ~p): Error preparing statement: ~p", [?MODULE, Name, SQL, Error]),
			throw(unable_to_prepare_statement)
end.

execute_statement(StatementName, Parameters, State) -> execute_statement(StatementName, Parameters, State, 0).
execute_statement(StatementName, Parameters, #state{connection=Connection, statements=Statements}, MaxRows) ->
	% MaxRows 0 means no limit
	case lists:keyfind(StatementName, 1, Statements) of
		{StatementName, Statement} ->
			case run_bind(Connection, Statement, Parameters) of
				ok ->
					case run_execute(Connection, Statement, MaxRows) of
						{Success, Result} when Success =:= ok orelse Success =:= partial -> {ok, Result};
						{error, Why} ->
							error_logger:error_msg("~p:execute_statement(~p, ..., ...): Error executing the prepared statement: ~p", [?MODULE, StatementName, Why]),
							{error, execute_statement}
					end;
				{error, Why} ->
					error_logger:error_msg("~p:execute_statement(~p, ..., ...): Error binding parameters: ~p", [?MODULE, StatementName, Why]),
					{error, binding_parameters}
			end;
		_Error ->
			error_logger:error_msg("~p:execute_statement(~p, ..., ~p): Prepared statement not found", [?MODULE, StatementName, Statements]),
			{error, statement_not_found}
	end.

return_single_result(StatementName, Parameters, BuildObjectFunction, State) ->
	Reply = get_single_result(StatementName, Parameters, BuildObjectFunction, State),
	{reply, Reply, State}.

get_single_result(StatementName, Parameters, BuildObjectFunction, State) ->
	case execute_statement(StatementName, Parameters, State) of
		{ok, []} -> not_found;
		{ok, Result} when length(Result) > 1 ->
			error_logger:error_msg("~p:get_single_result(~p, ~p, ..., ...): Query returned more than one result", [?MODULE, StatementName, Parameters]),
			more_than_one;
		{ok, [Row]} ->
			case BuildObjectFunction(Row) of
				invalid_record -> error;
				Object -> Object
			end;
		{error, _Error} -> error
	end.

return_multiple_results(StatementName, Parameters, MaxRows, BuildObjectFunction, State) ->
	Reply = get_multiple_results(StatementName, Parameters, MaxRows, BuildObjectFunction, State),
	{reply, Reply, State}.

get_multiple_results(StatementName, Parameters, MaxRows, BuildObjectFunction, State) ->
	case execute_statement(StatementName, Parameters, State, MaxRows) of
		{ok, Results} ->
			case build_list(Results, BuildObjectFunction) of
				invalid_record -> error;
				Objects -> Objects
			end;
		{error, _Error} -> error
	end.

return_reference_data(StatementName, State) ->
	return_multiple_results(StatementName, [], 0, fun(Row) -> build_reference_data(Row) end, State).

get_integer_value(StatementName, Parameters, State) ->
	case execute_statement(StatementName, Parameters, State) of
		{ok, [{IntegerValue}]} when is_integer(IntegerValue) -> {ok, IntegerValue};
		{_, _ErrorCause} -> nok
	end.

return_integer_value(StatementName, Parameters, State) ->
	case get_integer_value(StatementName, Parameters, State) of
		{ok, Count} -> Reply = Count;
		nok -> Reply = error
	end,
	{reply, Reply, State}.

return_exists(StatementName, Parameters, State) ->
	Reply = exists(StatementName, Parameters, State),
	{reply, Reply, State}.

exists(StatementName, Parameters, State) ->
	case get_integer_value(StatementName, Parameters, State) of
		{ok, Count} when Count =:= 0 -> false;
		{ok, Count} when Count > 0 -> true;
		nok -> error
	end.

return_exists_result(StatementName, Parameters, State) ->
	Reply = get_exists_result(StatementName, Parameters, State),
	{reply, Reply, State}.

get_exists_result(StatementName, Parameters, State) ->
	case execute_statement(StatementName, Parameters, State) of
		{ok, []} -> false;
		{ok, _List} -> true;
		{error, _Error} -> error
	end.

execute_deliveries_information_query(Queries, Params, #rs_navigation{sort=Sort, skip=Skip, max=Max}, State) ->
	% Get sort asc/desc query
	Query = get_sort_query(Sort, Queries),
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, Params, MaxResults, fun(Row) -> build_delivery(Row) end, State) of
		Deliveries when is_list(Deliveries) ->
			TrimmedList = trim_list(Deliveries, Skip, Max),
			[build_delivery_information(Delivery, State) || Delivery <- TrimmedList];
		Other -> Other
	end.

execute_dispatcher_deliveries_information_query(Queries, Params, ProductionTime, BufferTime, #rs_navigation{sort=Sort, skip=Skip, max=Max}, State) ->
	% Get sort asc/desc query
	Query = get_sort_query(Sort, Queries),
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, Params, MaxResults, fun(Row) -> build_delivery(Row) end, State) of
		Deliveries when is_list(Deliveries) ->
			TrimmedList = trim_list(Deliveries, Skip, Max),
			[build_dispatcher_delivery_information(Delivery, ProductionTime, BufferTime, State) || Delivery <- TrimmedList];
		Other -> Other
	end.

execute_orders_information_query(Queries, Params, #rs_navigation{sort=Sort, skip=Skip, max=Max}, State) ->
	% Get sort asc/desc query
	Query = get_sort_query(Sort, Queries),
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, Params, MaxResults, fun(Row) -> build_order(Row) end, State) of
		Orders when is_list(Orders) ->
			TrimmedList = trim_list(Orders, Skip, Max),
			[build_order_information(Order, State) || Order <- TrimmedList];
		Other -> Other
	end.

execute_orders_information_dispatcher_query(Queries, Params, ProductionTime, BufferTime, #rs_navigation{sort=Sort, skip=Skip, max=Max}, State) ->
	% Get sort asc/desc query
	Query = get_sort_query(Sort, Queries),
	MaxResults = get_max_results(Skip, Max),
	case get_multiple_results(Query, Params, MaxResults, fun(Row) -> build_order(Row) end, State) of
		Orders when is_list(Orders) ->
			TrimmedList = trim_list(Orders, Skip, Max),
			[build_order_information_dispatcher(Order, ProductionTime, BufferTime, State) || Order <- TrimmedList];
		Other -> Other
	end.

build_account_information(Account=#account{id=AccountId}, State) ->
	AccountInfos = get_multiple_results(get_account_infos, [AccountId], 0, fun(Row) -> build_account_info(Row) end, State),
	AccountAddress = get_multiple_results(get_account_address, [AccountId], 0, fun(Row) -> build_account_address(Row) end, State),
	#inf_account{account=Account, account_infos=AccountInfos, account_address=AccountAddress}.

build_zone_information(Zone=#zone{id=ZoneId}, State) ->
	PostalCodes = get_multiple_results(get_zone_postal_codes, [ZoneId], 0, fun(Row) -> build_zone_postal_code(Row) end, State),
	#inf_zone{zone=Zone, postal_codes=PostalCodes}.

build_user_information(User=#user{id=UserId}, State) ->
	{AccountUserInfo, Account} =
		case get_single_result(get_account_user_by_id_user, [UserId], fun(Row) -> build_account_user(Row) end, State) of
			AccountUserResult when is_record(AccountUserResult, account_user) ->
				case get_single_result(get_account, [AccountUserResult#account_user.id_account], fun(Row) -> build_account(Row) end, State) of
					AccountResult when is_record(AccountResult, account) ->
						{AccountUserResult, AccountResult};
					_ -> {AccountUserResult, undefined}
				end;
			_ -> {undefined, undefined}
		end,
	UserAddress = get_multiple_results(get_user_address, [UserId], 0, fun(Row) -> build_user_address(Row) end, State),
	Transports = get_multiple_results(get_courier_transports, [UserId], 0, fun(Row) -> build_courier_transport(Row) end, State),
	Zones = get_multiple_results(get_user_zones, [UserId], 0, fun(Row) -> build_user_zone(Row) end, State),
	#inf_user{user=User, account=Account, account_user=AccountUserInfo, user_address=UserAddress, transports=Transports, zones=Zones}.

build_delivery_information(Delivery=#delivery{id=DeliveryId}, State) ->
	Waypoints = get_multiple_results(get_delivery_waypoints, [DeliveryId], 0, fun(Row) -> build_u_delivery_waypoint_information(Row, State) end, State),
	Objects = get_multiple_results(get_delivery_objects, [DeliveryId], 0, fun(Row) -> build_delivery_object(Row) end, State),
	GeneratedDeliveries = get_multiple_results(get_generated_delivery_by_id_delivery, [DeliveryId], 0, fun(Row) -> build_generated_delivery(Row) end, State),
	Orders = [get_single_result(get_order, [OrderId], fun(Row) -> build_order_information(Row, State) end, State)|| #generated_delivery{id_order=OrderId} <- GeneratedDeliveries],
	#inf_delivery{delivery=Delivery, waypoints=Waypoints, objects=Objects, orders=Orders}.

build_dispatcher_delivery_information(Delivery=#delivery{id=DeliveryId}, ProductionTime, BufferTime, State) ->
	Waypoints = get_multiple_results(get_delivery_waypoints, [DeliveryId], 0, fun(Row) -> build_u_delivery_waypoint_information(Row, State) end, State),
	Objects = get_multiple_results(get_delivery_objects, [DeliveryId], 0, fun(Row) -> build_delivery_object(Row) end, State),
	GeneratedDeliveries = get_multiple_results(get_generated_delivery_by_id_delivery, [DeliveryId], 0, fun(Row) -> build_generated_delivery(Row) end, State),
	Orders = [get_single_result(get_order, [OrderId], fun(Row) -> build_order_information_dispatcher(Row, ProductionTime, BufferTime, State) end, State)|| #generated_delivery{id_order=OrderId} <- GeneratedDeliveries],
	#inf_delivery{delivery=Delivery, waypoints=Waypoints, objects=Objects, orders=Orders}.

build_u_delivery_waypoint_information(Waypoint=#u_delivery_waypoint{id_delivery=DeliveryId, id_waypoint=WaypointId}, State) ->
	case get_multiple_results(get_delivery_waypoint_details, [DeliveryId, WaypointId], 0, fun(Row) -> build_u_delivery_waypoint_detail(Row, State) end, State) of
		WaypointDetails when is_list(WaypointDetails) -> #inf_delivery_waypoint{waypoint=Waypoint, details=WaypointDetails};
		Other -> Other
	end;
build_u_delivery_waypoint_information(Row, State) ->
	case build_u_delivery_waypoint(Row) of
		DeliveryWaypoint when is_record(DeliveryWaypoint, u_delivery_waypoint) -> build_u_delivery_waypoint_information(DeliveryWaypoint, State);
		Other -> Other
	end.

build_order_information(Order=#order{id_order=OrderId}, State) ->
	OrderProds = get_multiple_results(get_order_prods, [OrderId], 0, fun(RowProd) -> build_order_prod_information(RowProd, State) end, State),
	Waypoints = get_multiple_results(get_order_waypoints, [OrderId], 0, fun(RowWaypoint) -> build_order_waypoint_information(RowWaypoint, State) end, State),
	Objects = get_multiple_results(get_order_objects, [OrderId], 0, fun(RowObject) -> build_order_object(RowObject) end, State),
	AdditionalInfo = #dispatcher_info{},
	CourierInfo = get_multiple_results(get_order_deliveries_couriers, [OrderId], 0, fun(Row) -> build_u_order_delivery_courier(Row) end, State),
	#inf_order{order=Order, waypoints=Waypoints, objects=Objects, order_prods=OrderProds, additional_info=AdditionalInfo, courier_info=CourierInfo};
build_order_information(Row, State) ->
	case build_order(Row) of
		Order when is_record(Order, order) -> build_order_information(Order, State);
		Other -> Other
	end.

build_order_information_dispatcher(Order=#order{id_order=OrderId, creation_date=CreationDate, start_prod_date=StartProdDate}, ProductionTime, BufferTime, State) ->
	OrderProds = get_multiple_results(get_order_prods, [OrderId], 0, fun(RowProd) -> build_order_prod_information(RowProd, State) end, State),
	Waypoints = get_multiple_results(get_order_waypoints, [OrderId], 0, fun(RowWaypoint) -> build_order_waypoint_information(RowWaypoint, State) end, State),
	Objects = get_multiple_results(get_order_objects, [OrderId], 0, fun(RowObject) -> build_order_object(RowObject) end, State),
	AdditionalInfo = get_additional_order_info(OrderId, CreationDate, StartProdDate, ProductionTime, BufferTime, State),
	CourierInfo = get_multiple_results(get_order_deliveries_couriers, [OrderId], 0, fun(Row) -> build_u_order_delivery_courier(Row) end, State),
	#inf_order{order=Order, waypoints=Waypoints, objects=Objects, order_prods=OrderProds, additional_info=AdditionalInfo, courier_info=CourierInfo};
build_order_information_dispatcher(Row, ProductionTime, BufferTime, State) ->
	case build_order(Row) of
		Order when is_record(Order, order) -> build_order_information_dispatcher(Order, ProductionTime, BufferTime, State);
		Other -> Other
	end.

build_order_waypoint_information(OrderWaypoint=#order_waypoint{id_order=OrderId, id_waypoint=WaypointId}, State) ->
	WaypointAddress = get_multiple_results(get_order_waypoint_address, [OrderId, WaypointId], 0, fun(Row) -> build_order_waypoint_address(Row) end, State),
	WaypointObjectActions = get_multiple_results(get_order_waypoint_object_actions, [OrderId, WaypointId], 0, fun(Row) -> build_order_waypoint_object_action(Row) end, State),
	#inf_order_waypoint{waypoint=OrderWaypoint, object_actions=WaypointObjectActions, address=WaypointAddress};
build_order_waypoint_information(Row, State) ->
	case build_order_waypoint(Row) of
		OrderWaypoint when is_record(OrderWaypoint, order_waypoint) -> build_order_waypoint_information(OrderWaypoint, State);
		Other -> Other
	end.

build_order_prod_information(OrderProd=#order_prod{id_order=OrderId, id_product=ProductId}, State) ->
	OrderProdOptions = get_multiple_results(get_order_prod_options, [OrderId, ProductId], 0, fun(Row) -> build_order_prod_option_information(Row, State) end, State),
	#inf_order_prod{order_prod=OrderProd, order_prod_options=OrderProdOptions};
build_order_prod_information(Row, State) ->
	case build_order_prod(Row) of
		OrderProd when is_record(OrderProd, order_prod) -> build_order_prod_information(OrderProd, State);
		Other -> Other
	end.

build_order_prod_option_information(OrderProdOption=#order_prod_option{id_order=OrderId, id_product=ProductId,
																	   id_prod_option=ProdOptionId}, State) ->
	OrderProdOptionEntries = get_multiple_results(get_order_prod_option_entries, [OrderId, ProductId, ProdOptionId], 0, fun(Row) -> build_order_prod_option_entry(Row) end, State),
	#inf_order_prod_option{order_prod_option=OrderProdOption, order_prod_option_entries=OrderProdOptionEntries};
	build_order_prod_option_information(Row, State) ->
	case build_order_prod_option(Row) of
		OrderProdOption when is_record(OrderProdOption, order_prod_option) -> build_order_prod_option_information(OrderProdOption, State);
		Other -> Other
	end.

% build delivery travel distance
build_delivery_travel_distance({IdDelivey, TravelDistance}) ->
	#delivery_travel_distance{id_delivery=IdDelivey, travel_distance=TravelDistance};
build_delivery_travel_distance(OtherRecord) ->
	error_logger:error_msg("~p:build_delivery_travel_distance(~p): Invalid record for build_delivery_travel_distance", [?MODULE, OtherRecord]),
	invalid_record.

get_additional_order_info(OrderId, CreationDate, StartProdDate, ProductionTime, BufferTime, State) ->
			 
	% Get travel distance from start to WP
	case get_single_result(get_delivery_travel_distance_by_order_id, [OrderId], fun(Row) -> build_delivery_travel_distance(Row) end, State) of
		DeliveryTravelDistance when is_record(DeliveryTravelDistance, delivery_travel_distance) -> 
		TravelDistance = DeliveryTravelDistance#delivery_travel_distance.travel_distance;
		_ -> TravelDistance = 0
	end,

	% Estimated Start Delivery Time 
	if
		StartProdDate =/= null andalso StartProdDate =/= undefined ->
			StartDeliveryTimeEst = add_minutes_to_db_timestamp(ProductionTime, StartProdDate);
		true ->
			% Adding buffer time
			CreationDate_Buffer = add_minutes_to_db_timestamp(BufferTime, CreationDate),
			% Adding production time
			StartDeliveryTimeEst = add_minutes_to_db_timestamp(ProductionTime, CreationDate_Buffer)
	end,

	#dispatcher_info{order_id=OrderId, start_delivery_time_est=StartDeliveryTimeEst, travel_distance=TravelDistance}.

build_user_location_information(Location=#user_location{id=LocationId}, State) ->
	case get_multiple_results(get_user_location_components, [LocationId], 0, fun(Row) -> build_user_location_component(Row) end, State) of
		Components when is_list(Components) ->
			case get_multiple_results(get_user_location_contacts, [LocationId], 0, fun(RowContact) -> build_user_location_contact(RowContact) end, State) of
				Contacts when is_list(Contacts) ->
					#inf_location{location=Location, components=Components, contacts=Contacts};
				Other -> Other
			end;
		Other -> Other
	end;
build_user_location_information(Row, State) ->
	case build_user_location(Row) of
		Location when is_record(Location, user_location) -> build_user_location_information(Location, State);
		Other -> Other
	end.

build_user_notifications_information([]) ->
	#inf_user_notifications{enabled=false};
build_user_notifications_information(UserNotifications) ->
	AllEnabled = lists:any(fun(#user_notification{enabled=Enabled}) -> Enabled end, UserNotifications),
	Ids = [IdType || #user_notification{enabled=ThisAllEnabled, id_type=IdType} <- UserNotifications, ThisAllEnabled =:= AllEnabled],
	#inf_user_notifications{enabled=AllEnabled, notification_type_ids=Ids}.

build_online_courier_info(SessionInfo=#session_info{id_user=UserId}, State) ->
	User = get_single_result(get_user, [UserId], fun(Row) -> build_user(Row) end, State),
	PhotoDocument =
		case get_multiple_results(get_user_documents_by_id_user_and_id_type_and_id_status,
		                          [UserId, ?DB_DOCUMENT_TYPE_PHOTO, ?DB_DOCUMENT_STATUS_VALID],
		                          0, fun(Row) -> build_document(Row) end, State) of
			[] -> undefined;
			[Document|_Rest] -> Document
		end,
	Photo =
		case PhotoDocument of
			undefined -> undefined;
			#document{name=Name, mimetype=Mimetype, base64_data=Base64Data} ->
				#file{name=Name, mimetype=Mimetype, base64_data=Base64Data}
		end,
	#inf_online_courier{session_info=SessionInfo, user=User, photo=Photo}.

build_online_courier_account_info(#account{id=AccountId, account_name=AccountName}) ->
	#inf_online_courier_account{id_account=AccountId, name=AccountName}.

run_bind(Connection, Statement, Parameters) ->
	case epgsql:bind(Connection, Statement, Parameters) of
		{error, sync_required} ->
			case epgsql:sync(Connection) of
				ok -> run_bind(Connection, Statement, Parameters);
				Other -> Other
			end;
		Other -> Other
	end.

run_execute(Connection, Statement, MaxRows) ->
	% MaxRows value 0 means no limit. On insert/update/delete the return is just one line, as we expect
	case epgsql:execute(Connection, Statement, MaxRows) of
		{error, sync_required} ->
			case epgsql:sync(Connection) of
				ok -> run_execute(Connection, Statement, MaxRows);
				Other -> Other
			end;
		Other -> Other
	end.

run_squery(Connection, SQL) ->
	case epgsql:squery(Connection, SQL) of
		{error, sync_required} ->
			case epgsql:sync(Connection) of
				ok -> run_squery(Connection, SQL);
				Other -> Other
			end;
		Other -> Other
	end.

% transaction function that uses run_squery
with_transaction(Connection, Function) ->
	try
		{ok, [], []} = run_squery(Connection, "BEGIN WORK"),
		Result = Function(),
		{ok, [], []} = run_squery(Connection, "COMMIT WORK"),
		Result
	catch
		_:Error ->
			error_logger:error_msg("~p:with_transaction(..., ...): Transaction rollback: ~p", [?MODULE, Error]),
			run_squery(Connection, "ROLLBACK WORK"),
			error
	end.

% Parameter type calculations
get_token_type_info(TokenTypeId, CurrentTimestamp, State) ->
	case get_single_result(get_token_type, [TokenTypeId], fun(Row) -> build_token_type(Row) end, State) of
		#token_type{multiple_uses=MultipleUses, uses=Uses, expires_in_minutes=ExpiresInMinutes} ->
			case MultipleUses of
				true -> RemainingUses = Uses;
				false -> RemainingUses = null
			end,
			case ExpiresInMinutes of
				null -> InvalidAfter = null;
				_ -> InvalidAfter = add_minutes_to_db_timestamp(ExpiresInMinutes, CurrentTimestamp)
			end,
			{ok, RemainingUses, InvalidAfter};
		_Error -> error
	end.

% Token creation rule (to be used inside transaction)
create_token_rule(Id, TokenTypeId, TokenParameters, OperationTimestamp, State) ->
	% t_token
	{ok, TokenId} = eb_token_util:generate_token(Id),
	{ok, RemainingUses, InvalidAfter} = get_token_type_info(TokenTypeId, OperationTimestamp, State),
	{ok, _} = execute_statement(create_token, [TokenId, TokenTypeId, RemainingUses, InvalidAfter, OperationTimestamp], State),
	% t_token_parameter
	CreateTokenParameter = fun F(_TokenId, [], _FState) -> ok;
	                           F(FTokenId, [#token_parameter{id_type=TypeId, value=Value}|Rest], FState) ->
	                               {ok, _} = execute_statement(create_token_parameter, [FTokenId, TypeId, Value], FState),
	                               F(FTokenId, Rest, FState)
	end,
	ok = CreateTokenParameter(TokenId, TokenParameters, State),
	{ok, TokenId}.

% Delivery waypoint creation rule (to be used inside transaction)
create_delivery_waypoints_rule(_Delivery, _FirstWaypointId, [], _OperationTimestamp, _State) -> ok;
create_delivery_waypoints_rule(Delivery, FirstWaypointId, [Waypoint|Rest], OperationTimestamp, State) ->
	ok = create_delivery_waypoint_rule(Delivery, FirstWaypointId, Waypoint, OperationTimestamp, State),
	create_delivery_waypoints_rule(Delivery, (FirstWaypointId + 1), Rest, OperationTimestamp, State).

create_delivery_waypoint_rule(DeliveryId, WaypointId,
							  #new_delivery_waypoint{order_id=OrderId, order_waypoint_id=OrderWaypointId,
													 travel_distance=TravelDistance, travel_time_est=TravelTimeEst,
													 stop_duration_est=StopDurationEst, object_actions=ObjectActions},
							  OperationTimestamp, State) ->
	% t_delivery_waypoint
	{ok, _} = execute_statement(create_delivery_waypoint, [DeliveryId, WaypointId, TravelDistance, TravelTimeEst,
														   StopDurationEst, ?DB_WAYPOINT_STATUS_WAITING,
														   OperationTimestamp], State),
	% t_generated_delivery_waypoint
	{ok, _} = execute_statement(create_generated_delivery_waypoint, [DeliveryId, WaypointId, OrderId, OrderWaypointId], State),
	% t_delivery_waypoint_object_action
	ok = create_delivery_waypoint_object_actions_rule(DeliveryId, WaypointId, ObjectActions, OperationTimestamp, State),
	ok.

% Generated delivery creation rule (to be used inside transaction)
create_generated_deliveries_rule([], _DeliveryId, _State) -> ok;
create_generated_deliveries_rule([OrderId|Rest], DeliveryId, State) ->
	ok = create_generated_delivery_rule(OrderId, DeliveryId, State),
	create_generated_deliveries_rule(Rest, DeliveryId, State).

create_generated_delivery_rule(OrderId, DeliveryId, State) ->
	{ok, _} = execute_statement(create_generated_delivery, [DeliveryId, OrderId], State),
	ok.

% Generated delivery waypoint creation rule (to be used inside transaction)
create_generated_delivery_waypoints_rule([], _DeliveryId, _State) -> ok;
create_generated_delivery_waypoints_rule([Gen|Rest], DeliveryId, State) ->
	ok = create_generated_delivery_waypoint_rule(Gen, DeliveryId, State),
	create_generated_delivery_waypoints_rule(Rest, DeliveryId, State).

create_generated_delivery_waypoint_rule(#generated_delivery_waypoint{id_order=OrderId, id_waypoint=OrderWaypointId}, DeliveryId, State) ->
	{ok, _} = execute_statement(create_generated_delivery_waypoint, [DeliveryId, 1, OrderId, OrderWaypointId], State),
	ok.

% Clean orders deliveries rule (to be used inside transaction)
clean_orders_deliveries([], _State) -> ok;
clean_orders_deliveries([OrderId|Rest], State) ->
	GeneratedDeliveries = get_multiple_results(get_generated_deliveries_by_id_order, [OrderId], 0,
	                                           fun(Row) -> build_generated_delivery(Row) end, State),
	DeliveryIds = [IdDelivery || #generated_delivery{id_delivery=IdDelivery} <- GeneratedDeliveries],
	ok = delete_deliveries(DeliveryIds, State),
	clean_orders_deliveries(Rest, State).

% Validate orders versions rule (to be used inside transaction)
validate_orders_versions([], _State) -> ok;
validate_orders_versions([#inf_order{order=#order{id_order=IdOrder, version=Version}}|Rest], State) ->
	#order{version=Version} = get_single_result(get_order, [IdOrder], fun(Row) -> build_order(Row) end, State),
	validate_orders_versions(Rest, State).

% Delete deliveries rule (to be used inside transaction)
delete_deliveries([], _State) -> ok;
delete_deliveries([DeliveryId|Rest], State) ->
	% t_generated_delivery_object
	{ok, _} = execute_statement(delete_generated_delivery_object_by_id_delivery, [DeliveryId], State),
	% t_generated_delivery_waypoint
	{ok, _} = execute_statement(delete_generated_delivery_waypoint_by_id_delivery, [DeliveryId], State),
	% t_generated_delivery
	{ok, _} = execute_statement(delete_generated_delivery_by_id_delivery, [DeliveryId], State),
	% t_delivery_courier_route
	{ok, _} = execute_statement(delete_delivery_courier_route, [DeliveryId], State),
	% t_delivery_object_action
	{ok, _} = execute_statement(delete_delivery_object_action, [DeliveryId], State),
	% t_delivery_object
	{ok, _} = execute_statement(delete_delivery_object, [DeliveryId], State),
	% t_delivery_waypoint
	{ok, _} = execute_statement(delete_delivery_waypoint, [DeliveryId], State),
	% t_delivery
	{ok, _} = execute_statement(delete_delivery, [DeliveryId], State),
	delete_deliveries(Rest, State).

% Delivery object creation rule (to be used inside transaction)
create_delivery_objects_rule(_DeliveryId, [], _State) -> ok;
create_delivery_objects_rule(DeliveryId, [Object|Rest], State) ->
	ok = create_delivery_object_rule(DeliveryId, Object, State),
	create_delivery_objects_rule(DeliveryId, Rest, State).
create_delivery_object_rule(DeliveryId, #new_delivery_object{order_id=OrderId, order_object_id=OrderObjectId,
															 object_id=ObjectId, reference=Reference, type_id=TypeId,
															 transport_auth=TransportAuth}, State) ->
	% t_delivery_object
	{ok, _} = execute_statement(create_delivery_object, [DeliveryId, ObjectId, Reference, TypeId, TransportAuth], State),
	% t_generated_delivery_object
	{ok, _} = execute_statement(create_generated_delivery_object, [DeliveryId, ObjectId, OrderId, OrderObjectId], State),
	ok.

create_delivery_waypoint_object_actions_rule(_DeliveryId, _WaypointId, [], _OperationTimestamp, _State) -> ok;
create_delivery_waypoint_object_actions_rule(DeliveryId, WaypointId, [ObjectAction|Rest], OperationTimestamp, State) ->
	ok = create_delivery_waypoint_object_action_rule(DeliveryId, WaypointId, ObjectAction, OperationTimestamp, State),
	create_delivery_waypoint_object_actions_rule(DeliveryId, WaypointId, Rest, OperationTimestamp, State).

create_delivery_waypoint_object_action_rule(DeliveryId, WaypointId,
											#new_delivery_object_action{object_id=ObjectId,
																		object_action_id=ObjectActionId},
											OperationTimestamp, State) ->
	% t_delivery_waypoint_object_action
	{ok, _} = execute_statement(create_delivery_waypoint_object_action,
								[DeliveryId, WaypointId, ObjectId, ObjectActionId, ?DB_OBJECT_STATUS_PENDING,
								 OperationTimestamp], State),
	ok.

% Order prods creation rule (to be used inside transaction)
create_order_prods_rule(_OrderId, [], _State) -> ok;
create_order_prods_rule(OrderId, [OrderProd|Rest], State) ->
	ok = create_order_prod_rule(OrderId, OrderProd, State),
	create_order_prods_rule(OrderId, Rest, State).

create_order_prod_rule(OrderId, #new_order_prod{id_product=ProductId, description=Description,
												name=Name, quantity=Quantity, gross_amount=GrossAmount,
												prod_options=ProdOptions}, State) ->
	% t_order_prod
	{ok, _} = execute_statement(create_order_prod, [OrderId, ProductId, Description, Name, Quantity, GrossAmount], State),
	ok = create_order_prod_options_rule(OrderId, ProductId, ProdOptions, State),
	ok.

% Order prod options creation rule (to be used inside transaction)
create_order_prod_options_rule(_OrderId, _ProductId, [], _State) -> ok;
create_order_prod_options_rule(OrderId, ProductId, [OrderProdOption|Rest], State) ->
	ok = create_order_prod_option_rule(OrderId, ProductId, OrderProdOption, State),
	create_order_prod_options_rule(OrderId, ProductId, Rest, State).

create_order_prod_option_rule(OrderId, ProductId, #new_order_prod_option{id_prod_option=ProdOptionId, type=Type,
																		 name=Name,
																		 prod_option_entries=ProdOptionEntries},
							  State) ->
	% t_order_prod_option
	{ok, _} = execute_statement(create_order_prod_option, [OrderId, ProductId, ProdOptionId, Type, Name], State),
	ok = create_order_prod_option_entries_rule(OrderId, ProductId, ProdOptionId, ProdOptionEntries, State),
	ok.

% Order prod option entries creation rule (to be used inside transaction)
create_order_prod_option_entries_rule(_OrderId, _ProductId, _ProdOptionId, [], _State) -> ok;
create_order_prod_option_entries_rule(OrderId, ProductId, ProdOptionId, [OrderProdOptionEntry|Rest], State) ->
	ok = create_order_prod_option_entry_rule(OrderId, ProductId, ProdOptionId, OrderProdOptionEntry, State),
	create_order_prod_option_entries_rule(OrderId, ProductId, ProdOptionId, Rest, State).

create_order_prod_option_entry_rule(OrderId, ProductId, ProdOptionId,
									#new_order_prod_option_entry{id_prod_option_entry=ProdOptionEntryId,
																 quantity=Quantity, name=Name, selected=Selected,
																 gross_amount=GrossAmount},
									State) ->
	% t_order_prod_option_entry
	{ok, _} = execute_statement(create_order_prod_option_entry, [OrderId, ProductId, ProdOptionId,
																 ProdOptionEntryId, Quantity, Name, Selected,
																 GrossAmount], State),
	ok.

% Order waypoint creation rule (to be used inside transaction)
create_order_waypoints_rule(_OrderId, _FirstWaypointId, [], _NewObjects, _State) -> ok;
create_order_waypoints_rule(OrderId, FirstWaypointId, [Waypoint|Rest], NewObjects, State) ->
	ok = create_order_waypoint_rule(OrderId, FirstWaypointId, Waypoint, NewObjects, State),
	create_order_waypoints_rule(OrderId, (FirstWaypointId + 1), Rest, NewObjects, State).

create_order_waypoint_rule(OrderId, WaypointId,
						   #new_order_waypoint{reference=Reference,address=Address,
											   formatted_address=FormattedAddress,
											   position=#position{latitude=Latitude, longitude=Longitude},
											   contact_info=ContactInfo, notes=Notes, signature=Signature,
											   stop_window=StopWindow, stop_duration=StopDuration,
											   object_actions=ObjectActions}, Objects, State) ->
	FLatitude = eb_util:round_float(Latitude, ?DB_DECIMAL_PLACES_COORDINATE),
	FLongitude = eb_util:round_float(Longitude, ?DB_DECIMAL_PLACES_COORDINATE),
	case ContactInfo of
		#contact_info{name=Name, phone_nr=PhoneNr, email=Email} ->
			ContactName = get_optional(Name),
			ContactPhone = get_optional(PhoneNr),
			ContactEmail = get_optional(Email);
		_ ->
			ContactName = null,
			ContactPhone = null,
			ContactEmail = null
	end,
	case StopWindow of
		#time_window{from=From, to=To} ->
			StopFrom = get_optional(From),
			StopTo = get_optional(To);
		_ ->
			StopFrom = null,
			StopTo = null
	end,
	% t_request_order_waypoint
	{ok, _} = execute_statement(create_order_waypoint,
								[OrderId, WaypointId, Reference, FLatitude, FLongitude, FormattedAddress,
								 ContactName, ContactPhone, ContactEmail, Notes, Signature, StopFrom, StopTo,
								 StopDuration], State),
	ok = create_order_waypoint_object_actions_rule(OrderId, WaypointId, Objects, ObjectActions, State),
	% t_order_waypoint_address
	ok = create_order_waypoint_addresses_rule(OrderId, WaypointId, Address, State),
	ok.

% Order waypoint object action creation rule (to be used inside transaction)
create_order_waypoint_object_actions_rule(_OrderId, _WaypointId, _NewObjects, [], _State) -> ok;
create_order_waypoint_object_actions_rule(OrderId, WaypointId, NewObjects, [ObjectAction|Rest], State) ->
	ok = create_order_waypoint_object_action_rule(OrderId, WaypointId, NewObjects, ObjectAction, State),
	create_order_waypoint_object_actions_rule(OrderId, WaypointId, NewObjects, Rest, State).

create_order_waypoint_object_action_rule(OrderId, WaypointId, NewObjects,
										 #new_object_action{object_reference=ObjectReference,
															object_action_id=ObjectActionId,
															object_action=ObjectAction}, State) ->
	case get_record_order_object(NewObjects, ObjectReference) of
		{ok, NewObjectRecord} ->
			ObjectId = NewObjectRecord#order_object.id_object,
			% t_order_waypoint_object_action
			{ok, _} = execute_statement(create_order_waypoint_object_action,
										[OrderId, WaypointId, ObjectId, ObjectActionId, ObjectAction], State),
			ok;
		{nok, not_found} -> not_found_object_reference
	end.

% Order waypoint object action creation rule (to be used inside transaction)
create_order_waypoint_addresses_rule(_OrderId, _WaypointId, [], _State) -> ok;
create_order_waypoint_addresses_rule(OrderId, WaypointId, [Address|Rest], State) ->
	ok = create_order_waypoint_address_rule(OrderId, WaypointId, Address, State),
	create_order_waypoint_addresses_rule(OrderId, WaypointId, Rest, State).

create_order_waypoint_address_rule(OrderId, WaypointId, #component_info{component=Component, value=Value}, State) ->
	% t_order_waypoint_address
	{ok, _} = execute_statement(create_order_waypoint_address, [OrderId, WaypointId, Component, Value], State),
	ok.

get_record_order_object([], _Reference) -> {nok, not_found};
get_record_order_object([NewObject=#order_object{reference=Reference}|_Rest], Reference) -> {ok, NewObject};
get_record_order_object([_|Rest], Reference) -> get_record_order_object(Rest, Reference).

% Request order objects creation rule (to be used inside transaction)
create_order_objects_rule(_OrderId, _FirstObjectId, [], _State) -> ok;
create_order_objects_rule(OrderId, FirstObjectId, [Object|Rest], State) ->
	{ok, NewObjectId} = create_order_object_rule(OrderId, FirstObjectId, Object, State),
	create_order_objects_rule(OrderId, NewObjectId, Rest, State).

create_order_object_rule(OrderId, ObjectId, #new_order_object{reference=Reference,
															  type_id=TypeId,
															  quantity=Quantity,
															  transport_auth=TransportAuth},
								 State) ->
	% t_request_order_object
	[{ok, _} = execute_statement(create_order_object, [OrderId, ObjectId+Qtd, Reference, TypeId,
													   TransportAuth], State) || Qtd <- lists:seq(1, Quantity)],
	{ok, ObjectId + Quantity}.

% User location components creation rule (to be used inside transaction)
create_user_location_components_rule(_LocationId, [], _State) -> ok;
create_user_location_components_rule(LocationId, [#component_info{component=Component, value=Value}|Rest], State) ->
	{ok, _} = execute_statement(create_user_location_component, [LocationId, Component, Value], State),
	create_user_location_components_rule(LocationId, Rest, State).

% User location contacts creation rule (to be used inside transaction)
create_user_location_contacts_rule(_LocationId, [], _State) -> ok;
create_user_location_contacts_rule(LocationId, [#contact_info{name=Name, phone_nr=PhoneNr, email=Email}|Rest], State) ->
	{ok, _} = execute_statement(create_user_location_contact, [LocationId, Name, PhoneNr, Email], State),
	create_user_location_contacts_rule(LocationId, Rest, State).

% Account additional info creation rule (to be used inside transaction)
create_account_additional_info(_AccountId, [], _State) -> ok;
create_account_additional_info(AccountId, [#additional_info{property=Property, value=Value}|Rest], State) ->
	{ok, _} = execute_statement(create_account_info, [AccountId, Property, Value], State),
	create_account_additional_info(AccountId, Rest, State).

% Account address components creation rule (to be used inside transaction)
create_account_address_components_rule(_AccountId, [], _State) -> ok;
create_account_address_components_rule(AccountId, [#component_info{component=Component, value=Value}|Rest], State) ->
	{ok, _} = execute_statement(create_account_address, [AccountId, Component, Value], State),
	create_account_address_components_rule(AccountId, Rest, State).

% User address components creation rule (to be used inside transaction)
create_user_address_components_rule(_UserId, [], _State) -> ok;
create_user_address_components_rule(UserId, [#component_info{component=Component, value=Value}|Rest], State) ->
	{ok, 1} = execute_statement(create_user_address, [UserId, Component, Value], State),
	create_user_address_components_rule(UserId, Rest, State).

% User zone creation rule (to be used inside transaction)
create_user_zone_rule(_UserId, [], _State) -> ok;
create_user_zone_rule(UserId, [ZoneId|Rest], State) ->
	{ok, 1} = execute_statement(create_user_zone, [UserId, ZoneId], State),
	create_user_zone_rule(UserId, Rest, State).

% User notification creation rule (to be used inside transaction)
create_user_notification_rule(_UserId, [], _Enabled, _State) -> ok;
create_user_notification_rule(UserId, [TypeId|Rest], Enabled, State) ->
	{ok, 1} = execute_statement(create_user_notification, [UserId, TypeId, Enabled], State),
	create_user_notification_rule(UserId, Rest, Enabled, State).

% Time operations
add_minutes_to_db_timestamp(MinutesToAdd, DBTimestamp) ->
	{CurrentSeconds, MicroSecondsToAdd} = eb_util:get_timestamp_as_secs_microsecs(DBTimestamp),
	% Adding operations
	{NewDate, {NewHours, NewMinutes, NewSeconds}} = calendar:gregorian_seconds_to_datetime(CurrentSeconds + (MinutesToAdd * 60)),
	% Add microseconds
	{NewDate, {NewHours, NewMinutes, (NewSeconds + MicroSecondsToAdd)}}.

subtract_minutes_to_db_timestamp(MinutesToSubtract, DBTimestamp) ->
	{CurrentSeconds, MicroSecondsToSubtract} = eb_util:get_timestamp_as_secs_microsecs(DBTimestamp),
	% Subtracting operations
	{NewDate, {NewHours, NewMinutes, NewSeconds}} = calendar:gregorian_seconds_to_datetime(CurrentSeconds - (MinutesToSubtract * 60)),
	% Subtract microseconds
	{NewDate, {NewHours, NewMinutes, (NewSeconds - MicroSecondsToSubtract)}}.

% Sort: List has only 2 elemens: [QueryAsc, QueryDesc]. Default is QueryAsc
get_sort_query(?NAVIGATION_SORT_DESC, [_, Query]) -> Query;
get_sort_query(_, [Query, _]) -> Query.

get_max_results(_, 0) -> 0; %no limits
get_max_results(Skip, Max) -> Skip + Max.

trim_list([], _, _) -> [];
trim_list(List, 0, 0) -> List;
trim_list(List, 0, Max) -> lists:sublist(List, Max);
trim_list(List, Skip, _) when length(List) < (Skip + 1) -> [];
trim_list(List, Skip, 0) ->
	Max = length(List),
	trim_list(List, Skip, Max);
trim_list(List, Skip, Max) -> lists:sublist(List, Skip + 1, Max).

get_optional(undefined) -> null;
get_optional(null) -> null;
get_optional(Other) -> Other.

% Get Float from DB
get_binary_to_float(null) -> null;
get_binary_to_float(Bin) -> binary_to_float(Bin).

% Generic list build
build_list([], _BuildObjectFunction) -> [];
build_list(Results, BuildObjectFunction) when is_list(Results) ->
	build_list([], Results, BuildObjectFunction).

build_list(FinalResults, [], _BuildObjectFunction) -> lists:reverse(FinalResults);
build_list(Acc, [Row|Rest], BuildObjectFunction) ->
	case BuildObjectFunction(Row) of
		invalid_record -> invalid_record;
		NewObj -> build_list([NewObj|Acc], Rest, BuildObjectFunction)
	end.

% t_user
build_user({Id, IdType, Username, IdStatus, CreationDate, StatusDate, LoginDate, Email, FirstName, LastName,
			TelephoneNr, FiscalID, Reference, MobileosId, BirthDay, BirthMonth, BirthYear, NationalId, Country,
			Rating, Version}) -> % All columns except password
	#user{id=Id, id_type=IdType, username=Username, id_status=IdStatus, creation_date=CreationDate,
		  status_date=StatusDate, login_date=LoginDate, email=Email, first_name=FirstName, last_name=LastName,
		  telephone_nr=TelephoneNr, fiscal_id=FiscalID, reference=Reference, mobileos_id=MobileosId,
		  birth_day=BirthDay, birth_month=BirthMonth, birth_year=BirthYear, national_id=NationalId,
		  country=Country, rating=Rating, version=Version};
build_user(OtherRecord) ->
	error_logger:error_msg("~p:build_user(~p): Invalid record for table t_user", [?MODULE, OtherRecord]),
	invalid_record.

% t_user_address
build_user_address({IdUser, Component, Value}) ->
	#user_address{id_user=IdUser, component=Component, value=Value};
build_user_address(OtherRecord) ->
	error_logger:error_msg("~p:build_user_address(~p): Invalid record for table t_user_address", [?MODULE, OtherRecord]),
	invalid_record.

% t_account
build_account({Id, UniqueKey, IdStatus, AccountName, FiscalId, TelephoneNr, Email, CreationDate, StatusDate, Occupancy, Version}) ->
	#account{id=Id, unique_key=UniqueKey, id_status=IdStatus, account_name=AccountName, fiscal_id=FiscalId, telephone_nr=TelephoneNr, email=Email, creation_date=CreationDate, status_date=StatusDate, occupancy=Occupancy, version=Version};
build_account(OtherRecord) ->
	error_logger:error_msg("~p:build_account(~p): Invalid record for table t_account", [?MODULE, OtherRecord]),
	invalid_record.

% t_account_info
build_account_info({IdAccount, Property, Value}) ->
	#account_info{id_account=IdAccount, property=Property, value=Value};
build_account_info(OtherRecord) ->
	error_logger:error_msg("~p:build_account_info(~p): Invalid record for table t_account_info", [?MODULE, OtherRecord]),
	invalid_record.

% t_account_address
build_account_address({IdAccount, Component, Value}) ->
	#account_address{id_account=IdAccount, component=Component, value=Value};
build_account_address(OtherRecord) ->
	error_logger:error_msg("~p:build_account_address(~p): Invalid record for table t_account_address", [?MODULE, OtherRecord]),
	invalid_record.

% t_zone_postal_code
build_zone_postal_code({IdZone, PostalCode}) ->
	#zone_postal_code{id_zone=IdZone, postal_code=PostalCode};
build_zone_postal_code(OtherRecord) ->
	error_logger:error_msg("~p:build_zone_postal_code(~p): Invalid record for table t_zone_postal_code", [?MODULE, OtherRecord]),
	invalid_record.

build_account_user({IdAccount, IdUser, IdType, IdDepartment, IdCostCenter}) ->
	#account_user{id_account=IdAccount, id_user=IdUser, id_type=IdType, id_department=IdDepartment, id_cost_center=IdCostCenter};
build_account_user(OtherRecord) ->
	error_logger:error_msg("~p:build_account_user(~p): Invalid record for table t_account_user", [?MODULE, OtherRecord]),
	invalid_record.

% t_department
build_department({DepartmentId, AccountId, Description}) ->
	#account_department{id_department=DepartmentId, id_account=AccountId, description=Description};
build_department(OtherRecord) ->
	error_logger:error_msg("~p:build_department(~p): Invalid record for table t_account_department", [?MODULE, OtherRecord]),
	invalid_record.

% t_parameterization
build_parameterization({Id, Value, Description, Version}) ->
	#parameterization{id=Id, value=Value, description=Description, version=Version};
build_parameterization(OtherRecord) ->
	error_logger:error_msg("~p:build_parameterization(~p): Invalid record for table t_parameterization", [?MODULE, OtherRecord]),
	invalid_record.

% t_contact_request
build_contact_request({Id, Email, Name, Subject, Content, IdStatus, CreationDate, StatusDate, IdUser, IdOperator, OperatorNotes, Version}) ->
	#contact_request{id=Id, email=Email, name=Name, subject=Subject, content=Content, id_status=IdStatus, creation_date=CreationDate,
	                 status_date=StatusDate, id_user=IdUser, id_operator_user=IdOperator, operator_notes=OperatorNotes, version=Version};
build_contact_request(OtherRecord) ->
	error_logger:error_msg("~p:build_contact_request(~p): Invalid record for table t_contact_request", [?MODULE, OtherRecord]),
	invalid_record.

% t_token_type
build_token_type({Id, Description, MultipleUses, Uses, ExpiresInMinutes, Version}) ->
	#token_type{id=Id, description=Description, multiple_uses=MultipleUses, uses=Uses, expires_in_minutes=ExpiresInMinutes, version=Version};
build_token_type(OtherRecord) ->
	error_logger:error_msg("~p:build_token_type(~p): Invalid record for table t_token_type", [?MODULE, OtherRecord]),
	invalid_record.

% t_token
build_token({Id, IdType, CreationDate, RemainingUses, InvalidAfter, Version}) ->
	#token{id=Id, id_type=IdType, creation_date=CreationDate, remaining_uses=RemainingUses, invalid_after=InvalidAfter, version=Version};
build_token(OtherRecord) ->
	error_logger:error_msg("~p:build_token(~p): Invalid record for table t_token", [?MODULE, OtherRecord]),
	invalid_record.

% t_token_parameter
build_token_parameter({IdToken, IdType, Value}) ->
	#token_parameter{id_token=IdToken, id_type=IdType, value=Value};
build_token_parameter(OtherRecord) ->
	error_logger:error_msg("~p:build_token_parameter(~p): Invalid record for table t_token_parameter", [?MODULE, OtherRecord]),
	invalid_record.

% t_notification_type
build_notification_type({Id, Description, NotifyClient}) ->
	#notification_type{id=Id, description=Description, notify_client=NotifyClient};
build_notification_type(OtherRecord) ->
	error_logger:error_msg("~p:build_notification_type(~p): Invalid record for table t_notification_type", [?MODULE, OtherRecord]),
	invalid_record.

% t_api_key
build_api_key({ApiKey, IdApiClient, Domain, Version}) ->
	#api_key{api_key=ApiKey, id_api_client=IdApiClient, domain=Domain, version=Version};
build_api_key(OtherRecord) ->
	error_logger:error_msg("~p:build_api_key(~p): Invalid record for table t_api_key", [?MODULE, OtherRecord]),
	invalid_record.

% t_delivery
build_delivery({Id, Reference, TransportTypeId, TransportId, Distance, Duration, Route, CourierId, Skip, CutTime, StatusId,
				CreationDate, StatusDate, Version}) ->
	#delivery{id=Id, reference=Reference, id_transport_type=TransportTypeId, id_transport=TransportId, distance=Distance, duration=Duration,
			  route=Route, id_courier=CourierId, skip=Skip, cut_time=CutTime, id_status=StatusId,
			  creation_date=CreationDate, status_date=StatusDate, version=Version};
build_delivery(OtherRecord) ->
	error_logger:error_msg("~p:build_delivery(~p): Invalid record for table t_delivery", [?MODULE, OtherRecord]),
	invalid_record.

% t_delivery_waypoint
build_delivery_waypoint({DeliveryId, WaypointId, TravelDistance, TravelTimeEst, TravelTimeReal, StopDurationEst,
						 StopDurationReal, CheckInDate, CheckOutDate, CheckInLat, CheckInLon, CheckOutLat,
						 CheckOutLon, StatusId, CreationDate, StatusDate, Version}) ->
	#delivery_waypoint{id_delivery=DeliveryId, id_waypoint=WaypointId, travel_distance=TravelDistance,
					   travel_time_est=TravelTimeEst, travel_time_real=TravelTimeReal,
					   stop_duration_est=StopDurationEst, stop_duration_real=StopDurationReal,
					   checkin_date=CheckInDate, checkout_date=CheckOutDate, checkin_lat=get_binary_to_float(CheckInLat),
					   checkin_lon=get_binary_to_float(CheckInLon), checkout_lat=get_binary_to_float(CheckOutLat),
					   checkout_lon=get_binary_to_float(CheckOutLon), id_status=StatusId, creation_date=CreationDate,
					   status_date=StatusDate, version=Version};
build_delivery_waypoint(OtherRecord) ->
	error_logger:error_msg("~p:build_delivery_waypoint(~p): Invalid record for table t_delivery_waypoint", [?MODULE, OtherRecord]),
	invalid_record.

build_waypoint_details({WaypointId, CheckinDate, CheckoutDate}) ->
	#waypoint_detail{id=WaypointId, checkin_date=CheckinDate, checkout_date=CheckoutDate};
build_waypoint_details(OtherRecord) ->
	error_logger:error_msg("~p:build_waypoint_details(~p): Invalid record for build_waypoint_details", [?MODULE, OtherRecord]),
	invalid_record.

% t_delivery_waypoint_rating
build_delivery_waypoint_rating({DeliveryId, DeliveryWaypointId, _RequestId, OrderId, WaypointId, RatingClient, RatingNotesClient,
								RatingCourier, RatingNotesCourier}) ->
	#delivery_waypoint_rating{id_delivery=DeliveryId, id_delivery_waypoint=DeliveryWaypointId,
							  id_order=OrderId, id_waypoint=WaypointId, rating_client=RatingClient,
							  rating_notes_client=RatingNotesClient, rating_courier=RatingCourier,
							  rating_notes_courier=RatingNotesCourier};
build_delivery_waypoint_rating(OtherRecord) ->
	error_logger:error_msg("~p:build_delivery_waypoint_rating(~p): Invalid record for table delivery_waypoint_rating", [?MODULE, OtherRecord]),
	invalid_record.

% t_delivery_object
build_delivery_object({DeliveryId, ObjectId, Reference, TypeId, TransportAuth, NonDelReason}) ->
	#delivery_object{id_delivery=DeliveryId, id_object=ObjectId, reference=Reference, id_type=TypeId,
					 transport_auth=TransportAuth, non_del_reason=NonDelReason};
build_delivery_object(OtherRecord) ->
	error_logger:error_msg("~p:build_delivery_object(~p): Invalid record for table t_delivery_object", [?MODULE, OtherRecord]),
	invalid_record.

% t_object_type
build_object_type({Id, Description, Size, Length, Width, Height, Weight, Volume}) ->
	% epgsql returns BigDecimal as binary. We need to cast the binary to float.
	#object_type{id=Id, description=Description, size=get_binary_to_float(Size),
				 length=get_binary_to_float(Length), width=get_binary_to_float(Width), height=get_binary_to_float(Height),
				 weight=get_binary_to_float(Weight), volume=get_binary_to_float(Volume)};
build_object_type(OtherRecord) ->
	error_logger:error_msg("~p:build_object_type(~p): Invalid record for table t_object_type", [?MODULE, OtherRecord]),
	invalid_record.


% t_transport_type
build_transport_type({Id, Description, Capacity}) ->
	% epgsql returns BigDecimal as binary. We need to cast the binary to float.
	FCapacity = get_binary_to_float(Capacity),
	#transport_type{id=Id, description=Description, capacity=FCapacity};
build_transport_type(OtherRecord) ->
	error_logger:error_msg("~p:build_transport_type(~p): Invalid record for table t_transport_type", [?MODULE, OtherRecord]),
	invalid_record.

% t_courier_transport
build_courier_transport({Id, IdUser, IdTransportType, Current, IdStatus, Description, RegistrationId, Color, CreationDate, StatusDate}) ->
	#courier_transport{id=Id, id_user=IdUser, id_transport_type=IdTransportType, current=Current, id_status=IdStatus,
					   description=Description, registration_id=RegistrationId, color=Color,
					   creation_date=CreationDate, status_date=StatusDate};
build_courier_transport(OtherRecord) ->
	error_logger:error_msg("~p:build_courier_transport(~p): Invalid record for table t_courier_transport", [?MODULE, OtherRecord]),
	invalid_record.

% t_order
build_order({OrderId, Reference, DistributionCenter, UserId, TypeId, TransportTypeId, OriginId, ClientName,
			 ClientPhoneNr, ClientEmail, ClientFiscalId, CutTime, StatusId, PaymentId, PaymentMethodId, DinerQty,
			 GrossTotal, Tip, Fee, CreationDate, StatusDate, StartProdDate, Version}) ->
	#order{id_order=OrderId, reference=Reference, distribution_center=DistributionCenter, id_user=UserId,
		   id_type=TypeId, id_transport_type=TransportTypeId, id_origin=OriginId, client_name=ClientName,
		   client_phone_nr=ClientPhoneNr, client_email=ClientEmail, client_fiscal_id=ClientFiscalId, cut_time=CutTime,
		   id_status=StatusId, id_payment=PaymentId, id_payment_method=PaymentMethodId, diner_qty=DinerQty,
		   gross_total=get_binary_to_float(GrossTotal), tip=get_binary_to_float(Tip), fee=get_binary_to_float(Fee),
		   creation_date=CreationDate, status_date=StatusDate, start_prod_date=StartProdDate, version=Version};
build_order(OtherRecord) ->
	error_logger:error_msg("~p:build_order(~p): Invalid record for table t_order", [?MODULE, OtherRecord]),
	invalid_record.

% t_order_prod
build_order_prod({OrderId, ProductId, Description, Name, Quantity, GrossAmount}) ->
	#order_prod{id_order=OrderId, id_product=ProductId, description=Description, name=Name, quantity=Quantity,
				gross_amount=get_binary_to_float(GrossAmount)};
build_order_prod(OtherRecord) ->
	error_logger:error_msg("~p:build_order_prod(~p): Invalid record for table t_order_prod", [?MODULE, OtherRecord]),
	invalid_record.

% t_order_prod_option
build_order_prod_option({OrderId, ProductId, ProdOptionId, Type, Name}) ->
	#order_prod_option{id_order=OrderId, id_product=ProductId, id_prod_option=ProdOptionId, type=Type, name=Name};
build_order_prod_option(OtherRecord) ->
	error_logger:error_msg("~p:build_order_prod_option(~p): Invalid record for table t_order_prod_option", [?MODULE, OtherRecord]),
	invalid_record.

% t_order_prod_option_entry
build_order_prod_option_entry({OrderId, ProductId, ProdOptionId, ProdOptionEntryId, Quantity, Name, Selected,
							   GrossAmount}) ->
	#order_prod_option_entry{id_order=OrderId, id_product=ProductId, id_prod_option=ProdOptionId,
							 id_prod_option_entry=ProdOptionEntryId, quantity=Quantity, name=Name,
							 selected=Selected, gross_amount=get_binary_to_float(GrossAmount)};
build_order_prod_option_entry(OtherRecord) ->
	error_logger:error_msg("~p:t_order_prod_option_entry(~p): Invalid record for table t_order_prod_option_entry", [?MODULE, OtherRecord]),
	invalid_record.

% t_order_waypoint
build_order_waypoint({OrderId, WaypointId, Reference, FormattedAddress, Latitude, Longitude,
                      ContactName, ContactPhoneNr, ContactEmail, Notes, Signature, StopFrom, StopTo, StopDuration}) ->
	#order_waypoint{id_order=OrderId, id_waypoint=WaypointId, reference=Reference,
					formatted_address=FormattedAddress, latitude=get_binary_to_float(Latitude),
					longitude=get_binary_to_float(Longitude), contact_name=ContactName,
					contact_phone_nr=ContactPhoneNr, contact_email=ContactEmail, notes=Notes,
					signature=Signature, stop_from=StopFrom, stop_to=StopTo, stop_duration=StopDuration};
build_order_waypoint(OtherRecord) ->
	error_logger:error_msg("~p:build_order_waypoint(~p): Invalid record for table t_order_waypoint", [?MODULE, OtherRecord]),
	invalid_record.

% t_order_waypoint_object_action
build_order_waypoint_object_action({OrderId, WaypointId, ObjectId, ActionId, Action}) ->
	#order_waypoint_object_action{id_order=OrderId, id_waypoint=WaypointId, id_object=ObjectId,
								  id_action=ActionId, action=Action};
build_order_waypoint_object_action(OtherRecord) ->
	error_logger:error_msg("~p:build_order_waypoint_object_action(~p): Invalid record for table t_order_waypoint_object_action", [?MODULE, OtherRecord]),
	invalid_record.

% t_order_waypoint_address
build_order_waypoint_address({OrderId, WaypointId, Component, Value}) ->
	#order_waypoint_address{id_order=OrderId, id_waypoint=WaypointId, component=Component, value=Value};
build_order_waypoint_address(OtherRecord) ->
	error_logger:error_msg("~p:build_order_waypoint_address(~p): Invalid record for table t_order_waypoint_address", [?MODULE, OtherRecord]),
	invalid_record.

% t_order_object
build_order_object({OrderId, ObjectId, Reference, TypeId, TransportAuth}) ->
		#order_object{id_order=OrderId, id_object=ObjectId, reference=Reference, id_type=TypeId,
					  transport_auth=TransportAuth};
build_order_object(OtherRecord) ->
	error_logger:error_msg("~p:build_order_object(~p): Invalid record for table t_order_object", [?MODULE, OtherRecord]),
	invalid_record.

% build_generated_delivery
build_generated_delivery({DeliveryId, OrderId}) ->
	#generated_delivery{id_delivery=DeliveryId, id_order=OrderId};
build_generated_delivery(OtherRecord) ->
	error_logger:error_msg("~p:build_generated_delivery(~p): Invalid record for table build_generated_delivery", [?MODULE, OtherRecord]),
	invalid_record.

% t_generated_delivery_waypoint
build_generated_delivery_waypoint({DeliveryId, DeliveryWaypointId, OrderId, WaypointId}) ->
	#generated_delivery_waypoint{id_delivery=DeliveryId, id_delivery_waypoint=DeliveryWaypointId, id_order=OrderId,
								 id_waypoint=WaypointId};
build_generated_delivery_waypoint(OtherRecord) ->
	error_logger:error_msg("~p:build_generated_delivery_waypoint(~p): Invalid record for table t_generated_delivery_waypoint", [?MODULE, OtherRecord]),
	invalid_record.

% t_generated_delivery_object
build_generated_delivery_object({DeliveryId, DeliveryObjectId, OrderId, ObjectId}) ->
	#generated_delivery_object{id_delivery=DeliveryId, id_delivery_object=DeliveryObjectId,
							   id_order=OrderId, id_object=ObjectId};
build_generated_delivery_object(OtherRecord) ->
	error_logger:error_msg("~p:build_generated_delivery_object(~p): Invalid record for table t_generated_delivery_object", [?MODULE, OtherRecord]),
	invalid_record.


% t_document
build_document({Id, IdUser, IdAccount, IdType, Name, Mimetype, Base64Data, IdStatus, CreationDate, StatusDate, Version}) ->
	#document{id=Id, id_user=IdUser, id_account=IdAccount, id_type=IdType, name=Name, mimetype=Mimetype, base64_data=Base64Data, id_status=IdStatus,
	          creation_date=CreationDate, status_date=StatusDate, version=Version};
build_document(OtherRecord) ->
	error_logger:error_msg("~p:build_document(~p): Invalid record for table t_document", [?MODULE, OtherRecord]),
	invalid_record.
build_document_summary({Id, IdUser, IdAccount, IdType, Name, IdStatus, CreationDate, StatusDate, Version}) ->
	#document_summary{id=Id, id_user=IdUser, id_account=IdAccount, id_type=IdType, name=Name, id_status=IdStatus, creation_date=CreationDate, status_date=StatusDate,
	                  version=Version};
build_document_summary(OtherRecord) ->
	error_logger:error_msg("~p:build_document_summary(~p): Invalid record for table t_document (summary)", [?MODULE, OtherRecord]),
	invalid_record.

% t_user_courier
build_user_courier({IdUser, IdCourier}) ->
	#user_courier{id_user=IdUser, id_courier=IdCourier};
build_user_courier(OtherRecord) ->
	error_logger:error_msg("~p:build_user_courier(~p): Invalid record for table t_user_courier", [?MODULE, OtherRecord]),
	invalid_record.

% t_user_location
build_user_location({Id, IdUser, Description, Latitude, Longitude}) ->
	% epgsql returns BigDecimal as binary. We need to cast the binary to float.
	FLatitude = get_binary_to_float(Latitude),
	FLongitude = get_binary_to_float(Longitude),
	#user_location{id=Id, id_user=IdUser, description=Description, latitude=FLatitude, longitude=FLongitude};
build_user_location(OtherRecord) ->
	error_logger:error_msg("~p:build_user_location(~p): Invalid record for table t_user_location", [?MODULE, OtherRecord]),
	invalid_record.

% t_user_location_component
build_user_location_component({IdLocation, Component, Value}) ->
	#user_location_component{id_location=IdLocation, component=Component, value=Value};
build_user_location_component(OtherRecord) ->
	error_logger:error_msg("~p:build_user_location_component(~p): Invalid record for table t_user_location_component", [?MODULE, OtherRecord]),
	invalid_record.

% t_user_location_contact
build_user_location_contact({IdContact, IdLocation, Name, PhoneNr, Email}) ->
	#user_location_contact{id_contact=IdContact, id_location=IdLocation, name=Name, phone_nr=PhoneNr, email=Email};
build_user_location_contact(OtherRecord) ->
	error_logger:error_msg("~p:build_user_location_contact(~p): Invalid record for table t_user_location_contact", [?MODULE, OtherRecord]),
	invalid_record.

build_reference_data({Id, Description}) ->
	#reference_data{id=Id, description=Description};
build_reference_data(OtherRecord) ->
	error_logger:error_msg("~p:build_reference_data(~p): Invalid record for reference_data", [?MODULE, OtherRecord]),
	invalid_record.

% t_oauth_provider
build_oauth_provider({Id, Description, ClientId, ClientSecret, AuthorizeEndpoint, AccessTokenEndpoint, ProfileEndpoint}) ->
	#oauth_provider{id=Id, description=Description, client_id=ClientId, client_secret=ClientSecret, authorize_endpoint=AuthorizeEndpoint, access_token_endpoint=AccessTokenEndpoint, profile_endpoint=ProfileEndpoint};
build_oauth_provider(OtherRecord) ->
	error_logger:error_msg("~p:build_oauth_provider(~p): Invalid record for table t_oauth_provider", [?MODULE, OtherRecord]),
	invalid_record.

% t_payment_provider
build_payment_provider({Id, Description, ClientId, ClientSecret, EntityId, RequestEndpoint, AssertionEndpoint,
						GetDetailsEndpoint}) ->
	#payment_provider{id=Id, description=Description, client_id=ClientId, client_secret=ClientSecret,
					  entity_id=EntityId, request_endpoint=RequestEndpoint, assertion_endpoint=AssertionEndpoint,
					  get_details_endpoint=GetDetailsEndpoint};
build_payment_provider(OtherRecord) ->
	error_logger:error_msg("~p:build_payment_provider(~p): Invalid record for table t_payment_provider", [?MODULE, OtherRecord]),
	invalid_record.

% t_voucher
build_voucher({Id, Code, Value, IdType, IdStatus, UsedValue, MaxTimes, TimesUsed, ExpirationDate, RestrictUserId, CreationDate, StatusDate, Version}) ->
	% epgsql returns BigDecimal as binary. We need to cast the binary to float.
	FValue = get_binary_to_float(Value),
	FUsedValue = get_binary_to_float(UsedValue),
	#voucher{voucher_id=Id, voucher_code=Code, value=FValue, voucher_type_id=IdType, voucher_status_id=IdStatus,
	         value_used=FUsedValue, max_times=MaxTimes, times_used=TimesUsed, expiration_date=ExpirationDate, restrict_user_id=RestrictUserId,
	         creation_date=CreationDate, status_date=StatusDate, version=Version};
build_voucher(OtherRecord) ->
	error_logger:error_msg("~p:build_voucher(~p): Invalid record for table t_voucher", [?MODULE, OtherRecord]),
	invalid_record.

% t_user_notification
build_user_notification({IdUser, IdType, Enabled}) ->
	#user_notification{id_user=IdUser, id_type=IdType, enabled=Enabled};
build_user_notification(OtherRecord) ->
	error_logger:error_msg("~p:build_user_notification(~p): Invalid record for table t_user_notification", [?MODULE, OtherRecord]),
	invalid_record.

% t_commercial_info
build_commercial_info({IdUser, About}) ->
	#commercial_info{id_user=IdUser, about=About};
build_commercial_info(OtherRecord) ->
	error_logger:error_msg("~p:build_commercial_info(~p): Invalid record for table t_commercial_info", [?MODULE, OtherRecord]),
	invalid_record.

% t_zone
build_zone({Id, Description, CenterLatitude, CenterLongitude}) ->
	% epgsql returns BigDecimal as binary. We need to cast the binary to float.
	FCenterLatitude = get_binary_to_float(CenterLatitude),
	FCenterLongitude = get_binary_to_float(CenterLongitude),
	#zone{id=Id, description=Description, center_latitude=FCenterLatitude, center_longitude=FCenterLongitude};
build_zone(OtherRecord) ->
	error_logger:error_msg("~p:build_zone(~p): Invalid record for table t_zone", [?MODULE, OtherRecord]),
	invalid_record.

% t_user_zone
build_user_zone({IdUser, IdZone}) ->
	#user_zone{id_user=IdUser, id_zone=IdZone};
build_user_zone(OtherRecord) ->
	error_logger:error_msg("~p:build_user_zone(~p): Invalid record for table t_user_zone", [?MODULE, OtherRecord]),
	invalid_record.

% Unions / Views
% t_account_user and t_user (get_account_users)
build_u_account_user({Id, IdAccountUserType, IdUserType, Username, IdStatus, CreationDate, StatusDate, Email, FirstName, LastName, TelephoneNr,
                      FiscalID, DepartmentId, CostCenterId, UserReference, Version}, State) ->
	TransportTypes = get_multiple_results(get_courier_transport_types, [Id], 0, fun(Row) -> build_courier_transport(Row) end, State),
	#u_account_user{id=Id, id_account_user_type=IdAccountUserType, id_user_type=IdUserType, username=Username, id_status=IdStatus,
	                creation_date=CreationDate, status_date=StatusDate, email=Email, first_name=FirstName, last_name=LastName,
	                telephone_nr=TelephoneNr, fiscal_id=FiscalID, department_id=DepartmentId, cost_center_id=CostCenterId,
	                transport_types=TransportTypes, user_reference=UserReference, version=Version};
build_u_account_user(OtherRecord, _State) ->
	error_logger:error_msg("~p:build_u_account_user(~p): Invalid record for prepared statement get_account_users", [?MODULE, OtherRecord]),
	invalid_record.

build_u_courier_info({Id, CreationDate, Email, FirstName, LastName, TelephoneNr}) ->
	#u_courier_info{id=Id, creation_date=CreationDate, email=Email, first_name=FirstName, last_name=LastName, telephone_nr=TelephoneNr};
build_u_courier_info(OtherRecord) ->
	error_logger:error_msg("~p:build_u_courier_info(~p): Invalid record for prepared_statement get_user_couriers", [?MODULE, OtherRecord]),
	invalid_record.

build_u_order_delivery_courier({DeliveryId, OrderId, CourierId}) ->
	#u_order_delivery_courier{id_delivery=DeliveryId, id_order=OrderId, id_courier=CourierId};
build_u_order_delivery_courier(OtherRecord) ->
	error_logger:error_msg("~p:build_u_order_delivery_courier(~p): Invalid record for prepared_statement get_order_deliveries_couriers", [?MODULE, OtherRecord]),
	invalid_record.

build_u_delivery_waypoint_object_action({ObjectId, ObjectReference, ObjectActionId, ObjectAction, StatusId}) ->
	#object_action{object_id=ObjectId, object_reference=ObjectReference, object_action_id=ObjectActionId,
				   object_action=ObjectAction, id_status=StatusId};
build_u_delivery_waypoint_object_action(OtherRecord) ->
	error_logger:error_msg("~p:build_u_delivery_waypoint_object_action(~p): Invalid record for prepared_statement get_delivery_waypoint_object_actions", [?MODULE, OtherRecord]),
	invalid_record.

build_u_delivery_waypoint({DeliveryId, WaypointId, Latitude, Longitude, TravelDistance, TravelTimeEst, TravelTimeReal,
						   StopDurationEst, StopDurationReal, CheckInDate, CheckOutDate, CheckInLat, CheckInLon, CheckOutLat,
						   CheckOutLon, StatusId, CreationDate, StatusDate, Version}) ->
	#u_delivery_waypoint{id_delivery=DeliveryId, id_waypoint=WaypointId, latitude=get_binary_to_float(Latitude),
						 longitude=get_binary_to_float(Longitude), travel_distance=TravelDistance, travel_time_est=TravelTimeEst,
						 travel_time_real=TravelTimeReal, stop_duration_est=StopDurationEst, stop_duration_real=StopDurationReal,
						 checkin_date=CheckInDate, checkout_date=CheckOutDate, checkin_lat=get_binary_to_float(CheckInLat),
						 checkin_lon=get_binary_to_float(CheckInLon), checkout_lat=get_binary_to_float(CheckOutLat),
						 checkout_lon=get_binary_to_float(CheckOutLon), id_status=StatusId, creation_date=CreationDate,
						 status_date=StatusDate, version=Version};
build_u_delivery_waypoint(OtherRecord) ->
	error_logger:error_msg("~p:build_u_delivery_waypoint(~p): Invalid record for prepared_statement get_delivery_waypoints", [?MODULE, OtherRecord]),
	invalid_record.

build_u_delivery_waypoint_detail({DeliveryId, DeliveryWaypointId, OrderId, WaypointId, Reference, FormatedAddress,
								  ContactName, ContactPhoneNr, ContactEmail, ClientName, ClientPhoneNr, ClientEmail, ClientFiscalId,
								  UserId, Notes, Signature, StopFrom, StopTo, StopDuration}, State) ->
	OrderWaypointAddress = get_multiple_results(get_order_waypoint_address, [OrderId, WaypointId], 0, fun(Row) -> build_order_waypoint_address(Row) end, State),
	ObjectActions = get_multiple_results(get_delivery_waypoint_object_actions, [DeliveryId, DeliveryWaypointId, OrderId, WaypointId], 0, fun(Row) -> build_u_delivery_waypoint_object_action(Row) end, State),
	#u_delivery_waypoint_detail{id_delivery=DeliveryId, id_delivery_waypoint=DeliveryWaypointId,
								id_order=OrderId, id_waypoint=WaypointId, reference=Reference, formatted_address=FormatedAddress,
								address=OrderWaypointAddress, contact_name=ContactName, contact_phone_nr=ContactPhoneNr,
								contact_email=ContactEmail, client_name=ClientName, client_phone_nr=ClientPhoneNr,
								client_email=ClientEmail, client_fiscal_id=ClientFiscalId, user_id=UserId, notes=Notes, signature=Signature, stop_from=StopFrom,
								stop_to=StopTo, stop_duration=StopDuration, %rating=DeliveryWaypointRatings,
								object_actions=ObjectActions};
build_u_delivery_waypoint_detail(OtherRecord, _State) ->
	error_logger:error_msg("~p:build_u_delivery_waypoint_detail(~p): Invalid record for prepared_statement get_delivery_waypoint_details", [?MODULE, OtherRecord]),
	invalid_record.

build_object_by_delivery({IdType, Quantity}) ->
	#objects_by_delivery{id_type=IdType, quantity=Quantity};
build_object_by_delivery(OtherRecord) ->
	error_logger:error_msg("~p:build_object_by_delivery(~p): Invalid record for build_object_by_delivery", [?MODULE, OtherRecord]),
	invalid_record.

build_component_by_delivery({Street1, Wp1, Street2, Wp2}) ->
	#streets_by_delivery{street1=Street1, wp1=Wp1, street2=Street2, wp2=Wp2};
build_component_by_delivery(OtherRecord) ->
	error_logger:error_msg("~p:build_component_by_delivery(~p): Invalid record for build_component_by_delivery", [?MODULE, OtherRecord]),
	invalid_record.
 
build_time_count_by_delivery({EstimatedRidingTime, EstimatedWaitingTime, NumWaypoints}) ->
	#time_count_by_delivery{estimated_riding_time=EstimatedRidingTime, estimated_waiting_time=EstimatedWaitingTime, 
							num_waypoints=NumWaypoints};
build_time_count_by_delivery(OtherRecord) ->
	error_logger:error_msg("~p:build_time_count_by_delivery(~p): Invalid record for build_time_count_by_delivery", [?MODULE, OtherRecord]),
	invalid_record.

build_account_slots_information({Id, SlotLower, SlotUpper, Occupancy, MaxOccupancy}, RequestingUserTypeId) ->
	OccupancyDiff = MaxOccupancy - Occupancy,
	if
		OccupancyDiff > 0 -> Availability = true;
		true -> Availability = false
	end,

	if
		RequestingUserTypeId =/= ?DB_USER_TYPE_DISPATCHER ->
			VisableOccupancy=null,
			VisableMaxOccupancy=null;	
		true -> 
			VisableOccupancy=Occupancy,
			VisableMaxOccupancy=MaxOccupancy
	end,

	#inf_account_slots{id=Id, lower_slot=SlotLower, upper_slot=SlotUpper, occupancy=VisableOccupancy, max_occupancy=VisableMaxOccupancy, availability=Availability}.

build_account_slots_information_temp({Id, SlotLower, SlotUpper, Occupancy, MaxOccupancy}, RequestingUserTypeId) ->
	OccupancyDiff = MaxOccupancy - Occupancy,
	if
		OccupancyDiff > 0 -> Availability = true;
		true -> Availability = false
	end,

	if
		RequestingUserTypeId =/= ?DB_USER_TYPE_DISPATCHER ->
			VisableOccupancy=null,
			VisableMaxOccupancy=null;	
		true -> 
			VisableOccupancy=Occupancy,
			VisableMaxOccupancy=MaxOccupancy
	end,

	#inf_account_slots_temp{id=Id, lower_slot=SlotLower, upper_slot=SlotUpper, occupancy=VisableOccupancy, max_occupancy=VisableMaxOccupancy, availability=Availability}.
