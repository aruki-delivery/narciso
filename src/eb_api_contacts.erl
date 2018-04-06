%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_api_contacts).

-include("eb_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_contact_request_statuses/0, get_contact_requests/1, get_contact_request/3, create_contact_request/3, update_contact_request/4,
         delete_contact_request/4]).

%
% Get all contact request statuses
%
get_contact_request_statuses() ->
	case eb_db_util:execute({get_contact_request_statuses}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end.

%
% Get all contact requests
%
get_contact_requests(RequestingUserTypeId) when is_integer(RequestingUserTypeId) ->
	case eb_db_util:execute({get_contact_requests}) of
		Results when is_list(Results) -> {ok, Results};
		_ -> {nok, error}
	end;
get_contact_requests(_InvalidRequestingUserTypeId) -> {nok, invalid_parameters}.

%
% Get contact request
%
get_contact_request(RequestingUserId, RequestingUserTypeId, ContactRequestId) when is_integer(RequestingUserId)
                                                                           andalso is_integer(RequestingUserTypeId)
                                                                           andalso is_integer(ContactRequestId) ->
	case verify_contact_request_permissions(RequestingUserId, RequestingUserTypeId, ContactRequestId) of
		{ok, Result} -> {ok, Result};
		{nok, not_found} -> {nok, not_found};
		_Other -> {nok, forbidden}
	end;
get_contact_request(_RequestingUserId, _RequestingUserTypeId, _TargetUserId) -> {nok, invalid_parameters}.

%
% Create contact request
%
create_contact_request(RequestingUserId, _RequestingUserTypeId, NewContactRequest) when is_record(NewContactRequest, new_contact_request) ->
	case sanitize(NewContactRequest) of
		{ok, SanitizedRecord} ->
			% Create the contact request
			case eb_db_util:execute({create_contact_request, SanitizedRecord, RequestingUserId}) of
				{ok, NewContactRequestId} -> {ok, NewContactRequestId};
				_ -> {nok, error}
			end;
		nok -> {nok, missing_values}
	end;
create_contact_request(_RequestingUserId, _RequestingUserTypeId, _NewContactRequest) -> {nok, invalid_parameters}.

%
% Change contact request
%
update_contact_request(RequestingUserId, RequestingUserTypeId, ContactRequestId, ChangeContactRequest) when is_integer(RequestingUserId)
                                                                                                    andalso is_integer(RequestingUserTypeId)
                                                                                                    andalso is_integer(ContactRequestId) ->
	case sanitize(ChangeContactRequest) of
		{ok, SanitizedChangeContactRequest=#change_contact_request{contact_request_status_id=IdStatus, version=Version}} ->
			case IdStatus of
       			undefined -> Action = continue;
       			_ ->
            		case eb_db_util:execute({exists_contact_status, IdStatus}) of 
						true -> Action = continue;
                  		false -> Action = {stop, unknown_contact_request_status};
						_ -> Action = {stop, invalid_contact_request_status}
					end
			end,
			case Action of
				{stop, Error} -> {nok, Error};
    			continue ->
					case verify_contact_request_permissions(RequestingUserId, RequestingUserTypeId, ContactRequestId) of
						{ok, Result=#contact_request{version=Version}} ->
							case eb_db_util:execute({update_contact_request, Result, SanitizedChangeContactRequest}) of
								{ok, NewVersion} -> {ok, NewVersion};
								_ -> {nok, error}
							end;
						{ok, _Result} -> {nok, version};
						{nok, not_found} -> {nok, not_found};
						{nok, forbidden} -> {nok, forbidden};
						_Other -> {nok, error}
					end			
			end;
		_Other -> {nok, missing_values}
	end;
update_contact_request(_RequestingUserId, _RequestingUserTypeId, _ContactRequestId, _ChangeContactRequest) -> {nok, invalid_parameters}.

%
% Delete contact request
%
delete_contact_request(RequestingUserId, RequestingUserTypeId, ContactRequestId, Version) when is_integer(RequestingUserId)
                                                                                       andalso is_integer(RequestingUserTypeId)
                                                                                       andalso is_integer(ContactRequestId) 
                                                                                       andalso is_integer(Version) ->
	case verify_contact_request_permissions(RequestingUserId, RequestingUserTypeId, ContactRequestId) of
		{ok, #contact_request{version=Version}} ->
			case eb_db_util:execute({delete_contact_request, ContactRequestId, Version}) of
				ok -> ok;
				_ -> {nok, error}
			end;	
		{ok, _Result} -> {nok, version};
		{nok, not_found} -> {nok, not_found};
		{nok, forbidden} -> {nok, forbidden};
		_Other -> {nok, error}
	end;
delete_contact_request(_RequestingUserId, _RequestingUserTypeId,  _ContactRequestId, _Version) -> {nok, invalid_parameters}.

%% ====================================================================
%% Internal functions
%% ====================================================================
sanitize(#new_contact_request{email=Email, name=Name, subject=Subject, content=Content}) ->
	try
		{ok, TrimPattern} = eb_util:get_trim_pattern(),
		% Trim binary strings
		{ok, FEmail} = eb_api_util:trim_mandatory(Email, TrimPattern),
		{ok, FName} = eb_api_util:trim_mandatory(Name, TrimPattern),
		{ok, FSubject} = eb_api_util:trim_mandatory(Subject, TrimPattern),
		{ok, FContent} = eb_api_util:trim_mandatory(Content, TrimPattern),
		% Size validations
		ok = eb_util:validate_size(FEmail, ?DB_FIELD_SIZE__CONTACT_REQUEST__EMAIL),
		ok = eb_util:validate_size(FName, ?DB_FIELD_SIZE__CONTACT_REQUEST__NAME),
		ok = eb_util:validate_size(FSubject, ?DB_FIELD_SIZE__CONTACT_REQUEST__SUBJECT),
		ok = eb_util:validate_size(FContent, ?DB_FIELD_SIZE__CONTACT_REQUEST__CONTENT),
		% Extra validations
		true = eb_util:is_valid_email(FEmail),
		% Build the return record
		SanitizedRecord = #new_contact_request{email=FEmail, name=FName, subject=FSubject, content=FContent},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#change_contact_request{version=Version}) when not is_integer(Version) -> nok;
sanitize(#change_contact_request{contact_request_status_id=StatusId, user_id=UserId, operator_notes=OperatorNotes, operator_id=OperatorUser, version=Version}) ->
	try
		true = eb_api_util:is_optional_integer(StatusId),
		true = eb_api_util:is_optional_integer(UserId),
		true = eb_api_util:is_optional_integer(OperatorUser),
		% Trim binary strings
		{ok, FOperatorNotes} = eb_api_util:trim_optional(OperatorNotes),
		% Size validations
		ok = eb_api_util:validate_size_optional(FOperatorNotes, ?DB_FIELD_SIZE__CONTACT_REQUEST__OPERATOR_NOTES),
		% Build the return record
		SanitizedRecord = #change_contact_request{contact_request_status_id=StatusId, user_id=UserId, operator_notes=FOperatorNotes, operator_id=OperatorUser, version=Version},
		{ok, SanitizedRecord}		
	catch
		_:_ -> nok
	end;
	
	
sanitize(_Other) -> nok.

verify_contact_request_permissions(RequestingUserId, RequestingUserTypeId, ContactRequestId) ->
	case eb_db_util:execute({get_contact_request, ContactRequestId}) of
		Result=#contact_request{id_user=RequestingUserId} -> {ok, Result};
		Result when is_record(Result, contact_request) andalso ?IS_OPERATOR(RequestingUserTypeId)-> {ok, Result};
		Result when is_record(Result, contact_request) -> {nok, forbidden};
		not_found -> {nok, not_found};
		_ -> {nok, error}
	end.
