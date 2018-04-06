%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_api_config).

-include("eb_constants.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_parameterizations/1, get_parameterization/2, update_parameterization/3, get_token_types/1, get_token_type/2, update_token_type/3,
         get_courier_app_config/0]).

%
% Get all parameterizations
%
get_parameterizations(RequestingUserTypeId) when is_integer(RequestingUserTypeId) ->
	case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
		ok ->
			case eb_db_util:execute({get_parameterizations}) of
				Results when is_list(Results) -> {ok, Results};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
get_parameterizations(_RequestingUserTypeId) ->{nok, invalid_parameters}.

%
% Get parameterization
%
get_parameterization(RequestingUserTypeId, ParameterizationId) when is_integer(RequestingUserTypeId) andalso is_integer(ParameterizationId) ->
	case eb_api_util:verify_operator_or_dispatcher_permissions(RequestingUserTypeId) of
		ok ->
			case eb_db_util:execute({get_parameterization, ParameterizationId}) of
				Result when is_record(Result, parameterization) -> {ok, Result};
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
get_parameterization(_RequestingUserTypeId, _InvalidId) -> {nok, invalid_parameters}.

%
% Update parameterization
%
update_parameterization(RequestingUserTypeId, ParameterizationId, ChangeParameterization) when is_integer(RequestingUserTypeId)
                                                                                       andalso is_integer(ParameterizationId) ->
	case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
		ok ->
			case sanitize(ChangeParameterization) of
				{ok, #change_parameterization{value=Value, version=Version}} ->
					case eb_db_util:execute({update_parameterization, ParameterizationId, Version, Value}) of
						{ok, NewVersion} ->
							% Flush the parameterization cache
							eb_cache_util:flush_db_parameter_cache(),
							{ok, NewVersion};
						not_found -> {nok, not_found};
						wrong_version -> {nok, version};
						_Other -> {nok, error}
					end;
				nok -> {nok, missing_values}
			end;
		nok -> {nok, forbidden}
	end;
update_parameterization(_RequestingUserTypeId, _ParameterizationId, _ChangeParameterization) -> {nok, invalid_parameters}.

%
% Get all token types
%
get_token_types(RequestingUserTypeId) when is_integer(RequestingUserTypeId) ->
	case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
		ok ->
			case eb_db_util:execute({get_token_types}) of
				Results when is_list(Results) -> {ok, Results};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
get_token_types(_RequestingUserTypeId) -> {nok, invalid_parameters}.

%
% Get a token type
%
get_token_type(RequestingUserTypeId, TokenTypeId) when is_integer(RequestingUserTypeId) andalso is_integer(TokenTypeId) ->
	case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
		ok ->
			case eb_db_util:execute({get_token_type, TokenTypeId}) of
				Result when is_record(Result, token_type) -> {ok, Result};
				not_found -> {nok, not_found};
				_ -> {nok, error}
			end;
		nok -> {nok, forbidden}
	end;
get_token_type(_RequestingUserTypeId, _TokenTypeId) -> {nok, invalid_parameters}.

%
% Update the token type configuration
%
update_token_type(RequestingUserTypeId, TokenTypeId, UpdateTokenType) when is_integer(RequestingUserTypeId) andalso is_integer(TokenTypeId) ->
	case eb_api_util:verify_operator_permissions(RequestingUserTypeId) of
		ok ->
			case sanitize(UpdateTokenType) of
				{ok, #update_token_type{multiple_uses=MultipleUses, uses=Uses, expires_in_minutes=ExpiresInMinutes, version=Version}} ->
					case eb_db_util:execute({update_token_type, TokenTypeId, Version, MultipleUses, Uses, ExpiresInMinutes}) of
						{ok, NewVersion} -> {ok, NewVersion};
						not_found -> {nok, not_found};
						wrong_version -> {nok, version};
						_Other -> {nok, error}
					end;
				nok -> {nok, missing_values}
			end;
		nok -> {nok, forbidden}
	end;
update_token_type(_RequestingUserTypeId, _TokenTypeId, _UpdateTokenType) -> {nok, invalid_parameters}.

%
% Get the courier app configuration
%
get_courier_app_config() ->
	case build_position_update() of
		{ok, PositionUpdate} -> {ok, #courier_app_config{position_update=PositionUpdate}};
		Other -> {nok, Other}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
build_position_update() ->
	case eb_cache_util:get_db_parameter(?DB_PARAM_COURIER_APP_UPDATE_FREQUENCY_AVAILABLE) of
		{ok, Available} ->
			case eb_cache_util:get_db_parameter(?DB_PARAM_COURIER_APP_UPDATE_FREQUENCY_WORKING) of
				{ok, Working} -> {ok, #position_update{available=Available, working=Working}};
				Other ->
					error_logger:error_msg("~p:build_position_update(): Unable to get parameter ?DB_PARAM_COURIER_APP_UPDATE_FREQUENCY_WORKING (~p): ~p~n", [?MODULE, ?DB_PARAM_COURIER_APP_UPDATE_FREQUENCY_WORKING, Other]),
					error
			end;
		Other ->
			error_logger:error_msg("~p:build_position_update(): Unable to get parameter ?DB_PARAM_COURIER_APP_UPDATE_FREQUENCY_AVAILABLE (~p): ~p~n", [?MODULE, ?DB_PARAM_COURIER_APP_UPDATE_FREQUENCY_AVAILABLE, Other]),
			error
	end.

sanitize(#change_parameterization{version=Version}) when not is_integer(Version) -> nok;
sanitize(#change_parameterization{value=Value, version=Version}) ->
	try
		{ok, FValue} = eb_api_util:trim_mandatory(Value),
		% Size validations
		ok = eb_util:validate_size(FValue, ?DB_FIELD_SIZE__PARAMETRIZATION__VALUE),
		% Build the return record
		SanitizedRecord = #change_parameterization{value=FValue, version=Version},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(#update_token_type{multiple_uses=MultipleUses}) when not is_boolean(MultipleUses) -> nok;
sanitize(#update_token_type{version=Version}) when not is_integer(Version) -> nok;
sanitize(#update_token_type{multiple_uses=MultipleUses, uses=Uses, expires_in_minutes=ExpiresInMinutes, version=Version}) ->
	try
		true = eb_api_util:is_positive_nullable_integer(Uses),
		true = eb_api_util:is_positive_nullable_integer(ExpiresInMinutes),
		% Build the return record
		SanitizedRecord = #update_token_type{multiple_uses=MultipleUses, uses=Uses, expires_in_minutes=ExpiresInMinutes, version=Version},
		{ok, SanitizedRecord}
	catch
		_:_ -> nok
	end;

sanitize(_Other) -> nok.
