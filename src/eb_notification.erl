%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_notification).

-include("eb_constants.hrl").

%% ====================================================================
%% Constants
%% ====================================================================
-define(TEMPLATE_MODULE, ebtmpltmdl).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0, create_user/3, reset_password/3, change_email/3, change_password/1, vrp_crash/0]).

%
% Initializations
%
init() ->
	case application:get_env(eb, templates_dir) of
		{ok, TemplatesDir} ->
			case erlydtl:compile_dir(TemplatesDir, ?TEMPLATE_MODULE) of
				{ok, _} -> ok;
				Error ->
					error_logger:error_msg("~p:init(): Unexpected error compiling the template in dir ~p: ~p", [?MODULE, TemplatesDir, Error]),
					{stop, error_compiling_templates}
			end;
		Error ->
			error_logger:error_msg("~p:init(): Unable to get templates_dir property from env.config: ~p", [?MODULE,  Error]),
			{stop, missing_config}
	end.

%
% Sends an email to the new user
%
create_user(Email, Token, RedirectURL) ->
	% Spawn a new process so the main process doesn't get locked while sending the notification
	Function = fun() ->
		case eb_cache_util:get_db_parameters([?DB_PARAM_STATIC_CONTENT_LOCATION, ?DB_PARAM_NOTIFICATION_REGISTER_USER_SENDER,
		                                      ?DB_PARAM_NOTIFICATION_REGISTER_USER_SUBJECT], ?MODULE, create_user) of
			[StaticContent, From, Subject] ->
				ActionURL = get_action_url(RedirectURL, Token),
				Parameters = [{action_url, ActionURL}, {static_content, StaticContent}],
				notify(create_user, Parameters, From, [Email], Subject);
			_Other -> error
		end
	end,
	spawn(Function).

%
% Sends an email with reset password instructions
%
reset_password(Email, Token, RedirectURL) ->
	% Spawn a new process so the main process doesn't get locked while sending the notification
	Function = fun() ->
		case eb_cache_util:get_db_parameters([?DB_PARAM_STATIC_CONTENT_LOCATION, ?DB_PARAM_NOTIFICATION_RESET_PASSWORD_SENDER,
		                                      ?DB_PARAM_NOTIFICATION_RESET_PASSWORD_SUBJECT], ?MODULE, reset_password) of
			[StaticContent, From, Subject] ->
				ActionURL = get_action_url(RedirectURL, Token),
				Parameters = [{action_url, ActionURL}, {static_content, StaticContent}],
				notify(reset_password, Parameters, From, [Email], Subject);
			_Other -> error
		end
	end,
	spawn(Function).

%
% Sends an email to the user when user email is changed
%
change_email(Email, Token, RedirectURL) ->
	% Spawn a new process so the main process doesn't get locked while sending the notification
	Function = fun() ->
		case eb_cache_util:get_db_parameters([?DB_PARAM_STATIC_CONTENT_LOCATION, ?DB_PARAM_NOTIFICATION_CHANGE_EMAIL_SENDER,
		                                      ?DB_PARAM_NOTIFICATION_CHANGE_EMAIL_SUBJECT], ?MODULE, change_email) of
			[StaticContent, From, Subject] ->
				ActionURL = get_action_url(RedirectURL, Token),
				Parameters = [{action_url, ActionURL}, {static_content, StaticContent}],
				notify(change_email, Parameters, From, [Email], Subject);
			_Other -> error
		end
	end,
	spawn(Function).

%
% Sends an email with change password warning
%
change_password(Email) ->
	% Spawn a new process so the main process doesn't get locked while sending the notification
	Function = fun() ->
		case eb_cache_util:get_db_parameters([?DB_PARAM_STATIC_CONTENT_LOCATION, ?DB_PARAM_NOTIFICATION_CHANGE_PASSWORD_SENDER,
		                                      ?DB_PARAM_NOTIFICATION_CHANGE_PASSWORD_SUBJECT], ?MODULE, change_password) of
			[StaticContent, From, Subject] ->
				Parameters = [{static_content, StaticContent}],
				notify(change_password, Parameters, From, [Email], Subject);
			_Other -> error
		end
	end,
	spawn(Function).

%
% Send an email notifying that the vrp algorithm crashed
%
vrp_crash() ->
	% Spawn a new process so the main process doesn't get locked while sending the notification
	Function = fun() ->
		case eb_cache_util:get_db_parameters([?DB_PARAM_STATIC_CONTENT_LOCATION, ?DB_PARAM_NOTIFICATION_VRP_CRASH_SENDER,
		                                      ?DB_PARAM_NOTIFICATION_VRP_CRASH_SUBJECT, ?DB_PARAM_NOTIFICATION_VRP_CRASH_EMAIL],
		                                     ?MODULE, vrp_crash) of
			[StaticContent, From, Subject, Email] ->
				Parameters = [{static_content, StaticContent}],
				SplitEmails = binary:split(Email, <<";">>),
				notify(vrp_crash, Parameters, From, SplitEmails, Subject);
			_Other -> error
		end
	end,
	spawn(Function).

%% ====================================================================
%% Internal functions
%% ====================================================================
get_action_url(RedirectURL, TokenId) ->
	case binary:match(RedirectURL, <<"?">>) of
		nomatch -> ParameterSeparator = <<"?">>;
		_ -> ParameterSeparator = <<"&">>
	end,
	URLEncodedToken = erlang:list_to_binary(http_uri:encode(erlang:binary_to_list(TokenId))),
	<<RedirectURL/binary, ParameterSeparator/binary, ?REDIRECT_TOKEN_PARAMETER/binary, <<"=">>/binary, URLEncodedToken/binary>>.

notify(Template, Parameters, From, To, Subject) ->
	% Generate the HTML to send
	IoList = ?TEMPLATE_MODULE:Template(Parameters),
	TmpBin = iolist_to_binary(IoList),
	EncodedBinHtml = TmpBin,
	% Send the email
	send_email(From, To, Subject, EncodedBinHtml).

send_email(From, ToList, Subject, Body) ->
	HeaderToList = [{<<"To">>, To} || To <- ToList],
	Headers = [{<<"From">>, From}|(HeaderToList ++ [{<<"Subject">>, Subject}])],
	Email = {<<"text">>, <<"html">>, Headers, [], Body},
	DataToSend = mimemail:encode(Email),
	gen_smtp_client:send({From, ToList, DataToSend}, [{relay, "127.0.0.1"}]).
