%%
%% Copyright 2017 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb_batch).

-include("eb_constants.hrl").

-behaviour(gen_server).

%% ====================================================================
%% Callback functions
%% ====================================================================
-callback handle_message(Message::any()) -> ok.

%% ====================================================================
%% Constants
%% ====================================================================
-define(SERVER, {local, ?MODULE}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([create_timer/3]).

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, [], []).

%
% Create timer utility
%
create_timer(SendAfter, Module, Message) ->
	erlang:start_timer(SendAfter, ?MODULE, {Module, Message}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

init([]) ->
	error_logger:info_msg("~p [~p] starting...\n", [?MODULE, self()]),
    {ok, #state{}}.

%% handle_call/3
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% handle_cast/2
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TimerRef, {Module, Message}}, State) ->
	erlang:cancel_timer(TimerRef),
	Function = fun() -> Module:handle_message(Message) end,
	spawn(Function),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% terminate/2
terminate(_Reason, _State) ->
    ok.

%% code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
