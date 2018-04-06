%%
%% Copyright 2015-17 U2D - Urban Dynamic Delivery Company, LDA - http://ud.delivery/
%%
-module(eb).

-export([start/2]).

start(normal, []) ->
  eb_app:start(normal, []).
