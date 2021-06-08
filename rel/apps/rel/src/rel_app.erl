%%%-------------------------------------------------------------------
%% @doc rel public API
%% @end
%%%-------------------------------------------------------------------

-module(rel_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rel_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
