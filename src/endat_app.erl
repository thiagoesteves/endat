%%%-------------------------------------------------------------------
%% @doc endat public API
%% @end
%%%-------------------------------------------------------------------

-module(endat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    endat_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
