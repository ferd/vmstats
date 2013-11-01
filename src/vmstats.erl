-module(vmstats).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(normal, []) ->
    vmstats_sup:start_link("vmstats").

stop(_) ->
    ok.
