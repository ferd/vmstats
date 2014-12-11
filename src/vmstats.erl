%%% vmstats is a tiny Erlang application to be used in conjuction with
%%% statsderl in order to gather running statistics of a virtual machine
%%% over its lifetime, helping diagnose or prevent problems in the long run.
-module(vmstats).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
    vmstats_sup:start_link().

stop(_) ->
    ok.
