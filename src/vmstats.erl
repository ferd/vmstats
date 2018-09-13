%%% vmstats is a tiny Erlang application to be used in conjuction with
%%% client app in order to gather running statistics of a virtual machine
%%% over its lifetime, helping diagnose or prevent problems in the long run.
-module(vmstats).
-behaviour(application).
-export([start/2, stop/1, child_spec/2]).

child_spec(Sink, BaseKey) ->
    {vmstats,
     {vmstats_server, start_link, [Sink, BaseKey]},
     permanent,
     1000,
     worker,
     [vmstats_server]}.

start(normal, []) ->
    {ok, Sink} = application:get_env(vmstats, sink),
    vmstats_sup:start_link(Sink, base_key()).

stop(_) ->
    ok.

-spec base_key() -> term().
base_key() ->
    case application:get_env(vmstats, base_key) of
        {ok, V} -> V;
        undefined -> "vmstats"
    end.

