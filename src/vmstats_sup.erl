-module(vmstats_sup).
-behaviour(supervisor).
%% Interface
-export([start_link/2]).
%% Internal Exports
-export([init/1]).

start_link(Sink, BaseKey) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Sink, BaseKey]).

init([Sink, BaseKey]) ->
    %% The stats are mixed in for all nodes. Differentiate keys by node name
    %% is the only way to make sure stats won't be mixed for all different
    %% systems. Hopefully, you remember to name nodes when you start them!
    ChildSpec = vmstats:child_spec(Sink, BaseKey),
    {ok, {{one_for_all,5,3600}, [ChildSpec]}}.
