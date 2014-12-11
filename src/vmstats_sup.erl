-module(vmstats_sup).
-behaviour(supervisor).
%% Interface
-export([start_link/0, start_link/1]).
%% Internal Exports
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(BaseKey) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [BaseKey]).

init(Args) ->
    %% The stats are mixed in for all nodes. Differentiate keys by node name
    %% is the only way to make sure stats won't be mixed for all different
    %% systems. Hopefully, you remember to name nodes when you start them!
    ChildSpec = {vmstats,
                 {vmstats_server, start_link, Args},
                 permanent,
                 1000,
                 worker,
                 [vmstats_server]},
    {ok, {{one_for_all,5,3600}, [ChildSpec]}}.
