-module(vmstats_sup).
-behaviour(supervisor).

%% public
-export([
    start_link/1
]).

%% private
-export([
    init/1
]).

-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

%% public
start_link(BaseKey) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, BaseKey).

%% private
init(BaseKey) ->
    {ok, {{one_for_all,5,3600}, [?CHILD(vmstats_server, [BaseKey])]}}.
