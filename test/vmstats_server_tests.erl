-module(vmstats_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% Gotta mock some stuff for this to work. The module
%% statsderl used here is a fake copy for the sake of
%% being able to write tests without dependencies.

timer_500ms_test() ->
    application:set_env(vmstats, delay, 500),
    Key = "",
    statsderl:start_link(),
    {ok, Pid} = vmstats_server:start_link(Key),
    unlink(Pid),
    timer:sleep(750),
    %% First match works
    ?assertMatch(
        [{"error_logger_queue_len", _, 1.00},
         {"memory.atom_used", _, 1.00},
         {"memory.binary", _, 1.00},
         {"memory.ets", _, 1.00},
         {"memory.procs_used", _, 1.00},
         {"memory.total", _, 1.00},
         {"modules", _, 1.00},
         {"proc_count", _, 1.00},
         {"proc_limit", _, 1.00},
         {"run_queue", _, 1.00}],
        lists:sort([{lists:flatten(K), V, Freq} || {K, V, Freq} <- statsderl:called()])
    ),
    timer:sleep(600),
    exit(Pid, shutdown),
    %% Done, we know it loops!
    ?assertMatch(
        [{"error_logger_queue_len", _, 1.00},
         {"memory.atom_used", _, 1.00},
         {"memory.binary", _, 1.00},
         {"memory.ets", _, 1.00},
         {"memory.procs_used", _, 1.00},
         {"memory.total", _, 1.00},
         {"modules", _, 1.00},
         {"proc_count", _, 1.00},
         {"proc_limit", _, 1.00},
         {"run_queue", _, 1.00}],
        lists:sort([{lists:flatten(K), V, Freq} || {K, V, Freq} <- statsderl:called()])
    ),
    ?assertEqual([], lists:sort(statsderl:called())),
    statsderl:stop().
