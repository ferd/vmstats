-module(vmstats_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% Gotta mock some stuff for this to work. The module
%% statsderl used here is a fake copy for the sake of
%% being able to write tests without dependencies.

timer_500ms_test() ->
    application:set_env(vmstats, delay, 500),
    Key = "key",
    statsderl:start_link(),
    {ok, Pid} = vmstats_server:start_link(statsderl, Key),
    unlink(Pid),
    timer:sleep(750),
    %% First match works
    ?assertMatch(
        [{"key.error_logger_queue_len", _, 1.00},
         {"key.gc.count",_,1.00},
         {"key.gc.words_reclaimed",_,1.00},
         {"key.io.bytes_in",_,1.00},
         {"key.io.bytes_out",_,1.00},
         {"key.memory.atom_used", _, 1.00},
         {"key.memory.binary", _, 1.00},
         {"key.memory.ets", _, 1.00},
         {"key.memory.procs_used", _, 1.00},
         {"key.memory.total", _, 1.00},
         {"key.messages_in_queues", _, 1.00},
         {"key.modules", _, 1.00},
         {"key.proc_count", _, 1.00},
         {"key.proc_limit", _, 1.00},
         {"key.reductions", _, 1.00},
         {"key.run_queue", _, 1.00}],
        lists:sort([{lists:flatten(K), V, Freq} || {K, V, Freq} <- statsderl:called()])
    ),
    timer:sleep(600),
    exit(Pid, shutdown),
    %% Done, we know it loops!
    ?assertMatch(
        [{"key.error_logger_queue_len", _, 1.00},
         {"key.gc.count",_,1.00},
         {"key.gc.words_reclaimed",_,1.00},
         {"key.io.bytes_in",_,1.00},
         {"key.io.bytes_out",_,1.00},
         {"key.memory.atom_used", _, 1.00},
         {"key.memory.binary", _, 1.00},
         {"key.memory.ets", _, 1.00},
         {"key.memory.procs_used", _, 1.00},
         {"key.memory.total", _, 1.00},
         {"key.messages_in_queues", _, 1.00},
         {"key.modules", _, 1.00},
         {"key.proc_count", _, 1.00},
         {"key.proc_limit", _, 1.00},
         {"key.reductions", _, 1.00},
         {"key.run_queue", _, 1.00}],
        lists:sort([{lists:flatten(K), V, Freq} || {K, V, Freq} <- statsderl:called()])
    ),
    ?assertEqual([], lists:sort(statsderl:called())),
    statsderl:stop().
