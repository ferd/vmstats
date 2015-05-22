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
        [{"key.error_logger_queue_len", _},
         {"key.gc.count", _},
         {"key.gc.words_reclaimed", _},
         {"key.io.bytes_in", _},
         {"key.io.bytes_out", _},
         {"key.memory.atom_used", _},
         {"key.memory.binary", _},
         {"key.memory.ets", _},
         {"key.memory.procs_used", _},
         {"key.memory.total", _},
         {"key.messages_in_queues", _},
         {"key.modules", _},
         {"key.proc_count", _},
         {"key.proc_limit", _},
         {"key.reductions", _},
         {"key.run_queue", _}],
        lists:sort([{lists:flatten(K), V} || {K, V} <- statsderl:called()])
    ),
    timer:sleep(600),
    exit(Pid, shutdown),
    %% Done, we know it loops!
    ?assertMatch(
        [{"key.error_logger_queue_len", _},
         {"key.gc.count", _},
         {"key.gc.words_reclaimed", _},
         {"key.io.bytes_in", _},
         {"key.io.bytes_out", _},
         {"key.memory.atom_used", _},
         {"key.memory.binary", _},
         {"key.memory.ets", _},
         {"key.memory.procs_used", _},
         {"key.memory.total", _},
         {"key.messages_in_queues", _},
         {"key.modules", _},
         {"key.proc_count", _},
         {"key.proc_limit", _},
         {"key.reductions", _},
         {"key.run_queue", _}],
        lists:sort([{lists:flatten(K), V} || {K, V} <- statsderl:called()])
    ),
    ?assertEqual([], lists:sort(statsderl:called())),
    statsderl:stop().
