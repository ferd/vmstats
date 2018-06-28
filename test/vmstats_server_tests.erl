-module(vmstats_server_tests).
-include_lib("eunit/include/eunit.hrl").

-ifdef(OTP_RELEASE).
-define(MATCH, [{"key.atom_count", _},
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
         {"key.port_count", _},
         {"key.port_limit", _},
         {"key.proc_count", _},
         {"key.proc_limit", _},
         {"key.reductions", _},
         {"key.run_queue", _}]).
-else.
-define(MATCH, [{"key.atom_count", _},
         {"key.error_logger_queue_len", _},
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
         {"key.port_count", _},
         {"key.port_limit", _},
         {"key.proc_count", _},
         {"key.proc_limit", _},
         {"key.reductions", _},
         {"key.run_queue", _}]).
-endif.

setup() ->
    application:set_env(vmstats, interval, 500),
    application:set_env(vmstats, key_separator, $.),
    application:set_env(vmstats, sched_time, false),
    application:set_env(vmstats, memory_metrics, [
      {total, total},
      {processes_used, procs_used},
      {atom_used, atom_used},
      {binary, binary},
      {ets, ets}
    ]),

    Key = "key",
    sample_sink:start_link(),
    {ok, Pid} = vmstats_server:start_link(sample_sink, Key),
    unlink(Pid),
    Pid.

teardown(_) -> sample_sink:stop().

get_values() ->
    lists:sort([{lists:flatten(K), V} || {K, V} <- sample_sink:called()]).

timer_500ms_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(Pid) ->
             [
              ?_test(begin
                         timer:sleep(750),
                         ?assertMatch(?MATCH, get_values())
                     end),
              ?_test(begin
                         timer:sleep(600),
                         exit(Pid, shutdown),
                         ?assertMatch(?MATCH, get_values())
                     end),
              ?_assertEqual([], get_values())
             ]
     end}.
