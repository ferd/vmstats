-module(vmstats_server).
-behaviour(gen_server).
-include_lib("system_stats/include/system_stats.hrl").

%% public
-export([
    start_link/1
]).

%% private
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-define(DELAY, 1000).
-define(PAGE_SIZE, 4096).
-define(TIMER_MSG, '#delay').

-record(state, {
    key :: string(),
    timer_ref :: reference(),
    prev_io :: {In::integer(), Out::integer()},
    prev_gc :: {GCs::integer(), Words::integer(), 0},
    system_stats :: #stats {}
}).

%% public
start_link(BaseKey) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, BaseKey, []).

%% private
init(BaseKey) ->
    Ref = erlang:start_timer(?DELAY, self(), ?TIMER_MSG),
    {{input, In}, {output, Out}} = erlang:statistics(io),
    PrevGC = erlang:statistics(garbage_collection),

    case os:type() of
        {_, linux} ->
            SystemStats = system_stats:proc_cpuinfo(system_stats_utils:new_stats()),
            SystemStats2 = system_stats:proc_stat(SystemStats),
            SystemStats3 = system_stats:proc_pid_stat(os:getpid(), SystemStats2),

            {ok, #state {
                key = [BaseKey, $.],
                timer_ref = Ref,
                prev_io = {In,Out},
                prev_gc = PrevGC,
                system_stats = SystemStats3
            }};
        _Else ->
            {ok, #state {
                key = [BaseKey, $.],
                timer_ref = Ref,
                prev_io = {In,Out},
                prev_gc = PrevGC
            }}
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TimerRef, ?TIMER_MSG}, #state {
        key = Key,
        timer_ref = TimerRef,
        prev_io = {OldIn, OldOut},
        prev_gc = {OldGCs, OldWords, _},
        system_stats = SystemStats
    } = State) ->

    % processes
    statsderl:gauge([Key, <<"proc_count">>], erlang:system_info(process_count), 1.00),
    statsderl:gauge([Key, <<"proc_limit">>], erlang:system_info(process_limit), 1.00),

    % messages in queues
    TotalMessages = lists:foldl(fun(Pid, Acc) ->
        case process_info(Pid, message_queue_len) of
            undefined -> Acc;
            {message_queue_len, Count} ->
                Count + Acc
        end
    end, 0, processes()),
    statsderl:gauge([Key , <<"messages_in_queues">>], TotalMessages, 1.00),

    % modules loaded
    statsderl:gauge([Key, <<"modules">>], length(code:all_loaded()), 1.00),

    % run queue
    statsderl:gauge([Key, <<"run_queue">>], erlang:statistics(run_queue), 1.00),

    % error_logger message queue length
    {_, MessageQueueLength} = process_info(whereis(error_logger), message_queue_len),
    statsderl:gauge([Key, <<"error_logger_queue_len">>], MessageQueueLength, 1.00),

    % vm memory usage
    MemoryKey = [Key, "memory."],
    Memory = erlang:memory(),
    statsderl:gauge([MemoryKey, <<"total">>], bytes_to_megabytes(proplists:get_value(total, Memory)), 1.00),
    statsderl:gauge([MemoryKey, <<"procs_used">>], bytes_to_megabytes(proplists:get_value(processes_used, Memory)), 1.00),
    statsderl:gauge([MemoryKey, <<"atom_used">>], bytes_to_megabytes(proplists:get_value(atom_used, Memory)), 1.00),
    statsderl:gauge([MemoryKey, <<"binary">>], bytes_to_megabytes(proplists:get_value(binary, Memory)), 1.00),
    statsderl:gauge([MemoryKey, <<"ets">>], bytes_to_megabytes(proplists:get_value(ets, Memory)), 1.00),

    % io
    {{input, In}, {output, Out}} = erlang:statistics(io),
    statsderl:increment([Key, <<"io.bytes_in">>], In - OldIn, 1.00),
    statsderl:increment([Key, <<"io.bytes_out">>], Out - OldOut, 1.00),

    % garbage collector
    GarbageCollector = {GCs, Words, _} = erlang:statistics(garbage_collection),
    statsderl:increment([Key, <<"gc.count">>], GCs - OldGCs, 1.00),
    statsderl:increment([Key, <<"gc.words_reclaimed">>], Words - OldWords, 1.00),

    % reductions
    {_, Reds} = erlang:statistics(reductions),
    statsderl:increment([Key, <<"reductions">>], Reds, 1.00),

    case os:type() of
        {_, linux} ->
            % system load
            SystemStats2 = system_stats:proc_load_avg(SystemStats),
            statsderl:gauge([Key, <<"system.load_1">>], SystemStats2#stats.load_1, 1.00),
            statsderl:gauge([Key, <<"system.load_5">>], SystemStats2#stats.load_5, 1.00),
            statsderl:gauge([Key, <<"system.load_15">>], SystemStats2#stats.load_15, 1.00),

            % system cpu %
            SystemStats3 = system_stats:proc_pid_stat(os:getpid(), SystemStats2),
            SystemStats4 = system_stats:proc_stat(SystemStats3),
            {Ucpu, Scpu} = system_stats_utils:cpu_percent(SystemStats, SystemStats4),
            CpuPercent = trunc(SystemStats4#stats.cpu_cores * (Ucpu + Scpu)),
            statsderl:gauge([Key, <<"system.cpu_percent">>], CpuPercent, 1.00),

            % system memory
            Vsize = trunc(bytes_to_megabytes(SystemStats4#stats.mem_vsize)),
            Rss = trunc(bytes_to_megabytes(?PAGE_SIZE * (SystemStats4#stats.mem_rss))),
            statsderl:gauge([Key, <<"system.vsize">>], Vsize, 1.00),
            statsderl:gauge([Key, <<"system.rss">>], Rss, 1.00),

            {noreply, State#state {
                timer_ref = erlang:start_timer(?DELAY, self(), ?TIMER_MSG),
                prev_io = {In, Out},
                prev_gc = GarbageCollector,
                system_stats = SystemStats4
            }};
        _Else ->
            {noreply, State#state {
                timer_ref = erlang:start_timer(?DELAY, self(), ?TIMER_MSG),
                prev_io = {In, Out},
                prev_gc = GarbageCollector
            }}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% private
bytes_to_megabytes(Bytes) -> Bytes / 1048576.
