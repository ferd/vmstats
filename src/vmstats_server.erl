%%% Main worker for vmstats. This module sits in a loop fired off with
%%% timers with the main objective of routinely sending data to Sink.
-module(vmstats_server).
-behaviour(gen_server).
%% Interface
-export([start_link/2, start_link/3]).
%% Internal Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(TIMER_MSG, '#interval').

-record(state, {sink :: atom(),
                key :: string(),
                key_separator :: char() | iodata(),
                memory_metrics :: [{erlang:memory_type(), atom()}],
                sched_time=disabled :: enabled | disabled,
                prev_sched :: [{integer(), integer(), integer()}] | undefined,
                timer_ref :: reference(),
                interval :: integer(), % milliseconds
                prev_io :: {In::integer(), Out::integer()},
                prev_gc :: {GCs::integer(), Words::integer()}}).
%%% INTERFACE
start_link(Sink, BaseKey) ->
    start_link(Sink, BaseKey, []).

start_link(Sink, BaseKey, Options) ->
    gen_server:start_link(?MODULE, {Sink, BaseKey, Options}, []).

%%% INTERNAL EXPORTS
init({Sink, BaseKey, UserOptions}) ->
    Options = UserOptions ++ application:get_all_env(vmstats),
    {interval, Interval} = lists:keyfind(interval, 1, Options),
    {key_separator, KeySeparator} = lists:keyfind(key_separator, 1, Options),
    {sched_time, SchedTimeEnabled} = lists:keyfind(sched_time, 1, Options),
    {memory_metrics, MemoryMetrics} = lists:keyfind(memory_metrics, 1, Options),

    Ref = erlang:start_timer(Interval, self(), ?TIMER_MSG),
    {{input, In}, {output, Out}} = erlang:statistics(io),
    {GCs, Words, _} = erlang:statistics(garbage_collection),

    State = #state{key = [BaseKey, KeySeparator],
                   key_separator = KeySeparator,
                   memory_metrics = MemoryMetrics,
                   sink = Sink,
                   timer_ref = Ref,
                   interval = Interval,
                   prev_io = {In, Out},
                   prev_gc = {GCs, Words}},

    erlang:system_flag(scheduler_wall_time, true),
    erlang:function_exported(Sink, start_stats, 0) andalso Sink:start_stats(),
    case SchedTimeEnabled of
        true ->
            {ok, State#state{sched_time = enabled,
                             prev_sched = scheduler_wall_time()}};
        false ->
            {ok, State#state{sched_time = disabled}}
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, R, ?TIMER_MSG}, S = #state{sink=Sink, key=K, key_separator=KS, memory_metrics=MM, interval=I, timer_ref=R}) ->
    %% Processes
    Sink:collect(gauge, [K,"proc_count"], erlang:system_info(process_count)),
    Sink:collect(gauge, [K,"proc_limit"], erlang:system_info(process_limit)),

    %% Ports
    Sink:collect(gauge, [K,"port_count"], erlang:system_info(port_count)),
    Sink:collect(gauge, [K,"port_limit"], erlang:system_info(port_limit)),

    %% Atom count, working only on Erlang 20+
    try erlang:system_info(atom_count) of
        AtomCount -> Sink:collect(gauge, [K, "atom_count"], AtomCount)
    catch
        _:badarg -> ok
    end,

    %% Messages in queues
    TotalMessages = lists:foldl(
        fun(Pid, Acc) ->
            case process_info(Pid, message_queue_len) of
                undefined -> Acc;
                {message_queue_len, Count} -> Count+Acc
            end
        end,
        0,
        processes()
    ),
    Sink:collect(gauge, [K,"messages_in_queues"], TotalMessages),

    %% Modules loaded
    Sink:collect(gauge, [K,"modules"], length(code:all_loaded())),

    %% Queued up processes (lower is better)
    Sink:collect(gauge, [K,"run_queue"], erlang:statistics(run_queue)),

    %% Erlang VM uptime stats.
    Sink:collect(timing, [K, "vm_uptime"], erlang:element(1, erlang:statistics(wall_clock))),

    %% Error logger backlog (lower is better)
    case whereis(error_logger) of
        undefined -> ok ;
        Pid ->
            {_, MQL} = process_info(Pid, message_queue_len),
            Sink:collect(gauge, [K,"error_logger_queue_len"], MQL)
    end,

    collect_memory_stats(Sink, [K, "memory", KS], MM),

    %% Incremental values
    IO = collect_io_stats(Sink, [K, "io", KS], S),
    GC = collect_gc_stats(Sink, [K, "gc", KS], S),

    %% Reductions across the VM, excluding current time slice, already incremental
    {_, Reds} = erlang:statistics(reductions),
    Sink:collect(counter, [K,"reductions"], Reds),

    SchedKey = [K, "scheduler_wall_time", KS],
    Sched = collect_sched_time_stats(Sink, SchedKey, S),

    Ref = erlang:start_timer(I, self(), ?TIMER_MSG),
    {noreply, S#state{timer_ref = Ref,
                      prev_sched = Sched,
                      prev_io = IO, prev_gc = GC}};
handle_info(_Msg, {state, _Key, _TimerRef, _Delay}) ->
    exit(forced_upgrade_restart);
handle_info(_Msg, {state, _Key, SchedTime, _PrevSched, _TimerRef, _Delay}) ->
    %% The older version may have had the scheduler time enabled by default.
    %% We could check for settings and preserve it in memory, but then it would
    %% be more confusing if the behaviour changes on the next restart.
    %% Instead, we show a warning and restart as usual.
    case {application:get_env(vmstats, sched_time), SchedTime} of
        {undefined, active} -> % default was on
            error_logger:warning_msg("vmstats no longer runs scheduler time by default. Restarting...");
        _ ->
            ok
    end,
    exit(forced_upgrade_restart);
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{sink=Sink}) ->
    erlang:function_exported(Sink, stop_stats, 0) andalso Sink:stop_stats(),
    ok.

%% Returns the two timeslices as a ratio of each other,
%% as a percentage so that StatsD gets to print something > 1
wall_time_diff(T1, T2) ->
    [{I, Active2-Active1, Total2-Total1}
     || {{I, Active1, Total1}, {I, Active2, Total2}} <- lists:zip(T1,T2)].

collect_io_stats(Sink, Key, #state{prev_io = {PrevIn, PrevOut}}) ->
    {{input, In}, {output, Out}} = erlang:statistics(io),
    Sink:collect(counter, [Key, "bytes_in"], In - PrevIn),
    Sink:collect(counter, [Key, "bytes_out"], Out - PrevOut),
    {In, Out}.

collect_gc_stats(Sink, Key, #state{prev_gc = {PrevGCs, PrevWords}}) ->
    {GCs, Words, _} = erlang:statistics(garbage_collection),
    Sink:collect(counter, [Key, "count"], GCs - PrevGCs),
    Sink:collect(counter, [Key, "words_reclaimed"], Words - PrevWords),
    {GCs, Words}.

%% There are more options available, but not all were kept.
%% Memory usage is in bytes.
collect_memory_stats(Sink, Key, MemoryMetrics) ->
    [begin
        MetricKey = atom_to_list(Name),
        MetricValue = erlang:memory(Metric),
        Sink:collect(gauge, [Key, MetricKey], MetricValue)
     end
     || {Metric, Name} <- MemoryMetrics].

collect_sched_time_stats(Sink, Key, #state{sched_time = enabled, prev_sched = PrevSched, key_separator = KS}) ->
    Sched = scheduler_wall_time(),
    [begin
        LSid = integer_to_list(Sid),
        Sink:collect(timing, [Key, LSid, KS, "active"], Active),
        Sink:collect(timing, [Key, LSid, KS, "total"], Total)
     end
     || {Sid, Active, Total} <- wall_time_diff(PrevSched, Sched)],
    Sched;

collect_sched_time_stats(_Sink, _Key, #state{prev_sched = PrevSched}) -> PrevSched.

scheduler_wall_time() ->
    lists:sort(erlang:statistics(scheduler_wall_time)).
