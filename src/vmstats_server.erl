%%% Main worker for vmstats. This module sits in a loop fired off with
%%% timers with the main objective of routinely sending data to Sink.
-module(vmstats_server).
-behaviour(gen_server).
%% Interface
-export([start_link/2]).
%% Internal Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(TIMER_MSG, '#delay').

-record(state, {sink :: atom(),
                key :: string(),
                sched_time :: enabled | disabled | unavailable,
                prev_sched :: [{integer(), integer(), integer()}],
                timer_ref :: reference(),
                delay :: integer(), % milliseconds
                prev_io :: {In::integer(), Out::integer()},
                prev_gc :: {GCs::integer(), Words::integer(), 0}}).
%%% INTERFACE
start_link(Sink, BaseKey) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Sink, BaseKey}, []).

%%% INTERNAL EXPORTS
init({Sink, BaseKey}) ->
    {ok, Delay} = application:get_env(vmstats, delay),
    Ref = erlang:start_timer(Delay, self(), ?TIMER_MSG),
    {{input, In}, {output, Out}} = erlang:statistics(io),
    {GCs, Words, _} = erlang:statistics(garbage_collection),

    State = #state{key = [BaseKey, $.],
                   sink = Sink,
                   timer_ref = Ref,
                   delay = Delay,
                   prev_io = {In, Out},
                   prev_gc = {GCs, Words}},

    case {sched_time_available(), application:get_env(vmstats, sched_time)} of
        {true, {ok,true}} ->
            {ok, State#state{sched_time = enabled,
                             prev_sched = scheduler_wall_time()}};
        {true, _} ->
            {ok, State#state{sched_time = disabled}};
        {false, _} ->
            {ok, State#state{sched_time = unavailable}}
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, R, ?TIMER_MSG}, S = #state{sink=Sink, key=K, delay=D, timer_ref=R}) ->
    %% Processes
    Sink:write(gauge, [K,"proc_count"], erlang:system_info(process_count)),
    Sink:write(gauge, [K,"proc_limit"], erlang:system_info(process_limit)),

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
    Sink:write(gauge, [K,"messages_in_queues"], TotalMessages),

    %% Modules loaded
    Sink:write(gauge, [K,"modules"], length(code:all_loaded())),

    %% Queued up processes (lower is better)
    Sink:write(gauge, [K,"run_queue"], erlang:statistics(run_queue)),

    %% Error logger backlog (lower is better)
    {_, MQL} = process_info(whereis(error_logger), message_queue_len),
    Sink:write(gauge, [K,"error_logger_queue_len"], MQL),

    write_memory_stats(Sink, [K, "memory."]),

    %% Incremental values
    IO = write_io_stats(Sink, [K, "io."], S),
    GC = write_gc_stats(Sink, [K, "gc."], S),

    %% Reductions across the VM, excluding current time slice, already incremental
    {_, Reds} = erlang:statistics(reductions),
    Sink:write(increment, [K,"reductions"], Reds),

    Sched = write_sched_time_stats(Sink, [K, "scheduler_wall_time."], S),

    Ref = erlang:start_timer(D, self(), ?TIMER_MSG),
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

terminate(_Reason, _State) ->
    ok.

%% Returns the two timeslices as a ratio of each other,
%% as a percentage so that StatsD gets to print something > 1
wall_time_diff(T1, T2) ->
    [{I, Active2-Active1, Total2-Total1}
     || {{I, Active1, Total1}, {I, Active2, Total2}} <- lists:zip(T1,T2)].

sched_time_available() ->
    try erlang:system_flag(scheduler_wall_time, true) of
        _ -> true
    catch
        error:badarg -> false
    end.

write_io_stats(Sink, Key, #state{prev_io = {PrevIn, PrevOut}}) ->
    {{input, In}, {output, Out}} = erlang:statistics(io),
    Sink:write(increment, [Key, "bytes_in"], In - PrevIn),
    Sink:write(increment, [Key, "bytes_out"], Out - PrevOut),
    {In, Out}.

write_gc_stats(Sink, Key, #state{prev_gc = {PrevGCs, PrevWords}}) ->
    {GCs, Words, _} = erlang:statistics(garbage_collection),
    Sink:write(increment, [Key, "count"], GCs - PrevGCs),
    Sink:write(increment, [Key, "words_reclaimed"], Words - PrevWords),
    {GCs, Words}.

%% There are more options available, but not all were kept.
%% Memory usage is in bytes.
write_memory_stats(Sink, Key) ->
    Sink:write(gauge, [Key, "total"], erlang:memory(total)),
    Sink:write(gauge, [Key, "procs_used"], erlang:memory(processes_used)),
    Sink:write(gauge, [Key, "atom_used"], erlang:memory(atom_used)),
    Sink:write(gauge, [Key, "binary"], erlang:memory(binary)),
    Sink:write(gauge, [Key, "ets"], erlang:memory(ets)).

write_sched_time_stats(Sink, Key, #state{sched_time = enabled, prev_sched = PrevSched}) ->
    Sched = scheduler_wall_time(),
    [begin
        LSid = integer_to_list(Sid),
        Sink:write(timing, [Key, LSid, ".active"], Active),
        Sink:write(timing, [Key, LSid, ".total"], Total)
     end
     || {Sid, Active, Total} <- wall_time_diff(PrevSched, Sched)],
    Sched;

write_sched_time_stats(_Sink, _Key, #state{prev_sched = PrevSched}) -> PrevSched.

scheduler_wall_time() ->
    lists:sort(erlang:statistics(scheduler_wall_time)).
