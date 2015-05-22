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
                             prev_sched = lists:sort(erlang:statistics(scheduler_wall_time))}};
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
    Sink:gauge([K,"proc_count"], erlang:system_info(process_count), 1.00),
    Sink:gauge([K,"proc_limit"], erlang:system_info(process_limit), 1.00),

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
    Sink:gauge([K,"messages_in_queues"], TotalMessages, 1.00),

    %% Modules loaded
    Sink:gauge([K,"modules"], length(code:all_loaded()), 1.00),

    %% Queued up processes (lower is better)
    Sink:gauge([K,"run_queue"], erlang:statistics(run_queue), 1.00),

    %% Error logger backlog (lower is better)
    {_, MQL} = process_info(whereis(error_logger), message_queue_len),
    Sink:gauge([K,"error_logger_queue_len"], MQL, 1.00),

    %% Memory usage. There are more options available, but not all were kept.
    %% Memory usage is in bytes.
    K2 = [K,"memory."],
    Mem = erlang:memory(),
    Sink:gauge([K2,"total"], proplists:get_value(total, Mem), 1.00),
    Sink:gauge([K2,"procs_used"], proplists:get_value(processes_used,Mem), 1.00),
    Sink:gauge([K2,"atom_used"], proplists:get_value(atom_used,Mem), 1.00),
    Sink:gauge([K2,"binary"], proplists:get_value(binary, Mem), 1.00),
    Sink:gauge([K2,"ets"], proplists:get_value(ets, Mem), 1.00),

    %% Incremental values
    IO = write_io_stats(Sink, [K, "io."], S),
    GC = write_gc_stats(Sink, [K, "gc."], S),

    %% Reductions across the VM, excluding current time slice, already incremental
    {_, Reds} = erlang:statistics(reductions),
    Sink:increment([K,"reductions"], Reds, 1.00),

    %% Scheduler wall time
    #state{sched_time=Sched, prev_sched=PrevSched} = S,
    case Sched of
        enabled ->
            NewSched = lists:sort(erlang:statistics(scheduler_wall_time)),
            [begin
                SSid = integer_to_list(Sid),
                Sink:timing([K,"scheduler_wall_time.",SSid,".active"], Active, 1.00),
                Sink:timing([K,"scheduler_wall_time.",SSid,".total"], Total, 1.00)
             end
             || {Sid, Active, Total} <- wall_time_diff(PrevSched, NewSched)],
            {noreply, S#state{timer_ref=erlang:start_timer(D, self(), ?TIMER_MSG),
                              prev_sched=NewSched, prev_io=IO, prev_gc=GC}};
        _ -> % disabled or unavailable
            {noreply, S#state{timer_ref=erlang:start_timer(D, self(), ?TIMER_MSG),
                              prev_io=IO, prev_gc=GC}}
    end;
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
    Sink:increment([Key, "bytes_in"], In - PrevIn, 1.00),
    Sink:increment([Key, "bytes_out"], Out - PrevOut, 1.00),
    {In, Out}.

write_gc_stats(Sink, Key, #state{prev_gc = {PrevGCs, PrevWords}}) ->
    {GCs, Words, _} = erlang:statistics(garbage_collection),
    Sink:increment([Key, "count"], GCs - PrevGCs, 1.00),
    Sink:increment([Key, "words_reclaimed"], Words - PrevWords, 1.00),
    {GCs, Words}.
