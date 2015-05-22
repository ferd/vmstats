-module(statsderl).
-export([start_link/0, increment/3, gauge/3, called/0, stop/0]).

start_link() ->
    spawn_link(fun() -> init() end).

increment(Key, Value, SampleRate) ->
    call({increment, Key, Value, SampleRate}).

gauge(Key, Value, SampleRate) ->
    call({gauge, Key, Value, SampleRate}).

called() -> call(called).

stop() -> call(stop).

init() ->
    register(?MODULE, self()),
    loop([]).

loop(Stack) ->
    receive
        {From, {increment, K, D, F}} ->
            reply(From, ok),
            loop([{K,D,F}|Stack]);
        {From, {gauge, K, D, F}} ->
            reply(From, ok),
            loop([{K,D,F}|Stack]);
        {From, called} ->
            reply(From, lists:reverse(Stack)),
            loop([]);
        {From, stop} ->
            reply(From, ok)
    end.


call(Msg) ->
    Ref = make_ref(),
    ?MODULE ! {{self(), Ref}, Msg},
    receive
        {Ref, Reply} -> Reply
    end.

reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.
