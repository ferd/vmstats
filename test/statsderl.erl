-module(statsderl).
-export([start_link/0, increment/2, gauge/2, called/0, stop/0]).

start_link() ->
    spawn_link(fun() -> init() end).

increment(Key, Value) ->
    call({increment, Key, Value}).

gauge(Key, Value) ->
    call({gauge, Key, Value}).

called() -> call(called).

stop() -> call(stop).

init() ->
    register(?MODULE, self()),
    loop([]).

loop(Stack) ->
    receive
        {From, {increment, K, D}} ->
            reply(From, ok),
            loop([{K,D}|Stack]);
        {From, {gauge, K, D}} ->
            reply(From, ok),
            loop([{K,D}|Stack]);
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
