-module(vmstats_statsderl).
-behaviour(vmstats_sink).

-export([write/3]).

write(increment, Key, Value) ->
    statsderl:increment(Key, Value);
write(gauge, Key, Value) ->
    statsderl:gauge(Key, Value);
write(timing, Key, Value) ->
    statsderl:timing(Key, Value).
