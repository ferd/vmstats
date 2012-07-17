# vmstats #

vmstats is a tiny Erlang app that works in conjunction with [statsderl](https://github.com/ferd/statsderl) in order to generate information on the Erlang VM for graphite logs.

The different fields include:
 - the error\_logger queue length
 - the number of modules loaded
 - the number of processes
 - the process limit
 - the length of the run queue
 - the scheduler usage as a percentage
 - memory used for ETS tables, atoms, processes, binaries and the total memory

## How to build ##

 `$ ./rebar compile`

## Other Stuff

It is recommended to leave the interval at 1000ms (1 second) as graphite seems to dampen missing data points on intervals larger than that, or to accumulate them when they're smaller. At roughly 1 second, the values seem to represent what the Erlang VM outputs the best.

## I was basing myself on 'master' and stuff started breaking!

You are likely using vmstats with an Erlang release prior to R15B. Switch away from master and use the tag "0.1.0" to get back to the functionning version you knew.
