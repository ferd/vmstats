# vmstats #

vmstats is a tiny Erlang app that works in conjunction with [statsderl](https://github.com/ferd/statsderl) in order to generate information on the Erlang VM for graphite logs.

The different fields include:
 - the error\_logger queue length
 - the number of modules loaded
 - the number of processes
 - the process limit
 - the length of the run queue
 - the scheduler usage as a percentage (disabled by default)
 - memory used for ETS tables, atoms, processes, binaries and the total memory

## How to build ##

 `$ ./rebar compile`

or

 `$ make`

## Other Stuff

Although it was recommended to leave the interval at 1000ms (1 second) as graphite seems to dampen missing data points on intervals larger than that, the newer version of this application allows more precise or longer delays given the use of guauges instead of increments.

## I want to use newer versions but disable scheduler wall time statistics ##

Scheduler wall time statistics are now disabled by default to keep in line with 0.1.0 behaviour, and after some bugs being reported when the Erlang scheduler would lock on such calls in R15B01 a few times in a day, never to unlock again. People who want to take the risk of running these statistics can do it by setting the `vmstats` env variable `sched_time` to `true`.

## I was basing myself on 'master' and stuff started breaking!

You are likely using vmstats with an Erlang release prior to R15B. Switch away from master and use the tag "0.1.0" to get back to the functionning version you knew.
