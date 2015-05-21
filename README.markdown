# vmstats #

vmstats is a tiny Erlang app that works in conjunction with [statsderl](https://github.com/lpgauth/statsderl) in order to generate information on the Erlang VM for graphite logs.

The different fields include:
 - the error\_logger queue length
 - the number of modules loaded
 - the number of processes
 - the process limit
 - the length of the run queue
 - the scheduler usage as a percentage (disabled by default)
 - memory used for ETS tables, atoms, processes, binaries and the total memory
 - garbage collection count per interval
 - words reclaimed in garbage collections per interval
 - reduction increment count per interval
 - IO data (bytes in and out) per interval
 - global amount of messages in queues on a node

## How to build ##

````sh
$ rebar get-deps compile
```

or

```sh
$ make
```

## Why scheduler wall time statistics disabled by default? ##

Scheduler wall time statistics are now disabled by default to keep in line with 0.1.0 behaviour, and after some bugs being reported when the Erlang scheduler would lock on such calls in R15B01 a few times in a day, never to unlock again. People who want to take the risk of running these statistics can do it by setting the `vmstats` env variable `sched_time` to `true`.

## I was basing myself on 'master' and stuff started breaking!

That's because you should use tags for stable versions instead! The changelog below should let you know what to expect.

## CHANGELOG ##

### 1.0.0 ###
- Updating rebar
- Updating statsderl to 0.3.5

### 0.2.3 ###
- Adding messages in queues metric, providing a global count of queued up messages in the system.


### 0.2.2 ###

- Adding garbage collection count per interval
- Adding words reclaimed in garbage collections per interval
- Adding reduction increment count per interval
- Adding IO data (bytes in and out) per interval
