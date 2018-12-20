# vmstats

[![CircleCI](https://circleci.com/gh/ferd/vmstats.svg?style=svg)](https://circleci.com/gh/ferd/vmstats)

vmstats is a tiny Erlang app that gathers metrics on the Erlang VM and sends them to a configurable sink (e.g., StatsD).

## Features

The different metrics that vmstats gathers include:
 - the `error_logger` queue length
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
 - the VM uptime

## Usage

vmstats can be built using `rebar3`:

```sh
$ rebar3 compile
```

Once you have vmstats set up, just add it to the list of applications to start
in order to start gathering data. You'll need a sink (a module that implements
the `vmstats_sink` behaviour) to send metrics to.

### Configuration

The following is a list of the possible options for the configuration of the
`vmstats` app:

  * `sink` - (module) a module that implements the `vmstats_sink` behaviour; vmstats metrics will be collected through this module.
  * `base_key` - (string) every metric name is prepended with this base key. Defaults to `"vmstats"`.
  * `key_separator` - (char) used as a separator between the parts of metric keys. Defaults to `$.`.
  * `interval` - (integer) the time (in milliseconds) between metric gatherings. Defaults to `1000` (1s).
  * `sched_time` - (boolean) whether to gather statistics about scheduler wall time. Defaults to `true`.
  * `memory_metrics` - (proplist of metric and key) what fields to collect statistics for.
                       Available fields can be found [here](http://erlang.org/doc/man/erlang.html#memory-1).
                       Default list is `[{total, total}, {processes_used, procs_used}, {atom_used, atom_used}, {binary, binary}, {ets, ets}]`.

### `vmstats_sink` behaviour

vmstats sinks must implement the `vmstats_sink` behaviour. This behaviour only
specifies one function:

```erlang
-callback collect(Type :: counter | gauge | timing,
                  Key :: iodata(),
                  Value :: term()) -> ok.
```

## I was basing myself on 'master' and stuff started breaking!

That's because you should use tags for stable versions instead! The [changelog](CHANGELOG.md) should let you know what to expect.

## Contributing

Make changes and be sure to test them (`$ rebar3 eunit`).

## Changelog

See the [CHANGELOG.md file](CHANGELOG.md).

## License

See the [license file](LICENSE).
