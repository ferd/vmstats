# Changelog

## 2.0.0

- Removing the `statsderl` dependency.
- Adding the `vmstats_sink` behaviour and making sinks configurable. Since version 2.0.0, `vmstats` doesn't rely on `statsderl` anymore: the sink metrics are written to is now configurable. A sink must implement the `vmstats_sink` behaviour.
- Adding a `key_separator` option to decide the separator to use when composing metric keys.
- Enabling scheduler wall time statistics by default.

## 1.0.0

- Updating rebar.
- Updating `statsderl` to 0.3.5.

## 0.2.3

- Adding messages in queues metric, providing a global count of queued up messages in the system.

## 0.2.2

- Adding garbage collection count per interval.
- Adding words reclaimed in garbage collections per interval.
- Adding reduction increment count per interval.
- Adding IO data (bytes in and out) per interval.
