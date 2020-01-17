# Changelog

## 2.4.0

- [Allow passing options directly to server instead of setting global env](https://github.com/ferd/vmstats/pull/29)
- [Add vmstats:child_spec/1 for external supervisors](https://github.com/ferd/vmstats/pull/22)
- [Add missing field value in tests](https://github.com/ferd/vmstats/pull/28)
- [Add vm_uptime function to vmstats](https://github.com/ferd/vmstats/pull/25)

## 2.3.1

- OTP-21 Support (thanks to @hauleth)

## 2.3.0

- Adding Atom counts (thanks to @Coffei and @lexmag)

## 2.2.0

- Making memory metrics configurable (thanks to @gootik)

## 2.1.0

- Adding port counts and port limit gauges

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
