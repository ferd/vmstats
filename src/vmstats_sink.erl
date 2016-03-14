-module(vmstats_sink).

-type collect_type() :: counter | gauge | timing.

-callback collect(Type :: collect_type(), Key :: iodata(), Value :: term()) -> ok.
-callback key_separator() -> Separator :: iodata().

-optional_callbacks([key_separator/0]).
