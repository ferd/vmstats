-module(vmstats_sink).

-type write_type() :: increment | gauge | timing.

-callback write(Type :: write_type(), Key :: [iodata()], Value :: term()) -> ok.
