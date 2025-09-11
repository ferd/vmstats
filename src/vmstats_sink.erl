-module(vmstats_sink).

-type collect_type() :: counter | gauge | timing.

-callback collect(Type :: collect_type(), Key :: iodata(), Value :: term()) -> ok.

-export_type([collect_type/0]).
