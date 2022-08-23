exception Abort of Source.region * string

exception Assert of Source.region * string

exception IO of Source.region * string

val trace : string -> unit

val run_string : string -> bool Lwt.t

val run_file : string -> bool Lwt.t

val host_funcs_registry : Host_funcs.registry
