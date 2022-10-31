type 'a t = 'a Internal_event.Simple.t

val emit : 'a t -> 'a -> unit Lwt.t

val proxy_getter_created : (string * string) t

val proxy_block_rpc : (string * string * string list) t

val tree_received : int64 t
