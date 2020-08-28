module type S = sig
  type 'a t = 'a Internal_event.Simple.t

  val emit : 'a t -> 'a -> unit Lwt.t

  val proxy_getter_created : (string * string) t

  val proxy_block_header : (string * string) t

  val proxy_block_rpc : (string * string * string list) t

  val no_tree_received : unit t

  val tree_received : int64 t
end

val logger : protocol_name:string -> (module S)
