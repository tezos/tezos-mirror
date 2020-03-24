open Internal_pervasives

(** Helpers to generate traffic in sandboxes. *)

module Michelson : sig
  val prepare_origination_of_id_script :
       ?delegate:string
    -> ?push_drops:int
    -> ?amount:string
    -> < application_name: string ; .. >
    -> name:string
    -> from:string
    -> protocol_kind:Tezos_protocol.Protocol_kind.t
    -> parameter:string
    -> init_storage:string
    -> (string list, [> System_error.t]) Asynchronous_result.t
end

module Random : sig
  val run :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> protocol:Tezos_protocol.t
    -> nodes:Tezos_node.t list
    -> clients:Tezos_client.t list
    -> until_level:int
    -> [> `Any]
    -> (unit, [> System_error.t]) Asynchronous_result.t
end

module Forge : sig
  val batch_transfer :
       ?protocol_kind:Tezos_protocol.Protocol_kind.t
    -> ?counter:int
    -> ?dst:(string * int) list
    -> src:string
    -> fee:float
    -> branch:string
    -> int
    -> Ezjsonm.value

  val endorsement :
       ?protocol_kind:Tezos_protocol.Protocol_kind.t
    -> branch:string
    -> int
    -> Ezjsonm.value
end
