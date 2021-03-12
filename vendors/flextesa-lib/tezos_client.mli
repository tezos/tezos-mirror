open Internal_pervasives
(** Wrapper around the main ["tezos-client"] application. *)

type t = {id: string; port: int; exec: Tezos_executable.t}
type client = t

val of_node : exec:Tezos_executable.t -> Tezos_node.t -> t
(** Create a client which is meant to communicate with a given node. *)

val no_node_client : exec:Tezos_executable.t -> t
(** Create a client not connected to a node (e.g. for ledger interaction). *)

val base_dir : t -> state:< paths: Paths.t ; .. > -> string
(** Get the path to the ["--base-dir"] option of the client. *)

(** {3 Build Scripts } *)

val client_call :
     ?wait:string
  -> < paths: Paths.t ; .. >
  -> client
  -> string list
  -> string list
(** Build the arguments for a given default tezos-client (not
    including the executable). *)

val client_command :
     ?wait:string
  -> < env_config: Environment_configuration.t ; paths: Paths.t ; .. >
  -> client
  -> string list
  -> unit Genspio.Language.t
(** Build a tezos-client command, the default [?wait] is ["none"]. *)

(** {3 Run Specific Client Commands } *)

val wait_for_node_bootstrap :
     < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; env_config: Environment_configuration.t
     ; .. >
  -> client
  -> (unit, [> System_error.t | Process_result.Error.t]) Asynchronous_result.t
(** Wait for the node to be bootstrapped. *)

val import_secret_key :
     < paths: Paths.t
     ; console: Console.t
     ; runner: Running_processes.State.t
     ; env_config: Environment_configuration.t
     ; .. >
     Base_state.t
  -> t
  -> name:string
  -> key:string
  -> (unit, [> System_error.t | Process_result.Error.t]) Asynchronous_result.t

val register_as_delegate :
     < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; env_config: Environment_configuration.t
     ; .. >
  -> client
  -> key_name:string
  -> (unit, [> System_error.t | Process_result.Error.t]) Asynchronous_result.t

val activate_protocol :
     < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; env_config: Environment_configuration.t
     ; .. >
  -> client
  -> Tezos_protocol.t
  -> (unit, [> System_error.t | Process_result.Error.t]) Asynchronous_result.t

val client_cmd :
     ?id_prefix:string
  -> ?verbose:bool
  -> ?wait:string
  -> < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; env_config: Environment_configuration.t
     ; runner: Running_processes.State.t
     ; .. >
  -> client:client
  -> string list
  -> (bool * Process_result.t, [> System_error.t]) Asynchronous_result.t

val successful_client_cmd :
     ?id_prefix:string
  -> ?verbose:bool
  -> ?wait:string
  -> < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; env_config: Environment_configuration.t
     ; .. >
     Base_state.t
  -> client:t
  -> string list
  -> ( < err: string list ; out: string list ; status: Unix.process_status >
     , [> Process_result.Error.t | System_error.t] )
     Asynchronous_result.t

val rpc :
     < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; env_config: Environment_configuration.t
     ; .. >
     Base_state.t
  -> client:t
  -> [< `Get | `Post of string]
  -> path:string
  -> ( Ezjsonm.value
     , [> Process_result.Error.t | System_error.t] )
     Asynchronous_result.t

val find_applied_in_mempool :
     < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; env_config: Environment_configuration.t
     ; runner: Running_processes.State.t
     ; .. >
  -> client:client
  -> f:(Ezjsonm.value -> bool)
  -> ( Ezjsonm.value option
     , [> Process_result.Error.t | System_error.t] )
     Asynchronous_result.t
(** Use RPCs to find an operation matching [~f] in the node's mempool. *)

val mempool_has_operation :
     < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; env_config: Environment_configuration.t
     ; .. >
  -> client:t
  -> kind:string
  -> (bool, [> Process_result.Error.t | System_error.t]) Asynchronous_result.t
(** Use RPCs to find an operation of kind [~kind] in the node's mempool. *)

val block_has_operation :
     < application_name: string
     ; console: Console.t
     ; env_config: Environment_configuration.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; .. >
  -> client:t
  -> level:int
  -> kind:string
  -> (bool, [> Process_result.Error.t | System_error.t]) Asynchronous_result.t
(** Use RPCs to find an operation of kind [~kind] in the node's chain
    at a given level. *)

val get_block_header :
     < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; env_config: Environment_configuration.t
     ; runner: Running_processes.State.t
     ; .. >
  -> client:t
  -> [`Head | `Level of int]
  -> ( Ezjsonm.value
     , [> Process_result.Error.t | System_error.t] )
     Asynchronous_result.t
(** Call the RPC ["/chains/main/blocks/<block>/header"]. *)

val list_known_addresses :
     < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; env_config: Environment_configuration.t
     ; runner: Running_processes.State.t
     ; .. >
  -> client:t
  -> ( (string * string) list
     , [> Process_result.Error.t | System_error.t] )
     Asynchronous_result.t

module Ledger : sig
  type hwm = {main: int; test: int; chain: Tezos_crypto.Chain_id.t option}

  val get_hwm :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:t
    -> uri:string
    -> (hwm, [> Process_result.Error.t | System_error.t]) Asynchronous_result.t

  val set_hwm :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:t
    -> uri:string
    -> level:int
    -> ( unit
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val show_ledger :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:t
    -> uri:string
    -> ( Tezos_protocol.Account.t
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val deauthorize_baking :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:t
    -> uri:string
    -> ( unit
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val get_authorized_key :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:t
    -> uri:string
    -> ( string option
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t
end

module Keyed : sig
  type t = {client: client; key_name: string; secret_key: string}

  val make : client -> key_name:string -> secret_key:string -> t

  val initialize :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> t
    -> ( < err: string list ; out: string list ; status: Unix.process_status >
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t
  (** Get the keyed-client ready to use (i.e. import the secret key). *)

  val bake :
       ?chain:string
    -> < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> t
    -> string
    -> ( unit
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val endorse :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; operations_log: Log_recorder.Operations.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> t
    -> string
    -> ( unit
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val generate_nonce :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> t
    -> string
    -> ( string
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val forge_and_inject :
       < application_name: string
       ; env_config: Environment_configuration.t
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> t
    -> json:Ezjsonm.value
    -> ( Ezjsonm.value
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t
end
