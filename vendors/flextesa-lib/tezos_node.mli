open Internal_pervasives

type custom_network = [`Json of Ezjsonm.value]

type t = private
  { id: string
  ; expected_connections: int
  ; rpc_port: int
  ; p2p_port: int
  ; peers: int list
  ; exec: Tezos_executable.t
  ; protocol: Tezos_protocol.t
  ; history_mode: [`Archive | `Full of int | `Rolling of int] option
  ; single_process: bool
  ; cors_origin: string option
  ; custom_network: custom_network option }

val compare : t -> t -> int
val equal : t -> t -> bool
val ef : t -> Easy_format.t
val pp : t Fmt.t

val make :
     ?cors_origin:string
  -> exec:Tezos_executable.t
  -> ?protocol:Tezos_protocol.t
  -> ?custom_network:[`Json of Ezjsonm.value]
  -> ?single_process:bool
  -> ?history_mode:[`Archive | `Full of int | `Rolling of int]
  -> string
  -> expected_connections:int
  -> rpc_port:int
  -> p2p_port:int
  -> int list
  -> t
(** Create a node value (inert, not started), see
   ["tezos-node run --help"] for corresponding parameters.

- [?single_process]: defaults to [true] (for now since multi-process
  validations still suffers from some bugs).
- [?history_mode]: defaults to leaving the node's default (i.e. [`Full]).
- [?cors_origin]: defaults to [Some "*"] (most permissive).
 *)

val data_dir : < paths: Paths.t ; .. > -> t -> string
val config_file : < paths: Paths.t ; .. > -> t -> string
val identity_file : < paths: Paths.t ; .. > -> t -> string
val log_output : < paths: Paths.t ; .. > -> t -> string
val exec_path : < paths: Paths.t ; .. > -> t -> string

val node_command :
     < env_config: Environment_configuration.t ; paths: Paths.t ; .. >
  -> t
  -> string list
  -> string list
  -> unit Genspio.Language.t

val run_command :
     < env_config: Environment_configuration.t ; paths: Paths.t ; .. >
  -> t
  -> unit Genspio.Language.t

val start_script :
     < env_config: Environment_configuration.t ; paths: Paths.t ; .. >
  -> t
  -> unit Genspio.Language.t

val process :
     < env_config: Environment_configuration.t ; paths: Paths.t ; .. >
  -> t
  -> Running_processes.Process.t

val protocol : t -> Tezos_protocol.t

val connections :
  t list -> [`Duplex of t * t | `From_to of t * t | `Missing of t * int] list

module History_modes : sig
  type 'error edit = t list -> (t list, 'error) Asynchronous_result.t

  val cmdliner_term :
       < manpager: Manpage_builder.State.t ; .. >
    -> [> System_error.t] edit Cmdliner.Term.t
end

module Config_file : sig
  val network : ?genesis_hash:string -> unit -> (string * Ezjsonm.value) list
  val default_network : (string * Ezjsonm.value) list
  val of_node : < paths: Paths.t ; .. > -> t -> string
end
