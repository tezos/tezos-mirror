open Internal_pervasives

type t = private
  { id: string
  ; expected_connections: int
  ; rpc_port: int
  ; p2p_port: int
  ; peers: int list
  ; exec: Tezos_executable.t
  ; protocol: Tezos_protocol.t
  ; history_mode: [`Full | `Archive | `Rolling] option }

val compare : t -> t -> int
val equal : t -> t -> bool
val ef : t -> Easy_format.t
val pp : Format.formatter -> t -> unit

val make :
     exec:Tezos_executable.t
  -> ?protocol:Tezos_protocol.t
  -> ?history_mode:[`Full | `Archive | `Rolling]
  -> string
  -> expected_connections:int
  -> rpc_port:int
  -> p2p_port:int
  -> int list
  -> t

val data_dir : config:< paths: Paths.t ; .. > -> t -> string
val config_file : config:< paths: Paths.t ; .. > -> t -> string
val identity_file : config:< paths: Paths.t ; .. > -> t -> string
val log_output : config:< paths: Paths.t ; .. > -> t -> string
val exec_path : config:< paths: Paths.t ; .. > -> t -> string

val node_command :
     t
  -> config:< paths: Paths.t ; .. >
  -> string list
  -> string list
  -> unit Genspio.Language.t

val run_command :
  t -> config:< paths: Paths.t ; .. > -> unit Genspio.Language.t

val start_script :
  t -> config:< paths: Paths.t ; .. > -> unit Genspio.Language.t

val process : < paths: Paths.t ; .. > -> t -> Running_processes.Process.t
val protocol : t -> Tezos_protocol.t

val connections :
  t list -> [`Duplex of t * t | `From_to of t * t | `Missing of t * int] list

module History_modes : sig
  type 'error edit = t list -> (t list, 'error) Asynchronous_result.t

  val cmdliner_term : unit -> [> System_error.t] edit Cmdliner.Term.t
end
