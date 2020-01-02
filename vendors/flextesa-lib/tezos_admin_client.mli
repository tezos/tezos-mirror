open Internal_pervasives
(** Wrapper around the [tezos-admin-client] application. *)

type t = private {id: string; port: int; exec: Tezos_executable.t}
(** [t] is very similar to {!Tezos_client.t}. *)

val of_client : exec:Tezos_executable.t -> Tezos_client.t -> t
val of_node : exec:Tezos_executable.t -> Tezos_node.t -> t

val make_command :
     < env_config: Environment_configuration.t ; paths: Paths.t ; .. >
  -> t
  -> string list
  -> unit Genspio.EDSL.t
(** Build a [Genspio.EDSL.t] command. *)

val successful_command :
     t
  -> < application_name: string
     ; console: Console.t
     ; env_config: Environment_configuration.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; .. >
  -> string list
  -> ( Process_result.t
     , [> Process_result.Error.t | System_error.t] )
     Asynchronous_result.t

val inject_protocol :
     t
  -> < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; env_config: Environment_configuration.t
     ; runner: Running_processes.State.t
     ; .. >
  -> path:string
  -> ( Process_result.t * string
     , [> Process_result.Error.t | System_error.t] )
     Asynchronous_result.t
