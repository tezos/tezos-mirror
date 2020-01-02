(** Helpers to run Kiln with a network-sandbox. *)

open Internal_pervasives

(** Generate Kiln ["./config/"] directories from sandbox parameters. *)
module Configuration_directory : sig
  type t = {path: string; clean: bool; p2p_port: int}

  val generate :
       < application_name: string
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> ?protocol_execs:(string * Tezos_executable.t * Tezos_executable.t) list
    -> t
    -> peers:int list
    -> sandbox_json:string
    -> nodes:string list
    -> bakers:(string * string) list
    -> network_string:string
    -> node_exec:Tezos_executable.t
    -> client_exec:Tezos_executable.t
    -> (unit, [> System_error.t]) Asynchronous_result.t

  val cli_term :
    < manpager: Manpage_builder.State.t ; .. > -> t option Cmdliner.Term.t
end
