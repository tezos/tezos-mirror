(** Manage a Kiln process next to a network-sandbox. *)

open Internal_pervasives

type t

val make :
     run:[`Dev_mode of string * string | `Docker of string]
  -> port:int
  -> ?postgres:[`Docker of int]
  -> pause_for_user:bool
  -> unit
  -> t
(** Configure a Kiln process-to-be, running on port [~port] and
    managing a PostgreSQL database on port [~postgres:(`Docker
    port)]. If [pause_for_user] is [true], !{start} will add an
    interactive pause to show the user the URI of the WebUI. *)

val default_docker_image : string
val default : t

val start :
     ?network_id:string
  -> < application_name: string
     ; console: Console.t
     ; paths: Paths.t
     ; pauser: Interactive_test.Pauser.t
     ; runner: Running_processes.State.t
     ; test_interactivity: Interactive_test.Interactivity.t
     ; .. >
  -> t
  -> node_uris:string list
  -> bakers:(string * string) list
  -> ( Running_processes.State.process_state option
       * Running_processes.State.process_state
     , [> `Lwt_exn of exn | `Waiting_for of string * [`Time_out]] )
     Asynchronous_result.t
(** Start the Kiln and Postgres processes. [~network_id] is usually
    the chain-id of the sandbox, [~node_uris] is the list or URIs given to
    the ["--nodes"] option, if [~bakers] is not [[]] the test will force
    [state#pauser] to pause for the user to add the baker addresses to
    Kiln. *)

val cli_term : unit -> t option Cmdliner.Term.t
(** Build a {!Cmdliner.Term.t} which provides options like
    ["--with-kiln"] or ["--kiln-docker-image"]. *)

module Configuration_directory : sig
  type t = {path: string; clean: bool; p2p_port: int}

  val generate :
       < application_name: string
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> t
    -> peers:int list
    -> sandbox_json:string
    -> nodes:string list
    -> bakers:(string * string) list
    -> network_string:string
    -> node_exec:Tezos_executable.t
    -> client_exec:Tezos_executable.t
    -> protocol_execs:(string * Tezos_executable.t * Tezos_executable.t) list
    -> (unit, [> Lwt_exception.t]) Asynchronous_result.t

  val cli_term : unit -> t option Cmdliner.Term.t
end
