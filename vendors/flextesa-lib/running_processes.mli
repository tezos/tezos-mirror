(** Run and monitor processes. *)

open Internal_pervasives

(** The definition of a process, for now, a process within a
    process-group or a Docker container. *)
module Process : sig
  type kind =
    [`Process_group | `Docker of string | `Process_group_script of string]

  type t = private
    {id: string; binary: string option; command: string list; kind: kind}

  val make_in_session : ?binary:string -> string -> kind -> string list -> t
  val genspio : string -> 'a Genspio.EDSL.t -> t

  val docker_run :
    string -> image:string -> options:string list -> args:string list -> t
end

(** The container for a list of running or not processes. *)
module State : sig
  type process_state = private
    {process: Process.t; lwt: Lwt_process.process_none}

  type t = private {processes: (string, process_state) Caml.Hashtbl.t}

  val pp : t Fmt.t
  val make : unit -> t
end

val output_path :
  < paths: Paths.t ; .. > -> Process.t -> [`Meta | `Stderr | `Stdout] -> string
(** Return the path (within {!Paths}'s root-path) where the process
    writes its output or metadata. *)

val ef_procesess :
  < paths: Paths.t ; .. > -> State.process_state list -> Easy_format.t

val ef : ?all:bool -> < runner: State.t ; .. > -> Easy_format.t

val start :
     < application_name: string ; paths: Paths.t ; runner: State.t ; .. >
  -> Process.t
  -> (State.process_state, [> System_error.t]) Asynchronous_result.t

val wait :
     < runner: State.t ; .. >
  -> State.process_state
  -> (Lwt_unix.process_status, [> System_error.t]) Asynchronous_result.t

val kill :
     < runner: State.t ; .. >
  -> State.process_state
  -> (unit, [> System_error.t]) Asynchronous_result.t

val wait_all :
  < runner: State.t ; .. > -> (unit, [> System_error.t]) Asynchronous_result.t

val kill_all :
  < runner: State.t ; .. > -> (unit, [> System_error.t]) Asynchronous_result.t

val find_process_by_id :
     ?only_running:bool
  -> < runner: State.t ; .. >
  -> f:(string -> bool)
  -> (State.process_state list, [> ]) Asynchronous_result.t

val run_cmdf :
     ?id_prefix:string
  -> < paths: Paths.t ; runner: State.t ; .. > Base_state.t
  -> ( 'a
     , unit
     , string
     , (Process_result.t, [> System_error.t]) Asynchronous_result.t )
     format4
  -> 'a
(** Run a shell command and wait for its end. *)

val run_successful_cmdf :
     < paths: Paths.t ; runner: State.t ; .. > Base_state.t
  -> ( 'a
     , unit
     , string
     , ( Process_result.t
       , [> System_error.t | Process_result.Error.t] )
       Asynchronous_result.t )
     format4
  -> 'a

val run_genspio :
     < paths: Paths.t ; runner: State.t ; .. > Base_state.t
  -> string
  -> 'a Genspio.Language.t
  -> (Lwt_unix.process_status, [> System_error.t]) Asynchronous_result.t

module Async : sig
  val run_cmdf :
       ?id_base:string
    -> < runner: State.t ; .. >
    -> f:(   State.process_state
          -> Lwt_process.process_full
          -> ('return_ok, ([> System_error.t] as 'error)) Asynchronous_result.t)
    -> ( 'c
       , unit
       , string
       , (Unix.process_status * 'return_ok, 'error) Asynchronous_result.t )
       format4
    -> 'c
  (** Run a shell command and run a function over the process data before waiting for its end. *)

  val fold_process :
       Lwt_process.process_full
    -> init:'a
    -> f:(   'a
          -> string
          -> string
          -> ( [< `Continue of 'a | `Done of 'a]
             , ([> System_error.t] as 'b) )
             Asynchronous_result.t)
    -> ('a, 'b) Asynchronous_result.t
  (** Fold over the output and error-output of a {!Lwt_process.process_full}, and  *)
end
