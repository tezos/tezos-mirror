(** Generic testing helpers *)

open Internal_pervasives

val dump_connections :
     < application_name: string ; console: Console.t ; .. >
  -> Tezos_node.t list
  -> (unit, [> System_error.t]) Asynchronous_result.t
(** Display all the P2P connections of a set of nodes, see
    {!Tezos_node.connections}. *)

val clear_root :
  < paths: Paths.t ; .. > -> (unit, [> System_error.t]) Asynchronous_result.t
(** Remove (["rm -fr .."]) the root-path of the current [state]. *)

val wait_for :
     ?attempts_factor:float
  -> < application_name: string ; console: Console.t ; .. >
  -> attempts:int
  -> seconds:float
  -> (   int
      -> ( [`Done of 'a | `Not_done of string]
         , ([> System_error.t | `Waiting_for of string * [`Time_out]]
            as
            'errors) )
         Asynchronous_result.t)
  -> ('a, 'errors) Asynchronous_result.t
(** Try to wait for an event. The pause between attempts is
    [(attempts_factor * attempts) + seconds] where the default
    [attempts_factor] is [0.]. *)

val kill_node :
     < runner: Running_processes.State.t ; .. >
  -> Tezos_node.t
  -> (unit, [> System_error.t]) Asynchronous_result.t
(** Kill a node's process. *)

val restart_node :
     client_exec:Tezos_executable.t
  -> < application_name: string
     ; console: Console.t
     ; env_config: Environment_configuration.t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; .. >
  -> Tezos_node.t
  -> (unit, [> System_error.t | Process_result.Error.t]) Asynchronous_result.t
(** Restart a killed node. *)

(** Stateful “message × count” log, see its use in, e.g.,
    ["./src/bin_flextesa/command_voting.ml"] where baked-levels
    are accumulated and then displayed. . *)
module Counter_log : sig
  type t

  val create : unit -> t
  val add : t -> string -> int -> unit
  val incr : t -> string -> unit
  val sum : t -> int
  val to_table_string : t -> string
end

module Netstat : sig
  val used_listening_ports :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> ( (int * [> `Tcp of int * string list]) list
       , [> System_error.t | Process_result.Error.t] )
       Asynchronous_result.t
  (** Call ["netstat"] to find TCP ports already in use. *)
end

module System_dependencies : sig
  module Error : sig
    type t = [`Precheck_failure of string]

    val pp : Caml.Format.formatter -> [< `Precheck_failure of string] -> unit
  end

  val precheck :
       ?using_docker:bool
    -> ?protocol_paths:string list
    -> ?executables:Tezos_executable.t list
    -> < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> [< `Or_fail]
    -> (unit, [> System_error.t | Error.t]) Asynchronous_result.t
end

module Shell_environement : sig
  type t

  val make : aliases:(string * string * string) list -> t
  (** Aliases are [(name, command, doc)] tuples. *)

  val build : < paths: Paths.t ; .. > -> clients:Tezos_client.t list -> t

  val write :
       < application_name: string ; .. >
    -> t
    -> path:string
    -> (unit, [> System_error.t]) Attached_result.t Lwt.t

  val help_command :
       < application_name: string ; console: Console.t ; .. >
    -> t
    -> path:string
    -> Console.Prompt.item
end
