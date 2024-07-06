(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** An agent is a middleware to communicate with a remote host. *)

(** Datatype for an agent *)
type t

(** [make ?binaries_path ~ssh_id ~point ~next_available_port ~name]
    creates an [agent] from the given parameters. [point] is the point on
    which the VM is reachabled. [next_available_port] should always provide
    an available port or raise [Not_found] otherwise. [name] is the name of
    the agent. [ssh_id] is a path to the private key that will be used for
    the ssh connection. *)
val make :
  ?cmd_wrapper:Gcloud.cmd_wrapper ->
  ssh_id:string ->
  point:string * int ->
  configuration:Configuration.t ->
  next_available_port:(unit -> int) ->
  name:string ->
  unit ->
  t

(** Encode an agent configuration. *)
val encoding : t Data_encoding.t

(** [name agent] returns the name of the agent. *)
val name : t -> string

(** [set_name agent name] sets the name of the agent to [name]. *)
val set_name : t -> string -> unit

(** [copy agent ~source] copy the file into the [agent] directory and
    returned the directory where the file can be found. It is assumed
    the [source] file does not exist on the agent machine. If the
    parent directory does not exist, it will be created. *)
val copy : t -> source:string -> string Lwt.t

(** [next_available_port agent] returns the next available port for
    this agent. Raises [Not_found] if no port is available. *)
val next_available_port : t -> int

(** [runner agent] returns the runner associated with the agent. *)
val runner : t -> Runner.t

(** [point agent] returns the point asociated with the agent. *)
val point : t -> string * int

(** [configuration t] the configuration of the agent. *)
val configuration : t -> Configuration.t

(** A wrapper to run a command on the VM of the agent. *)
val cmd_wrapper : t -> Gcloud.cmd_wrapper option
