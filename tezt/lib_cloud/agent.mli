(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** An agent is a middleware to communicate with a remote host. *)

(** Datatype for an agent *)
type t

(** [make ?zone ?ssh_id ?point ~configuration ~next_available_port ~name ()]
    creates an [agent] from the given parameters. [~next_available_port] should
    always provide an available port or raise [Not_found] otherwise. 
    [~name] is the name of the agent. [?ssh_id] and [?point] are used to potentially
    create a [runner] for the [agent]. *)
val make :
  ?zone:string ->
  ?ssh_id:string ->
  ?point:string * int ->
  configuration:Configuration.t ->
  next_available_port:(unit -> int) ->
  name:string ->
  unit ->
  t

(** Encode an agent configuration. *)
val encoding : t Data_encoding.t

(** [name agent] returns the name of the agent. *)
val name : t -> string

(** [vm_name agent] returns the vm name of the agent. *)
val vm_name : t -> string

(** [point agent] returns the point asociated with the agent. *)
val point : t -> (string * int) option

(** [next_available_port agent] returns the next available port for
    this agent. Raises [Not_found] if no port is available. *)
val next_available_port : t -> int

(** [runner agent] returns the runner associated with the agent. *)
val runner : t -> Runner.t option

(** [configuration t] the configuration of the agent. *)
val configuration : t -> Configuration.t

(** A wrapper to run a command on the VM of the agent. *)
val cmd_wrapper : t -> Gcloud.cmd_wrapper option

(** Run a command on the host machine of the VM. *)
val host_run_command : t -> string -> string list -> Process.t

(** Run a command on the docker image run by the agent.

    This command should not be used outside of the [tezt-cloud]
    library. It does not behave well when the scenario is interrupted
    and the process is still running. Instead, [Process.spawn
    ~runner:(Agent.runner agent)] should be used.

    The library uses it to ensure there won't be any check of the host
    when issuing for the first time an ssh connection.
 *)
val docker_run_command : t -> string -> string list -> Process.t

(** [copy ?refresh ?is_directory ?destination agent ~source] copies the file 
    into the [agent] directory and returns the directory where the file
    can be found if [?refresh] is set to [true]. It is assumed the [~source]
    file does not exist on the agent machine. If the parent directory does
    not exist, it will be created.

    If [?refresh] is [false], then the function returns the promise associated
    to the scenario given by [agent] and [?destination] and does not copy anything.

    It returns the destination the files was copied at. If
    [?destination] is not set, the destination is given by
    [configuration.default_binaries_path] concatenated to the
    [~source]. Otherwise, [?destination] is returned.

    If [configuration.docker_image] is an [Octez_release], binary
    files won't be copied and the destination will be the same as if
    [?destination] was not provided.

    If [?is_directory] is set, then the whole source is copied, including
    subdirectories. *)
val copy :
  ?consistency_check:bool ->
  ?refresh:bool ->
  ?is_directory:bool ->
  ?destination:string ->
  t ->
  source:string ->
  string Lwt.t
