(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** An agent is a middleware to communicate with a remote host. *)

(** Datatype for an agent *)
type t

module Configuration : sig
  type docker_image =
    | Gcp of {alias : string}
    | Octez_release of {tag : string}

  type vm = private {
    machine_type : string;
    disk_type : string option;
    disk_size_gb : int option;
    docker_image : docker_image;
    dockerbuild_args : (string * string) list;
    max_run_duration : int option;
    binaries_path : string;
    os : Types.Os.t;
  }

  type t = {name : string; vm : vm}

  val pp : Format.formatter -> t -> unit

  val uri_of_docker_image : docker_image -> string Lwt.t

  val registry_uri_of_docker_image : docker_image -> string option Lwt.t

  val docker_image_name : docker_image -> string

  (** [make ?machine_type ()] is a smart-constructor to make a VM
      configuration.

    Default value for [max_run_duration] is [7200].

    Default value for [machine_type] is [n1-standard-2].

    Default value for [disk_type] is [pd-ssd].

    Default value for [disk_size_gb] is [200].

    Default value for [docker_image] is [Custom {tezt_cloud}] where [tezt_cloud]
    is the value provided by the environment variable [$TEZT_CLOUD].
    *)
  val make :
    ?os:Types.Os.t ->
    ?binaries_path:string ->
    ?max_run_duration:int option ->
    ?machine_type:string ->
    ?disk_type:string ->
    ?disk_size_gb:int ->
    ?docker_image:docker_image ->
    ?dockerbuild_args:(string * string) list ->
    ?name:string ->
    unit ->
    t
end

(** [make ?zone ?ssh_id ?point ~configuration ~next_available_port ~vm_name
    ~artifacts_dir ()] creates an [agent] from the given parameters.
    [~next_available_port] should always provide an available port or raise
    [Not_found] otherwise. [~vm_name] is the name of the VM. [?ssh_id] and
    [?point] are used to potentially create a [runner] for the [agent].
    [daily_logs_dir] stands for the path to the agent's daily logs. *)
val make :
  ?zone:string ->
  ?ssh_id:string ->
  ?point:string * int ->
  configuration:Configuration.t ->
  next_available_port:(unit -> int) ->
  vm_name:string option ->
  process_monitor:Process_monitor.t option ->
  artifacts_dir:string option ->
  unit ->
  t

(** Encode an agent configuration. *)
val encoding : t Data_encoding.t

(** [name agent] returns the name provided in the agent configuration. *)
val name : t -> string

(** [vm_name agent] returns the name of the VM. *)
val vm_name : t -> string option

(** [point agent] returns the point asociated with the agent. *)
val point : t -> (string * int) option

(** [next_available_port agent] returns the next available port for
    this agent. Raises [Not_found] if no port is available. *)
val next_available_port : t -> int

(** [runner agent] returns the runner associated with the agent. *)
val runner : t -> Runner.t option

(** [configuration t] the configuration of the agent. *)
val configuration : t -> Configuration.t

(** [artifacts_dir agent] artifacts directory associated to the agent. *)
val artifacts_dir : t -> string option

(** A wrapper to run a command on the VM of the agent. *)
val cmd_wrapper : t -> Gcloud.cmd_wrapper option

(** Run a command on the host machine of the VM. *)
val host_run_command : t -> string -> string list -> Process.t

(** Returns the process monitor if any *)
val process_monitor : t -> Process_monitor.t option

(** Returns the service manager if any *)
val service_manager : t -> Service_manager.t option

(** Returns the path in which the agent aims it's data. *)
val temp_execution_path : unit -> string

(** Register a callback that will be executed as soon as the agent is shutting
    down. *)
val register_shutdown_callback : t -> (unit -> unit Lwt.t) -> unit

(** Run a command on the docker image run by the agent.

    This command should not be used outside of the [tezt-cloud]
    library. It does not behave well when the scenario is interrupted
    and the process is still running. Instead, [Process.spawn
    ~runner:(Agent.runner agent)] should be used.

    The library uses it to ensure there won't be any check of the host
    when issuing for the first time an ssh connection.

    [detach] Allows the command to be run in background and detaching from the
    owning terminal and parent process. In this case, a temporary file is
    automatically created in /tmp/tezt-$n with the name of the command as
    prefix (warning: it can causes duplicates).
 *)
val docker_run_command :
  ?name:string -> t -> ?detach:bool -> string -> string list -> Process.t

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

(** [scp agent ~is_directory ~source ~destination direction] runs scp to copy a
    file/directory from or to the agent, depending on the [direction]. *)
val scp :
  t ->
  is_directory:bool ->
  source:string ->
  destination:string ->
  [< `FromRunner | `ToRunner] ->
  unit Lwt.t
