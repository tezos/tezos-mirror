(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [auth_configure_docker ~hostname] allows to authenticate the
    current session with the docker registry hostname. This enables
    for example to push an image into a docker registry host by this
    hostname.

    Example of hostname: europe-west1-docker.pkg.dev.
 *)
val auth_configure_docker : hostname:string -> unit Lwt.t

(** [project_id] returns the default project id of the user with [gcloud]. *)
val project_id : unit -> string Lwt.t

(** [compute_ssh ~vm_name cmd args] allows to run a command on the remote host.
  *)
val compute_ssh :
  zone:string ->
  vm_name:string ->
  ssh_private_key_filename:string ->
  string ->
  string list ->
  Process.t

type cmd_wrapper = {cmd : string; args : string list}

(** [cmd_wrapper ~zone ~vm_name cmd args] is the wrapper to run a command on
    the vm [vm_name]. *)
val cmd_wrapper :
  zone:string ->
  vm_name:string ->
  ssh_private_key_filename:string ->
  cmd_wrapper

(** [get_ip_address_from_name ~zone name] allows to get the external IP
    address of a VM from its name [name]. *)
val get_ip_address_from_name : zone:string -> string -> string Lwt.t

val list_vms : prefix:string -> string Lwt.t

module DNS : sig
  val create_zone : domain:string -> zone:string -> unit -> unit Lwt.t

  val describe : zone:string -> unit -> string Lwt.t

  (** [get_fqdn ~zone ~name] returns the fully qualified domain name (FQDN)
      corresponding to the subdomain or hostname [name] in the GCP zone [zone] *)
  val get_fqdn : zone:string -> name:string -> string Lwt.t

  (** [get_value ~zone ~domain] returns the value associated to [domain] in the
      zone [zone]. For example, an ip associated to a hostname. *)
  val get_value : zone:string -> domain:string -> string option Lwt.t

  (** [add_subdomain ~zone ~name ~value] adds a dns entry for the domain name
    [name], associated to the value [value]. The value being an ip *)
  val add_subdomain : zone:string -> name:string -> value:string -> unit Lwt.t

  (** [remove_subdomain ~zone ~name ~value] removes the dns record associated
      to [name] and value [value]. If follows the gcloud dns record-sets
      semantics for removing values associated to the same key. *)
  val remove_subdomain :
    zone:string -> name:string -> value:string -> unit Lwt.t
end
