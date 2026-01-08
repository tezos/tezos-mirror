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

(** [project_id ()] returns the default project id of the user with [gcloud]. *)
val project_id : unit -> string Lwt.t

type cmd_wrapper = {cmd : string; args : string list}

(** [cmd_wrapper ~zone ~vm_name cmd args] is the wrapper to run a command on
    the vm [~vm_name]. *)
val cmd_wrapper :
  zone:string ->
  vm_name:string ->
  ssh_private_key_filename:string ->
  cmd_wrapper

(** [compute_ssh ~vm_name cmd args] allows to run a command on the remote host.
  *)
val compute_ssh :
  zone:string ->
  vm_name:string ->
  ssh_private_key_filename:string ->
  string ->
  string list ->
  Process.t

(** [get_ip_address_from_name ~zone name] allows to get the external IP
    address of a VM from its name [name]. *)
val get_ip_address_from_name : zone:string -> string -> string Lwt.t

(** [list_vms ~prefix] retrieves a list "RUNNING" VMs matching specified
    [~prefix]. *)
val list_vms : prefix:string -> string Lwt.t

(** [list_instances ?filter] retrieves a list of virtual machines with details
    in json format. Filter can be used to filter by name *)
val list_instances : ?filter:string -> unit -> JSON.t Lwt.t

(** [list_instance_groups ?filter] retrieves a list of virtual machines groups
    with details in json format. Filter can be used to filter by name *)
val list_instance_groups : ?filter:string -> unit -> JSON.t Lwt.t

(** [list_instance_templates ?filter] retrieves a list of virtual machines
    templates with details in json format. Filter can be used to filter by name *)
val list_instance_templates : ?filter:string -> unit -> JSON.t Lwt.t

(** [list_addresses ?filter] retrieves a list of virtual machines with details
    in json format. Filter can be used to filter by name *)
val list_addresses : ?filter:string -> unit -> JSON.t Lwt.t

(** [list_networks ?filter] retrieves a list of gcp networks with details in
    json format. Filter can be used to filter by name *)
val list_networks : ?filter:string -> unit -> JSON.t Lwt.t

(** [list_subnets ?filter] retrieves a list of gcp subnets with details in json
    format. Filter can be used to filter by name *)
val list_subnets : ?filter:string -> unit -> JSON.t Lwt.t

(** [list_firewalls ?filter] retrieves a list of virtual machines instances
    with details in json format. Filter can be used to filter by name *)
val list_firewalls : ?filter:string -> unit -> JSON.t Lwt.t

(** [list_iam_service_accounts ?filter] retrieves a list of service accounts
    with details in json format. Filter can be used to filter by name *)
val list_iam_service_accounts : ?filter:string -> unit -> JSON.t Lwt.t

(** [list_disks ?filter] retrieves a list of disks with details in json format.
    Filter can be used to filter by name *)
val list_disks : ?filter:string -> unit -> JSON.t Lwt.t

module DNS : sig
  (** [create_zone ~domain ~zone ()] creates a [~zone] associated with
      [~domain]. *)
  val create_zone : domain:string -> zone:string -> unit -> unit Lwt.t

  (** [list_zones ()] list available zones in google cloud project *)
  val list_zones : unit -> (string * string) list Lwt.t

  (** [describe ~zone ()] describes the given [~zone]. *)
  val describe : zone:string -> unit -> string Lwt.t

  (** [list ~zone ()] lists the DNS entries for the given [~zone]. *)
  val list : zone:string -> unit -> string Lwt.t

  (** [get_fqdn ~zone ~name] returns the fully qualified domain name (FQDN)
      corresponding to the subdomain or hostname [~name] in the GCP zone [~zone].

      The function returns [None] if the [zone] does not exists. *)
  val get_fqdn : zone:string -> name:string -> string option Lwt.t

  (** [get_value ~zone ~domain] returns the value associated to [~domain] in the
      zone [~zone]. For example, an ip associated to a hostname. *)
  val get_value : zone:string -> domain:string -> string option Lwt.t

  (** [find_zone_for_subdomain domain] finds a zone suitable to add a subdomain
      into. It relies on gcloud credentials to find authorized zones.
      returns [None] in case no zone is suitable, or [Some (zone, domain)] with
      domain the parent domain attached to the zone *)
  val find_zone_for_subdomain : string -> (string * string) option Lwt.t

  (** [add_subdomain ~zone ~name ~value] adds a dns entry for the domain name
    [~name], associated to the value [~value]. The value being an ip *)
  val add_subdomain : zone:string -> name:string -> value:string -> unit Lwt.t

  (** [remove_subdomain ~zone ~name ~value] removes the dns record associated
      to [~name] and value [~value]. If follows the gcloud dns record-sets
      semantics for removing values associated to the same key. *)
  val remove_subdomain :
    zone:string -> name:string -> value:string -> unit Lwt.t

  (** [set_subdomain ~agent_ip ~domain] registers the specified domain in an
     appropriate GCP zone.
     This function is destructive for previous DNS entries. It does not only
     add the new DNS, it also remove the previous ones if any.
     *)
  val set_subdomain : agent_ip:string -> domain:string -> unit Lwt.t
end
