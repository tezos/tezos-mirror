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

(** [delete_address ~name ~region ~project_id ()] deletes a single static IP
    address [name] in the given [region] for the specified [project_id].

    This is a low-level primitive that calls
    [gcloud compute addresses delete] with [--quiet] to suppress
    confirmation prompts.  It raises on failure (e.g. address not found,
    permission denied).

    Prefer {!delete_unused_addresses} for bulk cleanup with safety
    filters. *)
val delete_address :
  name:string -> region:string -> project_id:string -> unit -> unit Lwt.t

(** [delete_unused_addresses ~project_id ?name_filter ?max_age_hours ()]
    finds and deletes GCP static IP addresses that are unused (status
    [RESERVED]).

    This function is used in two contexts:

    {b (a) Post-destroy targeted cleanup.}  After [terraform destroy],
    call with [~name_filter:(Some workspace_name)] and no
    [max_age_hours].  Since we know the workspace has just been
    destroyed, any matching RESERVED address is genuinely orphaned.

    {b (b) Periodic project-wide garbage collection.}  Call with no
    [name_filter] and [~max_age_hours:24].  This catches orphaned
    addresses from any user that were missed by the targeted cleanup
    (e.g. terraform state corruption, interrupted process, etc.).

    {b Safety invariants:}

    - {b Scoping by name prefix.}  When provided, [name_filter] is
      passed to [gcloud --filter="name:{name_filter}"].  In the
      tezt-cloud naming convention, addresses are named
      ["{workspace}-{index:02d}"] where the workspace is
      ["{tezt_cloud}-{n}"].  Passing a workspace name like ["alice-0"]
      restricts to that workspace; passing ["alice"] covers all of
      Alice's workspaces.  When [None], all RESERVED addresses in the
      project are considered.

    - {b Only RESERVED addresses are deleted.}  Addresses that are
      [IN_USE] (attached to a VM) are never touched.  This is the
      primary safeguard against deleting IPs of a running scenario.

    - {b Age-based filtering} via [max_age_hours].  Protects against
      a race condition: between [terraform apply] creating an address
      and the VM attaching it (making it [IN_USE]), there is a short
      window where the address appears [RESERVED].  Setting
      [max_age_hours] (e.g. [Some 24]) avoids deleting those
      freshly-created addresses.  {b When [name_filter] is [None],
      [max_age_hours] should always be set} to avoid accidentally
      deleting another user's in-flight addresses.

    - {b Concurrent-safe.}  Individual deletion failures (e.g. another
      process already deleted the address) are caught and logged as
      warnings without aborting the remaining deletions.

    @param project_id    The GCP project identifier.
    @param name_filter   A prefix for the [name:] gcloud filter.
      When [None], all RESERVED addresses in the project are
      considered.
    @param max_age_hours  When set, only addresses whose
      [creationTimestamp] is strictly older than this many hours are
      deleted.  When [None], no age check is performed. *)
val delete_unused_addresses :
  project_id:string ->
  ?name_filter:string ->
  ?max_age_hours:int ->
  unit ->
  unit Lwt.t

(** [create_firewall_rule ~name ~network ~ports ~source_ranges ~priority ()]
    creates a GCP firewall rule allowing TCP traffic on [ports] from
    [source_ranges]. Used to open Netdata proxy ports ([Env.netdata_proxy_base_port]+) on the
    orchestrator when auth is enabled. *)
val create_firewall_rule :
  name:string ->
  network:string ->
  ports:int list ->
  source_ranges:string list ->
  priority:int ->
  unit ->
  unit Lwt.t

(** [delete_firewall_rule ~name ()] deletes the named firewall rule.
    Does not fail if the rule does not exist. *)
val delete_firewall_rule : name:string -> unit -> unit Lwt.t

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
