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
  string ->
  string list ->
  (Process.t, string) runnable

(** [get_ip_address_from_name ~zone name] allows to get the external IP
    address of a VM from its name [name]. *)
val get_ip_address_from_name : zone:string -> string -> string Lwt.t
