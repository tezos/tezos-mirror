(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t

type service = {
  listen_port : int;
  proxy_target : string;  (** e.g. ["http://127.0.0.1:13000"] *)
}

(** [run ~username ~password ~services ()] starts an nginx container with
    basic auth protecting the given services. Each service gets a [server {}]
    block proxying [listen_port] to [proxy_target].

    The htpasswd file is generated with restricted permissions (0o600). *)
val run :
  username:string -> password:string -> services:service list -> unit -> t Lwt.t

(** [add_service t service] adds a new proxy entry to the nginx configuration
    and reloads nginx. Used to add Netdata agents dynamically. *)
val add_service : t -> service -> unit Lwt.t

(** [shutdown t] stops the nginx container. *)
val shutdown : t -> unit Lwt.t
