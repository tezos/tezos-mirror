(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Nginx reverse proxy with HTTP basic authentication.

    When authentication is enabled ([--auth-username] and [--auth-password]),
    tezt-cloud monitoring services (Grafana, Prometheus, etc.) bind to
    [127.0.0.1] on internal ports inaccessible from outside the VM. This
    module runs an nginx container that listens on the original external
    ports and proxies requests to the internal services, requiring valid
    credentials via HTTP basic auth.

    The nginx configuration is generated dynamically from the list of
    services. New services (e.g. Netdata agents) can be added at runtime
    with [add_service], which regenerates the configuration and reloads
    nginx without downtime. *)

type t

(** A service describes a single reverse-proxy entry. When nginx receives
    a request on [listen_port], it forwards it to [proxy_target] after
    verifying basic auth credentials.

    For example, with [{ listen_port = 3000;
    proxy_target = "http://127.0.0.1:13000" }], any request to port [3000]
    is authenticated then proxied to [http://127.0.0.1:13000]. *)
type service = {listen_port : int; proxy_target : string}

(** [run ~username ~password ~services ()] starts an nginx container with
    basic auth protecting the given services. Each service gets a [server {}]
    block proxying [listen_port] to [proxy_target].

    The htpasswd file is generated with permissions readable by the
    nginx worker process (0o644). *)
val run :
  username:string -> password:string -> services:service list -> unit -> t Lwt.t

(** [add_service t service] adds a new proxy entry to the nginx configuration
    and reloads nginx. Used to add Netdata agents dynamically. *)
val add_service : t -> service -> unit Lwt.t

(** [shutdown t] stops the nginx container. *)
val shutdown : t -> unit Lwt.t
