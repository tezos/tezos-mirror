(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Cannot_perform_http_request
  | Connection_pool_internal_error of string

(** A pool of persistent HTTP connection targeting one endpoint in particular.
    See {!make}. *)
type t

(** [make ~n endpoint] creating a pool of at most [n] connections onto the server
    behind [endpoint]. In addition to establishing connections, [endpoint] is
    used to created the complete URI based on parts submitted in requests
    funnctions like {!get}.  *)
val make : ?ctx:Cohttp_lwt_unix.Net.ctx -> n:int -> Uri.t -> t

(** [clear t] closes all pre-established connections currently in [t]. *)
val clear : t -> unit Lwt.t

(** [warm t] pre-establishes as many connections with the endpoint as [t]’s
    capacity allows. *)
val warm : t -> unit Lwt.t

(** [get t path] perform a [GET path] request to the endpoint of [t], using one
    of the connections of the pool.

    If all pre-established connections are already used for concurrent
    requests, a new one is established, or the function is blocking until a
    connection becomes available if the pool has already reached full capacity.
    *)
val get :
  ?headers:Cohttp.Header.t ->
  ?query:(string * string trace) trace ->
  ?userinfo:string ->
  t ->
  string ->
  (Cohttp.Response.t * string, error trace) result Lwt.t

(** [post t path] perform a [POST path] request to the endpoint of [t], using
    one of the connections of the pool. See {!get} for caveats. *)
val post :
  ?headers:Cohttp.Header.t ->
  ?body:Cohttp_lwt.Body.t ->
  ?query:(string * string trace) trace ->
  ?userinfo:string ->
  t ->
  string ->
  (Cohttp.Response.t * string, error trace) result Lwt.t

(** [put t path] perform a [PUT path] request to the endpoint of [t], using
    one of the connections of the pool. See {!get} for caveats. *)
val put :
  ?headers:Cohttp.Header.t ->
  ?body:Cohttp_lwt.Body.t ->
  ?query:(string * string trace) trace ->
  ?userinfo:string ->
  t ->
  string ->
  (Cohttp.Response.t * string, error trace) result Lwt.t

(** [patch t path] perform a [PATCH path] request to the endpoint of [t], using
    one of the connections of the pool. See {!get} for caveats. *)
val patch :
  ?headers:Cohttp.Header.t ->
  ?body:Cohttp_lwt.Body.t ->
  ?query:(string * string trace) trace ->
  ?userinfo:string ->
  t ->
  string ->
  (Cohttp.Response.t * string, error trace) result Lwt.t

(** [delete t path] perform a [DELETE path] request to the endpoint of [t],
    using one of the connections of the pool. See {!get} for caveats. *)
val delete :
  ?headers:Cohttp.Header.t ->
  ?body:Cohttp_lwt.Body.t ->
  ?query:(string * string trace) trace ->
  ?userinfo:string ->
  t ->
  string ->
  (Cohttp.Response.t * string, error trace) result Lwt.t
