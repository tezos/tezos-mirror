(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** {1 Helper functions to build {!Dream} routes from {!Resto} services.} *)

(** [make_route service handler] builds a route from a handler that
    returns an output. *)
val make_route :
  ([< Resto.meth], unit, 'params, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  (params:'params -> query:'query -> 'input -> 'output Lwt.t) ->
  Dream.route

(** [make_tz_route service handler] builds a route from a handler that
    returns an output or an error. *)
val make_tz_route :
  ([< Resto.meth], unit, 'params, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  (params:'params -> query:'query -> 'input -> 'output tzresult Lwt.t) ->
  Dream.route

(** [make_opt_tz_route service handler] builds a route from a handler that
    returns an optional output or an error. If [handler] returns [None] the
    server answers with a 404 Not_Found response. *)
val make_opt_tz_route :
  ([< Resto.meth], unit, 'params, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  (params:'params -> query:'query -> 'input -> 'output option tzresult Lwt.t) ->
  Dream.route

(** [make_stream_route service handler] builds a route which streams the
    response from a handler that constructs an {!Lwt_stream.t}. The output
    stream is streamed as chunks in the response body.. *)
val make_stream_route :
  ([< Resto.meth], unit, 'params, 'query, 'input, 'output) Tezos_rpc.Service.t ->
  (params:'params ->
  query:'query ->
  'input ->
  ('output Lwt_stream.t * (unit -> unit)) Lwt.t) ->
  Dream.route
