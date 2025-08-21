(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Module providing OpenTelemetry tracing capabilities for Resto-based RPC
    services. This allows for profiling of RPC requests and responses. *)

(** [resto_callback ~port server conn request body] is a wrapper around the
    standard Resto callback that adds OpenTelemetry tracing information to RPC
    calls.

    This function intercepts RPC requests, creates spans with appropriate
    attributes (method, path, status code), and measures the duration of request
    processing. It also parses the header [traceparent] when present to allow
    traces across different services.

    @param port The port number on which the RPC server is listening
    @param server The Resto RPC server instance
    @param conn The connection information from the HTTP server
    @param request The HTTP request being processed
    @param body The request body
    @return The HTTP response action to be taken by the server *)
val resto_callback :
  port:int ->
  Tezos_rpc_http_server.RPC_server.server ->
  Conduit_lwt_unix.flow * Cohttp.Connection.t ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  Cohttp_lwt_unix.Server.response_action Lwt.t
