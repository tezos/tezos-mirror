(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Follow the OpenTelemetry specs for HTTP client,
    see https://opentelemetry.io/docs/specs/semconv/http/http-spans *)

(** [call_service media_types ?logger ?headers ~base service params query body]
    makes an HTTP request to the specified RPC service with OpenTelemetry
    tracing.

    This function follows the OpenTelemetry semantic conventions for HTTP client
    spans, automatically setting appropriate attributes like HTTP method, URL,
    status code, etc. It also adds the header [traceparent] to allow traces
    across different services.

    @param media_types List of accepted media types for the request
    @param logger Optional logger for RPC client operations
    @param headers Optional additional HTTP headers to include in the request
    @param base The base URI to which the request will be sent
    @param service The RPC service definition
    @param params The path parameters for the service
    @param query The query parameters for the service
    @param body The request body
    @return The result of the RPC call, wrapped in the Tezos error monad
*)
val call_service :
  Tezos_rpc_http.Media_type.t trace ->
  ?logger:Tezos_rpc_http_client_unix.RPC_client_unix.logger ->
  ?headers:(string * string) trace ->
  base:Uri.t ->
  ([< Resto.meth], unit, 'a, 'b, 'c, 'd) Tezos_rpc.Service.t ->
  'a ->
  'b ->
  'c ->
  'd tzresult Lwt.t

(** HTTP client span attributes that follow
    {{:https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client-span}Opentelemetry
    semantic conventions} *)
val span_attributes :
  Tezos_rpc_http.Media_type.t list ->
  [< Resto.meth] ->
  ?route:string ->
  Uri.t ->
  Opentelemetry.key_value list
