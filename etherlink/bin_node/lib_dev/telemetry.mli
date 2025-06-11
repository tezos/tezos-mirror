(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Follow the OpenTelemetry specs for JSON RPC servers,
    see https://opentelemetry.io/docs/specs/semconv/rpc/json-rpc/ *)
module Jsonrpc : sig
  open Rpc_encodings

  (** [trace_dispatch_with ~service_name method_ id k] traces the continuation
      [k] as the handler of the JSON RPC method [method_] sent by a client a
      given [id]. *)
  val trace_dispatch_with :
    ?websocket:bool ->
    service_name:string ->
    string ->
    JSONRPC.id_repr option ->
    (Opentelemetry.Scope.t -> 'a Lwt.t) ->
    'a Lwt.t

  (** [return_error err] decorates the current scope with the attributes
      related to [err]. *)
  val return_error : JSONRPC.error -> ('a, JSONRPC.error) result Lwt.t
end
