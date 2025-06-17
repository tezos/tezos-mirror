(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Follow the OpenTelemetry specs for HTTP client,
    see https://opentelemetry.io/docs/specs/semconv/http/http-spans *)

val trace_call :
  ?media_types:Tezos_rpc_http.Media_type.t list ->
  Uri.t ->
  ([< Resto.meth], unit, 'a, 'b, 'c, 'd) Tezos_rpc.Service.t ->
  'a ->
  'b ->
  (Opentelemetry.Scope.t -> 'r tzresult Lwt.t) ->
  'r tzresult Lwt.t
