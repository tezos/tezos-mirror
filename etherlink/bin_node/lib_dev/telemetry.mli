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

  (** [trace_batch_with ~service_name ~batch_size k] traces the continuation
      [k] as the handler of a batch of size [batch_size]. *)
  val trace_batch_with :
    service_name:string ->
    batch_size:int ->
    (Opentelemetry.Scope.t -> 'a Lwt.t) ->
    'a Lwt.t

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
  val return_error : JSONRPC.error -> JSONRPC.return_value Lwt.t
end

(** OpenTelemetry Etherlink-specific semantics conventions *)
module Attributes : sig
  module Transaction : sig
    (** Hex-encoded hash of the transaction being processed *)
    val hash : Ethereum_types.hash -> Opentelemetry.key_value
  end

  (** Tags to add to a span or event handling a given block.  *)
  module Block : sig
    (** Integer representation of the block number being processed *)
    val number : Ethereum_types.quantity -> Opentelemetry.key_value

    (** Integer representation of the amount of gas units necessary to process
        the block *)
    val execution_gas : Z.t -> Opentelemetry.key_value

    (** Integer representation of the number of transactions included in the
        block being processed *)
    val transaction_count : int -> Opentelemetry.key_value
  end
end
