(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Client to communicate with an EVM node over websockets *)

open Rpc_encodings

(** Type of client connected to a websocket server. *)
type t

type error +=
  | No_response of JSONRPC.request
  | Request_failed of JSONRPC.request * JSONRPC.error
  | Cannot_destruct of
      Ethereum_types.Subscription.kind * Ethereum_types.Subscription.id * string

exception Connection_closed

(** Subscriptions returned by [subscribe]. *)
type 'a subscription = {
  stream : 'a Lwt_stream.t;  (** The stream of events. *)
  unsubscribe : unit -> bool tzresult Lwt.t;
      (** A function to unsubscribe from events notifications. *)
}

type timeout = {
  timeout : float;
  on_timeout : [`Retry of int | `Retry_forever | `Fail];
}

(** Wrapper type for calling a JSONRPC method with an input *)
type (_, _) call =
  | Call :
      (module METHOD with type input = 'input and type output = 'output)
      * 'input
      -> ('input, 'output) call

type monitoring = {ping_timeout : float; ping_interval : float}

(** [create ?monitoring ?keep_alive media uri] creates an EVM node websocket
    client to an EVM node websocket server on [uri], communication is either
    JSON or binary depending on [media]. If [monitoring] is provided, the
    connection is monitored with the given parameters. If [keep_alive] is [true]
    (the default), requests will be retried (and the connection reestablished)
    if the connection is dropped. *)
val create :
  ?monitoring:monitoring -> ?keep_alive:bool -> Media_type.t -> Uri.t -> t

(** A uniquely identifying id for a websocket client. *)
val client_id : t -> int

(** [connect client] establishes the websocket connection with [client]. NOTE:
    the connection is established automatically when sending a request. *)
val connect : t -> unit tzresult Lwt.t

(** Disconnect the websocket client by sending a close frame and closing the
    connection. *)
val disconnect : t -> unit Lwt.t

(** Send a raw JSON RPC request on the websocket. *)
val send_jsonrpc_request :
  t -> ?timeout:timeout -> JSONRPC.request -> JSONRPC.value Lwt.t

(** [send_jsonrpc client (Call ((module Method), input))] makes a JSONRPC
    request with the provided [Method] and [input] to the websocket [client]. It
    returns the corresponding response. *)
val send_jsonrpc :
  t -> ?timeout:timeout -> ('input, 'output) call -> 'output tzresult Lwt.t

(** [subscribe client kind] creates a subscription of [kind] with the
      websocket [client]. It returns a stream with the notifications and a
      function to unsubscribe. *)
val subscribe :
  t ->
  ?timeout:timeout ->
  Subscribe.input ->
  ( Transaction_object.t,
    Transaction_receipt.t )
  Ethereum_types.Subscription.output
  tzresult
  subscription
  tzresult
  Lwt.t

(** [subscribe_newHeads client] is like [subscribe] but specialized for
      newHeads events. *)
val subscribe_newHeads :
  ?timeout:timeout ->
  t ->
  Transaction_object.t Ethereum_types.block tzresult subscription tzresult Lwt.t

(** [subscribe_newHeadNumbers client] is like [subscribe_newHeads] but only
    parses numbers in blocks. *)
val subscribe_newHeadNumbers :
  ?timeout:timeout ->
  t ->
  Ethereum_types.quantity tzresult subscription tzresult Lwt.t

(** [subscribe_newPendingTransactions client] is like [subscribe] but
      specialized for newPendingTransactions events. *)
val subscribe_newPendingTransactions :
  ?timeout:timeout ->
  t ->
  Ethereum_types.hash tzresult subscription tzresult Lwt.t

(** [subscribe_syncing client] is like [subscribe] but specialized for syncing
      events. *)
val subscribe_syncing :
  ?timeout:timeout ->
  t ->
  Ethereum_types.Subscription.sync_output tzresult subscription tzresult Lwt.t

(** [subscribe_logs ?address ?topics client] is like [subscribe] but
      specialized for logs events filtered by [address] and/or [topics]. *)
val subscribe_logs :
  ?address:Ethereum_types.Filter.filter_address ->
  ?topics:Ethereum_types.Filter.topic option list ->
  ?timeout:timeout ->
  t ->
  Ethereum_types.transaction_log tzresult subscription tzresult Lwt.t

(** Subscribe to L1/L2 levels associations. *)
val subscribe_l1_l2_levels :
  ?start_l1_level:int32 ->
  ?timeout:timeout ->
  t ->
  Ethereum_types.Subscription.l1_l2_levels_output tzresult subscription tzresult
  Lwt.t

(** [subscribe_newPreconfirmedReceipts ?timeout client] is like [subscribe] but
    specialized for NewPreconfirmedReceipts events. *)
val subscribe_newPreconfirmedReceipts :
  ?timeout:timeout ->
  t ->
  Transaction_receipt.t tzresult subscription tzresult Lwt.t
