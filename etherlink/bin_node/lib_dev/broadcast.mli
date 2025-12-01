(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [create_blueprint_stream ()] returns a new stream that can be used to be
    notified when a new blueprint is being successfully applied on top of the
    local state. *)
val create_blueprint_stream :
  unit -> Blueprint_types.Legacy.with_events Lwt_stream.t * Lwt_watcher.stopper

type common_transaction = Evm of string | Michelson of string

(** Preconfirmed transaction can either be internal or coming from the delayed inbox *)
type transaction =
  | Common of common_transaction
  | Delayed of Evm_events.Delayed_transaction.t

val transaction_encoding : transaction Data_encoding.t

(** Type of messages that are broadcasted to all evm nodes. *)
type message =
  | Blueprint of Blueprint_types.with_events
  | Finalized_levels of {
      l1_level : int32;
      start_l2_level : Ethereum_types.quantity;
      end_l2_level : Ethereum_types.quantity;
    }
  | Next_block_info of {
      timestamp : Time.Protocol.t;
      number : Ethereum_types.quantity;
    }
  | Included_transaction of {tx : transaction; hash : Ethereum_types.hash}

val message_encoding : message Data_encoding.t

(** [create_broadcast_stream ()] returns a new stream that can be used to be
    notified of all messages, including a new blueprint being successfully
    applied on top of the local state. *)
val create_broadcast_stream : unit -> message Lwt_stream.t * Lwt_watcher.stopper

(** [notify_blueprint blueprint] advertizes [blueprint] to both the blueprint
    stream and the broadcast stream. *)
val notify_blueprint : Blueprint_types.with_events -> unit

(** [notify_finalized_levels ~l1_level ~start_l2_level ~end_l2_level]
    advertizes the L1/L2 finalized levels to the broadcast stream. *)
val notify_finalized_levels :
  l1_level:int32 ->
  start_l2_level:Ethereum_types.quantity ->
  end_l2_level:Ethereum_types.quantity ->
  unit

(** [notify_next_block_info timestamp number] advertizes the next block [timestamp] and [number]
    to the broadcast stream *)
val notify_next_block_info : Time.Protocol.t -> Ethereum_types.quantity -> unit

(** [notify_inclusion tx] advertizes [tx] as the latest transaction to be included in the next block
    to the broadcast stream *)
val notify_inclusion : transaction -> Ethereum_types.hash -> unit

(** [create_receipt_stream ()] returns a new stream that can be used to be
    notified of pre-confirmed receipts after transactions are executed individualy. *)
val create_receipt_stream :
  unit -> Transaction_receipt.t Lwt_stream.t * Lwt_watcher.stopper

(** [notify_inclusion tx] advertizes [receipt] as the latest pre-confirmed receipt
    _only_ to the receipt stream *)
val notify_preconfirmed_receipt : Transaction_receipt.t -> unit
