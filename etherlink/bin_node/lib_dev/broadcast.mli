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
  | Dropped_transaction of {hash : Ethereum_types.hash; reason : string}

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

(** [notify_dropped ~hash ~reason] advertizes a dropped transaction with its [hash] and [reason]
    to the broadcast stream *)
val notify_dropped : hash:Ethereum_types.hash -> reason:string -> unit

(** Type representing the result of a transaction pre-confirmed execution. *)
type transaction_result = {
  hash : Ethereum_types.hash;
  (* If the transaction was successfully executed, [result] contains the corresponding receipt.
     Otherwise, it contains an error message explaining why the execution failed. *)
  result : (Transaction_receipt.t, string) result;
}

(** [create_transaction_result_stream ()] returns a new stream that can be used to be
    notified of pre-confirmed results after transactions are executed individually. *)
val create_transaction_result_stream :
  unit -> transaction_result Lwt_stream.t * Lwt_watcher.stopper

(** [notify_transaction_result tx] advertizes [tx] as the latest pre-confirmed result
    _only_ to the transaction result stream *)
val notify_transaction_result : transaction_result -> unit
