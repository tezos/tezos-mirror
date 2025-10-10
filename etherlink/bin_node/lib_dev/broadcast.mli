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

(** Type of messages that are broadcasted to all evm nodes. *)
type message =
  | Blueprint of Blueprint_types.with_events
  | Finalized_levels of {
      l1_level : int32;
      start_l2_level : Ethereum_types.quantity;
      end_l2_level : Ethereum_types.quantity;
    }

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

(** Represents a message related to preconfirmed transactions *)
type preconfirmation_message =
  | Block_timestamp of Time.Protocol.t
      (** Timestamp of the next created block *)
  | Preconfirmed_transaction of Transaction_object.t
      (** Indicates that the transaction
      has been validated and is ready for pre-execution *)

val preconfirmation_message_encoding : preconfirmation_message Data_encoding.t

(** [create_preconfirmation_stream ()] returns a stream that emits every preconfirmation
    message as soon as it is notified via [notify_new_block_timestamp] and [notify_preconfirmation].  
    This allows subscribers to monitor the sequence and timing of preconfirmed transactions. *)
val create_preconfirmation_stream :
  unit -> preconfirmation_message Lwt_stream.t * Lwt_watcher.stopper

val notify_new_block_timestamp : Time.Protocol.t -> unit

val notify_preconfirmation : Transaction_object.t -> unit
