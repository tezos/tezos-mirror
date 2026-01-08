(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  signer : Signer.map;
  maximum_number_of_chunks : int;
  tx_container : Services_backend_sig.ex_tx_container;
  sequencer_sunset_sec : int64;
  preconfirmation_stream_enabled : bool;
}

(** [force] defines if the block producer should force the creation of
    a block even if there is no txs to be included.*)
type force =
  | True  (** Force the creation of a block with the computed timestamp *)
  | False  (** Create a block iff there is txs to be included *)
  | With_timestamp of Time.Protocol.t
      (** Force the creation of a block with the provided timestamp *)

type preconfirmed_transactions_result = {
  accepted : Ethereum_types.hash list;
      (** Transactions that were validated by the
          preconfirm_transactions request. *)
  refused : Ethereum_types.hash list;
      (** Transactions that were invalidated by the preconfirm_transactions request. *)
  dropped : Ethereum_types.hash list;
      (** Transactions that were dropped by the
          preconfirm_transactions request when there is already too
          many transaction. *)
}

(** [start parameters] starts the events follower. *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the events follower. *)
val shutdown : unit -> unit Lwt.t

(** [produce_genesis ~timestamp ~parent_hash] creates the first empty block with
    the provided timestamp and parent hash *)
val produce_genesis :
  timestamp:Time.Protocol.t ->
  parent_hash:Ethereum_types.block_hash ->
  (unit, error trace) result Lwt.t

(** [produce_block ~force] takes the transactions in the tx pool and
    produces a block from it, returns the number of transaction in the
    block. The block is not produced if the list of transactions is
    empty and [force] is set to [False].*)
val produce_block :
  force:force -> [`Block_produced of int | `No_block] tzresult Lwt.t

type error += IC_disabled

(** [preconfirm_transactions ~transactions] validates each transaction
    in [transactions] and streams every successfully validated one to
    the preconfirmation pipeline. Fails with [IC_disabled] if instant
    confirmation is not enabled. *)
val preconfirm_transactions :
  transactions:(string * Tx_queue_types.transaction_object_t) list ->
  preconfirmed_transactions_result tzresult Lwt.t

module Internal_for_tests : sig
  val produce_block :
    with_delayed_transactions:bool ->
    force:force ->
    [`Block_produced of int | `No_block] tzresult Lwt.t

  val propose_next_block_timestamp :
    next_block_timestamp:Time.System.t -> unit tzresult Lwt.t
end
