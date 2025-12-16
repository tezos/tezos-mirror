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

(** [produce_block ~force ~timestamp] takes the transactions in the tx
    pool and produces a block from it, returns the number of
    transaction in the block. The block is not produced if the list of
    transactions is empty and [force] is set to [false].*)
val produce_block :
  force:bool ->
  timestamp:Time.Protocol.t ->
  [`Block_produced of int | `No_block] tzresult Lwt.t

(** [preconfirm_transactions ~transactions] validates each transaction in
    [transactions] and streams every successfully validated one to the
    preconfirmation pipeline. *)
val preconfirm_transactions :
  transactions:(string * Tx_queue_types.transaction_object_t) list ->
  Ethereum_types.hash list tzresult Lwt.t

module Internal_for_tests : sig
  val produce_block :
    with_delayed_transactions:bool ->
    force:bool ->
    timestamp:Time.Protocol.t ->
    [`Block_produced of int | `No_block] tzresult Lwt.t
end
