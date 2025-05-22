(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  cctxt : Client_context.wallet;
  smart_rollup_address : string;
  sequencer_key : Client_keys.sk_uri;
  maximum_number_of_chunks : int;
  chain_family : L2_types.chain_family;
  tx_container : (module Services_backend_sig.Tx_container);
}

(** [start parameters] starts the events follower. *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the events follower. *)
val shutdown : unit -> unit Lwt.t

(** [produce_block ~force ~timestamp] takes the transactions in the tx
    pool and produces a block from it, returns the number of
    transaction in the block. The block is not produced if the list of
    transactions is empty and [force] is set to [false].*)
val produce_block :
  force:bool ->
  timestamp:Time.Protocol.t ->
  [`Block_produced of int | `No_block] tzresult Lwt.t

module Internal_for_tests : sig
  val produce_block :
    with_delayed_transactions:bool ->
    force:bool ->
    timestamp:Time.Protocol.t ->
    [`Block_produced of int | `No_block] tzresult Lwt.t
end
