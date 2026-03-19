(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [finalize ctxt ~clst_contract ~staker redemption_requests] handles the
    finalization of [staker]'s [redemption_requests], moving the funds in
    [clst_contract] spendable balance, and returns the number of tez finalized
    and the remaining requests that are still slashable. It is then the
    responsibility of the CLST contract to transfer them to [staker] via an
    internal operation.
*)
val finalize :
  Raw_context.t ->
  clst_contract:Contract_repr.t ->
  staker:Contract_repr.t ->
  Storage.Unstake_request.requests ->
  ( Raw_context.t
    * Receipt_repr.balance_update_item list
    * Tez_repr.t
    * Storage.Unstake_request.requests,
    error trace )
  result
  Lwt.t

module For_RPC : sig
  type nonrec prepared_finalize_redemption = {
    finalizable : Storage.Unstake_request.requests;
    unfinalizable : Storage.Unstake_request.requests;
  }

  (** [split_redemption_requests ctxt redemption_requests] splits
      [redemption_requests] in finalizable and unfinalizable, according to the
      current cycle and {!Cycle_storage.greatest_unstake_finalization_cycle}. *)
  val split_redemption_requests :
    Raw_context.t ->
    (Cycle_repr.t * Tez_repr.t) list ->
    (prepared_finalize_redemption option, 'a) result Lwt.t
end
