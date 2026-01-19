(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [add_redemption_request ctxt staker cycle amount] registers an redemption
    request of [amount] tez from [staker] at cycle [cycle]. *)
val add_redemption_request :
  Raw_context.t ->
  Contract_repr.t ->
  Cycle_repr.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [finalize ctxt ~clst_contract ~staker] handles the finalization of [staker]'s
    redemption requests, moving the funds in [clst_contract] spendable balance,
    and returns the number of tez finalized. It is then the responsibility of
    the CLST contract to transfer them to [staker] via an internal operation.

    Redemption requests still under the slashing period remain untouched.
*)
val finalize :
  Raw_context.t ->
  clst_contract:Contract_repr.t ->
  staker:Contract_repr.t ->
  ( Raw_context.t * Receipt_repr.balance_update_item list * Tez_repr.t,
    error trace )
  result
  Lwt.t
