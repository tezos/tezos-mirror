(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let increase_deposit_only_call_from_token ctxt amount =
  let open Lwt_result_syntax in
  let* deposited_balance_before = Storage.Clst.Deposits_balance.get ctxt in
  let*? new_deposited_balance = Tez_repr.(deposited_balance_before +? amount) in
  Storage.Clst.Deposits_balance.update ctxt new_deposited_balance

let decrease_deposit_only_call_from_token ctxt amount =
  let open Lwt_result_syntax in
  let* deposited_balance_before = Storage.Clst.Deposits_balance.get ctxt in
  let*? new_deposited_balance = Tez_repr.(deposited_balance_before -? amount) in
  Storage.Clst.Deposits_balance.update ctxt new_deposited_balance

(* See {Unstaked_frozen_deposits_storage.get_all} *)
let get_all_redeemed_frozen_deposits ctxt =
  let open Lwt_result_syntax in
  let* unstaked_frozen_deposits_opt =
    Storage.Clst.Redeemed_frozen_deposits.find ctxt
  in
  let unslashable_cycle =
    Cycle_storage.greatest_unstake_finalizable_cycle ctxt
  in
  match unstaked_frozen_deposits_opt with
  | None -> return (Unstaked_frozen_deposits_repr.empty ~unslashable_cycle)
  | Some unstaked_frozen_deposits ->
      Lwt.return
      @@ Unstaked_frozen_deposits_repr.squash_unslashable
           ~unslashable_cycle
           unstaked_frozen_deposits

(* See {Unstaked_frozen_deposits_storage.update_balance} *)
let update_frozen_redeemed_deposit ~f ctxt cycle =
  let open Lwt_result_syntax in
  let* redeemed_frozen_deposits = get_all_redeemed_frozen_deposits ctxt in
  let*? redeemed_frozen_deposits =
    Unstaked_frozen_deposits_repr.update ~f cycle redeemed_frozen_deposits
  in
  let*! ctxt =
    Storage.Clst.Redeemed_frozen_deposits.add ctxt redeemed_frozen_deposits.t
  in
  return ctxt

let increase_redeemed_frozen_deposit_only_call_from_token ctxt cycle amount =
  let f current = Deposits_repr.(current +? amount) in
  update_frozen_redeemed_deposit ~f ctxt cycle

let decrease_redeemed_frozen_deposit_only_call_from_token ctxt cycle amount =
  let f Deposits_repr.{initial_amount; current_amount} =
    let open Result_syntax in
    let+ current_amount = Tez_repr.(current_amount -? amount) in
    Deposits_repr.{initial_amount; current_amount}
  in
  update_frozen_redeemed_deposit ~f ctxt cycle
