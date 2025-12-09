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
