(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let get ctxt delegate =
  let open Lwt_result_syntax in
  let+ frozen_deposits_opt =
    Storage.Contract.Frozen_deposits.find ctxt delegate
  in
  Option.value ~default:Deposits_repr.zero frozen_deposits_opt

let update_balance ctxt delegate f amount =
  let open Lwt_result_syntax in
  let delegate_contract = Contract_repr.Implicit delegate in
  let* frozen_deposits = get ctxt delegate_contract in
  let*? new_amount = f frozen_deposits.current_amount amount in
  let*! ctxt =
    Storage.Contract.Frozen_deposits.add
      ctxt
      delegate_contract
      {frozen_deposits with current_amount = new_amount}
  in
  return ctxt

let credit_only_call_from_token ctxt delegate amount =
  let open Lwt_result_syntax in
  let* ctxt = update_balance ctxt delegate Tez_repr.( +? ) amount in
  Stake_storage.add_stake ctxt delegate amount

let spend_only_call_from_token ctxt delegate amount =
  let open Lwt_result_syntax in
  let* ctxt = update_balance ctxt delegate Tez_repr.( -? ) amount in
  Stake_storage.remove_stake ctxt delegate amount

let update_initial_amount ctxt delegate_contract deposits_cap =
  let open Lwt_result_syntax in
  let* frozen_deposits = get ctxt delegate_contract in
  let*! ctxt =
    Storage.Contract.Frozen_deposits.add
      ctxt
      delegate_contract
      {frozen_deposits with initial_amount = deposits_cap}
  in
  return ctxt
