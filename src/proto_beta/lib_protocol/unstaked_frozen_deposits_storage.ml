(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let current_unslashable_cycle ctxt =
  let cycle = (Raw_context.current_level ctxt).cycle in
  let slashable_deposits_period =
    Constants_storage.slashable_deposits_period ctxt
  in
  let max_slashing_period = Constants_repr.max_slashing_period in
  Cycle_repr.sub cycle (slashable_deposits_period + max_slashing_period)

let get_all ctxt contract =
  let open Lwt_result_syntax in
  let* unstaked_frozen_deposits_opt =
    Storage.Contract.Unstaked_frozen_deposits.find ctxt contract
  in
  let unslashable_cycle = current_unslashable_cycle ctxt in
  match unstaked_frozen_deposits_opt with
  | None -> return (Unstaked_frozen_deposits_repr.empty ~unslashable_cycle)
  | Some unstaked_frozen_deposits ->
      Lwt.return
      @@ Unstaked_frozen_deposits_repr.squash_unslashable
           ~unslashable_cycle
           unstaked_frozen_deposits

let get ctxt delegate cycle =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit delegate in
  let+ unstaked_frozen_deposits = get_all ctxt contract in
  Unstaked_frozen_deposits_repr.get cycle unstaked_frozen_deposits

let balance ctxt delegate cycle =
  let open Lwt_result_syntax in
  let+ frozen_deposits = get ctxt delegate cycle in
  frozen_deposits.current_amount

let update_balance ~f ctxt delegate_contract cycle =
  let open Lwt_result_syntax in
  let* unstaked_frozen_deposits = get_all ctxt delegate_contract in
  let*? unstaked_frozen_deposits =
    Unstaked_frozen_deposits_repr.update ~f cycle unstaked_frozen_deposits
  in
  let*! ctxt =
    Storage.Contract.Unstaked_frozen_deposits.add
      ctxt
      delegate_contract
      unstaked_frozen_deposits.t
  in
  return ctxt

let credit_only_call_from_token ctxt staker cycle amount =
  let open Lwt_result_syntax in
  let delegate = Unstaked_frozen_staker_repr.delegate staker in
  let delegate_contract = Contract_repr.Implicit delegate in
  let f deposits = Deposits_repr.(deposits +? amount) in
  let* ctxt = Stake_storage.add_delegated_stake ctxt delegate amount in
  update_balance ~f ctxt delegate_contract cycle

let spend_only_call_from_token ctxt staker cycle amount =
  let open Lwt_result_syntax in
  let delegate = Unstaked_frozen_staker_repr.delegate staker in
  let delegate_contract = Contract_repr.Implicit delegate in
  let f Deposits_repr.{initial_amount; current_amount} =
    let open Result_syntax in
    let+ current_amount = Tez_repr.(current_amount -? amount) in
    Deposits_repr.{initial_amount; current_amount}
  in
  let* ctxt = Stake_storage.remove_delegated_stake ctxt delegate amount in
  update_balance ~f ctxt delegate_contract cycle

let decrease_initial_amount_only_for_stake_from_unstake ctxt delegate cycle
    amount =
  let delegate_contract = Contract_repr.Implicit delegate in
  let f Deposits_repr.{initial_amount; current_amount} =
    let open Result_syntax in
    let+ initial_amount = Tez_repr.(initial_amount -? amount) in
    Deposits_repr.{initial_amount; current_amount}
  in
  update_balance ~f ctxt delegate_contract cycle
