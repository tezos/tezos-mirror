(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
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

let already_slashed_for_double_endorsing ctxt delegate (level : Level_repr.t) =
  Storage.Slashed_deposits.find (ctxt, level.cycle) (level.level, delegate)
  >>=? function
  | None -> return_false
  | Some slashed -> return slashed.for_double_endorsing

let already_slashed_for_double_baking ctxt delegate (level : Level_repr.t) =
  Storage.Slashed_deposits.find (ctxt, level.cycle) (level.level, delegate)
  >>=? function
  | None -> return_false
  | Some slashed -> return slashed.for_double_baking

let punish_double_endorsing ctxt delegate (level : Level_repr.t) =
  let open Lwt_result_syntax in
  let* slashed =
    Storage.Slashed_deposits.find (ctxt, level.cycle) (level.level, delegate)
  in
  let updated_slashed =
    match slashed with
    | None -> {Storage.for_double_endorsing = true; for_double_baking = false}
    | Some slashed ->
        assert (Compare.Bool.(slashed.for_double_endorsing = false)) ;
        {slashed with for_double_endorsing = true}
  in
  let delegate_contract = Contract_repr.Implicit delegate in
  let* frozen_deposits = Frozen_deposits_storage.get ctxt delegate_contract in
  let slashing_ratio : Ratio_repr.t =
    Constants_storage.ratio_of_frozen_deposits_slashed_per_double_endorsement
      ctxt
  in
  let punish_value =
    Tez_repr.(
      div_exn
        (mul_exn frozen_deposits.initial_amount slashing_ratio.numerator)
        slashing_ratio.denominator)
  in
  let punishing_amount =
    Tez_repr.(min frozen_deposits.current_amount punish_value)
  in
  let reward, amount_to_burn = Tez_repr.div2_sub punishing_amount in
  let* ctxt, balance_updates =
    Token.transfer
      ctxt
      (`Frozen_deposits delegate)
      `Double_signing_punishments
      amount_to_burn
  in
  let*! ctxt =
    Storage.Slashed_deposits.add
      (ctxt, level.cycle)
      (level.level, delegate)
      updated_slashed
  in
  return (ctxt, reward, balance_updates)

let punish_double_baking ctxt delegate (level : Level_repr.t) =
  let open Lwt_result_syntax in
  let* slashed =
    Storage.Slashed_deposits.find (ctxt, level.cycle) (level.level, delegate)
  in
  let updated_slashed =
    match slashed with
    | None -> {Storage.for_double_baking = true; for_double_endorsing = false}
    | Some slashed ->
        assert (Compare.Bool.(slashed.for_double_baking = false)) ;
        {slashed with for_double_baking = true}
  in
  let delegate_contract = Contract_repr.Implicit delegate in
  let* frozen_deposits = Frozen_deposits_storage.get ctxt delegate_contract in
  let slashing_for_one_block =
    Constants_storage.double_baking_punishment ctxt
  in
  let punishing_amount =
    Tez_repr.(min frozen_deposits.current_amount slashing_for_one_block)
  in
  let reward, amount_to_burn = Tez_repr.div2_sub punishing_amount in
  let* ctxt, balance_updates =
    Token.transfer
      ctxt
      (`Frozen_deposits delegate)
      `Double_signing_punishments
      amount_to_burn
  in
  let*! ctxt =
    Storage.Slashed_deposits.add
      (ctxt, level.cycle)
      (level.level, delegate)
      updated_slashed
  in
  return (ctxt, reward, balance_updates)

let clear_outdated_slashed_deposits ctxt ~new_cycle =
  let max_slashable_period = Constants_storage.max_slashing_period ctxt in
  match Cycle_repr.(sub new_cycle max_slashable_period) with
  | None -> Lwt.return ctxt
  | Some outdated_cycle -> Storage.Slashed_deposits.clear (ctxt, outdated_cycle)
