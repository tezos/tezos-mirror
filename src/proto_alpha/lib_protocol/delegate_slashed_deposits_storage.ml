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

let already_slashed_for_double_attesting ctxt delegate (level : Level_repr.t) =
  let open Lwt_result_syntax in
  let* slashed_opt =
    Storage.Slashed_deposits.find (ctxt, level.cycle) (level.level, delegate)
  in
  match slashed_opt with
  | None -> return_false
  | Some slashed -> return slashed.for_double_attesting

let already_slashed_for_double_baking ctxt delegate (level : Level_repr.t) =
  let open Lwt_result_syntax in
  let* slashed_opt =
    Storage.Slashed_deposits.find (ctxt, level.cycle) (level.level, delegate)
  in
  match slashed_opt with
  | None -> return_false
  | Some slashed -> return slashed.for_double_baking

type reward_and_burn = {reward : Tez_repr.t; amount_to_burn : Tez_repr.t}

type punishing_amounts = {
  staked : reward_and_burn;
  unstaked : (Cycle_repr.t * reward_and_burn) list;
}

(** [punish_double_signing ctxt misbehaviour delegate level] record
    in the context that the given [delegate] has now been slashed for the
    double signing event [misbehaviour] for the given [level] and return the amounts of the
    frozen deposits to burn and to reward the denuncer.

    The double signing event corresponds to a field in {!Storage.slashed_level}.
*)
let punish_double_signing ctxt (misbehaviour : Misbehaviour.t) delegate
    (level : Level_repr.t) =
  let open Lwt_result_syntax in
  let* slashed_opt =
    Storage.Slashed_deposits.find (ctxt, level.cycle) (level.level, delegate)
  in
  let slashed =
    Option.value slashed_opt ~default:Storage.default_slashed_level
  in
  let already_slashed, updated_slashed, slashing_percentage =
    let Storage.{for_double_baking; for_double_attesting} = slashed in
    match misbehaviour with
    | Double_baking ->
        ( for_double_baking,
          {slashed with for_double_baking = true},
          Constants_storage
          .percentage_of_frozen_deposits_slashed_per_double_baking
            ctxt )
    | Double_attesting ->
        ( for_double_attesting,
          {slashed with for_double_attesting = true},
          Constants_storage
          .percentage_of_frozen_deposits_slashed_per_double_attestation
            ctxt )
  in
  assert (Compare.Bool.(already_slashed = false)) ;
  let delegate_contract = Contract_repr.Implicit delegate in
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  let global_limit_of_staking_over_baking =
    Constants_storage.adaptive_issuance_global_limit_of_staking_over_baking ctxt
  in
  let global_limit_of_staking_over_baking_plus_two =
    Int64.add (Int64.of_int global_limit_of_staking_over_baking) 2L
  in
  let compute_reward_and_burn (frozen_deposits : Deposits_repr.t) =
    let open Result_syntax in
    let punish_value =
      Tez_repr.(
        div_exn (mul_exn frozen_deposits.initial_amount slashing_percentage) 100)
    in
    let should_forbid, punishing_amount =
      if Tez_repr.(punish_value >= frozen_deposits.current_amount) then
        (true, frozen_deposits.current_amount)
      else (false, punish_value)
    in
    let* reward =
      Tez_repr.(
        punishing_amount /? global_limit_of_staking_over_baking_plus_two)
    in
    let+ amount_to_burn = Tez_repr.(punishing_amount -? reward) in
    (should_forbid, {reward; amount_to_burn})
  in
  let* frozen_deposits = Frozen_deposits_storage.get ctxt delegate_contract in
  let*? should_forbid, staked = compute_reward_and_burn frozen_deposits in
  let* unstaked =
    let oldest_slashable_cycle =
      Cycle_repr.sub level.cycle preserved_cycles
      |> Option.value ~default:Cycle_repr.root
    in
    let slashable_cycles =
      Cycle_repr.(oldest_slashable_cycle ---> level.cycle)
    in
    List.rev_map_es
      (fun cycle ->
        let* frozen_deposits =
          Unstaked_frozen_deposits_storage.get ctxt delegate cycle
        in
        let*? _should_forbid, reward_and_burn =
          compute_reward_and_burn frozen_deposits
        in
        return (cycle, reward_and_burn))
      slashable_cycles
  in
  let*! ctxt =
    Storage.Slashed_deposits.add
      (ctxt, level.cycle)
      (level.level, delegate)
      updated_slashed
  in
  let* slash_history_opt =
    Storage.Contract.Slashed_deposits.find ctxt delegate_contract
  in
  let slash_history = Option.value slash_history_opt ~default:[] in
  let slash_history =
    Storage.Slashed_deposits_history.add
      level.cycle
      slashing_percentage
      slash_history
  in
  let*! ctxt =
    Storage.Contract.Slashed_deposits.add ctxt delegate_contract slash_history
  in
  let should_forbid_from_history =
    let current_cycle = (Raw_context.current_level ctxt).cycle in
    let slashed_this_cycle =
      Storage.Slashed_deposits_history.get current_cycle slash_history
    in
    let slashed_previous_cycle =
      match Cycle_repr.pred current_cycle with
      | Some previous_cycle ->
          Storage.Slashed_deposits_history.get previous_cycle slash_history
      | None -> 0
    in
    Compare.Int.(slashed_this_cycle + slashed_previous_cycle >= 100)
  in
  let*! ctxt =
    if should_forbid || should_forbid_from_history then
      Delegate_storage.forbid_delegate ctxt delegate
    else Lwt.return ctxt
  in
  return (ctxt, {staked; unstaked})

let clear_outdated_slashed_deposits ctxt ~new_cycle =
  let max_slashable_period = Constants_storage.max_slashing_period ctxt in
  match Cycle_repr.(sub new_cycle max_slashable_period) with
  | None -> Lwt.return ctxt
  | Some outdated_cycle -> Storage.Slashed_deposits.clear (ctxt, outdated_cycle)
