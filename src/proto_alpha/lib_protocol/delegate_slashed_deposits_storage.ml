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

type reward_and_burn = {reward : Tez_repr.t; amount_to_burn : Tez_repr.t}

type punishing_amounts = {
  staked : reward_and_burn;
  unstaked : (Cycle_repr.t * reward_and_burn) list;
}

(** [punish_double_signing ~get ~set ~get_percentage ctxt delegate level] record
    in the context that the given [delegate] has now been slashed for the
    double signing event for the given [level] and return the amounts of the
    frozen deposits to burn and to reward the denuncer.

    The double signing event corresponds to a field in {!Storage.slashed_level},
    retrieved with [get] and set to true with [set].

    The part to burn is retrieved with [get_percentage].
*)
let punish_double_signing ~get ~set ~get_percentage ctxt delegate
    (level : Level_repr.t) =
  let open Lwt_result_syntax in
  let* slashed_opt =
    Storage.Slashed_deposits.find (ctxt, level.cycle) (level.level, delegate)
  in
  let slashed =
    Option.value slashed_opt ~default:Storage.default_slashed_level
  in
  assert (Compare.Bool.(get slashed = false)) ;
  let updated_slashed = set slashed in
  let delegate_contract = Contract_repr.Implicit delegate in
  let slashing_percentage = get_percentage ctxt in
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  let staking_over_baking_global_limit =
    Constants_storage.adaptive_inflation_staking_over_baking_global_limit ctxt
  in
  let staking_over_baking_global_limit_plus_two =
    Int64.add (Int64.of_int staking_over_baking_global_limit) 2L
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
      Tez_repr.(punishing_amount /? staking_over_baking_global_limit_plus_two)
    in
    let+ amount_to_burn = Tez_repr.(punishing_amount -? reward) in
    (should_forbid, {reward; amount_to_burn})
  in
  let* frozen_deposits = Frozen_deposits_storage.get ctxt delegate_contract in
  let*? should_forbid, staked = compute_reward_and_burn frozen_deposits in
  let*! ctxt =
    if should_forbid then Delegate_storage.forbid_delegate ctxt delegate
    else Lwt.return ctxt
  in
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
  return (ctxt, {staked; unstaked})

let punish_double_endorsing =
  let get Storage.{for_double_endorsing; _} = for_double_endorsing in
  let set slashed = Storage.{slashed with for_double_endorsing = true} in
  let get_percentage =
    Constants_storage
    .percentage_of_frozen_deposits_slashed_per_double_endorsement
  in
  punish_double_signing ~get ~set ~get_percentage

let punish_double_baking =
  let get Storage.{for_double_baking; _} = for_double_baking in
  let set slashed = Storage.{slashed with for_double_baking = true} in
  let get_percentage =
    Constants_storage.percentage_of_frozen_deposits_slashed_per_double_baking
  in
  punish_double_signing ~get ~set ~get_percentage

let clear_outdated_slashed_deposits ctxt ~new_cycle =
  let max_slashable_period = Constants_storage.max_slashing_period ctxt in
  match Cycle_repr.(sub new_cycle max_slashable_period) with
  | None -> Lwt.return ctxt
  | Some outdated_cycle -> Storage.Slashed_deposits.clear (ctxt, outdated_cycle)
