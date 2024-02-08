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

let already_denounced_for_double_attesting ctxt delegate (level : Level_repr.t)
    round =
  let open Lwt_result_syntax in
  let* denounced_opt =
    Storage.Already_denounced.find
      (ctxt, level.cycle)
      ((level.level, round), delegate)
  in
  match denounced_opt with
  | None -> return_false
  | Some denounced -> return denounced.for_double_attesting

let already_denounced_for_double_baking ctxt delegate (level : Level_repr.t)
    round =
  let open Lwt_result_syntax in
  let* denounced_opt =
    Storage.Already_denounced.find
      (ctxt, level.cycle)
      ((level.level, round), delegate)
  in
  match denounced_opt with
  | None -> return_false
  | Some denounced -> return denounced.for_double_baking

type reward_and_burn = {reward : Tez_repr.t; amount_to_burn : Tez_repr.t}

type punishing_amounts = {
  staked : reward_and_burn;
  unstaked : (Cycle_repr.t * reward_and_burn) list;
}

let punish_double_signing ctxt ~operation_hash
    (misbehaviour : Misbehaviour_repr.t) delegate (level : Level_repr.t)
    ~rewarded =
  let open Lwt_result_syntax in
  let* denounced_opt =
    Storage.Already_denounced.find
      (ctxt, level.cycle)
      ((level.level, misbehaviour.round), delegate)
  in
  let denounced =
    Option.value denounced_opt ~default:Storage.default_denounced
  in
  let already_denounced, updated_denounced, slashing_percentage =
    let Storage.{for_double_baking; for_double_attesting} = denounced in
    match misbehaviour.kind with
    | Double_baking ->
        ( for_double_baking,
          {denounced with for_double_baking = true},
          Constants_storage
          .percentage_of_frozen_deposits_slashed_per_double_baking
            ctxt )
    | Double_attesting ->
        ( for_double_attesting,
          {denounced with for_double_attesting = true},
          Constants_storage
          .percentage_of_frozen_deposits_slashed_per_double_attestation
            ctxt )
  in
  assert (Compare.Bool.(already_denounced = false)) ;
  let delegate_contract = Contract_repr.Implicit delegate in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let*! ctxt =
    Storage.Already_denounced.add
      (ctxt, level.cycle)
      ((level.level, misbehaviour.round), delegate)
      updated_denounced
  in
  let* slash_history_opt =
    Storage.Contract.Slashed_deposits.find ctxt delegate_contract
  in
  let slash_history = Option.value slash_history_opt ~default:[] in
  let previously_slashed_this_cycle =
    Storage.Slashed_deposits_history.get current_cycle slash_history
  in
  let slash_history =
    Storage.Slashed_deposits_history.add
      level.cycle
      slashing_percentage
      slash_history
  in
  let*! ctxt =
    Storage.Contract.Slashed_deposits.add ctxt delegate_contract slash_history
  in
  let*! ctxt = Forbidden_delegates_storage.forbid ctxt delegate in
  let* ctxt =
    if Percentage.(Compare.(previously_slashed_this_cycle >= p100)) then
      (* Do not store denunciations that have no effects .*) return ctxt
    else
      Pending_denunciations_storage.add_denunciation
        ctxt
        ~misbehaving_delegate:delegate
        operation_hash
        ~rewarded_delegate:rewarded
        misbehaviour
  in
  return ctxt

let clear_outdated_already_denounced ctxt ~new_cycle =
  let max_slashable_period = Constants_repr.max_slashing_period in
  match Cycle_repr.(sub new_cycle max_slashable_period) with
  | None -> Lwt.return ctxt
  | Some outdated_cycle -> Storage.Already_denounced.clear (ctxt, outdated_cycle)

let apply_and_clear_denunciations ctxt =
  let open Lwt_result_syntax in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let previous_cycle =
    match Cycle_repr.pred current_cycle with
    | None -> current_cycle
    | Some previous_cycle -> previous_cycle
  in
  let slashable_deposits_period =
    Constants_storage.slashable_deposits_period ctxt
  in
  let global_limit_of_staking_over_baking_plus_two =
    let global_limit_of_staking_over_baking =
      Constants_storage.adaptive_issuance_global_limit_of_staking_over_baking
        ctxt
    in
    Int64.add (Int64.of_int global_limit_of_staking_over_baking) 2L
  in
  let compute_reward_and_burn slashing_percentage
      (frozen_deposits : Deposits_repr.t) =
    let open Result_syntax in
    let punish_value =
      Tez_repr.mul_percentage
        ~rounding:`Down
        frozen_deposits.initial_amount
        slashing_percentage
    in
    let punishing_amount =
      Tez_repr.min punish_value frozen_deposits.current_amount
    in
    let* reward =
      Tez_repr.(
        punishing_amount /? global_limit_of_staking_over_baking_plus_two)
    in
    let+ amount_to_burn = Tez_repr.(punishing_amount -? reward) in
    {reward; amount_to_burn}
  in
  let* ctxt, balance_updates, remaining_denunciations =
    Pending_denunciations_storage.fold
      ctxt
      ~order:`Undefined
      ~init:(Ok (ctxt, [], []))
      ~f:(fun delegate denunciations acc ->
        let*? ctxt, balance_updates, remaining_denunciations = acc in
        (* Since the [max_slashing_period] is 2, and we want to apply denunciations at the
           end of this period, we "delay" the current cycle's misbehaviour's denunciations,
           while we apply the older denunciations.
           Indeed, we apply denunciations in the cycle following the misbehaviour, so that
           the time between the misbehaviour and the slashing is at most
           [max_slashing_period = 2] cycles. *)
        let denunciations_to_apply, denunciations_to_delay =
          if not (Constants_storage.adaptive_issuance_ns_enable ctxt) then
            (denunciations, [])
          else
            List.partition
              (fun denunciation ->
                let level =
                  denunciation.Denunciations_repr.misbehaviour.level
                in
                let misb_cycle =
                  (Level_repr.level_from_raw
                     ~cycle_eras:(Raw_context.cycle_eras ctxt)
                     level)
                    .cycle
                in
                Cycle_repr.(misb_cycle < current_cycle))
              denunciations
        in
        let+ ctxt, balance_updates =
          List.fold_left_es
            (fun (ctxt, balance_updates)
                 Denunciations_repr.{operation_hash; rewarded; misbehaviour} ->
              let slashing_percentage =
                match misbehaviour.kind with
                | Double_baking ->
                    Constants_storage
                    .percentage_of_frozen_deposits_slashed_per_double_baking
                      ctxt
                | Double_attesting ->
                    Constants_storage
                    .percentage_of_frozen_deposits_slashed_per_double_attestation
                      ctxt
              in
              let misbehaviour_cycle =
                (Level_repr.level_from_raw
                   ~cycle_eras:(Raw_context.cycle_eras ctxt)
                   misbehaviour.level)
                  .cycle
              in
              let get_initial_frozen_deposits_of_misbehaviour_cycle =
                if Cycle_repr.equal current_cycle misbehaviour_cycle then
                  Delegate_storage.initial_frozen_deposits
                else if Cycle_repr.equal previous_cycle misbehaviour_cycle then
                  Delegate_storage.initial_frozen_deposits_of_previous_cycle
                else fun (_ : Raw_context.t) (_ : Signature.public_key_hash) ->
                  return Tez_repr.zero
                (* (denunciation applied too late)
                   We could assert false, but we can also be permissive
                   while keeping the same invariants *)
              in
              let* frozen_deposits =
                let* initial_amount =
                  get_initial_frozen_deposits_of_misbehaviour_cycle
                    ctxt
                    delegate
                in
                let* current_amount =
                  Delegate_storage.current_frozen_deposits ctxt delegate
                in
                return Deposits_repr.{initial_amount; current_amount}
              in
              let*? staked =
                compute_reward_and_burn slashing_percentage frozen_deposits
              in
              let* init_to_burn_to_reward =
                let giver_baker =
                  `Frozen_deposits (Frozen_staker_repr.baker delegate)
                in
                let giver_stakers =
                  `Frozen_deposits
                    (Frozen_staker_repr.shared_between_stakers ~delegate)
                in
                let {amount_to_burn; reward} = staked in
                let* to_burn =
                  let+ {baker_part; stakers_part} =
                    Shared_stake.share
                      ~rounding:`Towards_baker
                      ctxt
                      delegate
                      amount_to_burn
                  in
                  [(giver_baker, baker_part); (giver_stakers, stakers_part)]
                in
                let* to_reward =
                  let+ {baker_part; stakers_part} =
                    Shared_stake.share
                      ~rounding:`Towards_baker
                      ctxt
                      delegate
                      reward
                  in
                  [(giver_baker, baker_part); (giver_stakers, stakers_part)]
                in
                return (to_burn, to_reward)
              in
              let* to_burn, to_reward =
                let oldest_slashable_cycle =
                  Cycle_repr.sub misbehaviour_cycle slashable_deposits_period
                  |> Option.value ~default:Cycle_repr.root
                in
                let slashable_cycles =
                  Cycle_repr.(oldest_slashable_cycle ---> misbehaviour_cycle)
                in
                List.fold_left_es
                  (fun (to_burn, to_reward) cycle ->
                    let* frozen_deposits =
                      Unstaked_frozen_deposits_storage.get ctxt delegate cycle
                    in
                    let*? {amount_to_burn; reward} =
                      compute_reward_and_burn
                        slashing_percentage
                        frozen_deposits
                    in
                    let giver =
                      `Unstaked_frozen_deposits
                        (Unstaked_frozen_staker_repr.Shared delegate, cycle)
                    in
                    return
                      ( (giver, amount_to_burn) :: to_burn,
                        (giver, reward) :: to_reward ))
                  init_to_burn_to_reward
                  slashable_cycles
              in
              let origin = Receipt_repr.Delayed_operation {operation_hash} in
              let* ctxt, punish_balance_updates =
                Token.transfer_n
                  ctxt
                  ~origin
                  to_burn
                  `Double_signing_punishments
              in
              let+ ctxt, reward_balance_updates =
                Token.transfer_n
                  ctxt
                  ~origin
                  to_reward
                  (`Contract (Contract_repr.Implicit rewarded))
              in
              ( ctxt,
                punish_balance_updates @ reward_balance_updates
                @ balance_updates ))
            (ctxt, balance_updates)
            denunciations_to_apply
        in
        ( ctxt,
          balance_updates,
          (delegate, denunciations_to_delay) :: remaining_denunciations ))
  in
  let*! ctxt = Pending_denunciations_storage.clear ctxt in
  let*! ctxt =
    List.fold_left_s
      (fun ctxt (delegate, current_cycle_denunciations) ->
        match current_cycle_denunciations with
        | [] -> Lwt.return ctxt
        | _ ->
            Pending_denunciations_storage.set_denunciations
              ctxt
              delegate
              current_cycle_denunciations)
      ctxt
      remaining_denunciations
  in
  return (ctxt, balance_updates)

module For_RPC = struct
  let pending_denunciations ctxt delegate =
    let open Lwt_result_syntax in
    let+ denunciations = Storage.Pending_denunciations.find ctxt delegate in
    Option.value denunciations ~default:[]

  let pending_denunciations_list ctxt =
    let open Lwt_syntax in
    let* r = Storage.Pending_denunciations.bindings ctxt in
    let r =
      List.map (fun (x, l) -> List.map (fun y -> (x, y)) l) r |> List.flatten
    in
    return r
end
