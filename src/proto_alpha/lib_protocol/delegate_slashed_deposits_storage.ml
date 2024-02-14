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

(* TODO #6918: Remove after P *)
let update_slashing_storage_for_p ctxt =
  let open Lwt_result_syntax in
  let*! ctxt =
    Storage.Contract.Slashed_deposits__Oxford.fold
      ctxt
      ~order:`Undefined
      ~init:ctxt
      ~f:(fun contract slashed_history ctxt ->
        let slashed_history =
          List.map
            (fun (cycle, percentage) ->
              (cycle, Percentage.convert_from_o_to_p percentage))
            slashed_history
        in
        Storage.Contract.Slashed_deposits.add ctxt contract slashed_history)
  in
  Storage.Contract.Slashed_deposits__Oxford.clear ctxt

type reward_and_burn = {reward : Tez_repr.t; amount_to_burn : Tez_repr.t}

type punishing_amounts = {
  staked : reward_and_burn;
  unstaked : (Cycle_repr.t * reward_and_burn) list;
}

let record_denunciation ctxt ~operation_hash
    (misbehaviour : Misbehaviour_repr.t) delegate ~rewarded =
  let open Lwt_result_syntax in
  let*! ctxt = Forbidden_delegates_storage.forbid ctxt delegate in
  Pending_denunciations_storage.add_denunciation
    ctxt
    ~misbehaving_delegate:delegate
    operation_hash
    ~rewarded_delegate:rewarded
    misbehaviour

let punish_double_signing ctxt ~operation_hash misbehaviour delegate
    (level : Level_repr.t) ~rewarded =
  let open Lwt_result_syntax in
  let* ctxt, was_already_denounced =
    Already_denounced_storage.add_denunciation
      ctxt
      delegate
      level
      misbehaviour.Misbehaviour_repr.round
      misbehaviour.kind
  in
  if was_already_denounced then
    (* This can only happen in the very specific case where a delegate
       has crafted at least three attestations (respectively
       preattestations) on the same level and round but with three
       different slots owned by this delegate. Indeed, this makes it
       possible to have two denunciations about the same delegate,
       level, round, and kind, but different slots. Such denunciations
       are considered identical by {!Already_denounced_storage}, which
       is good because the delegate shouldn't get slashed twice on the
       same level, round, and kind. However, {!Validate}'s conflict
       handler identifies denunciations via their slot rather than
       delegate for technical reasons (because the slot is readily
       available whereas retrieving the delegate requires a call to
       {!Delegate_sampler.slot_owner} which is in Lwt and thus
       incompatible with some signatures). Therefore, if these
       denunciations (which differ only in their slots) are both
       included in the same block, then they will both be successfully
       validated, and then [was_already_denounced] will be [true]
       during the application of the second one.

       In this unlikely scenario, we simply ignore the redundant
       denunciation silently. Returning an error or raising an
       exception here would cause the whole block application to fail,
       which we don't want. *)
    return ctxt
  else record_denunciation ctxt ~operation_hash misbehaviour delegate ~rewarded

(* Misbehaviour Map: orders denunciations for application.
   See {!Misbehaviour_repr.compare} for the order on misbehaviours:
   - by increasing level, then increasing round, then kind, ignoring the slot
   - for the kind: double baking > double attesting > double preattesting *)
module MisMap = Map.Make (Misbehaviour_repr)

let compute_punishing_amount slashing_percentage frozen_deposits =
  let punish_value =
    Tez_repr.mul_percentage
      ~rounding:`Down
      frozen_deposits.Deposits_repr.initial_amount
      slashing_percentage
  in
  Tez_repr.min punish_value frozen_deposits.Deposits_repr.current_amount

let compute_reward_and_burn ctxt slashing_percentage frozen_deposits =
  let open Result_syntax in
  let punishing_amount =
    compute_punishing_amount slashing_percentage frozen_deposits
  in
  let global_limit_of_staking_over_baking_plus_two =
    let global_limit_of_staking_over_baking =
      Constants_storage.adaptive_issuance_global_limit_of_staking_over_baking
        ctxt
    in
    Int64.add (Int64.of_int global_limit_of_staking_over_baking) 2L
  in
  let* reward =
    Tez_repr.(punishing_amount /? global_limit_of_staking_over_baking_plus_two)
  in
  let+ amount_to_burn = Tez_repr.(punishing_amount -? reward) in
  {reward; amount_to_burn}

let get_initial_frozen_deposits_of_misbehaviour_cycle ~current_cycle
    ~misbehaviour_cycle =
  let previous_cycle =
    match Cycle_repr.pred current_cycle with
    | None -> current_cycle
    | Some previous_cycle -> previous_cycle
  in
  if Cycle_repr.equal current_cycle misbehaviour_cycle then
    Delegate_storage.initial_frozen_deposits
  else if Cycle_repr.equal previous_cycle misbehaviour_cycle then
    Delegate_storage.initial_frozen_deposits_of_previous_cycle
  else fun (_ : Raw_context.t) (_ : Signature.public_key_hash) ->
    (* Denunciation applied too late.
       We could assert false, but we can also be permissive
       while keeping the same invariants. *)
    return Tez_repr.zero

let apply_and_clear_denunciations ctxt =
  let open Lwt_result_syntax in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let slashable_deposits_period =
    Constants_storage.slashable_deposits_period ctxt
  in
  (* Split denunciations into two groups: to be applied, and to be delayed *)
  let*! block_denunciations_map, remaining_denunciations =
    Pending_denunciations_storage.fold
      ctxt
      ~order:`Undefined
      ~init:(MisMap.empty, [])
      ~f:(fun delegate denunciations acc ->
        let block_map, remaining_denunciations = acc in
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
        let block_map =
          List.fold_left
            (fun block_map denunciation ->
              MisMap.update
                denunciation.Denunciations_repr.misbehaviour
                (function
                  | None ->
                      Some
                        (Signature.Public_key_hash.Map.singleton
                           delegate
                           denunciation)
                  | Some map ->
                      Some
                        (Signature.Public_key_hash.Map.update
                           delegate
                           (function
                             | None -> Some denunciation
                             | Some old_d -> Some old_d)
                           map))
                block_map)
            block_map
            denunciations_to_apply
        in
        Lwt.return
          ( block_map,
            (delegate, denunciations_to_delay) :: remaining_denunciations ))
  in
  (* Processes the applicable denunciations *)
  let* ctxt, balance_updates =
    MisMap.fold_es
      (fun ({Misbehaviour_repr.level = raw_level; round = _; kind; _} as miskey)
           denunciations_map
           acc ->
        let ctxt, balance_updates = acc in
        let level =
          Level_repr.level_from_raw
            ~cycle_eras:(Raw_context.cycle_eras ctxt)
            raw_level
        in
        let misbehaviour_cycle = level.cycle in
        let denunciations =
          Signature.Public_key_hash.Map.bindings denunciations_map
        in
        let denounced = List.map fst denunciations in
        let* ctxt, slashing_percentage =
          Slash_percentage.get ctxt ~kind ~level denounced
        in
        let+ ctxt, balance_updates =
          List.fold_left_es
            (fun (ctxt, balance_updates)
                 ( delegate,
                   Denunciations_repr.{operation_hash; rewarded; misbehaviour}
                 ) ->
              assert (
                Compare.Int.equal
                  (* This compare ignores the slot *)
                  (Misbehaviour_repr.compare miskey misbehaviour)
                  0) ;
              (* Validate ensures that [denunciations] contains [delegate] at most once *)
              let delegate_contract = Contract_repr.Implicit delegate in
              let* slash_history_opt =
                Storage.Contract.Slashed_deposits.find ctxt delegate_contract
              in
              let slash_history = Option.value slash_history_opt ~default:[] in
              let previous_total_slashing_percentage =
                Storage.Slashed_deposits_history.get level.cycle slash_history
              in
              let slash_history =
                Storage.Slashed_deposits_history.add
                  level.cycle
                  slashing_percentage
                  slash_history
              in
              let*! ctxt =
                Storage.Contract.Slashed_deposits.add
                  ctxt
                  delegate_contract
                  slash_history
              in

              let new_total_slashing_percentage =
                Storage.Slashed_deposits_history.get level.cycle slash_history
              in
              (* We do not slash above 100%: if the slashing percentage would
                 make the total sum of the slashing history above 100%, we rectify
                 it to reach exactly 100%. This also means that subsequent slashes
                 are effectively ignored (set to 0%) *)
              let slashing_percentage =
                Percentage.sub_bounded
                  new_total_slashing_percentage
                  previous_total_slashing_percentage
              in

              let* frozen_deposits =
                let* initial_amount =
                  get_initial_frozen_deposits_of_misbehaviour_cycle
                    ~current_cycle
                    ~misbehaviour_cycle
                    ctxt
                    delegate
                in
                let* current_amount =
                  Delegate_storage.current_frozen_deposits ctxt delegate
                in
                return Deposits_repr.{initial_amount; current_amount}
              in
              let*? staked =
                compute_reward_and_burn ctxt slashing_percentage frozen_deposits
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
                        ctxt
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
            denunciations
        in
        (ctxt, balance_updates))
      block_denunciations_map
      (ctxt, [])
  in
  (* Updates the storage to only contain the remaining denunciations *)
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
