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

let punish_double_signing ctxt ~operation_hash misbehaviour delegate ~rewarded =
  let open Lwt_result_syntax in
  let* ctxt, was_already_denounced =
    Already_denounced_storage.add_denunciation ctxt delegate misbehaviour
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

let split_reward_and_burn punishing_amount global_limit_of_staking_over_baking =
  let open Result_syntax in
  let global_limit_of_staking_over_baking_plus_two =
    Int64.add (Int64.of_int global_limit_of_staking_over_baking) 2L
  in
  let* reward =
    Tez_repr.(punishing_amount /? global_limit_of_staking_over_baking_plus_two)
  in
  let+ amount_to_burn = Tez_repr.(punishing_amount -? reward) in
  {reward; amount_to_burn}

let compute_reward_and_burn slashing_percentage frozen_deposits
    global_limit_of_staking_over_baking =
  let punishing_amount =
    compute_punishing_amount slashing_percentage frozen_deposits
  in
  split_reward_and_burn punishing_amount global_limit_of_staking_over_baking

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
  else fun (ctxt : Raw_context.t) (_ : Signature.public_key_hash) ->
    (* Denunciation applied too late.
       We could assert false, but we can also be permissive
       while keeping the same invariants. *)
    return (ctxt, Tez_repr.zero)

let update_block_denunciations_map_with delegate denunciations initial_block_map
    =
  List.fold_left
    (fun block_map denunciation ->
      MisMap.update
        denunciation.Denunciations_repr.misbehaviour
        (function
          | None ->
              Some
                (Signature.Public_key_hash.Map.singleton delegate denunciation)
          | Some map ->
              Some
                (Signature.Public_key_hash.Map.update
                   delegate
                   (function
                     | None -> Some denunciation | Some old_d -> Some old_d)
                   map))
        block_map)
    initial_block_map
    denunciations

(* Split denunciations into two groups: those to be applied, and those to be delayed. *)
let get_applicable_and_remaining_denunciations ctxt current_cycle =
  Storage.Pending_denunciations.fold
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
        List.partition
          (fun denunciation ->
            let level = denunciation.Denunciations_repr.misbehaviour.level in
            let misb_cycle =
              Level_repr.cycle_from_raw
                ~cycle_eras:(Raw_context.cycle_eras ctxt)
                level
            in
            Cycle_repr.(misb_cycle < current_cycle))
          denunciations
      in
      let new_block_map =
        update_block_denunciations_map_with
          delegate
          denunciations_to_apply
          block_map
      in
      let new_remaining_denunciations =
        (delegate, denunciations_to_delay) :: remaining_denunciations
      in
      Lwt.return (new_block_map, new_remaining_denunciations))

let apply_block_denunciations ctxt current_cycle block_denunciations_map =
  let slashable_deposits_period =
    Constants_storage.slashable_deposits_period ctxt
  in
  let open Lwt_result_syntax in
  let global_limit_of_staking_over_baking =
    Constants_storage.adaptive_issuance_global_limit_of_staking_over_baking ctxt
  in
  MisMap.fold_es
    (fun miskey denunciations_map acc ->
      let ctxt, balance_updates = acc in
      let misbehaviour_level = Level_storage.from_raw ctxt miskey.level in
      let misbehaviour_cycle = misbehaviour_level.cycle in
      let denunciations =
        Signature.Public_key_hash.Map.bindings denunciations_map
      in
      let denounced = List.map fst denunciations in
      let* ctxt, slashing_percentage =
        Slash_percentage.get ctxt miskey denounced
      in
      let+ ctxt, balance_updates =
        List.fold_left_es
          (fun (ctxt, balance_updates)
               ( delegate,
                 Denunciations_repr.{operation_hash; rewarded; misbehaviour} )
             ->
            assert (
              Compare.Int.equal
                (* This compare ignores the slot *)
                (Misbehaviour_repr.compare miskey misbehaviour)
                0) ;
            (* Validate ensures that [denunciations] contains [delegate] at most once *)
            let delegate_contract = Contract_repr.Implicit delegate in
            (* Oxford values *)
            let* slash_history_opt_o =
              Storage.Contract.Slashed_deposits__Oxford.find
                ctxt
                delegate_contract
            in
            let slash_history_o =
              Option.value slash_history_opt_o ~default:[]
              |> List.map (fun (a, b) -> (a, Percentage.convert_from_o_to_p b))
            in
            let* slash_history_opt =
              Storage.Slashed_deposits.find ctxt delegate
            in
            let slash_history = Option.value slash_history_opt ~default:[] in

            (* Concatenate both, Oxford first *)
            let slash_history =
              List.fold_left
                (fun acc (cycle, percentage) ->
                  Storage.Slashed_deposits_history.add cycle percentage acc)
                slash_history_o
                slash_history
            in

            let*! ctxt =
              Storage.Contract.Slashed_deposits__Oxford.remove
                ctxt
                delegate_contract
            in

            let previous_total_slashing_percentage =
              Storage.Slashed_deposits_history.get
                misbehaviour_cycle
                slash_history
            in
            let slash_history =
              Storage.Slashed_deposits_history.add
                misbehaviour_cycle
                slashing_percentage
                slash_history
            in
            let*! ctxt =
              Storage.Slashed_deposits.add ctxt delegate slash_history
            in
            let new_total_slashing_percentage =
              Storage.Slashed_deposits_history.get
                misbehaviour_cycle
                slash_history
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
            let* ctxt, frozen_deposits =
              let* ctxt, initial_amount =
                get_initial_frozen_deposits_of_misbehaviour_cycle
                  ~current_cycle
                  ~misbehaviour_cycle
                  ctxt
                  delegate
              in
              let* current_amount =
                Delegate_storage.current_frozen_deposits ctxt delegate
              in
              return (ctxt, Deposits_repr.{initial_amount; current_amount})
            in
            let punishing_amount =
              compute_punishing_amount slashing_percentage frozen_deposits
              (* Ensures: [punishing_amount <= current_amount]

                 where [current_amount = frozen_deposits.current_amount
                                       = own_frozen + staked_frozen]
              *)
            in
            let* {baker_part; stakers_part = _} =
              Shared_stake.share
                ~rounding:`Towards_baker
                ctxt
                delegate
                punishing_amount
              (* Ensures:

                 - [baker_part + stakers_part = punishing_amount]

                 - [baker_part / punishing_amount = own_frozen / (own_frozen + allowed_staked_frozen)]

                 where [allowed_staked_frozen] is [staked_frozen]
                 capped by the delegate's [limit_of_staking_over_baking],
                 which notably means that [allowed_staked_frozen <= staked_frozen]
                 i.e. [own_frozen + allowed_staked_frozen <= own_frozen + staked_frozen = current_amount]

                 Combining all of the above:

                 [baker_part / punishing_amount >= own_frozen / current_amount]

                 [(punishing_amount - stakers_part) / punishing_amount >= (current_amount - staked_frozen) / current_amount]

                 [1 - (stakers_part / punishing_amount) >= 1 - (staked_frozen / current_amount)]

                 [stakers_part / punishing_amount <= staked_frozen / current_amount]

                 [stakers_part <= staked_frozen * punishing_amount / current_amount]

                 Moreover, we know from above that [punishing_amount <= current_amount] so:

                 [stakers_part <= staked_frozen]
              *)
            in
            let* full_staking_balance =
              Stake_storage.get_full_staking_balance ctxt delegate
            in
            let own_frozen =
              Full_staking_balance_repr.own_frozen full_staking_balance
            in
            let actual_baker_part = Tez_repr.min baker_part own_frozen in
            let*? actual_stakers_part =
              Tez_repr.(punishing_amount -? actual_baker_part)
            in
            (* To avoid underflows, we need to guarantee that:
               - [actual_baker_part <= own_frozen] and
               - [actual_stakers_part <= staked_frozen]

               The [min] ensures that [actual_baker_part <= own_frozen].

               For [actual_stakers_part], let's examine two cases
               based on the [min]:

               - Case 1: [actual_baker_part = baker_part]

               [actual_stakers_part = punishing_amount - actual_baker_part
                 = punishing_amount - baker_part
                 = stakers_part
                 <= staked_frozen] as proven above

               - Case 2: [actual_baker_part = own_frozen]

               [actual_stakers_part = punishing_amount - actual_baker_part
                 = punishing_amount - own_frozen
                 <= current_amount - own_frozen
                     = own_frozen + staked_frozen - own_frozen
                     = staked_frozen]
            *)
            let*? {amount_to_burn = to_burn_baker; reward = to_reward_baker} =
              split_reward_and_burn
                actual_baker_part
                global_limit_of_staking_over_baking
            in
            let*? {amount_to_burn = to_burn_stakers; reward = to_reward_stakers}
                =
              split_reward_and_burn
                actual_stakers_part
                global_limit_of_staking_over_baking
            in
            let giver_baker =
              `Frozen_deposits (Frozen_staker_repr.baker delegate)
            in
            let giver_stakers =
              `Frozen_deposits
                (Frozen_staker_repr.shared_between_stakers ~delegate)
            in
            let init_to_burn =
              [(giver_baker, to_burn_baker); (giver_stakers, to_burn_stakers)]
            in
            let init_to_reward =
              [
                (giver_baker, to_reward_baker);
                (giver_stakers, to_reward_stakers);
              ]
            in
            let* to_burn, to_reward =
              let oldest_slashable_cycle =
                Cycle_repr.sub misbehaviour_cycle slashable_deposits_period
                |> Option.value ~default:Cycle_repr.root
              in
              let slashable_cycles =
                Cycle_repr.(oldest_slashable_cycle ---> current_cycle)
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
                      global_limit_of_staking_over_baking
                  in
                  let giver =
                    `Unstaked_frozen_deposits
                      (Unstaked_frozen_staker_repr.Shared delegate, cycle)
                  in
                  return
                    ( (giver, amount_to_burn) :: to_burn,
                      (giver, reward) :: to_reward ))
                (init_to_burn, init_to_reward)
                slashable_cycles
            in
            let origin = Receipt_repr.Delayed_operation {operation_hash} in
            let* ctxt, punish_balance_updates =
              Token.transfer_n ctxt ~origin to_burn `Double_signing_punishments
            in
            let+ ctxt, reward_balance_updates =
              Token.transfer_n
                ctxt
                ~origin
                to_reward
                (`Contract (Contract_repr.Implicit rewarded))
            in
            ( ctxt,
              punish_balance_updates @ reward_balance_updates @ balance_updates
            ))
          (ctxt, balance_updates)
          denunciations
      in
      (ctxt, balance_updates))
    block_denunciations_map
    (ctxt, [])

let apply_denunciations ctxt =
  let open Lwt_result_syntax in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  let*! applicable_denunciations_map, remaining_denunciations =
    get_applicable_and_remaining_denunciations ctxt current_cycle
  in
  let* ctxt, balance_updates =
    apply_block_denunciations ctxt current_cycle applicable_denunciations_map
  in
  return (ctxt, balance_updates, remaining_denunciations)

let apply_and_clear_denunciations ctxt =
  let open Lwt_result_syntax in
  let* ctxt, balance_updates, remaining_denunciations =
    apply_denunciations ctxt
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

module For_RPC = struct
  let get_pending_misbehaviour_map ctxt =
    Storage.Pending_denunciations.fold
      ctxt
      ~order:`Undefined
      ~init:MisMap.empty
      ~f:(fun delegate denunciations block_map ->
        let new_block_map =
          update_block_denunciations_map_with delegate denunciations block_map
        in
        Lwt.return new_block_map)

  let get_estimated_punished_amount ctxt delegate =
    let open Lwt_result_syntax in
    let current_cycle = (Raw_context.current_level ctxt).cycle in
    let* denunciations = Storage.Pending_denunciations.find ctxt delegate in
    match denunciations with
    | None | Some [] -> return Tez_repr.zero
    | Some denunciations ->
        let*! pending_misbehaviour_map = get_pending_misbehaviour_map ctxt in
        List.fold_left_es
          (fun estimated_punishing_amount denunciation ->
            let misbehaviour_key =
              denunciation.Denunciations_repr.misbehaviour
            in
            match MisMap.find misbehaviour_key pending_misbehaviour_map with
            | None ->
                (* Should not happen as [pending_misbehaviour_map] has been created
                   using the bindings of [Storage.Pending_denunciations] and
                   [denunciation] belongs to [Storage.Pending_denunciations]. *)
                return estimated_punishing_amount
            | Some denunciations ->
                let denounced_pkhs =
                  List.map
                    fst
                    (Signature.Public_key_hash.Map.bindings denunciations)
                in
                let* ctxt, slashing_percentage =
                  Slash_percentage.get ctxt misbehaviour_key denounced_pkhs
                in
                let misbehaviour_cycle =
                  (Level_storage.from_raw ctxt misbehaviour_key.level).cycle
                in
                let* frozen_deposits =
                  (* We ignore the context because this function is only used for RPCs *)
                  let* _ctxt, initial_amount =
                    get_initial_frozen_deposits_of_misbehaviour_cycle
                      ~current_cycle
                      ~misbehaviour_cycle
                      ctxt
                      delegate
                  in
                  let* current_amount =
                    Delegate_storage.current_frozen_deposits ctxt delegate
                  in
                  return {Deposits_repr.initial_amount; current_amount}
                in
                let punishing_amount =
                  compute_punishing_amount slashing_percentage frozen_deposits
                in
                let new_estimated_punishing_amount =
                  Tez_repr.(punishing_amount +? estimated_punishing_amount)
                in
                Lwt.return new_estimated_punishing_amount)
          Tez_repr.zero
          denunciations

  let get_estimated_punished_share ctxt delegate =
    let open Lwt_result_syntax in
    let* estimated_punished_amount =
      get_estimated_punished_amount ctxt delegate
    in
    Shared_stake.share
      ~rounding:`Towards_baker
      ctxt
      delegate
      estimated_punished_amount

  let get_estimated_shared_pending_slashed_amount ctxt delegate =
    let open Lwt_result_syntax in
    let* {baker_part; stakers_part} =
      get_estimated_punished_share ctxt delegate
    in
    Lwt.return Tez_repr.(baker_part +? stakers_part)

  let get_delegate_estimated_own_pending_slashed_amount ctxt ~delegate =
    let open Lwt_result_syntax in
    let+ {baker_part; stakers_part = _} =
      get_estimated_punished_share ctxt delegate
    in
    baker_part

  let get_estimated_own_pending_slashed_amount ctxt contract =
    let open Lwt_result_syntax in
    let* delegate_opt = Contract_delegate_storage.find ctxt contract in
    match delegate_opt with
    | None -> return Tez_repr.zero
    | Some delegate ->
        if Contract_repr.(equal (Contract_repr.Implicit delegate) contract) then
          get_delegate_estimated_own_pending_slashed_amount ctxt ~delegate
        else
          let* {baker_part = _; stakers_part} =
            get_estimated_punished_share ctxt delegate
          in
          let* num =
            let+ staking_pseudotokens =
              Staking_pseudotokens_storage.For_RPC.staking_pseudotokens_balance
                ctxt
                ~delegator:contract
            in
            Staking_pseudotoken_repr.to_int64 staking_pseudotokens
          in
          let* den =
            let+ frozen_deposits_pseudotokens =
              Staking_pseudotokens_storage.For_RPC
              .get_frozen_deposits_pseudotokens
                ctxt
                ~delegate
            in
            Staking_pseudotoken_repr.to_int64 frozen_deposits_pseudotokens
          in
          Lwt.return (Tez_repr.mul_ratio ~rounding:`Up stakers_part ~num ~den)
end
