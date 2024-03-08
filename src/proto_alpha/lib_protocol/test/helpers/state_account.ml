(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [State_account] is dedicated to operations in [State.t] that would modify
    an [account_state]. This includes any operation like [transfer], [stake],
    [unstake], [set_delegate_parameters], anything related to slashing or
    rewards, etc... *)

open Adaptive_issuance_helpers
include Tez_staking_helpers
include Account_helpers

let add_liquid_rewards amount account_name account_map =
  let f account =
    let liquid = Tez.(account.liquid +! amount) in
    {account with liquid}
  in
  update_account ~f account_name account_map

let add_frozen_rewards amount account_name account_map =
  let f account =
    let actual_edge =
      Q.(
        mul account.parameters.edge_of_baking_over_staking (1_000_000_000 // 1)
        |> to_int |> of_int
        |> mul (1 // 1_000_000_000))
    in
    let frozen_deposits =
      Frozen_tez.add_tez_to_all_current
        ~edge:actual_edge
        amount
        account.frozen_deposits
    in
    {account with frozen_deposits}
  in
  update_account ~f account_name account_map

let apply_burn amount src_name account_map =
  let f src = {src with liquid = Tez.(src.liquid -! amount)} in
  update_account ~f src_name account_map

let apply_transfer amount src_name dst_name account_map =
  match
    (String.Map.find src_name account_map, String.Map.find dst_name account_map)
  with
  | Some src, Some _ ->
      if Tez.(src.liquid < amount) then
        (* Invalid amount: operation will fail *)
        account_map
      else
        let f_src src =
          let liquid = Tez.(src.liquid -! amount) in
          {src with liquid}
        in
        let f_dst dst =
          let liquid = Tez.(dst.liquid +! amount) in
          {dst with liquid}
        in
        let account_map = update_account ~f:f_src src_name account_map in
        update_account ~f:f_dst dst_name account_map
  | None, _ -> fail_account_not_found "apply_transfer.src" src_name
  | _, None -> fail_account_not_found "apply_transfer.dst" dst_name

let stake_from_unstake amount current_cycle consensus_rights_delay delegate_name
    account_map =
  match String.Map.find delegate_name account_map with
  | None -> fail_account_not_found "stake_from_unstake" delegate_name
  | Some ({unstaked_frozen; frozen_deposits; slashed_cycles; _} as account) ->
      let oldest_slashable_cycle =
        Cycle.(sub current_cycle (consensus_rights_delay + 1))
        |> Option.value ~default:Cycle.root
      in
      if
        List.exists
          (fun x -> Cycle.(x >= oldest_slashable_cycle))
          slashed_cycles
      then (account_map, amount)
      else
        let unstaked_frozen =
          List.sort
            (fun (Unstaked_frozen.{cycle = cycle1; _} : Unstaked_frozen.r)
                 {cycle = cycle2; _} -> Cycle.compare cycle2 cycle1)
            unstaked_frozen
        in
        let rec aux acc_unstakes rem_amount rem_unstakes =
          match rem_unstakes with
          | [] -> (acc_unstakes, rem_amount)
          | (Unstaked_frozen.{initial; _} as h) :: t ->
              if Tez.(rem_amount = zero) then
                (acc_unstakes @ rem_unstakes, Tez.zero)
              else if Tez.(rem_amount >= initial) then
                let h = Unstaked_frozen.sub_unstake initial delegate_name h in
                let rem_amount = Tez.(rem_amount -! initial) in
                aux (acc_unstakes @ [h]) rem_amount t
              else
                let h =
                  Unstaked_frozen.sub_unstake rem_amount delegate_name h
                in
                (acc_unstakes @ [h] @ t, Tez.zero)
        in
        let unstaked_frozen, rem_amount = aux [] amount unstaked_frozen in
        let frozen_deposits =
          Frozen_tez.add_current
            Tez.(amount -! rem_amount)
            delegate_name
            frozen_deposits
        in
        let account = {account with unstaked_frozen; frozen_deposits} in
        let account_map =
          update_account ~f:(fun _ -> account) delegate_name account_map
        in
        (account_map, rem_amount)

let tez_to_pseudo ~round amount delegate_account =
  let {staking_delegate_denominator; frozen_deposits; _} = delegate_account in
  let total_q = Frozen_tez.total_co_current_q frozen_deposits.co_current in
  let total, rem = Partial_tez.to_tez_rem total_q in
  assert (Q.(equal rem zero)) ;
  if Tez.(equal total zero) then Tez.to_z amount
  else
    let r = Tez.ratio amount total in
    let p = Q.(r * of_bigint staking_delegate_denominator) in
    Tez.(of_q ~round p |> to_z)

let pseudo_to_partial_tez amount_pseudo delegate_account =
  let {staking_delegate_denominator; frozen_deposits; _} = delegate_account in
  let total_q = Frozen_tez.total_co_current_q frozen_deposits.co_current in
  let total, rem = Partial_tez.to_tez_rem total_q in
  assert (Q.(equal rem zero)) ;
  if Z.(equal staking_delegate_denominator zero) then Q.of_bigint amount_pseudo
  else
    let q = Q.(amount_pseudo /// staking_delegate_denominator) in
    Tez.mul_q total q

(* tez_q <= amount *)
let stake_values_real amount delegate_account =
  let pseudo = tez_to_pseudo ~round:`Down amount delegate_account in
  let tez_q = pseudo_to_partial_tez pseudo delegate_account in
  (pseudo, tez_q)

(* returned_amount <= amount *)
let unstake_values_real amount delegate_account =
  let pseudo = tez_to_pseudo ~round:`Up amount delegate_account in
  let tez_q = pseudo_to_partial_tez pseudo delegate_account in
  if Tez.equal (Tez.of_q ~round:`Down tez_q) amount then (pseudo, tez_q)
  else
    let pseudo = Z.(pseudo - one) in
    (pseudo, pseudo_to_partial_tez pseudo delegate_account)

let apply_stake amount current_cycle consensus_rights_delay staker_name
    account_map =
  match String.Map.find staker_name account_map with
  | None -> fail_account_not_found "apply_stake" staker_name
  | Some staker -> (
      match staker.delegate with
      | None ->
          (* Invalid operation: no delegate *)
          account_map
      | Some delegate_name ->
          let old_account_map = account_map in
          (* If self stake, then try to stake from unstake.
             Returns the amount that remains to be staked from liquid *)
          let account_map, amount =
            if delegate_name = staker_name then
              stake_from_unstake
                amount
                current_cycle
                consensus_rights_delay
                staker_name
                account_map
            else (account_map, amount)
          in
          if Tez.(staker.liquid < amount) then
            (* Not enough liquid balance: operation will fail *)
            old_account_map
          else if delegate_name = staker_name then
            (* If self stake: increase frozen deposits and decrease liquid balance.
               "add_current" is easy to resolve since there is no pseudotokens *)
            let f delegate =
              let frozen_deposits =
                Frozen_tez.add_current
                  amount
                  staker_name
                  delegate.frozen_deposits
              in
              let liquid = Tez.(delegate.liquid -! amount) in
              {delegate with frozen_deposits; liquid}
            in
            update_account ~f delegate_name account_map
          else
            (* If external stake: *)
            let delegate_account =
              String.Map.find delegate_name account_map
              |> Option.value_f ~default:(fun _ -> assert false)
            in
            (* Call stake_values_real to know the actual amount staked and the pseudotokens minted *)
            (* amount_q would be the effective stake on the delegate's side, while
               amount is the amount removed from the liquid balance *)
            let pseudo, amount_q = stake_values_real amount delegate_account in
            let f_staker staker =
              let liquid = Tez.(staker.liquid -! amount) in
              let staking_delegator_numerator =
                Z.add staker.staking_delegator_numerator pseudo
              in
              {staker with liquid; staking_delegator_numerator}
            in
            let f_delegate delegate =
              (* The difference between the actual amount and the effective amount is
                 "distributed" amongst current stake holders.
                 Indeed, when trading in "amount", the staker receives "pseudo" pseudotokens
                 valued at "amount_q". So the total amount of value is increased by "amount_q".
                 Then, "portion" is added to the total, so it must be distributed.
                 This means that the order is important: first you add_current_q, then
                 you add the portion to all *)
              let portion = Partial_tez.(of_tez amount - amount_q) in
              let frozen_deposits =
                Frozen_tez.add_current_q
                  amount_q
                  staker_name
                  delegate.frozen_deposits
              in
              let co_current =
                Frozen_tez.add_q_to_all_co_current
                  portion
                  frozen_deposits.co_current
              in
              let frozen_deposits = {frozen_deposits with co_current} in
              let staking_delegate_denominator =
                Z.add delegate.staking_delegate_denominator pseudo
              in
              {delegate with frozen_deposits; staking_delegate_denominator}
            in
            let account_map =
              update_account ~f:f_staker staker_name account_map
            in
            update_account ~f:f_delegate delegate_name account_map)

let apply_unstake cycle amount staker_name account_map =
  match String.Map.find staker_name account_map with
  | None -> fail_account_not_found "apply_unstake.staker" staker_name
  | Some staker -> (
      match staker.delegate with
      | None -> (* Invalid operation: no delegate *) account_map
      | Some delegate_name -> (
          match String.Map.find delegate_name account_map with
          | None ->
              fail_account_not_found "apply_unstake.delegate" delegate_name
          | Some delegate ->
              if delegate_name = staker_name then
                (* Case self stake *)
                (* No pseudotokens : no problem *)
                let frozen_deposits, amount_unstaked =
                  Frozen_tez.sub_current
                    amount
                    staker_name
                    delegate.frozen_deposits
                in
                let unstaked_frozen =
                  Unstaked_frozen.add_unstake
                    cycle
                    amount_unstaked
                    staker_name
                    delegate.unstaked_frozen
                in
                let delegate =
                  {delegate with frozen_deposits; unstaked_frozen}
                in
                update_account ~f:(fun _ -> delegate) delegate_name account_map
              else
                (* Case external stake *)
                let staked_amount =
                  Frozen_tez.get staker_name delegate.frozen_deposits
                in
                let pseudotokens, amount_q =
                  if Partial_tez.(staked_amount <= of_tez amount) then
                    (* Unstake all case *)
                    (staker.staking_delegator_numerator, staked_amount)
                  else
                    (* The staker requests "amount".
                       It translates to some "pseudotokens", valued at "amount_q".
                       If those pseudotokens would give strictly more than the requested amount,
                       then give one less pseudotoken. The actual amount unstaked is always lower than
                       the requested amount (except in the unstake all case) *)
                    unstake_values_real amount delegate
                in
                (* Actual unstaked amount (that will be finalized) *)
                let amount = Partial_tez.to_tez ~round:`Down amount_q in
                (* Delta from pseudotokens' value, to be redistributed amongst all remaining stakers
                   (including current if still staking) *)
                let portion = Partial_tez.(amount_q - of_tez amount) in
                let f_staker staker =
                  (* The staker's account representation doesn't change much,
                     the unstake request is stored on the delegate's side *)
                  let staking_delegator_numerator =
                    Z.sub staker.staking_delegator_numerator pseudotokens
                  in
                  {staker with staking_delegator_numerator}
                in
                let account_map =
                  update_account ~f:f_staker staker_name account_map
                in
                let f_delegate delegate =
                  let staking_delegate_denominator =
                    Z.sub delegate.staking_delegate_denominator pseudotokens
                  in
                  (* Just like in stake *)
                  (* Do the effective unstake *)
                  let frozen_deposits =
                    Frozen_tez.sub_current_q
                      amount_q
                      staker_name
                      delegate.frozen_deposits
                  in
                  (* Apply the delta *)
                  let co_current =
                    Frozen_tez.add_q_to_all_co_current
                      portion
                      frozen_deposits.co_current
                  in
                  let frozen_deposits = {frozen_deposits with co_current} in
                  (* Add unstake request
                     Note that "amount" might not be the initial requested amount *)
                  let unstaked_frozen =
                    Unstaked_frozen.add_unstake
                      cycle
                      amount
                      staker_name
                      delegate.unstaked_frozen
                  in
                  {
                    delegate with
                    staking_delegate_denominator;
                    frozen_deposits;
                    unstaked_frozen;
                  }
                in
                update_account ~f:f_delegate delegate_name account_map))

let apply_unslashable_f cycle delegate =
  let amount_unslashable, unstaked_frozen =
    Unstaked_frozen.pop_cycle cycle delegate.unstaked_frozen
  in
  let unstaked_finalizable =
    Unstaked_finalizable.add_from_poped_ufd
      amount_unslashable
      delegate.unstaked_finalizable
  in
  {delegate with unstaked_frozen; unstaked_finalizable}

(* Updates unstaked unslashable values for given account *)
let apply_unslashable cycle account_name account_map =
  update_account ~f:(apply_unslashable_f cycle) account_name account_map

(* Updates unstaked unslashable values in all accounts *)
let apply_unslashable_for_all cycle account_map =
  String.Map.map (apply_unslashable_f cycle) account_map

let apply_finalize staker_name account_map =
  match String.Map.find staker_name account_map with
  | None -> fail_account_not_found "apply_finalize" staker_name
  | Some _staker ->
      (* Because an account can still have finalizable funds from a delegate
         that is not its own, we iterate over all of them *)
      String.Map.fold
        (fun delegate_name delegate account_map_acc ->
          match
            String.Map.find staker_name delegate.unstaked_finalizable.map
          with
          | None -> account_map_acc
          | Some amount ->
              let f_staker staker =
                let liquid = Tez.(staker.liquid +! amount) in
                {staker with liquid}
              in
              let f_delegate delegate =
                let map =
                  String.Map.remove
                    staker_name
                    delegate.unstaked_finalizable.map
                in
                {
                  delegate with
                  unstaked_finalizable =
                    {delegate.unstaked_finalizable with map};
                }
              in
              let account_map_acc =
                update_account ~f:f_staker staker_name account_map_acc
              in
              update_account ~f:f_delegate delegate_name account_map_acc)
        account_map
        account_map

let apply_slashing
    ( culprit,
      Protocol.Denunciations_repr.{rewarded; misbehaviour; operation_hash = _}
    ) constants account_map =
  let find_account_name_from_pkh_exn pkh account_map =
    match
      Option.map
        fst
        String.Map.(
          choose
          @@ filter
               (fun _ account ->
                 Signature.Public_key_hash.equal pkh account.pkh)
               account_map)
    with
    | None -> assert false
    | Some x -> x
  in
  let slashed_cycle =
    Block.current_cycle_of_level
      ~blocks_per_cycle:
        constants.Protocol.Alpha_context.Constants.Parametric.blocks_per_cycle
      ~current_level:(Protocol.Raw_level_repr.to_int32 misbehaviour.level)
  in
  let culprit_name = find_account_name_from_pkh_exn culprit account_map in
  let rewarded_name = find_account_name_from_pkh_exn rewarded account_map in
  let slashed_pct =
    match misbehaviour.kind with
    | Double_baking ->
        constants
          .Protocol.Alpha_context.Constants.Parametric
           .percentage_of_frozen_deposits_slashed_per_double_baking
    | Double_attesting | Double_preattesting ->
        constants.percentage_of_frozen_deposits_slashed_per_double_attestation
  in
  let get_total_supply acc_map =
    String.Map.fold
      (fun _name
           {
             pkh = _;
             contract = _;
             delegate = _;
             parameters = _;
             liquid;
             bonds;
             frozen_deposits;
             unstaked_frozen;
             unstaked_finalizable;
             staking_delegator_numerator = _;
             staking_delegate_denominator = _;
             frozen_rights = _;
             slashed_cycles = _;
           }
           tot ->
        Tez.(
          liquid +! bonds
          +! Frozen_tez.total_current frozen_deposits
          +! Unstaked_frozen.sum_current unstaked_frozen
          +! Unstaked_finalizable.total unstaked_finalizable
          +! tot))
      acc_map
      Tez.zero
  in
  let total_before_slash = get_total_supply account_map in
  let slash_culprit
      ({frozen_deposits; unstaked_frozen; frozen_rights; _} as acc) =
    let base_rights =
      CycleMap.find slashed_cycle frozen_rights
      |> Option.value ~default:Tez.zero
    in
    let frozen_deposits, slashed_frozen =
      Frozen_tez.slash base_rights slashed_pct frozen_deposits
    in
    let slashed_pct_q = Protocol.Percentage.to_q slashed_pct in
    let slashed_pct = Q.(100 // 1 * slashed_pct_q |> to_int) in
    let unstaked_frozen, slashed_unstaked =
      Unstaked_frozen.slash
        ~slashable_deposits_period:constants.consensus_rights_delay
        slashed_cycle
        slashed_pct
        unstaked_frozen
    in
    ( {acc with frozen_deposits; unstaked_frozen},
      slashed_frozen :: slashed_unstaked )
  in
  let culprit_account =
    String.Map.find culprit_name account_map
    |> Option.value_f ~default:(fun () ->
           fail_account_not_found "apply_slashing" culprit_name)
  in
  let slashed_culprit_account, total_slashed = slash_culprit culprit_account in
  let account_map =
    update_account
      ~f:(fun _ -> slashed_culprit_account)
      culprit_name
      account_map
  in
  let total_after_slash = get_total_supply account_map in
  let portion_reward =
    constants.adaptive_issuance.global_limit_of_staking_over_baking + 2
  in
  (* For each container slashed, the snitch gets a reward transferred. It gets rounded
     down each time *)
  let reward_to_snitch =
    List.map
      (fun x -> Tez.mul_q x Q.(1 // portion_reward) |> Tez.of_q ~round:`Down)
      total_slashed
    |> List.fold_left Tez.( +! ) Tez.zero
  in
  let account_map =
    add_liquid_rewards reward_to_snitch rewarded_name account_map
  in
  let actual_total_burnt_amount =
    Tez.(total_before_slash -! total_after_slash -! reward_to_snitch)
  in
  (account_map, actual_total_burnt_amount)

(* Given cycle is the cycle for which the rights are computed, usually current +
   consensus rights delay *)
let update_frozen_rights_cycle cycle account_map =
  String.Map.map
    (fun ({frozen_deposits; frozen_rights; _} as acc) ->
      let total_frozen = Frozen_tez.total_current frozen_deposits in
      let frozen_rights = CycleMap.add cycle total_frozen frozen_rights in
      {acc with frozen_rights})
    account_map
