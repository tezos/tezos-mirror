(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Misbehaviour_repr = struct
  open Protocol.Misbehaviour_repr

  let pp fmt {level; round; kind} =
    Format.fprintf
      fmt
      "misbehaviour: %s at level %a round %a"
      (match kind with
      | Double_baking -> "double baking"
      | Double_attesting -> "double attesting"
      | Double_preattesting -> "double preattesting")
      Protocol.Raw_level_repr.pp
      level
      Protocol.Round_repr.pp
      round

  include Compare.Make (struct
    type t = Protocol.Misbehaviour_repr.t

    let compare = Protocol.Misbehaviour_repr.compare
  end)

  let from_duplicate_operation (type a)
      (duplicate_op :
        a Protocol.Alpha_context.Kind.consensus Protocol.Alpha_context.operation)
      =
    let ( ({slot = _; level; round; block_payload_hash = _} :
            Protocol.Alpha_context.consensus_content),
          kind ) =
      match duplicate_op.protocol_data.contents with
      | Single (Preattestation consensus_content) ->
          (consensus_content, Double_preattesting)
      | Single (Attestation {consensus_content; _}) ->
          (consensus_content, Double_attesting)
    in
    let level =
      Protocol.Alpha_context.Raw_level.Internal_for_tests.to_repr level
    in
    let round = Protocol.Alpha_context.Round.Internal_for_tests.to_repr round in
    {level; round; kind}

  let check_from_duplicate_operation ~loc misbehaviour duplicate_op =
    Assert.equal
      ~loc
      equal
      "misbehaviours are not equal"
      pp
      misbehaviour
      (from_duplicate_operation duplicate_op)

  let from_duplicate_block (b : Block.t) =
    let open Result_wrap_syntax in
    let open Result_syntax in
    let*@ level = Protocol.Raw_level_repr.of_int32 b.header.shell.level in
    let*@ round = Protocol.Fitness_repr.round_from_raw b.header.shell.fitness in
    return {kind = Double_baking; level; round}
end

module Denunciations_repr = struct
  open Protocol.Denunciations_repr

  let pp_item fmt {operation_hash = _; rewarded; misbehaviour} =
    Format.fprintf
      fmt
      "rewarded: %a; %a"
      Signature.Public_key_hash.pp
      rewarded
      Misbehaviour_repr.pp
      misbehaviour

  let compare_item_except_hash
      {operation_hash = _; rewarded = r1; misbehaviour = m1}
      {operation_hash = _; rewarded = r2; misbehaviour = m2} =
    Compare.or_else (Protocol.Misbehaviour_repr.compare m1 m2) @@ fun () ->
    Signature.Public_key_hash.compare r1 r2
end

module Full_denunciation = struct
  open Protocol.Denunciations_repr

  type t = Signature.Public_key_hash.t * item

  let pp fmt (culprit, item) =
    Format.fprintf
      fmt
      "culprit: %a; %a"
      Signature.Public_key_hash.pp
      culprit
      Denunciations_repr.pp_item
      item

  let compare_except_hash (culprit1, item1) (culprit2, item2) =
    Compare.or_else (Signature.Public_key_hash.compare culprit1 culprit2)
    @@ fun () -> Denunciations_repr.compare_item_except_hash item1 item2

  let check_same_lists_any_order ~loc list1 list2 =
    Assert.equal_list_any_order
      ~loc
      ~compare:compare_except_hash
      "denunciation lists are not the same (not taking order into account)"
      pp
      list1
      list2
end

let apply_slashing_account
    ( culprit,
      Protocol.Denunciations_repr.{rewarded; misbehaviour; operation_hash = _}
    ) constants (account_map : State_account.account_map) =
  let open State_account in
  let find_account_name_from_pkh_exn pkh account_map =
    match
      Option.map
        fst
        String.Map.(
          choose
          @@ filter
               (fun _ account ->
                 Signature.Public_key_hash.equal pkh account.State_account.pkh)
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
             State_account.pkh = _;
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

let apply_slashing_state
    ( culprit,
      Protocol.Denunciations_repr.{rewarded; misbehaviour; operation_hash} )
    (state : State.t) : State.t * Tez_helpers.t =
  let account_map, total_burnt =
    apply_slashing_account
      (culprit, {rewarded; misbehaviour; operation_hash})
      state.constants
      state.account_map
  in
  (* TODO: add culprit's stakers *)
  let log_updates =
    List.map
      (fun x -> fst @@ State.find_account_from_pkh x state)
      [culprit; rewarded]
  in
  let state = State.update_map ~log_updates ~f:(fun _ -> account_map) state in
  (state, total_burnt)

let apply_all_slashes_at_cycle_end current_cycle (state : State.t) : State.t =
  let to_slash_later, to_slash_now =
    if
      not
        state.constants
          .Protocol.Alpha_context.Constants.Parametric.adaptive_issuance
          .ns_enable
    then ([], state.pending_slashes)
    else
      List.partition
        (fun (_, Protocol.Denunciations_repr.{misbehaviour; _}) ->
          let cycle =
            Block.current_cycle_of_level
              ~blocks_per_cycle:
                state.constants
                  .Protocol.Alpha_context.Constants.Parametric.blocks_per_cycle
              ~current_level:
                (Protocol.Raw_level_repr.to_int32 misbehaviour.level)
          in
          Protocol.Alpha_context.Cycle.(cycle = current_cycle))
        state.pending_slashes
  in
  let state, total_burnt =
    List.fold_left
      (fun (acc_state, acc_total) x ->
        let state, burnt = apply_slashing_state x acc_state in
        (state, Tez_helpers.(acc_total +! burnt)))
      (state, Tez_helpers.zero)
      to_slash_now
  in
  let total_supply = Tez_helpers.(state.total_supply -! total_burnt) in
  {state with pending_slashes = to_slash_later; total_supply}
