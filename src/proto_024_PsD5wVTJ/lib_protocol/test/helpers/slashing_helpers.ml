(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let slashing_percentage ~block_before_slash misbehaviour ~all_culprits =
  let open Lwt_result_wrap_syntax in
  let* ctxt = Block.get_alpha_ctxt block_before_slash in
  let raw_ctxt = Protocol.Alpha_context.Internal_for_tests.to_raw ctxt in
  let*@ _, slashing_pct =
    Protocol.Slash_percentage.get raw_ctxt misbehaviour all_culprits
  in
  return slashing_pct

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
    let level, round, kind =
      match duplicate_op.protocol_data.contents with
      | Single (Preattestation {level; round; _}) ->
          (level, round, Double_preattesting)
      | Single (Attestation {consensus_content = {level; round; _}; _}) ->
          (level, round, Double_attesting)
      | Single
          (Preattestations_aggregate {consensus_content = {level; round; _}; _})
        ->
          (level, round, Double_preattesting)
      | Single
          (Attestations_aggregate {consensus_content = {level; round; _}; _}) ->
          (level, round, Double_attesting)
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

  let slashing_percentage ~block_before_slash
      (misbehaviour : Protocol.Misbehaviour_repr.t) all_denunciations_to_apply =
    let all_culprits =
      List.filter
        (fun (_, (den : Protocol.Denunciations_repr.item)) ->
          Misbehaviour_repr.equal misbehaviour den.misbehaviour)
        all_denunciations_to_apply
      |> List.map fst
      |> List.sort_uniq Signature.Public_key_hash.compare
    in
    slashing_percentage ~block_before_slash misbehaviour ~all_culprits
end

let apply_slashing_account all_denunciations_to_apply
    ( culprit,
      Protocol.Denunciations_repr.{rewarded; misbehaviour; operation_hash = _}
    ) (block_before_slash : Block.t) (state : State.t) =
  let open Lwt_result_syntax in
  let open State_account in
  let constants = state.constants in
  let (account_map : State_account.account_map) = state.account_map in
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
  Log.info
    "Slashing %a for %a"
    Signature.Public_key_hash.pp
    culprit
    Misbehaviour_repr.pp
    misbehaviour ;
  let* slashed_pct =
    Full_denunciation.slashing_percentage
      ~block_before_slash
      misbehaviour
      all_denunciations_to_apply
  in
  let slash_culprit
      ({frozen_deposits; unstaked_frozen; frozen_rights; parameters; _} as acc)
      =
    Log.info
      "Slashing %a for %a with frozen deposits: { %a }"
      Signature.Public_key_hash.pp
      acc.pkh
      Misbehaviour_repr.pp
      misbehaviour
      Frozen_tez.pp
      frozen_deposits ;
    let base_rights =
      CycleMap.find slashed_cycle frozen_rights
      |> Option.value ~default:Tez.zero
    in
    Log.info "Base rights: %a" Tez.pp base_rights ;
    let frozen_deposits, burnt_frozen, rewarded_frozen =
      Frozen_tez.slash
        ~limit:parameters.limit_of_staking_over_baking
        state.constants
        base_rights
        slashed_pct
        frozen_deposits
    in
    Log.info
      "Slashed %a of frozen deposits@."
      Protocol.Percentage.Internal_for_tests.pp_human
      slashed_pct ;
    let unstaked_frozen, slashed_unstaked =
      Unstaked_frozen.slash
        state.constants
        ~slashable_deposits_period:constants.consensus_rights_delay
        slashed_cycle
        slashed_pct
        unstaked_frozen
    in
    ( {acc with frozen_deposits; unstaked_frozen},
      (burnt_frozen, rewarded_frozen) :: slashed_unstaked )
  in
  let culprit_account =
    String.Map.find culprit_name account_map
    |> Option.value_f ~default:(fun () ->
           fail_account_not_found "apply_slashing" culprit_name)
  in
  let slashed_culprit_account, total_slashed = slash_culprit culprit_account in
  Log.info "Slashed %a@." Signature.Public_key_hash.pp culprit_account.pkh ;
  let account_map =
    update_account
      ~f:(fun _ -> slashed_culprit_account)
      culprit_name
      account_map
  in
  (* For each container slashed, the snitch gets a reward transferred. It gets rounded
     down each time *)
  let reward_to_snitch =
    List.map snd total_slashed |> List.fold_left Tez.( +! ) Tez.zero
  in
  let account_map =
    add_liquid_rewards reward_to_snitch rewarded_name account_map
  in
  let total_burnt_amount =
    List.map fst total_slashed |> List.fold_left Tez.( +! ) Tez.zero
  in
  Log.info "Total burnt amount: %a" Tez.pp total_burnt_amount ;
  return (account_map, total_burnt_amount)

let apply_slashing_state all_denunciations_to_apply
    ( culprit,
      Protocol.Denunciations_repr.{rewarded; misbehaviour; operation_hash} )
    block_before_slash (state : State.t) :
    (State.t * Tez_helpers.t) tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* account_map, total_burnt =
    apply_slashing_account
      all_denunciations_to_apply
      (culprit, {rewarded; misbehaviour; operation_hash})
      block_before_slash
      state
  in
  (* TODO: add culprit's stakers *)
  let log_updates =
    List.map
      (fun x -> fst @@ State.find_account_from_pkh x state)
      [culprit; rewarded]
  in
  let state = State.update_map ~log_updates ~f:(fun _ -> account_map) state in
  return (state, total_burnt)

let apply_all_slashes_at_cycle_end current_cycle (block_before_slash : Block.t)
    (state : State.t) : State.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let to_slash_now, to_slash_later =
    List.partition
      (fun (_, Protocol.Denunciations_repr.{misbehaviour; _}) ->
        let misb_cycle =
          Block.current_cycle_of_level
            ~blocks_per_cycle:
              state.constants
                .Protocol.Alpha_context.Constants.Parametric.blocks_per_cycle
            ~current_level:(Protocol.Raw_level_repr.to_int32 misbehaviour.level)
        in
        let open Protocol.Alpha_context in
        let misb_slashing_cycle =
          Cycle.add misb_cycle Constants.slashing_delay
        in
        if Cycle.(equal misb_slashing_cycle current_cycle) then
          (* Slash now *)
          true
        else if
          Cycle.(
            misb_slashing_cycle > current_cycle && misb_cycle <= current_cycle)
        then (* Slash later *) false
        else
          Test.fail
            ~__LOC__
            "Pending slash for %a (cycle %a) should not exist at the end of \
             cycle %a"
            Misbehaviour_repr.pp
            misbehaviour
            Cycle.pp
            misb_cycle
            Cycle.pp
            current_cycle)
      state.pending_slashes
  in
  (* Sort to_slash_now by level+round *)
  let to_slash_now =
    List.sort
      (fun (_, item1) (_, item2) ->
        Denunciations_repr.compare_item_except_hash item1 item2)
      to_slash_now
  in
  let* state, total_burnt =
    List.fold_left_es
      (fun (acc_state, acc_total) x ->
        let* state, burnt =
          apply_slashing_state to_slash_now x block_before_slash acc_state
        in
        return (state, Tez_helpers.(acc_total +! burnt)))
      (state, Tez_helpers.zero)
      to_slash_now
  in
  let total_supply = Tez_helpers.(state.total_supply -! total_burnt) in
  return {state with pending_slashes = to_slash_later; total_supply}
