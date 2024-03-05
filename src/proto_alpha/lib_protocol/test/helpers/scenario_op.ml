(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module gathers many protocol operations in the form of scenarios.
    This includes (but is not limited to) transfers and such (stake,
    unstake...), as well as various ways to forge double signings. *)

open Log_helpers
open State_account
open Adaptive_issuance_helpers
open Scenario_dsl
open Scenario_base
open Scenario_bake
open Tez_helpers.Ez_tez

(** Set delegate parameters for the given delegate *)
let set_delegate_params delegate_name parameters : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      (* Simple example of action_atom definition: *)
      let delegate = State.find_account delegate_name state in
      Log.info
        ~color:action_color
        "[Set delegate parameters for \"%s\"]"
        delegate_name ;
      (* Define the operation *)
      let* operation =
        set_delegate_parameters (B block) delegate.contract ~parameters
      in
      (* Update state *)
      let wait = state.constants.delegate_parameters_activation_delay - 1 in
      let state =
        {
          state with
          param_requests =
            (delegate_name, parameters, wait) :: state.param_requests;
        }
      in
      (* Return both *)
      return (state, [operation]))

(** Add a new account with the given name *)
let add_account name : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      Log.info ~color:action_color "[Add account \"%s\"]" name ;
      let new_account = Account.new_account () in
      let pkh = new_account.pkh in
      let contract = Protocol.Alpha_context.Contract.Implicit pkh in
      let account_state =
        init_account ~pkh ~contract ~parameters:default_params ()
      in
      let state = State.update_account name account_state state in
      return state)

(** Reveal operation *)
let reveal name : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let account = State.find_account name state in
      Log.info ~color:action_color "[Reveal \"%s\"]" name ;
      let* acc = Account.find account.pkh in
      let* operation =
        Op.revelation ~fee:Protocol.Alpha_context.Tez.zero (B block) acc.pk
      in
      return (state, [operation]))

(** Transfer from src to dst *)
let transfer src_name dst_name amount : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      let dst = State.find_account dst_name state in
      let amount = quantity_to_tez src.liquid amount in
      Log.info
        ~color:action_color
        "[Transfer \"%s\" -> \"%s\" (%aꜩ)]"
        src_name
        dst_name
        Tez.pp
        amount ;
      let* operation =
        Op.transaction ~fee:Tez.zero (B block) src.contract dst.contract amount
      in
      let state = State.apply_transfer amount src_name dst_name state in
      return (state, [operation]))

(** Set delegate for src. If [delegate_name_opt = None], then unset current delegate *)
let set_delegate src_name delegate_name_opt : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      let delegate_pkh_opt =
        match delegate_name_opt with
        | None ->
            Log.info ~color:action_color "[Unset delegate of \"%s\"]" src_name ;
            None
        | Some delegate_name ->
            let delegate = State.find_account delegate_name state in
            Log.info
              ~color:action_color
              "[Set delegate \"%s\" for \"%s\"]"
              delegate_name
              src_name ;
            Some delegate.pkh
      in
      let cycle = Block.current_cycle block in
      let* operation =
        Op.delegation ~fee:Tez.zero (B block) src.contract delegate_pkh_opt
      in
      let balance = balance_of_account src_name state.account_map in
      let state =
        if Q.(equal balance.staked_b zero) then state
        else
          let state = State.apply_unstake cycle Tez.max_tez src_name state in
          (* Changing delegate applies finalize if unstake happened *)
          State.apply_finalize src_name state
      in
      let state = State.update_delegate src_name delegate_name_opt state in
      return (state, [operation]))

(** Stake operation *)
let stake src_name stake_value : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      Log.info
        ~color:action_color
        "[Stake for \"%s\" (%a)]"
        src_name
        tez_quantity_pp
        stake_value ;
      (* Stake applies finalize *before* the stake *)
      let state = State.apply_finalize src_name state in
      let amount = quantity_to_tez src.liquid stake_value in
      let current_cycle = Block.current_cycle block in
      let* operation = stake (B block) src.contract amount in
      let state = State.apply_stake amount current_cycle src_name state in
      return (state, [operation]))

(** unstake operation *)
let unstake src_name unstake_value : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      Log.info
        ~color:action_color
        "[Unstake for \"%s\" (%a)]"
        src_name
        tez_quantity_pp
        unstake_value ;
      let stake_balance =
        (balance_of_account src_name state.account_map).staked_b
        |> Partial_tez.to_tez ~round:`Down
      in
      let amount = quantity_to_tez stake_balance unstake_value in
      let* operation = unstake (B block) src.contract amount in
      let cycle = Block.current_cycle block in
      let balance = balance_of_account src_name state.account_map in
      let state =
        if Q.(equal balance.staked_b zero) then state
        else
          let state = State.apply_unstake cycle amount src_name state in
          State.apply_finalize src_name state
      in
      return (state, [operation]))

(** finalize unstake operation *)
let finalize_unstake src_name : (t, t) scenarios =
  exec_op (fun (block, state) ->
      let open Lwt_result_syntax in
      let src = State.find_account src_name state in
      Log.info ~color:action_color "[Finalize_unstake for \"%s\"]" src_name ;
      let* operation = finalize_unstake (B block) src.contract in
      let state = State.apply_finalize src_name state in
      return (state, [operation]))

(* ======== Slashing ======== *)

let check_pending_slashings ~loc (block, state) : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* denunciations_rpc = Context.get_denunciations (B block) in
  Slashing_helpers.Full_denunciation.check_same_lists_any_order
    ~loc
    denunciations_rpc
    state.State.pending_slashes

(** Double attestation helpers *)
let order_attestations ~correct_order op1 op2 =
  let oph1 = Protocol.Alpha_context.Operation.hash op1 in
  let oph2 = Protocol.Alpha_context.Operation.hash op2 in
  let c = Operation_hash.compare oph1 oph2 in
  if correct_order then if c < 0 then (op1, op2) else (op2, op1)
  else if c < 0 then (op2, op1)
  else (op1, op2)

let op_double_attestation ?(correct_order = true) op1 op2 ctxt =
  let e1, e2 = order_attestations ~correct_order op1 op2 in
  Op.double_attestation ctxt e1 e2

let op_double_preattestation ?(correct_order = true) op1 op2 ctxt =
  let e1, e2 = order_attestations ~correct_order op1 op2 in
  Op.double_preattestation ctxt e1 e2

let order_block_hashes ~correct_order bh1 bh2 =
  let hash1 = Protocol.Alpha_context.Block_header.hash bh1 in
  let hash2 = Protocol.Alpha_context.Block_header.hash bh2 in
  let c = Block_hash.compare hash1 hash2 in
  if correct_order then if c < 0 then (bh1, bh2) else (bh2, bh1)
  else if c < 0 then (bh2, bh1)
  else (bh1, bh2)

let op_double_baking ?(correct_order = true) bh1 bh2 ctxt =
  let bh1, bh2 = order_block_hashes ~correct_order bh1 bh2 in
  Op.double_baking ctxt bh1 bh2

let double_bake_ delegate_name (block, state) =
  let open Lwt_result_syntax in
  Log.info ~color:event_color "Double baking with %s" delegate_name ;
  let delegate = State.find_account delegate_name state in
  let* operation =
    Adaptive_issuance_helpers.unstake (B block) delegate.contract Tez.one_mutez
  in
  let* forked_block =
    Block.bake ~policy:(By_account delegate.pkh) ~operation block
  in
  (* includes pending operations *)
  let* main_branch, state = bake ~baker:delegate_name (block, state) in
  let evidence = op_double_baking main_branch.header forked_block.header in
  let*? misbehaviour =
    Slashing_helpers.Misbehaviour_repr.from_duplicate_block main_branch
  in
  let dss =
    {State.culprit = delegate.pkh; denounced = false; evidence; misbehaviour}
  in
  let state =
    {state with double_signings = dss :: state.State.double_signings}
  in
  return (main_branch, state)

(* Note: advances one block *)
let double_bake delegate_name : (t, t) scenarios =
  exec (double_bake_ delegate_name)

(* [other_bakers] can be used to force using specific bakers to avoid
   reusing forbidden ones *)
let double_attest_op ?other_bakers ~op ~op_evidence ~kind delegate_name
    (block, state) =
  let open Lwt_result_syntax in
  Log.info
    ~color:event_color
    "Double %s with %s"
    (match kind with
    | Protocol.Misbehaviour_repr.Double_preattesting -> "preattesting"
    | Double_attesting -> "attesting"
    | Double_baking -> assert false)
    delegate_name ;
  let delegate = State.find_account delegate_name state in
  let* baker, _, _, _ =
    Block.get_next_baker ?policy:state.baking_policy block
  in
  let* other_baker1, other_baker2 =
    match other_bakers with
    | Some (ob1, ob2) ->
        let ob1 = (State.find_account ob1 state).pkh in
        let ob2 = (State.find_account ob2 state).pkh in
        return (ob1, ob2)
    | None -> Context.get_first_different_bakers (B block)
  in
  let other_baker =
    if not (Signature.Public_key_hash.equal baker other_baker2) then
      other_baker2
    else other_baker1
  in
  let* forked_block = Block.bake ~policy:(By_account other_baker) block in
  let* forked_block = Block.bake ?policy:state.baking_policy forked_block in
  (* includes pending operations *)
  let* block, state = bake (block, state) in
  let* main_branch, state = bake (block, state) in
  let* attestation_a = op ~delegate:delegate.pkh forked_block in
  let* attestation_b = op ~delegate:delegate.pkh main_branch in
  let evidence = op_evidence attestation_a attestation_b in
  let dss =
    {
      State.culprit = delegate.pkh;
      denounced = false;
      evidence;
      misbehaviour =
        Slashing_helpers.Misbehaviour_repr.from_duplicate_operation
          attestation_a;
    }
  in
  let state =
    {state with double_signings = dss :: state.State.double_signings}
  in
  return (main_branch, state)

let double_attest_ =
  double_attest_op
    ~op:(fun ~delegate block -> Op.raw_attestation ~delegate block)
    ~op_evidence:op_double_attestation
    ~kind:Double_attesting

(* Note: advances two blocks *)
let double_attest ?other_bakers delegate_name : (t, t) scenarios =
  exec (double_attest_ ?other_bakers delegate_name)

let double_preattest_ =
  double_attest_op
    ~op:(fun ~delegate block -> Op.raw_preattestation ~delegate block)
    ~op_evidence:op_double_preattestation
    ~kind:Double_preattesting

(* Note: advances two blocks *)
let double_preattest ?other_bakers delegate_name : (t, t) scenarios =
  exec (double_preattest_ ?other_bakers delegate_name)

let cycle_from_level blocks_per_cycle level =
  let current_cycle = Int32.div level blocks_per_cycle in
  let current_cycle = Cycle.add Cycle.root (Int32.to_int current_cycle) in
  current_cycle

let pct_from_kind (block : Block.t) = function
  | Protocol.Misbehaviour_repr.Double_baking ->
      Protocol.Percentage.to_q
        block.constants.percentage_of_frozen_deposits_slashed_per_double_baking
      |> Q.(mul (100 // 1))
      |> Q.to_int
  | Double_attesting | Double_preattesting ->
      Protocol.Percentage.to_q
        block.constants
          .percentage_of_frozen_deposits_slashed_per_double_attestation
      |> Q.(mul (100 // 1))
      |> Q.to_int

let get_pending_slashed_pct_for_delegate (block, state) delegate =
  let rec aux r = function
    | [] -> r
    | (culprit, {Protocol.Denunciations_repr.misbehaviour; _}) :: t ->
        if Signature.Public_key_hash.equal delegate culprit then
          let new_r = r + pct_from_kind block misbehaviour.kind in
          if new_r >= 100 then 100 else aux new_r t
        else aux r t
  in
  aux 0 state.State.pending_slashes

let update_state_denunciation (block, state)
    {State.culprit; denounced; evidence = _; misbehaviour} =
  let open Lwt_result_syntax in
  if denounced then
    (* If the double signing has already been denounced, a second denunciation should fail *)
    return (state, denounced)
  else
    let*? block_level = Context.get_level (B block) in
    let next_level =
      Protocol.Alpha_context.Raw_level.(to_int32 @@ succ block_level)
    in
    let inclusion_cycle =
      cycle_from_level block.constants.blocks_per_cycle next_level
    in
    let ds_level = Protocol.Raw_level_repr.to_int32 misbehaviour.level in
    let ds_cycle = cycle_from_level block.constants.blocks_per_cycle ds_level in
    if Cycle.(ds_cycle > inclusion_cycle) then
      (* The denunciation is trying to be included too early *)
      return (state, denounced)
    else if
      Cycle.(
        add ds_cycle Protocol.Constants_repr.max_slashing_period
        <= inclusion_cycle)
    then
      (* The denunciation is too late and gets refused. *)
      return (state, denounced)
    else if get_pending_slashed_pct_for_delegate (block, state) culprit >= 100
    then
      (* Culprit has been slashed too much, a denunciation is not added to the list.
         TODO: is the double signing treated as included, or can it be included in the
         following cycle? *)
      return (state, denounced)
    else
      (* for simplicity's sake (lol), the block producer and the payload producer are the same
         We also assume that the current state baking policy will be used for the next block *)
      let* rewarded, _, _, _ =
        Block.get_next_baker ?policy:state.baking_policy block
      in
      let culprit_name, culprit_account =
        State.find_account_from_pkh culprit state
      in
      let state =
        State.update_account
          culprit_name
          {
            culprit_account with
            slashed_cycles = inclusion_cycle :: culprit_account.slashed_cycles;
          }
          state
      in
      let new_pending_slash =
        ( culprit,
          {
            Protocol.Denunciations_repr.rewarded;
            misbehaviour;
            operation_hash = Operation_hash.zero;
            (* unused *)
          } )
      in
      (* TODO: better log... *)
      Log.info
        ~color:event_color
        "Including denunciation (misbehaviour cycle %a)"
        Cycle.pp
        ds_cycle ;
      let state =
        State.
          {
            state with
            pending_slashes = new_pending_slash :: state.pending_slashes;
          }
      in
      return (state, true)

let make_denunciations_ ?(filter = fun {State.denounced; _} -> not denounced)
    (block, state) =
  let open Lwt_result_syntax in
  let* () = check_pending_slashings ~loc:__LOC__ (block, state) in
  let make_op state ({State.evidence; _} as dss) =
    if filter dss then
      let* state, denounced = update_state_denunciation (block, state) dss in
      return (Some (evidence (B block), {dss with denounced}, state))
    else return None
  in
  let rec make_op_list dss_list state r_op r_dss =
    match dss_list with
    | d :: t -> (
        let* new_op = make_op state d in
        match new_op with
        | None -> make_op_list t state r_op (d :: r_dss)
        | Some (op, p_dss, new_state) ->
            make_op_list t new_state (op :: r_op) (p_dss :: r_dss))
    | [] -> return @@ (state, List.rev r_op, List.rev r_dss)
  in
  let* state, operations, double_signings =
    make_op_list state.double_signings state [] []
  in
  let state = {state with double_signings} in
  return (state, operations)

(* Important note: do not change the baking policy behaviour once denunciations are made,
   until the operations are included in a block (by default the next block) *)
let make_denunciations ?filter () = exec_op (make_denunciations_ ?filter)

(** Create an account and give an initial balance funded by [funder] *)
let add_account_with_funds name ~funder amount =
  add_account name --> transfer funder name amount --> reveal name
