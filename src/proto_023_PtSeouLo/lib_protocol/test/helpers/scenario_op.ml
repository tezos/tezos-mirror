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

let normalize_parameters
    {limit_of_staking_over_baking; edge_of_baking_over_staking} state =
  let actual_limit =
    Q.(
      mul limit_of_staking_over_baking (1_000_000 // 1)
      |> to_int |> of_int
      |> mul (1 // 1_000_000))
    |> Q.max Q.zero
    |> Q.min
         (Q.of_int
            state.State.constants.adaptive_issuance
              .global_limit_of_staking_over_baking)
  in
  let actual_edge =
    Q.(
      mul edge_of_baking_over_staking (1_000_000_000 // 1)
      |> to_int |> of_int
      |> mul (1 // 1_000_000_000))
    |> Q.max Q.zero |> Q.min Q.one
  in
  {
    limit_of_staking_over_baking = actual_limit;
    edge_of_baking_over_staking = actual_edge;
  }

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
      let norm_parameters = normalize_parameters parameters state in
      let wait = state.constants.delegate_parameters_activation_delay in
      let state =
        {
          state with
          param_requests =
            (delegate_name, norm_parameters, wait) :: state.param_requests;
        }
      in
      (* Return both *)
      return (state, [operation]))

(** Add a new account with the given name *)
let add_account ?algo name : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      Log.info ~color:action_color "[Add account \"%s\"]" name ;
      let new_account = Account.new_account ?algo () in
      let pkh = new_account.pkh in
      let contract = Protocol.Alpha_context.Contract.Implicit pkh in
      let account_state =
        init_account ~name ~pkh ~contract ~parameters:default_params ()
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
      let state =
        State.update_account name {account with revealed = true} state
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

let current_cycle block =
  (* operation will be baked in next block *)
  let predecessor_cycle = Block.current_cycle block in
  if Block.last_block_of_cycle block then Cycle.succ predecessor_cycle
  else predecessor_cycle

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
      let is_not_changing_delegate =
        Option.equal String.equal delegate_name_opt src.delegate
      in
      let current_cycle = current_cycle block in
      let* operation =
        Op.delegation ~fee:Tez.zero (B block) src.contract delegate_pkh_opt
      in
      let balance = balance_of_account src_name state.account_map in
      let state =
        if Q.(equal balance.staked_b zero) || is_not_changing_delegate then
          state
        else
          let state =
            State.apply_unstake current_cycle Tez.max_tez src_name state
          in
          (* Changing delegate applies finalize if unstake happened *)
          State.apply_finalize src_name state
      in
      let state = State.update_delegate src_name delegate_name_opt state in
      (* update delegate activation status *)
      let state =
        (* if self delegating *)
        if Option.equal String.equal delegate_name_opt (Some src_name) then
          let activity_cycle = current_cycle in
          State.update_account_f
            src_name
            (Account_helpers.update_activity state.constants activity_cycle)
            state
        else state
      in
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
      let current_cycle = current_cycle block in
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
      let cycle = current_cycle block in
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

let update_consensus_key_ ?proof_signer ?(force_no_signer = false) ~ck_name
    src_name (block, state) =
  let open Lwt_result_syntax in
  let delegate = State.find_account src_name state in
  let ck = State.find_account ck_name state in
  let* {pk = consensus_pk; pkh = consensus_pkh; sk = _} = Account.find ck.pkh in
  Log.info
    ~color:action_color
    "[Update consensus key for \"%s\" to %a]"
    src_name
    Signature.Public_key_hash.pp
    consensus_pkh ;
  let current_cycle = current_cycle block in
  let proof_signer =
    match
      (force_no_signer, proof_signer, (consensus_pk : Signature.public_key))
    with
    | true, _, _ -> None
    | false, Some proof_signer_name, _ ->
        Some (State.find_account proof_signer_name state).contract
    | false, None, Signature.Bls _ ->
        Some (Protocol.Alpha_context.Contract.Implicit consensus_pkh)
    | _ -> None
  in
  let* operation =
    Op.update_consensus_key
      ?proof_signer
      ~fee:Tez_helpers.zero
      (B block)
      delegate.contract
      consensus_pk
  in
  let state =
    State.apply_update_consensus_key src_name current_cycle consensus_pkh state
  in
  return (state, [operation])

let update_consensus_key ?proof_signer ?(force_no_signer = false) ~ck_name
    src_name : (t, t) scenarios =
  exec_op
    (update_consensus_key_ ?proof_signer ~force_no_signer ~ck_name src_name)

let update_companion_key_ ?proof_signer ?(force_no_signer = false) ~ck_name
    src_name (block, state) =
  let open Lwt_result_syntax in
  let delegate = State.find_account src_name state in
  let ck = State.find_account ck_name state in
  let* {pk = companion_pk; pkh = companion_pkh; sk = _} = Account.find ck.pkh in
  Log.info
    ~color:action_color
    "[Update companion key for \"%s\" to %a]"
    src_name
    Signature.Public_key_hash.pp
    companion_pkh ;
  let current_cycle = current_cycle block in
  let proof_signer =
    match
      (force_no_signer, proof_signer, (companion_pk : Signature.public_key))
    with
    | true, _, _ -> None
    | false, Some proof_signer_name, _ ->
        Some (State.find_account proof_signer_name state).contract
    | false, None, Signature.Bls _ ->
        Some (Protocol.Alpha_context.Contract.Implicit companion_pkh)
    | _ -> None
  in
  let* operation =
    Op.update_companion_key
      ?proof_signer
      ~fee:Tez_helpers.zero
      (B block)
      delegate.contract
      companion_pk
  in
  let state =
    State.apply_update_companion_key src_name current_cycle companion_pkh state
  in
  return (state, [operation])

let update_companion_key ?proof_signer ?(force_no_signer = false) ~ck_name
    src_name : (t, t) scenarios =
  exec_op
    (update_companion_key_ ?proof_signer ~force_no_signer ~ck_name src_name)

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

(** [double_bake_op delegate_names (block, state)] performs a double baking with
    the given delegate names. The first delegate in the list bakes the new main
    branch. All delegates (including the first) will bake two other blocks at
    the same level/different round.  *)
let double_bake_op delegate_names (block, state) =
  let open Lwt_result_syntax in
  Log.info
    ~color:event_color
    "Double baking with (%s)"
    (String.concat ", " delegate_names) ;
  let delegates =
    List.map
      (fun delegate_name -> State.find_account delegate_name state)
      delegate_names
  in
  let* main_branch, state =
    bake
      ~baker:(WithExceptions.Option.get ~loc:__LOC__ @@ List.hd delegate_names)
      (block, state)
  in
  let* state =
    List.fold_left_es
      (fun state delegate ->
        let* operation =
          Adaptive_issuance_helpers.unstake
            (B block)
            delegate.contract
            Tez.one_mutez
        in
        let* forked_block1 =
          Block.bake ~policy:(By_account delegate.pkh) block
        in
        let* forked_block2 =
          Block.bake ~policy:(By_account delegate.pkh) ~operation block
        in
        (* includes pending operations *)
        let evidence =
          op_double_baking forked_block1.header forked_block2.header
        in
        let*? misbehaviour =
          Slashing_helpers.Misbehaviour_repr.from_duplicate_block forked_block1
        in
        let dss =
          {
            State.culprit = delegate.pkh;
            denounced = false;
            evidence;
            misbehaviour;
          }
        in
        return
          {
            state with
            State.double_signings = dss :: state.State.double_signings;
          })
      state
      delegates
  in
  return (main_branch, state)

(* Note: advances one block *)
let double_bake delegate_name : (t, t) scenarios =
  exec (double_bake_op [delegate_name])

let double_bake_many delegate_names : (t, t) scenarios =
  exec (double_bake_op delegate_names)

(** [double_attest_op ?other_bakers ~op ~op_evidence ~kind delegate_names
  (block, state)] performs a double (pre)attestation with the given delegate
  names. Starting at block level `n`, it creates two 2-block branches and all
  delegates will (pre)attest the two blocks at level `n+2`. [other_bakers] can
  be used to force using specific bakers to avoid reusing forbidden ones *)
let double_attest_op ?other_bakers ~op ~op_evidence ~kind delegate_names
    (block, state) =
  let open Lwt_result_syntax in
  Log.info
    ~color:event_color
    "Double %s with %a"
    (match kind with
    | Protocol.Misbehaviour_repr.Double_preattesting -> "preattesting"
    | Double_attesting -> "attesting"
    | Double_baking -> assert false)
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
    delegate_names ;
  let delegates =
    List.map
      (fun delegate_name -> State.find_account delegate_name state)
      delegate_names
  in
  let* baker, _, _, _ =
    Block.get_next_baker ?policy:state.baking_policy block
  in
  Log.info "Baker: %a" Signature.Public_key_hash.pp baker ;
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
  Log.info "Other baker: %a" Signature.Public_key_hash.pp other_baker ;
  Log.info "Bake 1 block with %a" Signature.Public_key_hash.pp baker ;
  let* forked_block = Block.bake ~policy:(By_account other_baker) block in
  Log.info "Bake 1 block " ;
  let* forked_block = Block.bake ?policy:state.baking_policy forked_block in
  Log.info "Baked two blocks" ;
  (* includes pending operations *)
  let* block, state = bake (block, state) in
  let* main_branch, state = bake (block, state) in
  List.fold_left_es
    (fun (main_branch, state) delegate ->
      let manager_pkh = delegate.pkh in
      let* attestation_a = op ~manager_pkh ~attested_block:forked_block in
      let* attestation_b = op ~manager_pkh ~attested_block:main_branch in
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
      let state : State.t =
        {state with double_signings = dss :: state.State.double_signings}
      in
      return (main_branch, state))
    (main_branch, state)
    delegates

let double_attest_ =
  double_attest_op
    ~op:(fun ~manager_pkh ~attested_block ->
      Op.raw_attestation ~manager_pkh attested_block)
    ~op_evidence:op_double_attestation
    ~kind:Double_attesting

(* Note: advances two blocks *)
let double_attest_many ?other_bakers delegate_names : (t, t) scenarios =
  exec (double_attest_ ?other_bakers delegate_names)

let double_attest ?other_bakers delegate_name : (t, t) scenarios =
  double_attest_many ?other_bakers [delegate_name]

let double_preattest_ =
  double_attest_op
    ~op:(fun ~manager_pkh ~attested_block ->
      Op.raw_preattestation ~manager_pkh attested_block)
    ~op_evidence:op_double_preattestation
    ~kind:Double_preattesting

(* Note: advances two blocks *)
let double_preattest_many ?other_bakers delegate_names : (t, t) scenarios =
  exec (double_preattest_ ?other_bakers delegate_names)

let double_preattest ?other_bakers delegate_name : (t, t) scenarios =
  double_preattest_many ?other_bakers [delegate_name]

let cycle_from_level blocks_per_cycle level =
  let current_cycle = Int32.div level blocks_per_cycle in
  let current_cycle = Cycle.add Cycle.root (Int32.to_int current_cycle) in
  current_cycle

let update_state_denunciation (block, state)
    {State.culprit; denounced; evidence = _; misbehaviour} =
  let open Lwt_result_syntax in
  if denounced then (
    (* If the double signing has already been denounced, a second denunciation should fail *)
    Log.info ~color:event_color "Denunciation already included" ;
    return (state, denounced))
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
    if Cycle.(ds_cycle > inclusion_cycle) then (
      (* The denunciation is trying to be included too early *)
      Log.info ~color:event_color "Denunciation too early" ;
      return (state, denounced))
    else if
      Cycle.(
        add ds_cycle Protocol.Constants_repr.denunciation_period
        < inclusion_cycle)
    then (
      (* The denunciation is too late and gets refused. *)
      Log.info ~color:event_color "Denunciation too late" ;
      return (state, denounced))
    else
      (* for simplicity's sake (lol), the block producer and the payload producer are the same
         We also assume that the current state baking policy will be used for the next block *)
      let* rewarded, _, _, _ =
        Block.get_next_baker ?policy:state.State.baking_policy block
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

(** [make_denunciations_op ?single ?rev ?filter ()] denounces all double signers
  in the state. If [single] is set, only one denunciation is made. If [rev] is
  set, the denunciations are made in reverse order. If [filter] is set, only the
  double signers for which the filter returns true are denounced. *)
let make_denunciations_op ?(single = false) ?(rev = false)
    ?(filter = fun {State.denounced; _} -> not denounced) (block, state) =
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
        let open State in
        let* new_op = make_op state d in
        match new_op with
        | None -> make_op_list t state r_op (d :: r_dss)
        | Some (op, p_dss, new_state) ->
            Log.info
              ~color:event_color
              "Denouncing %a for %s at level %a round %a"
              Signature.Public_key_hash.pp
              d.culprit
              (match d.misbehaviour.kind with
              | Double_baking -> "double baking"
              | Double_attesting -> "double attesting"
              | Double_preattesting -> "double preattesting")
              Protocol.Raw_level_repr.pp
              d.misbehaviour.level
              Protocol.Round_repr.pp
              d.misbehaviour.round ;
            if single then
              return @@ (new_state, op :: r_op, List.rev @@ (p_dss :: t))
            else make_op_list t new_state (op :: r_op) (p_dss :: r_dss))
    | [] -> return @@ (state, r_op, r_dss)
  in
  let* state, operations, double_signings =
    make_op_list
      (if rev then state.double_signings else List.rev state.double_signings)
      state
      []
      []
  in
  let state = {state with double_signings} in
  return (state, operations)

(* Important note: do not change the baking policy behaviour once denunciations are made,
   until the operations are included in a block (by default the next block) *)
let make_denunciations ?single ?rev ?filter () =
  exec_op (make_denunciations_op ?single ?rev ?filter)

(** Create an account and give an initial balance funded by [funder] *)
let add_account_with_funds ?algo name ~funder amount =
  add_account ?algo name --> transfer funder name amount --> reveal name

let start_payload : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_, state) -> return {state with State.operation_mode = Wait})

let batch ~source : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_, state) ->
      Log.info ~color:batch_color "[--- Batch for %s ---]" source ;
      return
        {state with State.source_batch = Some source; operation_mode = Batch})

let add_batch_to_operations ?(mode_after = State.Bake) ((block, state) : t) :
    State.t tzresult Lwt.t =
  let open State in
  let open Lwt_result_syntax in
  let ops = state.pending_batch in
  let source = find_account (Stdlib.Option.get state.source_batch) state in
  let* batch =
    Op.batch_operations
      ~recompute_counters:true
      ~source:source.contract
      (B block)
      ops
  in
  let state = add_pending_operations [batch] state in
  return {state with source_batch = None; operation_mode = mode_after}

let end_batch ?(bake_after = false) ?(mode_after = State.Bake) () :
    (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_unit (fun _ ->
      Log.info ~color:batch_color "[--- End Batch ---]" ;
      return_unit)
  --> exec_state (add_batch_to_operations ~mode_after)
  --> if bake_after then next_block else noop
