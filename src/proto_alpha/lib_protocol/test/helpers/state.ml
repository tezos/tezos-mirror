(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Adaptive_issuance_helpers
open State_account
open Log_helpers

type double_signing_state = {
  culprit : Signature.Public_key_hash.t;
  evidence : Context.t -> Protocol.Alpha_context.packed_operation;
  denounced : bool;
  misbehaviour : Protocol.Misbehaviour_repr.t;
}

(** Type of the state *)
type t = {
  account_map : account_map;
  total_supply : Tez.t;
  constants : Protocol.Alpha_context.Constants.Parametric.t;
  param_requests : (string * staking_parameters * int) list;
  activate_ai : bool;
  baking_policy : Block.baker_policy option;
  last_level_rewards : Protocol.Alpha_context.Raw_level.t;
  snapshot_balances : (string * balance) list String.Map.t;
  saved_rate : Q.t option;
  burn_rewards : bool;
  pending_operations : Protocol.Alpha_context.packed_operation list;
  pending_slashes :
    (Signature.Public_key_hash.t * Protocol.Denunciations_repr.item) list;
  double_signings : double_signing_state list;
  ai_activation_cycle : Protocol.Alpha_context.Cycle.t option;
}

(** Expected number of cycles before staking parameters get applied *)
let param_wait state = state.constants.delegate_parameters_activation_delay + 1

(** Expected number of cycles before staking unstaked funds get unfrozen *)
let unstake_wait state =
  let pc = state.constants.consensus_rights_delay in
  let msp = Protocol.Constants_repr.max_slashing_period in
  pc + msp

(** From a name, returns the corresponding account *)
let find_account (account_name : string) (state : t) : account_state =
  match String.Map.find account_name state.account_map with
  | None ->
      Log.error "State.find_account: account %s not found" account_name ;
      assert false
  | Some r -> r

let find_account_from_pkh (pkh : Signature.public_key_hash) (state : t) :
    string * account_state =
  String.Map.filter
    (fun _ acc -> Signature.Public_key_hash.equal pkh acc.pkh)
    state.account_map
  |> String.Map.choose
  |> function
  | None ->
      Log.error
        "State.find_account_from_pkh: account %a not found"
        Signature.Public_key_hash.pp
        pkh ;
      assert false
  | Some (name, acc) -> (name, acc)

let liquid_delegated ~name state =
  let open Result_syntax in
  String.Map.fold_e
    (fun _delegator account acc ->
      match account.delegate with
      | Some delegate when not @@ String.equal delegate name -> return acc
      | None -> return acc
      | _ -> Tez.(acc +? account.liquid))
    state.account_map
    Tez.zero

(** Returns true iff account is a delegate *)
let is_self_delegate (account_name : string) (state : t) : bool =
  let acc = find_account account_name state in
  match acc.delegate with
  | None -> false
  | Some del_name -> String.equal del_name account_name

let update_map ?(log_updates = []) ~(f : account_map -> account_map) (state : t)
    : t =
  let log_updates = List.sort_uniq String.compare log_updates in
  let new_state = {state with account_map = f state.account_map} in
  List.iter
    (fun x ->
      log_debug_balance_update x state.account_map new_state.account_map)
    log_updates ;
  new_state

let apply_burn amount src_name (state : t) : t =
  let f = apply_burn amount src_name in
  let state = update_map ~log_updates:[src_name] ~f state in
  {state with total_supply = Tez.(state.total_supply -! amount)}

let apply_transfer amount src_name dst_name (state : t) : t =
  let f = apply_transfer amount src_name dst_name in
  update_map ~log_updates:[src_name; dst_name] ~f state

let apply_stake amount current_cycle staker_name (state : t) : t =
  let f =
    apply_stake
      amount
      current_cycle
      state.constants.consensus_rights_delay
      staker_name
  in
  update_map ~log_updates:[staker_name] ~f state

let apply_unstake cycle amount staker_name (state : t) : t =
  let f = apply_unstake cycle amount staker_name in
  update_map ~log_updates:[staker_name] ~f state

let apply_finalize staker_name (state : t) : t =
  let f = apply_finalize staker_name in
  update_map ~log_updates:[staker_name] ~f state

let apply_unslashable current_cycle account_name (state : t) : t =
  let unstake_wait = unstake_wait state in
  match Cycle.sub current_cycle unstake_wait with
  | None -> state
  | Some cycle ->
      let f = apply_unslashable cycle account_name in
      update_map ~log_updates:[account_name] ~f state

let apply_unslashable_for_all current_cycle (state : t) : t =
  let unstake_wait = unstake_wait state in
  match Cycle.sub current_cycle unstake_wait with
  | None -> state
  | Some cycle ->
      let f = apply_unslashable_for_all cycle in
      (* no log *)
      update_map ~f state

let apply_rewards ~(baker : string) block (state : t) : t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let {last_level_rewards; total_supply; constants = _; _} = state in
  let*? current_level = Context.get_level (B block) in
  let current_cycle = Block.current_cycle block in
  (* We assume one block per minute *)
  let* rewards_per_block = Context.get_issuance_per_minute (B block) in
  if Tez.(rewards_per_block = zero) then return state
  else
    let delta_time =
      Protocol.Alpha_context.Raw_level.diff current_level last_level_rewards
      |> Int64.of_int32
    in
    let {parameters = _; pkh; _} = find_account baker state in
    let delta_rewards = Tez_helpers.(rewards_per_block *! delta_time) in
    if delta_time = 1L then
      Log.info ~color:tez_color "+%aꜩ" Tez.pp rewards_per_block
    else assert false ;
    let* to_liquid =
      portion_of_rewards_to_liquid_for_cycle
        ?policy:state.baking_policy
        (B block)
        current_cycle
        pkh
        delta_rewards
    in
    let to_frozen = Tez.(delta_rewards -! to_liquid) in
    let state = update_map ~f:(add_liquid_rewards to_liquid baker) state in
    let state = update_map ~f:(add_frozen_rewards to_frozen baker) state in
    let*? total_supply = Tez.(total_supply +? delta_rewards) in
    return {state with last_level_rewards = current_level; total_supply}

let apply_slashing
    ( culprit,
      Protocol.Denunciations_repr.{rewarded; misbehaviour; operation_hash} )
    (state : t) : t * Tez.t =
  let account_map, total_burnt =
    apply_slashing
      (culprit, {rewarded; misbehaviour; operation_hash})
      state.constants
      state.account_map
  in
  (* TODO: add culprit's stakers *)
  let log_updates =
    List.map (fun x -> fst @@ find_account_from_pkh x state) [culprit; rewarded]
  in
  let state = update_map ~log_updates ~f:(fun _ -> account_map) state in
  (state, total_burnt)

let apply_all_slashes_at_cycle_end current_cycle (state : t) : t =
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
          Cycle.(cycle = current_cycle))
        state.pending_slashes
  in
  let state, total_burnt =
    List.fold_left
      (fun (acc_state, acc_total) x ->
        let state, burnt = apply_slashing x acc_state in
        (state, Tez.(acc_total +! burnt)))
      (state, Tez.zero)
      to_slash_now
  in
  let total_supply = Tez.(state.total_supply -! total_burnt) in
  {state with pending_slashes = to_slash_later; total_supply}

(** Given an account name and new account state, updates [state] accordingly
      Preferably use other specific update functions *)
let update_account (account_name : string) (value : account_state) (state : t) :
    t =
  let account_map = String.Map.add account_name value state.account_map in
  {state with account_map}

let update_delegate account_name delegate_name_opt state : t =
  let account = find_account account_name state in
  update_account account_name {account with delegate = delegate_name_opt} state

let add_pending_operations operations state =
  {state with pending_operations = state.pending_operations @ operations}

let pop_pending_operations state =
  ({state with pending_operations = []}, state.pending_operations)

let log_model_autostake name pkh old_cycle op ~optimal amount =
  Log.debug
    "Model Autostaking: at end of cycle %a, %s(%a) to reach optimal stake %a \
     %s %a"
    Cycle.pp
    old_cycle
    name
    Signature.Public_key_hash.pp
    pkh
    Tez.pp
    optimal
    op
    Tez.pp
    (Tez.of_mutez amount)

let apply_autostake ~name ~old_cycle
    ({
       pkh;
       contract = _;
       delegate;
       parameters = _;
       liquid;
       bonds = _;
       frozen_deposits;
       unstaked_frozen;
       unstaked_finalizable;
       staking_delegator_numerator = _;
       staking_delegate_denominator = _;
       frozen_rights = _;
       slashed_cycles = _;
     } :
      account_state) state =
  let open Result_syntax in
  if Some name <> delegate then (
    Log.debug
      "Model Autostaking: %s <> %s, noop@."
      name
      (Option.value ~default:"None" delegate) ;
    return state)
  else
    let* current_liquid_delegated = liquid_delegated ~name state in
    let current_frozen = Frozen_tez.total_current frozen_deposits in
    let current_unstaked_frozen_delegated =
      Unstaked_frozen.sum_current unstaked_frozen
    in
    let current_unstaked_final_delegated =
      Unstaked_finalizable.total unstaked_finalizable
    in
    let power =
      Tez.(
        current_liquid_delegated +! current_frozen
        +! current_unstaked_frozen_delegated +! current_unstaked_final_delegated
        |> to_mutez |> Z.of_int64)
    in
    let optimal =
      Tez.of_z
        (Z.cdiv
           power
           (Z.of_int (state.constants.limit_of_delegation_over_baking + 1)))
    in
    let autostaked =
      Int64.(sub (Tez.to_mutez optimal) (Tez.to_mutez current_frozen))
    in
    let state = apply_unslashable (Cycle.succ old_cycle) name state in
    let state = apply_finalize name state in
    (* stake or unstake *)
    let new_state =
      if autostaked > 0L then (
        log_model_autostake ~optimal name pkh old_cycle "stake" autostaked ;
        apply_stake
          Tez.(min liquid (of_mutez autostaked))
          (Cycle.succ old_cycle)
          name
          state)
      else if autostaked < 0L then (
        log_model_autostake
          ~optimal
          name
          pkh
          old_cycle
          "unstake"
          (Int64.neg autostaked) ;
        apply_unstake
          (Cycle.succ old_cycle)
          (Tez_helpers.of_mutez Int64.(neg autostaked))
          name
          state)
      else (
        log_model_autostake
          ~optimal
          name
          pkh
          old_cycle
          "only finalize"
          autostaked ;
        state)
    in
    return new_state

(** Applies when baking the last block of a cycle *)
let apply_end_cycle current_cycle block state : t tzresult Lwt.t =
  let open Lwt_result_syntax in
  Log.debug ~color:time_color "Ending cycle %a" Cycle.pp current_cycle ;
  let* launch_cycle_opt =
    Context.get_adaptive_issuance_launch_cycle (B block)
  in
  (* Apply all slashes *)
  let state = apply_all_slashes_at_cycle_end current_cycle state in
  (* Sets initial frozen for future cycle *)
  let state =
    update_map
      ~f:
        (update_frozen_rights_cycle
           (Cycle.add
              current_cycle
              (state.constants.consensus_rights_delay + 1)))
      state
  in
  (* Apply autostaking *)
  let*? state =
    if not state.constants.adaptive_issuance.autostaking_enable then Ok state
    else
      match launch_cycle_opt with
      | Some launch_cycle when Cycle.(current_cycle >= launch_cycle) -> Ok state
      | None | Some _ ->
          String.Map.fold_e
            (fun name account state ->
              apply_autostake ~name ~old_cycle:current_cycle account state)
            state.account_map
            state
  in
  (* Apply parameter changes *)
  let state, param_requests =
    List.fold_left
      (fun (state, remaining_requests) (name, params, wait) ->
        if wait > 0 then (state, (name, params, wait - 1) :: remaining_requests)
        else
          let src = find_account name state in
          let state =
            update_account name {src with parameters = params} state
          in
          (state, remaining_requests))
      (state, [])
      state.param_requests
  in
  return {state with param_requests}

(** Applies when baking the first block of a cycle.
      Technically nothing special happens, but we need to update the unslashable unstakes
      since it's done lazily *)
let apply_new_cycle new_cycle state : t =
  apply_unslashable_for_all new_cycle state
