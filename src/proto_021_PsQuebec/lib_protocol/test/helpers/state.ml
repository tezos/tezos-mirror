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
  force_ai_vote_yes : bool;
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
  force_attest_all : bool;
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

let update_map_es ?(log_updates = [])
    ~(f : account_map -> account_map tzresult Lwt.t) (state : t) :
    t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let log_updates = List.sort_uniq String.compare log_updates in
  let* account_map = f state.account_map in
  let new_state = {state with account_map} in
  List.iter
    (fun x ->
      log_debug_balance_update x state.account_map new_state.account_map)
    log_updates ;
  return new_state

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
