(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open State_account
open State
open Scenario_dsl
open Log_helpers
open Scenario_base
open Adaptive_issuance_helpers

(** Applies when baking the last block of a cycle *)
let apply_end_cycle current_cycle block state : State.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  Log.debug ~color:time_color "Ending cycle %a" Cycle.pp current_cycle ;
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
  let*? state = State_ai_flags.Autostake.run_at_cycle_end block state in
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
let apply_new_cycle new_cycle state : State.t =
  apply_unslashable_for_all new_cycle state

(** After baking and applying rewards in state *)
let check_all_balances block state : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let State.{account_map; total_supply; _} = state in
  let* actual_total_supply = Context.get_total_supply (B block) in
  let*! r1 =
    String.Map.fold_s
      (fun name account acc ->
        log_debug_balance name account_map ;
        let* () = log_debug_rpc_balance name (Implicit account.pkh) block in
        let*! r =
          assert_balance_check ~loc:__LOC__ (B block) name account_map
        in
        Assert.join_errors r acc)
      account_map
      Result.return_unit
  in
  let*! r2 =
    Assert.equal
      ~loc:__LOC__
      Tez.equal
      "Total supplies do not match"
      Tez.pp
      actual_total_supply
      total_supply
  in
  Assert.join_errors r1 r2

let check_issuance_rpc block : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* We assume one block per minute *)
  let* rewards_per_block = Context.get_issuance_per_minute (B block) in
  let* total_supply = Context.get_total_supply (B block) in
  let* expected_issuance = Context.get_ai_expected_issuance (B block) in
  let* () =
    match expected_issuance with
    | ei :: _ ->
        (* We assume only the fixed portion is issued *)
        Assert.equal_tez
          ~loc:__LOC__
          rewards_per_block
          ei.baking_reward_fixed_portion
    | _ -> failwith "expected_issuance rpc: unexpected value"
  in
  let* yearly_rate = Context.get_ai_current_yearly_rate (B block) in
  let* yearly_rate_exact = Context.get_ai_current_yearly_rate_exact (B block) in
  let yr = float_of_string yearly_rate in
  let yre = Q.to_float yearly_rate_exact in
  (* Precision for yearly rate is 0.001 *)
  let* () =
    Assert.equal
      ~loc:__LOC__
      (fun x y -> Float.(abs (x -. y) <= 0.001))
      "Yearly rate (float)"
      Format.pp_print_float
      yr
      yre
  in
  (* Divided by 525_600 minutes per year, x100 because rpc returns a pct *)
  let issuance_from_rate =
    Tez.(
      mul_q total_supply Q.(div yearly_rate_exact ~$525_600_00)
      |> of_q ~round:`Down)
  in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Tez.equal
      "Issuance"
      Tez.pp
      rewards_per_block
      issuance_from_rate
  in
  return_unit

(** Bake a block, with the given baker and the given operations. *)
let bake ?baker : t -> t tzresult Lwt.t =
 fun (block, state) ->
  let open Lwt_result_wrap_syntax in
  let policy =
    match baker with
    | None -> state.baking_policy
    | Some baker ->
        let {pkh; _} =
          try State.find_account baker state
          with Not_found ->
            Log.info
              ~color:warning_color
              "Invalid baker: %s not found. Aborting"
              baker ;
            assert false
        in
        Some (Block.By_account pkh)
  in
  let* baker, _, _, _ = Block.get_next_baker ?policy block in
  let baker_name, {contract = baker_contract; _} =
    State.find_account_from_pkh baker state
  in
  Log.info
    ~color:time_color
    "Baking level %d with %s"
    (Int32.to_int (Int32.succ Block.(block.header.shell.level)))
    baker_name ;
  let current_cycle = Block.current_cycle block in
  let adaptive_issuance_vote =
    if state.activate_ai then
      Protocol.Alpha_context.Per_block_votes.Per_block_vote_on
    else Per_block_vote_pass
  in
  let* () = check_issuance_rpc block in
  let state, operations = State.pop_pending_operations state in
  let* block, state =
    let* block', _metadata =
      Block.bake_with_metadata ?policy ~adaptive_issuance_vote ~operations block
    in
    if state.burn_rewards then
      (* Incremental mode *)
      let* i =
        Incremental.begin_construction ?policy ~adaptive_issuance_vote block
      in
      let* block_rewards = Context.get_issuance_per_minute (B block') in
      let ctxt = Incremental.alpha_ctxt i in
      let*@ context, _ =
        Protocol.Alpha_context.Token.transfer
          ctxt
          (`Contract baker_contract)
          `Burned
          block_rewards
      in
      let i = Incremental.set_alpha_ctxt i context in
      let* i = List.fold_left_es Incremental.add_operation i operations in
      let* block = Incremental.finalize_block i in
      let state = State.apply_burn block_rewards baker_name state in
      return (block, state)
    else return (block', state)
  in
  let* state =
    State_ai_flags.AI_Activation.check_activation_cycle block state
  in
  let* state = State.apply_rewards ~baker:baker_name block state in
  (* First block of a new cycle *)
  let new_current_cycle = Block.current_cycle block in
  let* state =
    if Protocol.Alpha_context.Cycle.(current_cycle = new_current_cycle) then
      return state
    else (
      Log.info
        ~color:time_color
        "Cycle %d"
        (Protocol.Alpha_context.Cycle.to_int32 new_current_cycle |> Int32.to_int) ;
      return @@ apply_new_cycle new_current_cycle state)
  in
  (* Dawn of a new cycle *)
  let* state =
    if not (Block.last_block_of_cycle block) then return state
    else apply_end_cycle current_cycle block state
  in
  let* () = check_all_balances block state in
  return (block, state)

(** Bake until a cycle is reached, using [bake] instead of [Block.bake]
    Should be slower because checks balances at the end of every block (avoidable in some cases) *)
let bake_until_next_cycle : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let open Lwt_result_syntax in
  let current_cycle = Block.current_cycle init_block in
  let rec step (old_block, old_state) =
    let step_cycle = Block.current_cycle old_block in
    if Protocol.Alpha_context.Cycle.(step_cycle > current_cycle) then
      return (old_block, old_state)
    else
      let* new_block, new_state = bake (old_block, old_state) in
      step (new_block, new_state)
  in
  step (init_block, init_state)

(* ======== Operations ======== *)

(** Bake a single block *)
let next_block =
  exec (fun input ->
      Log.info ~color:action_color "[Next block]" ;
      bake input)

(** Bake a single block with a specific baker *)
let next_block_with_baker baker =
  exec (fun input ->
      Log.info ~color:action_color "[Next block (baker %s)]" baker ;
      bake ~baker input)

(** Bake until the end of a cycle *)
let next_cycle_ input =
  Log.info ~color:action_color "[Next cycle]" ;
  bake_until_next_cycle input

(** Bake until the end of a cycle *)
let next_cycle = exec next_cycle_

(** Executes an operation: f should return a new state and a list of operations, which are then applied *)
let exec_op f =
  let open Lwt_result_syntax in
  Action
    (fun ((block, _state) as input) ->
      let* state, ops = f input in
      let state = State.add_pending_operations ops state in
      return (block, state))
  --> next_block

(** Waiting functions *)
let rec wait_n_cycles n =
  if n <= 0 then noop
  else if n = 1 then next_cycle
  else wait_n_cycles (n - 1) --> next_cycle

let rec wait_n_blocks n =
  if n <= 0 then noop
  else if n = 1 then next_block
  else wait_n_blocks (n - 1) --> next_block

let wait_cycle_f_es (condition : t -> t -> bool tzresult Lwt.t) :
    (t, t) scenarios =
  let open Lwt_result_syntax in
  exec (fun init_t ->
      let rec bake_while t =
        let* b = condition init_t t in
        if b then return t
        else
          let* t = next_cycle_ t in
          bake_while t
      in
      bake_while init_t)

(** Waits until [condition init_t current_t] is fulfilled.
    It is checked on the first block of every cycle. If it returns false,
    another cycle is baked, until it succeeds.
*)
let wait_cycle_f (condition : t -> t -> bool) : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec (fun init_t ->
      let rec bake_while t =
        if condition init_t t then return t
        else
          let* t = next_cycle_ t in
          bake_while t
      in
      bake_while init_t)

(** Wait until we are in a cycle satisfying the given condition.
    Fails if AI_activation is requested and AI is not set to be activated in the future. *)
let wait_cycle_until condition =
  let to_, done_ =
    let rec get_names condition =
      match condition with
      | `AI_activation -> ("AI activation", "AI activated")
      | `delegate_parameters_activation ->
          ("delegate parameters activation", "delegate parameters activated")
      | `And (cond1, cond2) ->
          let to1, done1 = get_names cond1 in
          let to2, done2 = get_names cond2 in
          (to1 ^ " and " ^ to2, done1 ^ " and " ^ done2)
    in
    get_names condition
  in
  let condition (init_block, init_state) =
    let open Lwt_result_syntax in
    let rec stopper condition =
      match condition with
      | `AI_activation ->
          fun (block, _state) ->
            if init_state.State.activate_ai then
              let* launch_cycle = get_launch_cycle ~loc:__LOC__ init_block in
              let current_cycle = Block.current_cycle block in
              return Cycle.(current_cycle >= launch_cycle)
            else assert false
      | `delegate_parameters_activation ->
          fun (block, _state) ->
            let init_cycle = Block.current_cycle init_block in
            let cycles_to_wait =
              init_state.constants.delegate_parameters_activation_delay
            in
            return
              Cycle.(Block.current_cycle block >= add init_cycle cycles_to_wait)
      | `And (cond1, cond2) ->
          let stop1 = stopper cond1 in
          let stop2 = stopper cond2 in
          fun (block, state) ->
            let* b1 = stop1 (block, state) in
            let* b2 = stop2 (block, state) in
            return (b1 && b2)
    in
    stopper condition
  in
  log ~color:time_color "Fast forward to %s" to_
  --> wait_cycle_f_es condition
  --> log ~color:event_color "%s" done_

(** Wait until AI activates.
    Fails if AI is not set to be activated in the future. *)
let wait_ai_activation =
  wait_cycle_until `AI_activation
  --> exec_unit (fun (block, state) ->
          assert (State_ai_flags.AI.enabled block state) ;
          return_unit)

(** wait delegate_parameters_activation_delay cycles  *)
let wait_delegate_parameters_activation =
  wait_cycle_until `delegate_parameters_activation

let wait_n_cycles_f (n_cycles : t -> int) =
  let condition ((init_block, _init_state) as t_init)
      ((current_block, _current_state) as _t_current) =
    let n = n_cycles t_init in
    let init_cycle = Block.current_cycle init_block in
    let current_cycle = Block.current_cycle current_block in
    Cycle.(current_cycle >= add init_cycle n)
  in
  wait_cycle_f condition

let wait_n_cycles_f_es (n_cycles : t -> int tzresult Lwt.t) =
  let open Lwt_result_syntax in
  let condition ((init_block, _init_state) as t_init)
      ((current_block, _current_state) as _t_current) =
    let* n = n_cycles t_init in
    let init_cycle = Block.current_cycle init_block in
    let current_cycle = Block.current_cycle current_block in
    return Cycle.(current_cycle >= add init_cycle n)
  in
  wait_cycle_f_es condition
