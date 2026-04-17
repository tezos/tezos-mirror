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
open Scenario_attestation

(** Applies when baking the last block of a cycle *)
let apply_end_cycle current_cycle previous_block block state :
    State.t tzresult Lwt.t =
  let open Lwt_result_wrap_syntax in
  Log.debug ~color:time_color "Ending cycle %a" Cycle.pp current_cycle ;
  (* Apply all slashes *)
  let* state =
    Slashing_helpers.apply_all_slashes_at_cycle_end
      current_cycle
      previous_block
      state
  in
  (* Sets initial frozen for future cycle *)
  let* state = update_map_es ~f:(compute_future_frozen_rights block) state in
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
let check_all_balances _full_metadata (block, state) : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let State.{account_map; total_supply; _} = state in
  let* actual_total_supply = Context.get_total_supply (B block) in
  let*! r1 =
    String.Map.fold_s
      (fun name account acc ->
        if account.revealed then (
          log_debug_balance name account_map ;
          let* () = log_debug_rpc_balance name (Implicit account.pkh) block in
          let*! r =
            assert_balance_check ~loc:__LOC__ (B block) name account_map
          in
          Assert.join_errors r acc)
        else return_unit)
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

(** Misc checks at block end *)
let check_misc _full_metadata (block, state) : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let State.{account_map; _} = state in
  String.Map.fold_s
    (fun name account acc ->
      match account.delegate with
      | Some x when String.equal x name && account.revealed ->
          let ufd_state =
            List.map
              (fun ({cycle; current; _} : Unstaked_frozen.r) ->
                (cycle, current))
              account.unstaked_frozen
          in
          let ufnlz_state =
            Unstaked_finalizable.total account.unstaked_finalizable
          in
          let ufd_state_map =
            List.fold_left
              (fun acc (cycle, v) -> CycleMap.add cycle v acc)
              CycleMap.empty
              ufd_state
          in
          let* u_rpc =
            Context.Delegate.unstaked_frozen_deposits (B block) account.pkh
          in
          let u_rpc =
            List.map
              (fun ({cycle; deposit} :
                     Plugin.Delegate_services.deposit_per_cycle)
                 -> (cycle, deposit))
              u_rpc
          in
          let finalizable_cycle =
            Cycle.sub
              (Block.current_cycle block)
              (state.State.constants.consensus_rights_delay + 2)
          in
          let ufnlz_rpc, ufd_rpc =
            match finalizable_cycle with
            | None -> (Tez.zero, u_rpc)
            | Some finalizable_cycle -> (
                match
                  List.partition
                    (fun (cycle, _) -> Cycle.equal cycle finalizable_cycle)
                    u_rpc
                with
                | [], l -> (Tez.zero, l)
                | [(_, s)], l -> (s, l)
                | _ -> assert false)
          in
          let*! r1 = Assert.equal_tez ~loc:__LOC__ ufnlz_rpc ufnlz_state in
          let*! r2 =
            List.fold_left
              (fun acc (cycle, v) ->
                let state_val =
                  CycleMap.find cycle ufd_state_map
                  |> Option.value ~default:Tez.zero
                in
                let*! r = Assert.equal_tez ~loc:__LOC__ v state_val in
                let*! acc in
                Assert.join_errors r acc)
              return_unit
              ufd_rpc
          in
          let*! r = Assert.join_errors r1 r2 in
          let* deactivated_rpc =
            Context.Delegate.deactivated (B block) account.pkh
          in
          let current_cycle = Block.current_cycle block in
          let ctxt_cycle =
            if not (Block.last_block_of_cycle block) then current_cycle
            else Cycle.succ current_cycle
          in
          let deactivated =
            Scenario_activity.is_inactive state.constants ctxt_cycle account
          in
          let*! r3 =
            Assert.equal_bool ~loc:__LOC__ deactivated deactivated_rpc
          in
          let*! r = Assert.join_errors r r3 in
          Assert.join_errors r acc
      | _ -> Lwt.return acc)
    account_map
    Result.return_unit

let check_issuance_rpc _metadata (block, _state) : unit tzresult Lwt.t =
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

let check_ai_launch_cycle_is_zero ~loc block =
  let open Lwt_result_syntax in
  let* ai_launch_cycle = Context.get_adaptive_issuance_launch_cycle (B block) in
  let ai_launch_cycle = WithExceptions.Option.get ~loc ai_launch_cycle in
  if not Cycle.(equal ai_launch_cycle root) then
    Test.fail ~__LOC__:loc "AI launch cycle should always be zero" ;
  return_unit

(** Apply all operations pending in the [state].
    It is imperative that the list of pending operations in the state
    is empty before finalizing the block, either manually, or by calling this function. *)
let apply_all_pending_operations_ : t_incr -> t_incr tzresult Lwt.t =
 fun (i, state) ->
  let open Lwt_result_wrap_syntax in
  let state, operations = State.pop_pending_operations state in
  let* i = List.fold_left_es Incremental.add_operation i operations in
  return (i, state)

(** finalize the payload of the next block. Can start including preattestations. *)
let finalize_payload_ ?payload_round ?baker : t -> t_incr tzresult Lwt.t =
 fun ((block, state) as input) ->
  let open Lwt_result_wrap_syntax in
  (* Before going incremental mode, apply the [force_attest_all] *)
  let* block, state =
    if Int32.(block.header.shell.level <> zero) && state.force_attest_all then
      attest_with_all_ input
    else Lwt_result.return input
  in
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
  let payload_round =
    match payload_round with
    | Some _ -> payload_round
    | None -> state.payload_round
  in
  let* baker, _, _, _ = Block.get_next_baker ?policy block in
  let baker_name, {contract = baker_contract; _} =
    State.find_account_from_pkh baker state
  in
  let* level = Plugin.RPC.current_level Block.rpc_ctxt block in
  let* next_level =
    let* ctxt = Context.get_alpha_ctxt (B block) in
    return (Protocol.Alpha_context.Level.succ ctxt level)
  in
  assert (Protocol.Alpha_context.Cycle.(level.cycle = Block.current_cycle block)) ;
  Log.info
    ~color:time_color
    "Baking level %d (cycle %ld) with %s"
    (Int32.to_int (Int32.succ Block.(block.header.shell.level)))
    (Protocol.Alpha_context.Cycle.to_int32 next_level.cycle)
    baker_name ;
  let* block' =
    Block.bake
      ~baking_mode:Baking
      ?policy
      ~operations:state.pending_operations
      block
  in
  let* i =
    Incremental.begin_construction
      ?payload_round
      ~payload:state.pending_operations
      ?policy
      block
  in
  let* i, state =
    if state.burn_rewards then
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
      let state = State.apply_burn block_rewards baker_name state in
      return (i, state)
    else return (i, state)
  in
  apply_all_pending_operations_ (i, state)

let finalize_payload ?payload_round ?baker () : (t, t_incr) scenarios =
  exec (finalize_payload_ ?payload_round ?baker)

let finalize_block_ : t_incr -> t tzresult Lwt.t =
 fun ((_, state) as input) ->
  let open Lwt_result_wrap_syntax in
  assert (List.is_empty state.pending_operations) ;
  (* Before going finalizing the block, apply the [force_preattest_all] *)
  let* i, state =
    if state.force_preattest_all then preattest_with_all_ input
    else Lwt_result.return input
  in
  let* block, block_metadata = Incremental.finalize_block_with_metadata i in
  let metadata = (block_metadata, List.rev (Incremental.rev_tickets i)) in
  let previous_block = Incremental.predecessor i in
  let baker = Incremental.delegate i in
  let baker_name, _ = State.find_account_from_pkh baker.pkh state in
  (* Update baker activity *)
  let state =
    Scenario_activity.update_activity
      baker_name
      state
      (Block.current_cycle block)
  in
  let* () = check_ai_launch_cycle_is_zero ~loc:__LOC__ block in
  let* state = State.apply_rewards ~baker:baker_name block state in
  let current_cycle = Block.current_cycle previous_block in
  let new_future_current_cycle = Cycle.succ (Block.current_cycle block) in
  (* Dawn of a new cycle: apply cycle end operations *)
  let* state =
    if not (Block.last_block_of_cycle block) then return state
    else apply_end_cycle current_cycle previous_block block state
  in
  let* () =
    List.iter_es
      (fun f -> f metadata (block, state))
      state.check_finalized_every_block
  in
  let* () =
    List.iter_es
      (fun f -> f metadata (block, state))
      state.check_finalized_current_block
  in
  (* Dawn of a new cycle: update finalizables *)
  (* Note: this is done after the checks, because it is not observable by RPCs by calling
     the previous block (which is still in the previous cycle *)
  let* state =
    if not (Block.last_block_of_cycle block) then return state
    else (
      Log.info
        ~color:time_color
        "Cycle %d"
        (Protocol.Alpha_context.Cycle.to_int32 new_future_current_cycle
        |> Int32.to_int) ;
      return @@ apply_new_cycle new_future_current_cycle state)
  in
  let state =
    {
      state with
      check_finalized_current_block = [];
      previous_metadata = Some metadata;
      grandparent = previous_block;
      grandgrandparent = state.grandparent;
    }
  in
  return (block, state)

let finalize_block : (t_incr, t) scenarios = exec finalize_block_

(** Bake a block, with the given baker and the given operations. *)
let bake ?baker : t -> t tzresult Lwt.t =
 fun input ->
  let ( |=> ) = Lwt_result.bind in
  finalize_payload_ ?baker input |=> finalize_block_

let rec repeat n f acc =
  let open Lwt_result_syntax in
  if n <= 0 then return acc
  else
    let* acc = f acc in
    repeat (n - 1) f acc

(* adopted from tezt/lib_tezos/client.ml *)
let bake_until_level ?(log_message = true) ~target_level : t -> t tzresult Lwt.t
    =
 fun (init_block, init_state) ->
  let open Lwt_result_syntax in
  if log_message then Log.info "Bake until level %d." target_level ;
  let current_level = Int32.to_int @@ Block.current_level init_block in
  if target_level < current_level then
    Test.fail
      "bake_until_level(%d): already at level %d"
      target_level
      current_level ;
  let* final_block, final_state =
    repeat (target_level - current_level) bake (init_block, init_state)
  in
  let final_level = Int32.to_int @@ Block.current_level final_block in
  Check.((final_level = target_level) ~__LOC__ int)
    ~error_msg:"Expected level=%R, got %L" ;
  return (final_block, final_state)

let bake_until condition : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let blocks_per_cycle = Int32.to_int init_block.constants.blocks_per_cycle in
  let fs = Format.asprintf in
  let target_level, log_str =
    match condition with
    | `Level n -> (n, fs "Bake until level %d" n)
    | `Cycle (target_cycle, level_cond) ->
        let cycle_offset = target_cycle * blocks_per_cycle in
        let level, str =
          match level_cond with
          | `Level n -> (cycle_offset + n, fs "level position %d" n)
          | `First_level -> (cycle_offset, "first level")
          | `Last_level -> (cycle_offset + blocks_per_cycle - 1, "last level")
          | `Before_last_level ->
              (cycle_offset + blocks_per_cycle - 2, "level before last level")
        in
        (level, fs "Bake until cycle %d, %s (level %d)" target_cycle str level)
  in
  Log.info "%s" log_str ;
  bake_until_level ~log_message:false ~target_level (init_block, init_state)

(** Bake until a cycle is reached, using [bake] instead of [Block.bake] *)
let bake_until_next_cycle : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let next_cycle =
    Int32.to_int @@ Cycle.to_int32 @@ Cycle.succ
    @@ Block.current_cycle init_block
  in
  bake_until (`Cycle (next_cycle, `First_level)) (init_block, init_state)

(** Bake all the remaining blocks of the current cycle *)
let bake_until_dawn_of_next_cycle : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let current_cycle =
    Int32.to_int @@ Cycle.to_int32 @@ Block.current_cycle init_block
  in
  bake_until (`Cycle (current_cycle, `Last_level)) (init_block, init_state)

let bake_until_next_cycle_end_but_one : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let current_cycle =
    Int32.to_int @@ Cycle.to_int32 @@ Block.current_cycle init_block
  in
  let next_cycle =
    Int32.to_int @@ Cycle.to_int32 @@ Cycle.succ
    @@ Block.current_cycle init_block
  in
  let target_cycle =
    if Block.last_block_of_cycle init_block then next_cycle else current_cycle
  in
  bake_until (`Cycle (target_cycle, `Before_last_level)) (init_block, init_state)

(* ======== Operations ======== *)

(** Bake a single block *)
let next_block_ input =
  Log.info ~color:action_color "[Next block]" ;
  bake input

let next_block = exec next_block_

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

let dawn_of_next_cycle =
  exec (fun input ->
      Log.info ~color:action_color "[Dawn of next cycle]" ;
      bake_until_dawn_of_next_cycle input)

(** Executes an operation: f should return a new state and a list of operations, which are then applied *)
let exec_op f =
  let open Lwt_result_syntax in
  exec (fun ((block, _state) as input) ->
      let* state, ops = f input in
      match state.operation_mode with
      | Bake ->
          let state = State.add_pending_operations ops state in
          next_block_ (block, state)
      | Wait ->
          let state = State.add_pending_operations ops state in
          return (block, state)
      | Batch ->
          let state = State.add_pending_batch ops state in
          return (block, state))

(** Waiting functions *)
let wait_n_cycles n = loop n next_cycle

let wait_n_blocks n = loop n next_block

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
  let condition a b = return @@ condition a b in
  wait_cycle_f_es condition

(** Wait until we are in a cycle satisfying the given condition.
    Fails if AI_activation is requested and AI is not set to be activated in the future. *)
let wait_cycle_until condition =
  let to_, done_ =
    let rec get_names condition =
      match condition with
      | `delegate_parameters_activation ->
          ("delegate parameters activation", "delegate parameters activated")
      | `right_before_delegate_parameters_activation ->
          ( "right before delegate parameters activation",
            "delegate parameters will activate next cycle" )
      | `And (cond1, cond2) ->
          let to1, done1 = get_names cond1 in
          let to2, done2 = get_names cond2 in
          (to1 ^ " and " ^ to2, done1 ^ " and " ^ done2)
    in
    get_names condition
  in
  let condition (init_block, init_state) =
    let rec stopper condition =
      match condition with
      | `delegate_parameters_activation ->
          fun (block, _state) ->
            let init_cycle = Block.current_cycle init_block in
            let cycles_to_wait =
              (* Delegate parameters wait for at least
                 [delegate_parameters_activation_delay] **full
                 cycles** to activate, so we need to add 1 to the
                 number of cycles to wait. *)
              init_state.constants.delegate_parameters_activation_delay + 1
            in
            Cycle.(Block.current_cycle block >= add init_cycle cycles_to_wait)
      | `right_before_delegate_parameters_activation ->
          fun (block, _state) ->
            let init_cycle = Block.current_cycle init_block in
            let cycles_to_wait =
              init_state.constants.delegate_parameters_activation_delay
            in
            Cycle.(Block.current_cycle block >= add init_cycle cycles_to_wait)
      | `And (cond1, cond2) ->
          let stop1 = stopper cond1 in
          let stop2 = stopper cond2 in
          fun (block, state) ->
            let b1 = stop1 (block, state) in
            let b2 = stop2 (block, state) in
            b1 && b2
    in
    stopper condition
  in
  log ~color:time_color "Fast forward to %s" to_
  --> wait_cycle_f condition
  --> log ~color:event_color "%s" done_

(** wait delegate_parameters_activation_delay cycles  *)
let wait_delegate_parameters_activation =
  wait_cycle_until `delegate_parameters_activation

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

let wait_n_cycles_f (n_cycles : t -> int) =
  let open Lwt_result_syntax in
  let n_cycles n = return @@ n_cycles n in
  wait_n_cycles_f_es n_cycles
