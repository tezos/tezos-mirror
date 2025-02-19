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

(** Misc checks at block end *)
let check_misc block state : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let State.{account_map; _} = state in
  String.Map.fold_s
    (fun name account acc ->
      match account.delegate with
      | Some x when String.equal x name ->
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
                     Plugin.Alpha_services.Delegate.deposit_per_cycle) ->
                (cycle, deposit))
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
            Cycle.add
              account.last_seen_activity
              state.constants.tolerated_inactivity_period
            < ctxt_cycle
          in
          let*! r3 =
            Assert.equal_bool ~loc:__LOC__ deactivated deactivated_rpc
          in
          let*! r = Assert.join_errors r r3 in
          Assert.join_errors r acc
      | _ -> Lwt.return acc)
    account_map
    Result.return_unit

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

let attest_all_ =
  let open Lwt_result_syntax in
  fun (block, state) ->
    let* rights = Plugin.RPC.Attestation_rights.get Block.rpc_ctxt block in
    let delegates_rights =
      match rights with
      | [{level = _; delegates_rights; estimated_time = _}] -> delegates_rights
      | _ ->
          (* Cannot happen: RPC called to return only current level,
             so the returned list should only contain one element. *)
          assert false
    in
    let* dlgs =
      List.map
        (fun {
               Plugin.RPC.Attestation_rights.delegate;
               consensus_key = _;
               first_slot;
               attestation_power;
             } ->
          Tezt.Check.(
            (attestation_power > 0)
              int
              ~__LOC__
              ~error_msg:"Attestation power should be greater than 0, got %L") ;
          (delegate, first_slot))
        delegates_rights
      |> List.filter_es (fun (delegate, _slot) ->
             let* is_forbidden =
               Context.Delegate.is_forbidden (B block) delegate
             in
             return (not is_forbidden))
    in
    let* ops =
      List.map_es
        (fun (delegate, slot) -> Op.attestation ~delegate ~slot block)
        dlgs
    in
    let state = State.add_pending_operations ops state in
    return (block, state)

(* Does not produce a new block *)
let attest_all = exec attest_all_

let check_ai_launch_cycle_is_zero ~loc block =
  let open Lwt_result_syntax in
  let* ai_launch_cycle = Context.get_adaptive_issuance_launch_cycle (B block) in
  let ai_launch_cycle = WithExceptions.Option.get ~loc ai_launch_cycle in
  if not Cycle.(equal ai_launch_cycle root) then
    Test.fail ~__LOC__:loc "AI launch cycle should always be zero" ;
  return_unit

(** Bake a block, with the given baker and the given operations. *)
let bake ?baker : t -> t tzresult Lwt.t =
 fun (block, state) ->
  let open Lwt_result_wrap_syntax in
  let previous_block = block in
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
  let current_cycle = Block.current_cycle block in
  let* level = Plugin.RPC.current_level Block.rpc_ctxt block in
  assert (Protocol.Alpha_context.Cycle.(level.cycle = Block.current_cycle block)) ;
  Log.info
    ~color:time_color
    "Baking level %d (cycle %ld) with %s"
    (Int32.to_int (Int32.succ Block.(block.header.shell.level)))
    (Protocol.Alpha_context.Cycle.to_int32 level.cycle)
    baker_name ;
  let adaptive_issuance_vote =
    if state.force_ai_vote_yes then
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
  let baker_acc = State.find_account baker_name state in
  (* update baker and attesters activity *)
  let update_activity delegate_account =
    Account_helpers.update_activity
      delegate_account
      state.constants
      (Block.current_cycle block)
  in
  let* attesters =
    let open Tezos_raw_protocol_022_PsRiotum.Alpha_context in
    let* ctxt = Context.get_alpha_ctxt (B previous_block) in
    List.filter_map_es
      (fun op ->
        let ({protocol_data = Operation_data protocol_data; _}
              : packed_operation) =
          op
        in
        match protocol_data.contents with
        | Single (Attestation {consensus_content; _}) ->
            let*@ _, owner =
              Stake_distribution.slot_owner
                ctxt
                (Level.from_raw ctxt consensus_content.level)
                consensus_content.slot
            in
            return_some owner.delegate
        | _ -> return_none)
      operations
  in
  let state =
    State.update_map
      ~f:(fun acc_map ->
        let acc_map =
          String.Map.add baker_name (update_activity baker_acc) acc_map
        in
        List.fold_left
          (fun acc_map delegate_pkh ->
            let delegate_name, delegate_acc =
              State.find_account_from_pkh delegate_pkh state
            in
            String.Map.add delegate_name (update_activity delegate_acc) acc_map)
          acc_map
          attesters)
      state
  in
  let* () = check_ai_launch_cycle_is_zero ~loc:__LOC__ block in
  let* state = State.apply_rewards ~baker:baker_name block state in
  let new_future_current_cycle = Cycle.succ (Block.current_cycle block) in
  (* Dawn of a new cycle: apply cycle end operations *)
  let* state =
    if not (Block.last_block_of_cycle block) then return state
    else apply_end_cycle current_cycle previous_block block state
  in
  let* () = check_all_balances block state in
  let* () = check_misc block state in
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
  let* block, state =
    if state.force_attest_all then attest_all_ (block, state)
    else return (block, state)
  in
  return (block, state)

let rec repeat n f acc =
  let open Lwt_result_syntax in
  if n <= 0 then return acc
  else
    let* acc = f acc in
    repeat (n - 1) f acc

(* adopted from tezt/lib_tezos/client.ml *)
let bake_until_level ~target_level : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let open Lwt_result_syntax in
  Log.info "Bake until level %d." target_level ;
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

let bake_until ~target_cycle condition : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let blocks_per_cycle = Int32.to_int init_block.constants.blocks_per_cycle in
  let target_level, str =
    match condition with
    | `New_cycle -> (target_cycle * blocks_per_cycle, "first block")
    | `Cycle_end -> (((target_cycle + 1) * blocks_per_cycle) - 1, "last block")
    | `Cycle_end_but_one ->
        (((target_cycle + 1) * blocks_per_cycle) - 2, "last but one block")
  in
  Log.info "Bake until cycle %d (level %d); %s" target_cycle target_level str ;
  bake_until_level ~target_level (init_block, init_state)

(** Bake until a cycle is reached, using [bake] instead of [Block.bake] *)
let bake_until_next_cycle : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let next_cycle =
    Int32.to_int @@ Cycle.to_int32 @@ Cycle.succ
    @@ Block.current_cycle init_block
  in
  bake_until `New_cycle ~target_cycle:next_cycle (init_block, init_state)

(** Bake all the remaining blocks of the current cycle *)
let bake_until_dawn_of_next_cycle : t -> t tzresult Lwt.t =
 fun (init_block, init_state) ->
  let current_cycle =
    Int32.to_int @@ Cycle.to_int32 @@ Block.current_cycle init_block
  in
  bake_until `Cycle_end ~target_cycle:current_cycle (init_block, init_state)

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
  bake_until `Cycle_end_but_one ~target_cycle (init_block, init_state)

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
