(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module gathers the basic operations used in test scenarios. This
    includes starting a scenario, baking, checking and manipulating the state,
    and various wait functions *)

open State_account
open State
open Scenario_dsl
open Log_helpers
open Adaptive_issuance_helpers

(** For [assert_failure], when expected error does not match the actual error. *)
type error += Unexpected_error

(** Usual threaded state for the tests. Contains the current block, pending operations
    and the known [State.t] *)
type t = Block.t * State.t

let log ?color s =
  let open Lwt_result_syntax in
  exec_unit (fun _ ->
      Log.info ?color s ;
      return_unit)

(* ======== Baking ======== *)

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
        join_errors r acc)
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
  join_errors r1 r2

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
      return @@ State.apply_new_cycle new_current_cycle state)
  in
  (* Dawn of a new cycle *)
  let* state =
    if not (Block.last_block_of_cycle block) then return state
    else State.apply_end_cycle current_cycle block state
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

(* ======== State updates ======== *)

(** Sets the de facto baker for all future blocks *)
let set_baker baker : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      let {pkh; _} = State.find_account baker state in
      return {state with State.baking_policy = Some (Block.By_account pkh)})

(** Exclude a list of delegates from baking *)
let exclude_bakers bakers : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      let bakers_pkh =
        List.map (fun baker -> (State.find_account baker state).pkh) bakers
      in
      return
        {state with State.baking_policy = Some (Block.Excluding bakers_pkh)})

(** Unsets the baking policy, it returns to default ([By_round 0]) *)
let unset_baking_policy : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      return {state with State.baking_policy = None})

(** Creates a snapshot of the current balances for the given account names.
    Can be used to check that balances at point A and B in the execution of a test
    are the same (either nothing happened, or a succession of actions resulted in
    getting the same values as before *)
let snapshot_balances snap_name names_list : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      Log.debug
        ~color:low_debug_color
        "Snapshoting balances as \"%s\""
        snap_name ;
      let balances =
        List.map
          (fun name -> (name, balance_of_account name state.State.account_map))
          names_list
      in
      let snapshot_balances =
        String.Map.add snap_name balances state.snapshot_balances
      in
      return {state with snapshot_balances})

(** Check balances against a previously defined snapshot *)
let check_snapshot_balances
    ?(f =
      fun ~name ~old_balance ~new_balance ->
        assert_balance_equal ~loc:__LOC__ name old_balance new_balance)
    snap_name : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_unit (fun (_block, state) ->
      Log.debug
        ~color:low_debug_color
        "Checking evolution of balances between \"%s\" and now"
        snap_name ;
      let snapshot_balances =
        String.Map.find snap_name state.State.snapshot_balances
      in
      match snapshot_balances with
      | None ->
          Log.debug
            ~color:warning_color
            "\"%s\" snapshot not found..."
            snap_name ;
          return_unit
      | Some snapshot_balances ->
          let* () =
            List.iter_es
              (fun (name, old_balance) ->
                let new_balance =
                  balance_of_account name state.State.account_map
                in
                f ~name ~old_balance ~new_balance)
              snapshot_balances
          in
          return_unit)

(** Save the current issuance rate for future use *)
let save_current_rate : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (block, state) ->
      let* rate = Context.get_ai_current_yearly_rate_exact (B block) in
      return {state with State.saved_rate = Some rate})

(** Check that [f saved_rate current_rate] is true. [f] is typically a comparison function *)
let check_rate_evolution (f : Q.t -> Q.t -> bool) : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_unit (fun (block, state) ->
      let* new_rate = Context.get_ai_current_yearly_rate_exact (B block) in
      let previous_rate = state.State.saved_rate in
      match previous_rate with
      | None -> failwith "check_rate_evolution: no rate previously saved"
      | Some previous_rate ->
          if f previous_rate new_rate then return_unit
          else
            failwith
              "check_rate_evolution: assertion failed@.previous rate: %a@.new \
               rate: %a"
              Q.pp_print
              previous_rate
              Q.pp_print
              new_rate)

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

(* ======== Misc functions ========*)

let check_failure_aux ?expected_error :
    ('a -> 'b tzresult Lwt.t) -> 'a -> 'a tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun f input ->
    Log.info ~color:assert_block_color "Entering failing scenario..." ;
    let*! output = f input in
    match output with
    | Ok _ -> failwith "Unexpected success"
    | Error e -> (
        match expected_error with
        | None ->
            Log.info ~color:assert_block_color "Rollback" ;
            return input
        | Some exp_e ->
            let exp_e = exp_e input in
            if e = exp_e then (
              Log.info ~color:assert_block_color "Rollback" ;
              return input)
            else (
              Log.info
                ~color:Log.Color.FG.red
                "Unexpected error:@.%a@.Expected:@.%a@."
                (Format.pp_print_list pp)
                e
                (Format.pp_print_list pp)
                exp_e ;
              tzfail Unexpected_error))

let check_fail_and_rollback ?expected_error :
    ('a, 'b) single_scenario -> 'a -> 'a tzresult Lwt.t =
 fun sc input -> check_failure_aux ?expected_error (run_scenario sc) input

(** Useful function to test expected failures: runs the given branch until it fails,
    then rollbacks to before execution. Fails if the given branch Succeeds *)
let assert_failure ?expected_error : ('a, 'b) scenarios -> ('a, 'a) scenarios =
 fun scenarios ->
  match unfold_scenarios scenarios with
  | [] -> Empty
  | [(sc, _, _)] -> exec (check_fail_and_rollback ?expected_error sc)
  | _ ->
      exec (fun _ ->
          failwith "Error: assert_failure used with branching scenario")

(** Loop *)
let rec loop n : ('a, 'a) scenarios -> ('a, 'a) scenarios =
 fun scenario ->
  (* If branching scenarios with k branches, returns a scenario with k^n branches *)
  if n = 0 then Empty
  else if n = 1 then scenario
  else loop (n - 1) scenario --> scenario

let rec loop_action n : ('a -> 'a tzresult Lwt.t) -> ('a, 'a) scenarios =
 fun f ->
  if n = 0 then Empty
  else if n = 1 then exec f
  else loop_action (n - 1) f --> exec f

(** Check a specific balance field for a specific account is equal to a specific amount *)
let check_balance_field src_name field amount : (t, t) scenarios =
  let open Lwt_result_syntax in
  let check = Assert.equal_tez ~loc:__LOC__ amount in
  let check' a = check (Partial_tez.to_tez ~round:`Down a) in
  exec_state (fun (block, state) ->
      let src = State.find_account src_name state in
      let src_balance, src_total =
        balance_and_total_balance_of_account src_name state.account_map
      in
      let* rpc_balance, rpc_total =
        get_balance_from_context (B block) src.contract
      in
      let* () =
        match field with
        | `Liquid ->
            let* () = check rpc_balance.liquid_b in
            check src_balance.liquid_b
        | `Bonds ->
            let* () = check rpc_balance.bonds_b in
            check src_balance.bonds_b
        | `Staked ->
            let* () = check' rpc_balance.staked_b in
            check' src_balance.staked_b
        | `Unstaked_frozen_total ->
            let* () = check rpc_balance.unstaked_frozen_b in
            check src_balance.unstaked_frozen_b
        | `Unstaked_finalizable ->
            let* () = check rpc_balance.unstaked_finalizable_b in
            check src_balance.unstaked_finalizable_b
        | `Total ->
            let* () = check rpc_total in
            check src_total
      in
      return state)

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
let wait_cycle condition =
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
  exec_unit (fun _ ->
      Log.info ~color:time_color "Fast forward to %s" to_ ;
      return_unit)
  --> wait_cycle_f_es condition
  --> exec_unit (fun _ ->
          Log.info ~color:event_color "%s" done_ ;
          return_unit)

(** Wait until AI activates.
    Fails if AI is not set to be activated in the future. *)
let wait_ai_activation = wait_cycle `AI_activation

(** wait delegate_parameters_activation_delay cycles  *)
let wait_delegate_parameters_activation =
  wait_cycle `delegate_parameters_activation
