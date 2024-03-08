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

(** For [assert_failure], when expected error does not match the actual error. *)
type error += Unexpected_error

(** Usual threaded state for the tests. Contains the current block, pending operations
    and the known [State.t] *)
type t = Block.t * State.t

let log ?(level = Cli.Logs.Info) ?color format =
  Format.kasprintf
    (fun s ->
      exec_unit (fun _ ->
          Log.log ~level ?color "%s" s ;
          return_unit))
    format

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
