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

(** Usual threaded state for the tests. Contains the current block
    and the known [State.t] *)
type t = Block.t * State.t

(** Threaded state when constructing a block step by step in incremental mode.
    The operation metadata list is built as operations are getting applied. *)
type t_incr = Incremental.t * State.t

let log ?(level = Cli.Logs.Info) ?color format =
  Format.kasprintf
    (fun s ->
      exec_unit (fun _ ->
          Log.log ~level ?color "%s" s ;
          Lwt_result_syntax.return_unit))
    format

(* ======== State updates ======== *)

(** Sets the de facto baker for all future blocks *)
let set_baker ?min_round baker : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      let {pkh; _} = State.find_account baker state in
      match min_round with
      | None ->
          return {state with State.baking_policy = Some (Block.By_account pkh)}
      | Some min_round ->
          return
            {
              state with
              State.baking_policy =
                Some (Block.By_account_with_minimal_round (pkh, min_round));
            })

(** Exclude a list of delegates from baking *)
let exclude_bakers bakers : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      let bakers_pkh =
        List.map (fun baker -> (State.find_account baker state).pkh) bakers
      in
      let log_list =
        List.combine_drop bakers bakers_pkh
        |> List.map (fun (name, pkh) ->
               Format.asprintf "%s(%a)" name Signature.Public_key_hash.pp pkh)
      in
      Log.log
        ~level:Cli.Logs.Info
        ~color:event_color
        "Excluding bakers: [ %s ]"
        (String.concat ", " log_list) ;
      return
        {state with State.baking_policy = Some (Block.Excluding bakers_pkh)})

let set_payload_round (payload_round : int option) : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) -> return {state with State.payload_round})

let set_baked_round ?payload_round (round : int) : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_state (fun (_block, state) ->
      return
        {
          state with
          State.baking_policy = Some (Block.By_round round);
          payload_round;
        })

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

let check_failure_aux ?(loc = __LOC__) ~expected_error :
    ('a -> 'b tzresult Lwt.t) -> 'a -> 'a tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun f input ->
    Log.info ~color:assert_block_color "Entering failing scenario..." ;
    let*! output = f input in
    match output with
    | Ok _ -> failwith "%s: Unexpected success@." loc
    | Error err ->
        let* () = expected_error input err in
        Log.info
          ~color:assert_block_color
          "Scenario successfully failed. Rollback." ;
        return input

let check_fail_and_rollback ?(loc = __LOC__) ~expected_error :
    ('a, 'b) single_scenario -> 'a -> 'a tzresult Lwt.t =
 fun sc input -> check_failure_aux ~loc ~expected_error (run_scenario sc) input

(** Useful function to test expected failures: runs the given branch until it fails,
    then rollbacks to before execution. Fails if the given branch Succeeds *)
let assert_failure ?(loc = __LOC__) ~expected_error :
    ('a, 'b) scenarios -> ('a, 'a) scenarios =
 fun scenarios ->
  match unfold_scenarios scenarios with
  | [] -> Empty
  | [(sc, _, _)] -> exec (check_fail_and_rollback ~loc ~expected_error sc)
  | _ ->
      exec (fun _ ->
          failwith "%s: Error: assert_failure used with branching scenario" loc)

(** Check a scenario does not fail, and rolls back to before the assert *)
let assert_success ?(loc = __LOC__) : ('a, 'b) scenarios -> ('a, 'a) scenarios =
 fun scenarios ->
  match unfold_scenarios scenarios with
  | [] -> Empty
  | [(sc, _, _)] ->
      exec
        (let open Lwt_result_syntax in
         fun input ->
           Log.info ~color:assert_block_color "Entering temporary scenario..." ;
           let* _ = run_scenario sc input in
           Log.info
             ~color:assert_block_color
             "Temporary scenario succeeded. Rollback." ;
           return input)
  | _ ->
      exec (fun _ ->
          failwith "%s: Error: assert_success used with branching scenario" loc)

let assert_failure_in_check_snapshot_balances ~loc ?f snap_name =
  assert_failure
    ~expected_error:(fun _ errs ->
      Error_helpers.expect_failwith
        ~loc
        ~str:(Str.regexp ".*\n.*is not equal to.*")
        errs)
    (check_snapshot_balances ?f snap_name)

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
let check_balance_field ?(loc = __LOC__) src_name field amount :
    (t, t) scenarios =
  let open Lwt_result_syntax in
  let check field_name =
    let loc = sf "%s - unexpected %s for %S" loc field_name src_name in
    Assert.equal_tez ~loc amount
  in
  let check' field_name a =
    check field_name (Partial_tez.to_tez ~round:`Down a)
  in
  let check_z field_name =
    let loc = sf "%s - unexpected %s for %S" loc field_name src_name in
    Assert.equal_z ~loc (Z.of_int64 (Tez.to_mutez amount))
  in
  exec_unit (fun (block, state) ->
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
            let* () = check "spendable_balance" rpc_balance.liquid_b in
            check "spendable_balance" src_balance.liquid_b
        | `Bonds ->
            let* () = check "frozen_bonds" rpc_balance.bonds_b in
            check "frozen_bonds" src_balance.bonds_b
        | `Staked ->
            let* () = check' "staked_balance" rpc_balance.staked_b in
            check' "staked_balance" src_balance.staked_b
        | `Unstaked_frozen_total ->
            let* () =
              check "unstaked_frozen_total" rpc_balance.unstaked_frozen_b
            in
            check "unstaked_frozen_total" src_balance.unstaked_frozen_b
        | `Unstaked_finalizable ->
            let* () =
              check
                "unstaked_finalizable_total"
                rpc_balance.unstaked_finalizable_b
            in
            check
              "unstaked_finalizable_total"
              src_balance.unstaked_finalizable_b
        | `Total ->
            let* () = check "full_balance" rpc_total in
            check "full_balance" src_total
        | `Pseudotokens ->
            let* () =
              check_z "pseudotokens" rpc_balance.staking_delegator_numerator_b
            in
            check_z "pseudotokens" src_balance.staking_delegator_numerator_b
      in
      return_unit)

let assert_failure_in_check_balance_field ~loc src_name field amount =
  assert_failure
    ~expected_error:(fun _ errs ->
      Error_helpers.expect_failwith
        ~loc
        ~str:(Str.regexp ".*\n.*Tez aren't equal.*")
        errs)
    (check_balance_field src_name field amount)

let check_balance_fields ?(loc = __LOC__) src_name ~liquid ~staked
    ?(unstaked_frozen_total = Tez.zero) ?(unstaked_finalizable = Tez.zero) () =
  check_balance_field ~loc src_name `Staked staked
  --> check_balance_field ~loc src_name `Liquid liquid
  --> check_balance_field
        ~loc
        src_name
        `Unstaked_frozen_total
        unstaked_frozen_total
  --> check_balance_field
        ~loc
        src_name
        `Unstaked_finalizable
        unstaked_finalizable

let with_metadata f (block, state) =
  match state.previous_metadata with
  | None -> assert false
  | Some metadata -> f metadata (block, state)

let exec_metadata f = exec_unit (with_metadata f)
