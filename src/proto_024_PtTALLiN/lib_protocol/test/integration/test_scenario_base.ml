(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Scenario, State
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_base.ml
    Subject:      Test basic functionality of the scenario framework.
*)

open Scenario

let test_expected_error =
  assert_failure
    ~expected_error:(fun _ errs ->
      Error_helpers.expect_failwith
        ~loc:__LOC__
        ~str:(Str.regexp_string "")
        errs)
    (exec (fun _ -> failwith ""))
  --> assert_failure
        ~expected_error:(fun _ errs ->
          Error_helpers.expect_failwith
            ~str:(Str.regexp ".*expected a specific error.*")
            ~loc:__LOC__
            errs)
        (assert_failure
           ~expected_error:(fun _ errs ->
             Error_helpers.check_error_constructor_name
               ~loc:__LOC__
               ~expected:
                 Protocol.Apply
                 .Staking_to_delegate_that_refuses_external_staking
               errs)
           (exec (fun _ -> failwith "")))

let check_rpc_level ~loc block =
  let open Lwt_result_syntax in
  let* level = Plugin.RPC.current_level Block.rpc_ctxt block in
  Log.info
    ~color:Log.Color.FG.gray
    "Level RPC %a"
    Protocol.Alpha_context.Level.pp_full
    level ;
  let current_level = Block.current_level block in
  let* () =
    Assert.equal_int32
      ~loc
      (Protocol.Alpha_context.Raw_level.to_int32 level.level)
      current_level
  in
  let* () =
    Assert.equal
      ~loc
      Protocol.Alpha_context.Cycle.( = )
      "Cycle aren't equal"
      Protocol.Alpha_context.Cycle.pp
      level.cycle
      (Block.current_cycle block)
  in
  (* In the tests, we assume that we start from level = 0. *)
  let* () = Assert.equal_int32 ~loc level.level_position current_level in
  let* () =
    Assert.equal_int32
      ~loc
      level.cycle_position
      (Int32.rem current_level block.constants.blocks_per_cycle)
  in
  return_unit

let print_rpc_block =
  exec_unit (fun (block, _state) -> check_rpc_level ~loc:__LOC__ block)

let test_bake_until_next_cycle =
  let check_is_first_level_of_cycle ~loc =
    exec_unit (fun (block, _state) ->
        let open Lwt_result_syntax in
        let* () = check_rpc_level ~loc block in
        let is_fst_lvl = Block.first_block_of_cycle block in
        Assert.is_true ~loc is_fst_lvl)
  in
  let wait_n_blocks_and_then_next_cycle ~loc n =
    print_rpc_block
    --> loop n (next_block --> print_rpc_block)
    --> next_cycle
    --> check_is_first_level_of_cycle ~loc
  in
  let blocks_per_cycle = 8l in
  init_constants ~blocks_per_cycle ()
  --> begin_test ["delegate"]
  --> List.fold_left
        (fun acc i ->
          acc
          |+ Tag (string_of_int i ^ " blocks to bake")
             --> wait_n_blocks_and_then_next_cycle ~loc:__LOC__ i)
        Empty
        (Stdlib.List.init Int32.(to_int blocks_per_cycle + 1) (fun i -> i))

let test_bake_until_cycle_end =
  let check_is_last_level_of_cycle ~loc =
    exec_unit (fun (block, _state) ->
        let open Lwt_result_syntax in
        let* () = check_rpc_level ~loc block in
        let is_last_lvl = Block.last_block_of_cycle block in
        Assert.is_true ~loc is_last_lvl)
  in
  let wait_n_blocks_and_then_cycle_end ~loc n =
    print_rpc_block
    --> loop n (next_block --> print_rpc_block)
    --> exec bake_until_dawn_of_next_cycle
    --> check_is_last_level_of_cycle ~loc
  in
  let blocks_per_cycle = 8l in
  init_constants ~blocks_per_cycle ()
  --> begin_test ["delegate"]
  --> List.fold_left
        (fun acc i ->
          acc
          |+ Tag (string_of_int i ^ " blocks to bake")
             --> wait_n_blocks_and_then_cycle_end ~loc:__LOC__ i)
        Empty
        (Stdlib.List.init Int32.(to_int blocks_per_cycle + 1) (fun i -> i))

let test_bake_until_cycle_end_but_one =
  let check_is_last_but_one_level_of_cycle ~loc =
    exec_unit (fun (block, _state) ->
        let open Lwt_result_syntax in
        let* () = check_rpc_level ~loc block in
        let is_last_but_one_lvl = Block.last_but_one_block_of_cycle block in
        Assert.is_true ~loc is_last_but_one_lvl)
  in
  let wait_n_blocks_and_then_cycle_end_but_one ~loc n =
    print_rpc_block
    --> loop n (next_block --> print_rpc_block)
    --> exec bake_until_next_cycle_end_but_one
    --> check_is_last_but_one_level_of_cycle ~loc
  in
  let blocks_per_cycle = 8l in
  init_constants ~blocks_per_cycle ()
  --> begin_test ["delegate"]
  --> List.fold_left
        (fun acc i ->
          acc
          |+ Tag (string_of_int i ^ " blocks to bake")
             --> wait_n_blocks_and_then_cycle_end_but_one ~loc:__LOC__ i)
        Empty
        (Stdlib.List.init Int32.(to_int blocks_per_cycle + 1) (fun i -> i))

let bake_until_tests =
  tests_of_scenarios
  @@ [
       ("Test bake until next cycle", test_bake_until_next_cycle);
       ("Test bake until cycle end", test_bake_until_cycle_end);
       ("Test bake until cycle end but one", test_bake_until_cycle_end_but_one);
     ]

(* test_expected_error has a different type *)
let tests =
  tests_of_scenarios
  @@ [("Test expected error in assert failure", test_expected_error)]

let () =
  register_tests
    ~__FILE__
    ~tags:["protocol"; "scenario"; "base"]
    (tests @ bake_until_tests)
