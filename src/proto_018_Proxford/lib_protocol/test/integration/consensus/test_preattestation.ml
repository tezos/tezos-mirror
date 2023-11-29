(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (preattestation)
    Invocation: dune exec src/proto_018_Proxford/lib_protocol/test/integration/consensus/main.exe \
                 -- --file test_preattestation.ml
*)

open Protocol
open Alpha_context

(****************************************************************)
(*                    Utility functions                         *)
(****************************************************************)

let init_genesis ?policy () =
  let open Lwt_result_syntax in
  let* genesis, _contracts = Context.init_n ~consensus_threshold:0 5 () in
  let* b = Block.bake ?policy genesis in
  return (genesis, b)

(****************************************************************)
(*                      Tests                                   *)
(****************************************************************)

(** Test that the preattestation's branch does not affect its
    validity. *)
let test_preattestation_with_arbitrary_branch () =
  let open Lwt_result_syntax in
  let* genesis, _contract = Context.init1 () in
  let* blk = Block.bake genesis in
  let* operation = Op.preattestation ~branch:Block_hash.zero blk in
  let* inc = Incremental.begin_construction ~mempool_mode:true blk in
  let* _inc = Incremental.validate_operation inc operation in
  return_unit

(** Consensus operation for future level : apply a preattestation with a level in the future *)
let test_consensus_operation_preattestation_for_future_level () =
  let open Lwt_result_syntax in
  let* _genesis, pred = init_genesis () in
  let raw_level = Raw_level.of_int32 (Int32.of_int 10) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~attested_block:pred
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_future_level {kind; _}
        when kind = Validate_errors.Consensus.Preattestation ->
          true
      | _ -> false)
    Preattestation
    Mempool

(** Consensus operation for old level : apply a preattestation with a level in the past *)
let test_consensus_operation_preattestation_for_old_level () =
  let open Lwt_result_syntax in
  let* _genesis, grandparent = init_genesis () in
  let* pred = Block.bake grandparent in
  let raw_level = Raw_level.of_int32 (Int32.of_int 0) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~attested_block:pred
    ~level
    ~error:(function
      | Validate_errors.Consensus.Consensus_operation_for_old_level {kind; _}
        when kind = Validate_errors.Consensus.Preattestation ->
          true
      | _ -> false)
    Preattestation
    Mempool

(** Consensus operation for future round : apply a preattestation with a round in the future *)
let test_consensus_operation_preattestation_for_future_round () =
  let open Lwt_result_wrap_syntax in
  let* _genesis, pred = init_genesis () in
  let*?@ round = Round.of_int 21 in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~attested_block:pred
    ~round
    Preattestation
    Mempool

(** Consensus operation for old round : apply a preattestation with a round in the past *)
let test_consensus_operation_preattestation_for_old_round () =
  let open Lwt_result_wrap_syntax in
  let* _genesis, pred = init_genesis ~policy:(By_round 10) () in
  let*?@ round = Round.of_int 0 in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~attested_block:pred
    ~round
    Preattestation
    Mempool

(** Consensus operation on competing proposal : apply a preattestation on a competing proposal *)
let test_consensus_operation_preattestation_on_competing_proposal () =
  let open Lwt_result_syntax in
  let* _genesis, pred = init_genesis () in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~attested_block:pred
    ~block_payload_hash:Block_payload_hash.zero
    Preattestation
    Mempool

(** Unexpected preattestations in block : apply a preattestation with an incorrect round *)
let test_unexpected_preattestations_in_blocks () =
  let open Lwt_result_syntax in
  let* _genesis, pred = init_genesis () in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~attested_block:pred
    ~error:(function
      | Validate_errors.Consensus.Unexpected_preattestation_in_block -> true
      | _ -> false)
    Preattestation
    Application

(** Round too high : apply a preattestation with a too high round *)
let test_too_high_round () =
  let open Lwt_result_wrap_syntax in
  let* _genesis, pred = init_genesis () in
  let raw_level = Raw_level.of_int32 (Int32.of_int 2) in
  let level = match raw_level with Ok l -> l | Error _ -> assert false in
  let*?@ round = Round.of_int 1 in
  Consensus_helpers.test_consensus_operation
    ~loc:__LOC__
    ~attested_block:pred
    ~round
    ~level
    ~error:(function
      | Validate_errors.Consensus.Preattestation_round_too_high _ -> true
      | _ -> false)
    Preattestation
    Construction

(** Duplicate preattestation : apply a preattestation that has already been applied. *)
let test_duplicate_preattestation () =
  let open Lwt_result_syntax in
  let* genesis, _ = init_genesis () in
  let* b = Block.bake genesis in
  let* inc = Incremental.begin_construction ~mempool_mode:true b in
  let* operation = Op.preattestation b in
  let* inc = Incremental.add_operation inc operation in
  let* operation = Op.preattestation b in
  let*! res = Incremental.add_operation inc operation in
  Assert.proto_error_with_info
    ~loc:__LOC__
    res
    "Double inclusion of consensus operation"

(** Preattestation for next level *)
let test_preattestation_for_next_level () =
  let open Lwt_result_syntax in
  let* genesis, _ = init_genesis () in
  Consensus_helpers.test_consensus_op_for_next
    ~genesis
    ~kind:`Preattestation
    ~next:`Level

(** Preattestation for next round *)
let test_preattestation_for_next_round () =
  let open Lwt_result_syntax in
  let* genesis, _ = init_genesis () in
  Consensus_helpers.test_consensus_op_for_next
    ~genesis
    ~kind:`Preattestation
    ~next:`Round

let tests =
  let module AppMode = Test_preattestation_functor.BakeWithMode (struct
    let name = "AppMode"

    let baking_mode = Block.Application
  end) in
  let module ConstrMode = Test_preattestation_functor.BakeWithMode (struct
    let name = "ConstrMode"

    let baking_mode = Block.Baking
  end) in
  AppMode.tests @ ConstrMode.tests
  @ [
      Tztest.tztest
        "Preattestation with arbitrary branch"
        `Quick
        test_preattestation_with_arbitrary_branch;
      Tztest.tztest
        "Preattestation for future level"
        `Quick
        test_consensus_operation_preattestation_for_future_level;
      Tztest.tztest
        "Preattestation for old level"
        `Quick
        test_consensus_operation_preattestation_for_old_level;
      Tztest.tztest
        "Preattestation for future round"
        `Quick
        test_consensus_operation_preattestation_for_future_round;
      Tztest.tztest
        "Preattestation for old round"
        `Quick
        test_consensus_operation_preattestation_for_old_round;
      Tztest.tztest
        "Preattestation on competing proposal"
        `Quick
        test_consensus_operation_preattestation_on_competing_proposal;
      Tztest.tztest
        "Unexpected preattestations in blocks"
        `Quick
        test_unexpected_preattestations_in_blocks;
      Tztest.tztest "Preattestations round too high" `Quick test_too_high_round;
      Tztest.tztest
        "Duplicate preattestation"
        `Quick
        test_duplicate_preattestation;
      Tztest.tztest
        "Preattestation for next level"
        `Quick
        test_preattestation_for_next_level;
      Tztest.tztest
        "Preattestation for next round"
        `Quick
        test_preattestation_for_next_round;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("preattestation", tests)]
  |> Lwt_main.run
