(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Protocol
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/validate/main.exe \
                  -- --file test_mempool.ml
    Subject:      Integration > Validate > Mempool mode
*)

open Protocol
open Alpha_context
module Mempool = Mempool_validation

let extract_values ctxt (b : Block.t) =
  let predecessor_level =
    Level.from_raw ctxt (Raw_level.of_int32_exn b.header.shell.level)
  in
  let fitness =
    Fitness.from_raw b.header.shell.fitness |> function
    | Ok v -> v
    | Error _ -> assert false
  in
  let predecessor_round = Fitness.round fitness in
  let predecessor_hash = b.header.shell.predecessor in
  (predecessor_level, predecessor_round, predecessor_hash)

let op_with_hash op = (Operation.hash_packed op, op)

let expect_ok_added ~__LOC__ x =
  match x with
  | Ok (mempool, Mempool.Added) -> mempool
  | _ -> Format.kasprintf Stdlib.failwith "%s: expected added" __LOC__

let expect_conflict ~__LOC__ x =
  match (x : ('a, Mempool.add_error) result) with
  | Error (Add_conflict _) -> ()
  | _ -> Format.kasprintf Stdlib.failwith "%s: expected conflict" __LOC__

let expect_conflict_handled ~__LOC__ kind x =
  match (x : (Mempool.t * Mempool.add_result, Mempool.add_error) result) with
  | Ok (mempool, kind') when kind = kind' -> mempool
  | _ ->
      Format.kasprintf Stdlib.failwith "%s: expected handled conflict" __LOC__

let handler_always_keep ~existing_operation:_ ~new_operation:_ = `Keep

let handler_always_replace ~existing_operation:_ ~new_operation:_ = `Replace

let assert_empty_mempool ~__LOC__ mempool =
  let operations = Mempool.operations mempool in
  Assert.equal_bool
    ~loc:__LOC__
    true
    (Environment.Operation_hash.Map.is_empty operations)

let assert_operation_present_in_mempool ~__LOC__ mempool ophl =
  let operations = Mempool.operations mempool in
  let resulting_mempool_operations =
    Environment.Operation_hash.Map.bindings operations
    |> List.map fst
    |> List.sort Operation_hash.compare
  in
  let expected_operations = List.sort Operation_hash.compare ophl in
  Assert.assert_equal_list
    ~loc:__LOC__
    Operation_hash.equal
    "operations present in mempool"
    Operation_hash.pp
    resulting_mempool_operations
    expected_operations

let test_simple () =
  let open Lwt_result_syntax in
  let* block, (c1, c2) = Context.init2 () in
  let* ctxt =
    let+ incr = Incremental.begin_construction block in
    Incremental.alpha_ctxt incr
  in
  let predecessor_level, predecessor_round, predecessor_hash =
    extract_values ctxt block
  in
  let vs, mempool =
    Mempool.init
      ctxt
      Chain_id.zero
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  let* op1 = Op.transaction (B block) c1 c2 Tez.one_cent in
  let op1 = op_with_hash op1 in
  let* op1' = Op.transaction (B block) c1 c2 Tez.one in
  let op1' = op_with_hash op1' in
  let* op2 = Op.transaction (B block) c2 c1 Tez.one in
  let op2 = op_with_hash op2 in
  let*! res = Mempool.add_operation vs mempool op1 in
  let mempool = expect_ok_added ~__LOC__ res in
  let*! res = Mempool.add_operation vs mempool op2 in
  let mempool = expect_ok_added ~__LOC__ res in
  let*! res = Mempool.add_operation vs mempool op1' in
  let () = expect_conflict ~__LOC__ res in
  return_unit

let test_imcompatible_mempool () =
  let open Lwt_result_syntax in
  let* block, _ = Context.init1 ~consensus_threshold:0 () in
  let* block = Block.bake block in
  let* ctxt =
    let+ incr = Incremental.begin_construction block in
    Incremental.alpha_ctxt incr
  in
  let predecessor_level, predecessor_round, predecessor_hash =
    extract_values ctxt block
  in
  let (_vs : Mempool.validation_info), mempool1 =
    Mempool.init
      ctxt
      Chain_id.zero
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  (* Create a second mempool on a different block *)
  let* block2 = Block.bake block in
  let* ctxt2 =
    let+ incr = Incremental.begin_construction block2 in
    Incremental.alpha_ctxt incr
  in
  let predecessor_level, predecessor_round, predecessor_hash2 =
    extract_values ctxt2 block2
  in
  let (_vs : Mempool.validation_info), mempool2 =
    Mempool.init
      ctxt2
      Chain_id.zero
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash:predecessor_hash2
  in
  let () =
    match Mempool.merge mempool1 mempool2 with
    | Error Mempool.Incompatible_mempool -> ()
    | Error (Merge_conflict _) ->
        Format.kasprintf
          Stdlib.failwith
          "%s: expected incompatible mempool"
          __LOC__
    | Ok _ -> Format.kasprintf Stdlib.failwith "%s: expected conflict" __LOC__
  in
  return_unit

let test_merge () =
  let open Lwt_result_syntax in
  let* block, (c1, c2) = Context.init2 () in
  let* ctxt =
    let+ incr = Incremental.begin_construction block in
    Incremental.alpha_ctxt incr
  in
  let predecessor_level, predecessor_round, predecessor_hash =
    extract_values ctxt block
  in
  let vs, mempool_i =
    Mempool.init
      ctxt
      Chain_id.zero
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  (* Build two mempool with a conflicting operation and check that the
     merge fails and succeeds when a conflict handler is provided *)
  let* op1 = Op.transaction (B block) c1 c2 Tez.one_cent in
  let op1 = op_with_hash op1 in
  let* op2 = Op.transaction (B block) c2 c1 Tez.one in
  let op2 = op_with_hash op2 in
  let*! res = Mempool.add_operation vs mempool_i op1 in
  let mempool1 = expect_ok_added ~__LOC__ res in
  let*! res = Mempool.add_operation vs mempool_i op2 in
  let mempool2 = expect_ok_added ~__LOC__ res in
  let merged_non_conflicting_mempool =
    match Mempool.merge mempool1 mempool2 with
    | Ok mempool -> mempool
    | _ ->
        Format.kasprintf Stdlib.failwith "%s: expected succesful merge" __LOC__
  in
  let* op1' = Op.transaction (B block) c1 c2 Tez.one in
  let op1' = op_with_hash op1' in
  let*! res = Mempool.add_operation vs mempool_i op1' in
  let mempool3 = expect_ok_added ~__LOC__ res in
  let*! res = Mempool.add_operation vs mempool3 op2 in
  let mempool3 = expect_ok_added ~__LOC__ res in
  let () =
    match Mempool.merge merged_non_conflicting_mempool mempool3 with
    | Error (Merge_conflict _) -> ()
    | _ -> Format.kasprintf Stdlib.failwith "%s: expected conflict" __LOC__
  in
  let merged_mempool_replace =
    match
      Mempool.merge
        ~conflict_handler:handler_always_replace
        merged_non_conflicting_mempool
        mempool3
    with
    | Ok mempool -> mempool
    | _ ->
        Format.kasprintf Stdlib.failwith "%s: expected succesful merge" __LOC__
  in
  let* () =
    assert_operation_present_in_mempool
      ~__LOC__
      merged_mempool_replace
      (List.map fst [op1'; op2])
  in
  let merged_mempool_keep =
    match
      Mempool.merge
        ~conflict_handler:handler_always_keep
        merged_non_conflicting_mempool
        mempool3
    with
    | Ok mempool -> mempool
    | _ ->
        Format.kasprintf Stdlib.failwith "%s: expected succesful merge" __LOC__
  in
  let* () =
    assert_operation_present_in_mempool
      ~__LOC__
      merged_mempool_keep
      (List.map fst [op1; op2])
  in
  (* Check that merging a mempool with itself is a success and returns
     the identity *)
  let* () =
    match Mempool.merge mempool1 mempool1 with
    | Ok mempool ->
        let expected_operations =
          Environment.Operation_hash.Map.bindings (Mempool.operations mempool1)
          |> List.map fst
        in
        assert_operation_present_in_mempool ~__LOC__ mempool expected_operations
    | Error _ -> assert false
  in
  return_unit

let test_add_invalid_operation () =
  let open Lwt_result_syntax in
  let* block, c1 = Context.init1 () in
  let* ctxt =
    let+ incr = Incremental.begin_construction block in
    Incremental.alpha_ctxt incr
  in
  let predecessor_level, predecessor_round, predecessor_hash =
    extract_values ctxt block
  in
  let vs, mempool_i =
    Mempool.init
      ctxt
      Chain_id.zero
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  let* op1 = Op.transaction (B block) c1 c1 ~gas_limit:Zero Tez.one_cent in
  let op1 = op_with_hash op1 in
  let*! res = Mempool.add_operation vs mempool_i op1 in
  match res with
  | Error (Mempool.Validation_error _) -> return_unit
  | Error _ -> Stdlib.failwith "unexpected error"
  | Ok _ -> Stdlib.failwith "unexpected success"

let test_add_and_replace () =
  let open Lwt_result_syntax in
  let* block, (c1, c2) = Context.init2 () in
  let* ctxt =
    let+ incr = Incremental.begin_construction block in
    Incremental.alpha_ctxt incr
  in
  let predecessor_level, predecessor_round, predecessor_hash =
    extract_values ctxt block
  in
  let info, mempool_i =
    Mempool.init
      ctxt
      Chain_id.zero
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  (* Try adding a conflicting operation using both handler strategy *)
  let* op1 = Op.transaction (B block) c1 c2 Tez.one_cent in
  let op1 = op_with_hash op1 in
  let* op1' = Op.transaction (B block) c1 c2 Tez.one in
  let op1' = op_with_hash op1' in
  let*! res = Mempool.add_operation info mempool_i op1 in
  let mempool = expect_ok_added ~__LOC__ res in
  let*! res = Mempool.add_operation info mempool op1' in
  let () = expect_conflict ~__LOC__ res in
  let*! res =
    Mempool.add_operation
      ~conflict_handler:handler_always_keep
      info
      mempool
      op1'
  in
  let final_mempool = expect_conflict_handled ~__LOC__ Unchanged res in
  let* () =
    assert_operation_present_in_mempool ~__LOC__ final_mempool [fst op1]
  in
  let*! res =
    Mempool.add_operation
      ~conflict_handler:handler_always_replace
      info
      mempool
      op1'
  in
  let final_mempool =
    expect_conflict_handled ~__LOC__ (Replaced {removed = fst op1}) res
  in
  let* () =
    assert_operation_present_in_mempool ~__LOC__ final_mempool [fst op1']
  in
  return_unit

let test_remove_operation () =
  let open Lwt_result_syntax in
  let* block, (c1, c2) = Context.init2 () in
  let* ctxt =
    let+ incr = Incremental.begin_construction block in
    Incremental.alpha_ctxt incr
  in
  let predecessor_level, predecessor_round, predecessor_hash =
    extract_values ctxt block
  in
  let info, mempool_i =
    Mempool.init
      ctxt
      Chain_id.zero
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash
  in
  let* op1 = Op.transaction (B block) c1 c2 Tez.one_cent in
  let op1 = op_with_hash op1 in
  let* op2 = Op.transaction (B block) c1 c2 Tez.one in
  let op2 = op_with_hash op2 in
  (* Add one operation to the mempoolg *)
  let*! res = Mempool.add_operation info mempool_i op1 in
  let mempool = expect_ok_added ~__LOC__ res in
  let* () = assert_operation_present_in_mempool ~__LOC__ mempool [fst op1] in
  (* Try removing unknown operation and check that the mempool is unchanged *)
  let mempool = Mempool.remove_operation mempool (fst op2) in
  let* () = assert_operation_present_in_mempool ~__LOC__ mempool [fst op1] in
  (* Try removing known operation and ensure that the mempool is empty  *)
  let empty_mempool = Mempool.remove_operation mempool (fst op1) in
  assert_empty_mempool ~__LOC__ empty_mempool

let tests =
  [
    Tztest.tztest "simple" `Quick test_simple;
    Tztest.tztest "incompatible mempool" `Quick test_imcompatible_mempool;
    Tztest.tztest "merge" `Quick test_merge;
    Tztest.tztest "adding invalid operation" `Quick test_add_invalid_operation;
    Tztest.tztest
      "adding operation with conflict handler"
      `Quick
      test_add_and_replace;
    Tztest.tztest "remove operations" `Quick test_remove_operation;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("mempool", tests)] |> Lwt_main.run
