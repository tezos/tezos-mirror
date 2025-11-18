(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    Component:  Protocol (combined operations)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_combined_operations.ml

    Subject:    Multiple manager operations can be grouped in one
                batch, ensuring their ordered and atomic application
                (that is, either they all succeed, or none of them is
                applied).

                More precisely, if any operation in the batch fails at
                application time, then the previously applied
                operations are backtracked, leaving the context
                unchanged, and the remaining operations are
                skipped. However, note that the fees for the batch are
                collected by the baker regardless of the success or
                failure of the application of the batch.

                All manager operations in a batch must have the same
                source.

                There may be overlap with
                [lib_protocol/test/integration/validate/test_validation_batch.ml].
*)

open Protocol
open Alpha_context
open Error_helpers

let ten_tez = Tez_helpers.of_int 10

let gas_limit = Op.Custom_gas (Alpha_context.Gas.Arith.integral_of_int_exn 3000)

(** Groups ten transactions between the same parties. *)
let test_multiple_transfers () =
  let open Lwt_result_syntax in
  let* blk, (c1, c2, c3) = Context.init3 () in
  let* ops =
    List.map_es
      (fun _ -> Op.transaction ~gas_limit (B blk) c1 c2 Tez.one)
      (1 -- 10)
  in
  let* operation = Op.combine_operations ~source:c1 (B blk) ops in
  let baker_pkh = Context.Contract.pkh c3 in
  let* inc =
    Incremental.begin_construction ~policy:(By_account baker_pkh) blk
  in
  let* c1_old_balance = Context.Contract.balance (I inc) c1 in
  let* c2_old_balance = Context.Contract.balance (I inc) c2 in
  let* inc = Incremental.add_operation inc operation in
  let* () =
    Assert.balance_was_debited
      ~loc:__LOC__
      (I inc)
      c1
      c1_old_balance
      (Tez_helpers.of_int 10)
  in
  let* () =
    Assert.balance_was_credited
      ~loc:__LOC__
      (I inc)
      c2
      c2_old_balance
      (Tez_helpers.of_int 10)
  in
  return_unit

(** Groups ten delegated originations. *)
let test_multiple_origination_and_delegation () =
  let open Lwt_result_syntax in
  let* blk, (c1, c2) = Context.init2 () in
  let n = 10 in
  let* {parametric = {origination_size; cost_per_byte; _}; _} =
    Context.get_constants (B blk)
  in
  let delegate_pkh = Context.Contract.pkh c2 in
  (* Deploy n smart contracts with dummy scripts from c1 *)
  let* originations =
    List.map_es
      (fun i ->
        Op.contract_origination
          ~gas_limit
          ~delegate:delegate_pkh
          ~counter:(Manager_counter.Internal_for_tests.of_int i)
          ~fee:Tez.zero
          ~script:Op.dummy_script
          ~credit:(Tez_helpers.of_int 10)
          (B blk)
          c1)
      (1 -- n)
  in
  (* These computed originated contracts are not the ones really created *)
  (* We will extract them from the tickets *)
  let originations_operations, _ = List.split originations in
  let* operation =
    Op.combine_operations ~source:c1 (B blk) originations_operations
  in
  let* inc = Incremental.begin_construction blk in
  let* c1_old_balance = Context.Contract.balance (I inc) c1 in
  let* inc = Incremental.add_operation inc operation in
  (* To retrieve the originated contracts, it is easier to extract them
     from the tickets. Else, we could (could we ?) hash each combined
     operation individually. *)
  let tickets = Incremental.rev_tickets inc in
  let open Apply_results in
  let tickets =
    List.fold_left
      (fun acc -> function
        | No_operation_metadata -> assert false
        | Operation_metadata {contents} ->
            to_list (Contents_result_list contents) @ acc)
      []
      tickets
    |> List.rev
  in
  let new_contracts =
    List.map
      (function
        | Contents_result
            (Manager_operation_result
               {
                 operation_result =
                   Applied (Origination_result {originated_contracts = [h]; _});
                 _;
               }) ->
            h
        | _ -> assert false)
      tickets
  in
  (* Previous balance - (Credit (n * 10tz) + Origination cost (n tz)) *)
  let*? origination_burn =
    Tez_helpers.(cost_per_byte *? Int64.of_int origination_size)
  in
  let*? origination_total_cost =
    Tez_helpers.(origination_burn *? Int64.of_int n)
  in
  let*? t = Tez_helpers.( *? ) Op.dummy_script_cost 10L in
  let*? t = Tez_helpers.( +? ) (Tez_helpers.of_int (10 * n)) t in
  let*? total_cost = Tez_helpers.( +? ) origination_total_cost t in
  let* () =
    Assert.balance_was_debited ~loc:__LOC__ (I inc) c1 c1_old_balance total_cost
  in
  List.iter_es
    (fun c ->
      let c = Contract.Originated c in
      Assert.balance_is ~loc:__LOC__ (I inc) c (Tez_helpers.of_int 10))
    new_contracts

let expect_apply_failure =
  let open Lwt_result_syntax in
  function
  | Environment.Ecoproto_error err :: _ ->
      Assert.test_error_encodings err ;
      let error_info =
        Error_monad.find_info_of_error (Environment.wrap_tzerror err)
      in
      if error_info.title = "Balance too low" then return_unit
      else failwith "unexpected error"
  | _ -> failwith "balance too low should fail"

(** Groups three operations, the middle one failing.
    Checks that the receipt is consistent.
    Variant without fees. *)
let test_failing_operation_in_the_middle () =
  let open Lwt_result_syntax in
  let* blk, (c1, c2) = Context.init2 () in
  let* op1 = Op.transaction ~gas_limit ~fee:Tez.zero (B blk) c1 c2 Tez.one in
  let* op2 =
    Op.transaction ~gas_limit ~fee:Tez.zero (B blk) c1 c2 Tez_helpers.max_tez
  in
  let* op3 = Op.transaction ~gas_limit ~fee:Tez.zero (B blk) c1 c2 Tez.one in
  let operations = [op1; op2; op3] in
  let* operation = Op.combine_operations ~source:c1 (B blk) operations in
  let* inc = Incremental.begin_construction blk in
  let* c1_old_balance = Context.Contract.balance (I inc) c1 in
  let* c2_old_balance = Context.Contract.balance (I inc) c2 in
  let* inc = Incremental.add_operation ~expect_apply_failure inc operation in
  let tickets = Incremental.rev_tickets inc in
  let open Apply_results in
  let tickets =
    List.fold_left
      (fun acc -> function
        | No_operation_metadata -> assert false
        | Operation_metadata {contents} ->
            to_list (Contents_result_list contents) @ acc)
      []
      tickets
  in
  (match tickets with
  | Contents_result
      (Manager_operation_result {operation_result = Backtracked _; _})
    :: Contents_result
         (Manager_operation_result {operation_result = Failed (_, trace); _})
    :: Contents_result
         (Manager_operation_result {operation_result = Skipped _; _})
    :: _ ->
      let trace_string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace trace
      in
      let expect =
        Format.asprintf "Balance of contract %a too low" Context.Contract.pp c1
      in
      assert (Astring.String.is_infix ~affix:expect trace_string)
  | _ -> assert false) ;
  let* () = Assert.balance_is ~loc:__LOC__ (I inc) c1 c1_old_balance in
  let* () = Assert.balance_is ~loc:__LOC__ (I inc) c2 c2_old_balance in
  return_unit

(** Groups three operations, the middle one failing.
    Checks that the receipt is consistent.
    Variant with fees, that should be spent even in case of failure. *)
let test_failing_operation_in_the_middle_with_fees () =
  let open Lwt_result_syntax in
  let* blk, (c1, c2) = Context.init2 () in
  let* op1 = Op.transaction ~fee:Tez.one (B blk) c1 c2 Tez.one in
  let* op2 = Op.transaction ~fee:Tez.one (B blk) c1 c2 Tez_helpers.max_tez in
  let* op3 = Op.transaction ~fee:Tez.one (B blk) c1 c2 Tez.one in
  let operations = [op1; op2; op3] in
  let* operation = Op.combine_operations ~source:c1 (B blk) operations in
  let* inc = Incremental.begin_construction blk in
  let* c1_old_balance = Context.Contract.balance (I inc) c1 in
  let* c2_old_balance = Context.Contract.balance (I inc) c2 in
  let* inc = Incremental.add_operation ~expect_apply_failure inc operation in
  let tickets = Incremental.rev_tickets inc in
  let open Apply_results in
  let tickets =
    List.fold_left
      (fun acc -> function
        | No_operation_metadata -> assert false
        | Operation_metadata {contents} ->
            to_list (Contents_result_list contents) @ acc)
      []
      tickets
  in
  (match tickets with
  | Contents_result
      (Manager_operation_result {operation_result = Backtracked _; _})
    :: Contents_result
         (Manager_operation_result {operation_result = Failed (_, trace); _})
    :: Contents_result
         (Manager_operation_result {operation_result = Skipped _; _})
    :: _ ->
      let trace_string =
        Format.asprintf "%a" Environment.Error_monad.pp_trace trace
      in
      let expect =
        Format.asprintf "Balance of contract %a too low" Context.Contract.pp c1
      in
      assert (Astring.String.is_infix ~affix:expect trace_string)
  | _ -> assert false) ;
  (* In the presence of a failure, all the fees are collected. Even for skipped operations. *)
  let* () =
    Assert.balance_was_debited
      ~loc:__LOC__
      (I inc)
      c1
      c1_old_balance
      (Tez_helpers.of_int 3)
  in
  let* () = Assert.balance_is ~loc:__LOC__ (I inc) c2 c2_old_balance in
  return_unit

let test_wrong_signature_in_the_middle () =
  let open Lwt_result_syntax in
  let* blk, (c1, c2) = Context.init2 ~consensus_threshold_size:0 () in
  let* op1 = Op.transaction ~gas_limit ~fee:Tez.one (B blk) c1 c2 Tez.one in
  let* op2 = Op.transaction ~gas_limit ~fee:Tez.one (B blk) c2 c1 Tez.one in
  (* Make legit transfers, performing reveals *)
  let* b = Block.bake ~operations:[op1; op2] blk in
  (* Make c2 reach counter 5 *)
  let* operation = Op.transaction ~gas_limit ~fee:Tez.one (B b) c2 c1 Tez.one in
  let* b = Block.bake ~operation b in
  let* operation = Op.transaction ~gas_limit ~fee:Tez.one (B b) c2 c1 Tez.one in
  let* b = Block.bake ~operation b in
  let* operation = Op.transaction ~gas_limit ~fee:Tez.one (B b) c2 c1 Tez.one in
  let* b = Block.bake ~operation b in
  (* Cook transactions for actual test *)
  let* op1 = Op.transaction ~gas_limit ~fee:Tez.one (B b) c1 c2 Tez.one in
  let* op2 = Op.transaction ~gas_limit ~fee:Tez.one (B b) c1 c2 Tez.one in
  let* op3 = Op.transaction ~gas_limit ~fee:Tez.one (B b) c1 c2 Tez.one in
  let* spurious_operation =
    Op.transaction ~gas_limit ~fee:Tez.one (B b) c2 c1 Tez.one
  in
  let operations = [op1; op2; op3] in
  let* operation =
    Op.combine_operations ~spurious_operation ~source:c1 (B b) operations
  in
  let* inc = Incremental.begin_construction b in
  let* (_ : Incremental.t) =
    let expect_failure =
      expect_inconsistent_sources ~loc:__LOC__ ~first_source:c1 ~source:c2
    in
    Incremental.add_operation ~expect_failure inc operation
  in
  return_unit

let test_inconsistent_counters () =
  let open Lwt_result_syntax in
  let* blk, (c1, c2) = Context.init2 () in
  let* op1 = Op.transaction ~gas_limit ~fee:Tez.one (B blk) c1 c2 Tez.one in
  let* op2 = Op.transaction ~gas_limit ~fee:Tez.one (B blk) c2 c1 Tez.one in
  (* Make legit transfers, performing reveals *)
  let* b = Block.bake ~operations:[op1; op2] blk in
  (* Now, counter c1 = counter c2 = 1, Op.transaction builds with counter + 1 *)
  let* op1 =
    Op.transaction
      ~gas_limit
      ~fee:Tez.one
      (B b)
      c1
      c2
      ~counter:(Manager_counter.Internal_for_tests.of_int 1)
      Tez.one
  in
  let* op2 =
    Op.transaction
      ~gas_limit
      ~fee:Tez.one
      (B b)
      c1
      c2
      ~counter:(Manager_counter.Internal_for_tests.of_int 2)
      Tez.one
  in
  let* op2' =
    Op.transaction
      ~gas_limit
      ~fee:Tez.one
      (B b)
      c1
      c2
      ~counter:(Manager_counter.Internal_for_tests.of_int 2)
      (Tez.of_mutez_exn 5_000L)
  in
  let* op3 =
    Op.transaction
      ~gas_limit
      ~fee:Tez.one
      (B b)
      c1
      c2
      ~counter:(Manager_counter.Internal_for_tests.of_int 3)
      Tez.one
  in
  let* op4 =
    Op.transaction
      ~gas_limit
      ~fee:Tez.one
      (B b)
      c1
      c2
      ~counter:(Manager_counter.Internal_for_tests.of_int 4)
      Tez.one
  in
  (* Canari: Check counters are ok *)
  let* op = Op.batch_operations ~source:c1 (B b) [op1; op2; op3; op4] in
  let* inc = Incremental.begin_construction b in
  let* (_ : Incremental.t) = Incremental.add_operation inc op in
  (* Gap in counter in the following op *)
  let* op = Op.batch_operations ~source:c1 (B b) [op1; op2; op4] in
  let* (_ : Incremental.t) =
    let expect_failure =
      expect_inconsistent_counters_int
        ~loc:__LOC__
        ~source:c1
        ~previous_counter:3
        ~counter:5
    in
    Incremental.add_operation ~expect_failure inc op
  in
  (* Same counter used twice in the following op *)
  let* op = Op.batch_operations ~source:c1 (B b) [op1; op2; op2'] in
  let* (_ : Incremental.t) =
    let expect_failure =
      expect_inconsistent_counters_int
        ~loc:__LOC__
        ~source:c1
        ~previous_counter:3
        ~counter:3
    in
    Incremental.add_operation ~expect_failure inc op
  in
  return_unit

let tests =
  [
    Tztest.tztest "multiple transfers" `Quick test_multiple_transfers;
    Tztest.tztest
      "multiple originations and delegations"
      `Quick
      test_multiple_origination_and_delegation;
    Tztest.tztest
      "Failing operation in the middle"
      `Quick
      test_failing_operation_in_the_middle;
    Tztest.tztest
      "Failing operation in the middle (with fees)"
      `Quick
      test_failing_operation_in_the_middle_with_fees;
    Tztest.tztest
      "Failing operation (wrong manager in the middle of a pack)"
      `Quick
      test_wrong_signature_in_the_middle;
    Tztest.tztest
      "Inconsistent counters in batch"
      `Quick
      test_inconsistent_counters;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("combined", tests)] |> Lwt_main.run
