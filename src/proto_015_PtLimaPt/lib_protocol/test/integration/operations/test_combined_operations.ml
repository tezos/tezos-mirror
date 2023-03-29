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
    Invocation: dune exec src/proto_015_PtLimaPt/lib_protocol/test/integration/main.exe
    Subject:    Multiple operations can be grouped in one ensuring their
                deterministic application.

                If an invalid operation is present in this group of
                operations, the previously applied operations are
                backtracked leaving the context unchanged and the
                following operations are skipped. Fees attributed to the
                operations are collected by the baker nonetheless.

                Only manager operations are allowed in multiple transactions.
                They must all belong to the same manager as there is only one
                signature.
*)

open Protocol
open Alpha_context

let ten_tez = Test_tez.of_int 10

let gas_limit = Op.Custom_gas (Alpha_context.Gas.Arith.integral_of_int_exn 3000)

(** Groups ten transactions between the same parties. *)
let test_multiple_transfers () =
  Context.init3 () >>=? fun (blk, (c1, c2, c3)) ->
  List.map_es
    (fun _ -> Op.transaction ~gas_limit (B blk) c1 c2 Tez.one)
    (1 -- 10)
  >>=? fun ops ->
  Op.combine_operations ~source:c1 (B blk) ops >>=? fun operation ->
  let baker_pkh = Context.Contract.pkh c3 in
  Incremental.begin_construction ~policy:(By_account baker_pkh) blk
  >>=? fun inc ->
  Context.Contract.balance (I inc) c1 >>=? fun c1_old_balance ->
  Context.Contract.balance (I inc) c2 >>=? fun c2_old_balance ->
  Incremental.add_operation inc operation >>=? fun inc ->
  Assert.balance_was_debited
    ~loc:__LOC__
    (I inc)
    c1
    c1_old_balance
    (Test_tez.of_int 10)
  >>=? fun () ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (I inc)
    c2
    c2_old_balance
    (Test_tez.of_int 10)
  >>=? fun () -> return_unit

(** Groups ten delegated originations. *)
let test_multiple_origination_and_delegation () =
  Context.init2 () >>=? fun (blk, (c1, c2)) ->
  let n = 10 in
  Context.get_constants (B blk)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  let delegate_pkh = Context.Contract.pkh c2 in
  (* Deploy n smart contracts with dummy scripts from c1 *)
  List.map_es
    (fun i ->
      Op.contract_origination
        ~gas_limit
        ~delegate:delegate_pkh
        ~counter:(Z.of_int i)
        ~fee:Tez.zero
        ~script:Op.dummy_script
        ~credit:(Test_tez.of_int 10)
        (B blk)
        c1)
    (1 -- n)
  >>=? fun originations ->
  (* These computed originated contracts are not the ones really created *)
  (* We will extract them from the tickets *)
  let originations_operations, _ = List.split originations in
  Op.combine_operations ~source:c1 (B blk) originations_operations
  >>=? fun operation ->
  Incremental.begin_construction blk >>=? fun inc ->
  Context.Contract.balance (I inc) c1 >>=? fun c1_old_balance ->
  Incremental.add_operation inc operation >>=? fun inc ->
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
  Test_tez.(cost_per_byte *? Int64.of_int origination_size)
  >>?= fun origination_burn ->
  Test_tez.(origination_burn *? Int64.of_int n)
  >>?= fun origination_total_cost ->
  Test_tez.( *? ) Op.dummy_script_cost 10L
  >>? Test_tez.( +? ) (Test_tez.of_int (10 * n))
  >>? Test_tez.( +? ) origination_total_cost
  >>?= fun total_cost ->
  Assert.balance_was_debited ~loc:__LOC__ (I inc) c1 c1_old_balance total_cost
  >>=? fun () ->
  List.iter_es
    (fun c ->
      let c = Contract.Originated c in
      Assert.balance_is ~loc:__LOC__ (I inc) c (Test_tez.of_int 10))
    new_contracts

let expect_apply_failure = function
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
  Context.init2 () >>=? fun (blk, (c1, c2)) ->
  Op.transaction ~gas_limit ~fee:Tez.zero (B blk) c1 c2 Tez.one >>=? fun op1 ->
  Op.transaction ~gas_limit ~fee:Tez.zero (B blk) c1 c2 Test_tez.max_tez
  >>=? fun op2 ->
  Op.transaction ~gas_limit ~fee:Tez.zero (B blk) c1 c2 Tez.one >>=? fun op3 ->
  let operations = [op1; op2; op3] in
  Op.combine_operations ~source:c1 (B blk) operations >>=? fun operation ->
  Incremental.begin_construction blk >>=? fun inc ->
  Context.Contract.balance (I inc) c1 >>=? fun c1_old_balance ->
  Context.Contract.balance (I inc) c2 >>=? fun c2_old_balance ->
  Incremental.add_operation ~expect_apply_failure inc operation >>=? fun inc ->
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
  Assert.balance_is ~loc:__LOC__ (I inc) c1 c1_old_balance >>=? fun () ->
  Assert.balance_is ~loc:__LOC__ (I inc) c2 c2_old_balance >>=? fun () ->
  return_unit

(** Groups three operations, the middle one failing.
    Checks that the receipt is consistent.
    Variant with fees, that should be spent even in case of failure. *)
let test_failing_operation_in_the_middle_with_fees () =
  Context.init2 () >>=? fun (blk, (c1, c2)) ->
  Op.transaction ~fee:Tez.one (B blk) c1 c2 Tez.one >>=? fun op1 ->
  Op.transaction ~fee:Tez.one (B blk) c1 c2 Test_tez.max_tez >>=? fun op2 ->
  Op.transaction ~fee:Tez.one (B blk) c1 c2 Tez.one >>=? fun op3 ->
  let operations = [op1; op2; op3] in
  Op.combine_operations ~source:c1 (B blk) operations >>=? fun operation ->
  Incremental.begin_construction blk >>=? fun inc ->
  Context.Contract.balance (I inc) c1 >>=? fun c1_old_balance ->
  Context.Contract.balance (I inc) c2 >>=? fun c2_old_balance ->
  Incremental.add_operation ~expect_apply_failure inc operation >>=? fun inc ->
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
  Assert.balance_was_debited
    ~loc:__LOC__
    (I inc)
    c1
    c1_old_balance
    (Test_tez.of_int 3)
  >>=? fun () ->
  Assert.balance_is ~loc:__LOC__ (I inc) c2 c2_old_balance >>=? fun () ->
  return_unit

let test_wrong_signature_in_the_middle () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (blk, (c1, c2)) ->
  Op.transaction ~gas_limit ~fee:Tez.one (B blk) c1 c2 Tez.one >>=? fun op1 ->
  Op.transaction ~gas_limit ~fee:Tez.one (B blk) c2 c1 Tez.one >>=? fun op2 ->
  (* Make legit transfers, performing reveals *)
  Block.bake ~operations:[op1; op2] blk >>=? fun b ->
  (* Make c2 reach counter 5 *)
  Op.transaction ~gas_limit ~fee:Tez.one (B b) c2 c1 Tez.one
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.transaction ~gas_limit ~fee:Tez.one (B b) c2 c1 Tez.one
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.transaction ~gas_limit ~fee:Tez.one (B b) c2 c1 Tez.one
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  (* Cook transactions for actual test *)
  Op.transaction ~gas_limit ~fee:Tez.one (B b) c1 c2 Tez.one >>=? fun op1 ->
  Op.transaction ~gas_limit ~fee:Tez.one (B b) c1 c2 Tez.one >>=? fun op2 ->
  Op.transaction ~gas_limit ~fee:Tez.one (B b) c1 c2 Tez.one >>=? fun op3 ->
  Op.transaction ~gas_limit ~fee:Tez.one (B b) c2 c1 Tez.one
  >>=? fun spurious_operation ->
  let operations = [op1; op2; op3] in

  Op.combine_operations ~spurious_operation ~source:c1 (B b) operations
  >>=? fun operation ->
  let expect_failure = function
    | Environment.Ecoproto_error
        (Validate_errors.Manager.Inconsistent_sources as err)
      :: _ ->
        Assert.test_error_encodings err ;
        return_unit
    | _ ->
        failwith
          "Packed operation has invalid source in the middle : operation \
           expected to fail."
  in
  Incremental.begin_construction b >>=? fun inc ->
  Incremental.add_operation ~expect_failure inc operation >>=? fun _inc ->
  return_unit

let expect_inconsistent_counters list =
  if
    List.exists
      (function
        | Environment.Ecoproto_error
            Validate_errors.Manager.Inconsistent_counters ->
            true
        | _ -> false)
      list
  then return_unit
  else
    failwith
      "Packed operation has inconsistent counters : operation expected to fail \
       but got errors: %a."
      Error_monad.pp_print_trace
      list

let test_inconsistent_counters () =
  Context.init2 () >>=? fun (blk, (c1, c2)) ->
  Op.transaction ~gas_limit ~fee:Tez.one (B blk) c1 c2 Tez.one >>=? fun op1 ->
  Op.transaction ~gas_limit ~fee:Tez.one (B blk) c2 c1 Tez.one >>=? fun op2 ->
  (* Make legit transfers, performing reveals *)
  Block.bake ~operations:[op1; op2] blk >>=? fun b ->
  (* Now, counter c1 = counter c2 = 1, Op.transaction builds with counter + 1 *)
  Op.transaction ~gas_limit ~fee:Tez.one (B b) c1 c2 ~counter:Z.one Tez.one
  >>=? fun op1 ->
  Op.transaction
    ~gas_limit
    ~fee:Tez.one
    (B b)
    c1
    c2
    ~counter:(Z.of_int 2)
    Tez.one
  >>=? fun op2 ->
  Op.transaction
    ~gas_limit
    ~fee:Tez.one
    (B b)
    c1
    c2
    ~counter:(Z.of_int 2)
    (Tez.of_mutez_exn 5_000L)
  >>=? fun op2' ->
  Op.transaction
    ~gas_limit
    ~fee:Tez.one
    (B b)
    c1
    c2
    ~counter:(Z.of_int 3)
    Tez.one
  >>=? fun op3 ->
  Op.transaction
    ~gas_limit
    ~fee:Tez.one
    (B b)
    c1
    c2
    ~counter:(Z.of_int 4)
    Tez.one
  >>=? fun op4 ->
  (* Canari: Check counters are ok *)
  Op.batch_operations ~source:c1 (B b) [op1; op2; op3; op4] >>=? fun op ->
  Incremental.begin_construction b >>=? fun inc ->
  Incremental.add_operation inc op >>=? fun _ ->
  (* Gap in counter in the following op *)
  Op.batch_operations ~source:c1 (B b) [op1; op2; op4] >>=? fun op ->
  Incremental.add_operation ~expect_failure:expect_inconsistent_counters inc op
  >>=? fun _ ->
  (* Same counter used twice in the following op *)
  Op.batch_operations ~source:c1 (B b) [op1; op2; op2'] >>=? fun op ->
  Incremental.add_operation ~expect_failure:expect_inconsistent_counters inc op
  >>=? fun _ -> return_unit

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
