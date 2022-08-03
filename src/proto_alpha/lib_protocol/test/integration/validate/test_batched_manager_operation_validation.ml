(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic-Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission  is hereby granted, free of charge, to any person obtaining a  *)
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
    Component:  Protocol (validate manager)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/validate/main.exe \
                -- test "^Batched"
    Subject:    Validation of batched manager operation.
*)

open Protocol
open Alpha_context
open Manager_operation_helpers

(** {2 Tests on operation batches} *)

(** Revelation should not occur elsewhere than in first position
   in a batch.*)
let batch_reveal_in_the_middle_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [
     Environment.Ecoproto_error
       Validate_errors.Manager.Incorrect_reveal_position;
    ] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  validate_ko_diagnostic infos op expect_failure

let test_batch_reveal_in_the_middle kind1 kind2 () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  let* counter =
    Context.Contract.counter
      (B infos.ctxt.block)
      (contract_of infos.accounts.source)
  in
  let counter = Z.succ counter in
  let* operation1 =
    select_op
      {
        (operation_req_default kind1) with
        force_reveal = Some false;
        counter = Some counter;
      }
      infos
  in
  let counter = Z.succ counter in
  let* reveal =
    mk_reveal
      {
        (operation_req_default K_Reveal) with
        fee = Some Tez.one_mutez;
        counter = Some counter;
      }
      infos
  in
  let counter = Z.succ counter in
  let* operation2 =
    select_op
      {
        (operation_req_default kind2) with
        force_reveal = Some false;
        counter = Some counter;
      }
      infos
  in
  let* batch =
    Op.batch_operations
      ~recompute_counters:false
      ~source:(contract_of infos.accounts.source)
      (Context.B infos.ctxt.block)
      [operation1; reveal; operation2]
  in
  batch_reveal_in_the_middle_diagnostic infos [batch]

let generate_batches_reveal_in_the_middle () =
  create_Tztest_batches
    test_batch_reveal_in_the_middle
    "Reveal should only occur at the beginning of a batch."
    revealed_subjects

(** A batch of manager operation contains at most one Revelation.*)
let batch_two_reveals_diagnostic (infos : infos) op =
  let expected_failure errs =
    match errs with
    | [
     Environment.Ecoproto_error
       Validate_errors.Manager.Incorrect_reveal_position;
    ] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  validate_ko_diagnostic infos op expected_failure

let test_batch_two_reveals kind () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  let* counter =
    Context.Contract.counter
      (B infos.ctxt.block)
      (contract_of infos.accounts.source)
  in
  let counter = Z.succ counter in
  let* reveal =
    mk_reveal
      {
        (operation_req_default K_Reveal) with
        fee = Some Tez.one_mutez;
        counter = Some counter;
      }
      infos
  in
  let counter = Z.succ counter in
  let* reveal1 =
    mk_reveal
      {
        (operation_req_default K_Reveal) with
        fee = Some Tez.one_mutez;
        counter = Some counter;
      }
      infos
  in
  let counter = Z.succ counter in
  let* operation =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some false;
        counter = Some counter;
      }
      infos
  in
  let* batch =
    Op.batch_operations
      ~recompute_counters:false
      ~source:(contract_of infos.accounts.source)
      (Context.B infos.ctxt.block)
      [reveal; reveal1; operation]
  in
  batch_two_reveals_diagnostic infos [batch]

let generate_tests_batches_two_reveals () =
  create_Tztest
    test_batch_two_reveals
    "Only one revelation per batch."
    revealed_subjects

(** Every manager operation in a batch concerns the same source.*)
let batch_two_sources_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [Environment.Ecoproto_error Validate_errors.Manager.Inconsistent_sources]
      ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  validate_ko_diagnostic infos op expect_failure

let test_batch_two_sources kind1 kind2 () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  let source = contract_of infos.accounts.source in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source in
  let counter = Z.succ counter in
  let* operation1 =
    select_op
      {
        (operation_req_default kind1) with
        force_reveal = Some true;
        counter = Some counter;
      }
      infos
  in
  let infos =
    let source2 =
      match infos.accounts.del with None -> assert false | Some s -> s
    in
    {infos with accounts = {infos.accounts with source = source2}}
  in
  let* operation2 =
    select_op
      {(operation_req_default kind2) with force_reveal = Some false}
      infos
  in
  let* batch =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [operation1; operation2]
  in
  batch_two_sources_diagnostic infos [batch]

let generate_batches_two_sources () =
  create_Tztest_batches
    test_batch_two_sources
    "Only one source per batch."
    revealed_subjects

(** Counters in a batch should be a sequence from the successor of
   the stored counter associated to source in the initial context. *)
let test_batch_inconsistent_counters kind1 kind2 () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  let source = contract_of infos.accounts.source in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source in
  let fee = Some Tez.one_mutez in
  let op_infos = operation_req_default K_Reveal in
  let op_infos = {{op_infos with fee} with counter = Some counter} in
  let* reveal = mk_reveal op_infos infos in
  let counter0 = counter in
  let counter = Z.succ counter in
  let counter2 = Z.succ counter in
  let counter3 = Z.succ counter2 in
  let operation counter kind =
    select_op
      {
        (operation_req_default kind) with
        counter = Some counter;
        force_reveal = Some false;
      }
      infos
  in
  let op_counter = operation counter in
  let op_counter0 = operation counter0 in
  let op_counter2 = operation counter2 in
  let op_counter3 = operation counter3 in
  let* op1 = op_counter kind1 in
  let* op2 = op_counter kind2 in
  let* batch_same =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op1; op2]
  in
  let* op1 = op_counter2 kind1 in
  let* op2 = op_counter3 kind2 in
  let* batch_in_the_future =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op1; op2]
  in
  let* op1 = op_counter kind1 in
  let* op2 = op_counter3 kind2 in
  let* batch_missing_one =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op1; op2]
  in
  let* op1 = op_counter2 kind1 in
  let* op2 = op_counter kind2 in
  let* batch_inverse =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op1; op2]
  in
  let* op1 = op_counter0 kind1 in
  let* op2 = op_counter kind2 in
  let* batch_in_the_past =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op1; op2]
  in
  let expect_failure errs =
    match errs with
    | [Environment.Ecoproto_error Validate_errors.Manager.Inconsistent_counters]
      ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  let* i = Incremental.begin_construction infos.ctxt.block in
  let* _ = Incremental.add_operation ~expect_failure i batch_same in
  let* _ = Incremental.add_operation ~expect_failure i batch_in_the_future in
  let* _ = Incremental.add_operation ~expect_failure i batch_missing_one in
  let* _ = Incremental.add_operation ~expect_failure i batch_inverse in
  let* _ = Incremental.add_operation ~expect_failure i batch_in_the_past in
  return_unit

let generate_batches_inconsistent_counters () =
  create_Tztest_batches
    test_batch_inconsistent_counters
    "Counters in a batch should be a sequence."
    revealed_subjects

(** A batch that consumes all the balance for fees can only face the total
   consumption at the end of the batch. *)
let test_batch_emptying_balance_in_the_middle kind1 kind2 () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  let source = contract_of infos.accounts.source in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source in
  let* init_bal = Context.Contract.balance (B infos.ctxt.block) source in
  let counter = counter in
  let* reveal =
    mk_reveal
      {(operation_req_default K_Reveal) with counter = Some counter}
      infos
  in
  let counter = Z.succ counter in
  let operation fee =
    select_op
      {
        (operation_req_default kind1) with
        force_reveal = Some false;
        counter = Some counter;
        fee = Some fee;
      }
      infos
  in
  let counter = Z.succ counter in
  let operation2 fee =
    select_op
      {
        (operation_req_default kind2) with
        force_reveal = Some false;
        counter = Some counter;
        fee = Some fee;
      }
      infos
  in
  let* op_case1 = operation init_bal in
  let* op2_case1 = operation2 Tez.zero in
  let* case1 =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op_case1; op2_case1]
  in
  let* i = Incremental.begin_construction infos.ctxt.block in
  let expect_failure errs =
    match errs with
    | [Environment.Ecoproto_error (Contract_storage.Empty_implicit_contract _)]
      ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  let* _ = Incremental.add_operation i case1 ~expect_failure in
  return_unit

let generate_batches_emptying_balance_in_the_middle () =
  create_Tztest_batches
    test_batch_emptying_balance_in_the_middle
    "Fee payment emptying balance should occurs at the end of the batch."
    revealed_subjects

(** A batch of manager operation must not exceed the initial available gas in the block. *)
let test_batch_exceeding_block_gas ~mempool_mode kind1 kind2 () =
  let open Lwt_result_syntax in
  let ctxt_req =
    {ctxt_req_default with hard_gas_limit_per_block = Some gb_limit}
  in
  let* infos = init_ctxt ctxt_req in
  let source = contract_of infos.accounts.source in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source in
  let g_limit = Gas.Arith.add gb_limit Gas.Arith.(integral_of_int_exn 1) in
  let half_limit =
    Gas.Arith.add half_gb_limit Gas.Arith.(integral_of_int_exn 1)
  in
  let* reveal =
    mk_reveal
      {(operation_req_default K_Reveal) with counter = Some counter}
      infos
  in
  let counter = Z.succ counter in
  let operation gas_limit =
    select_op
      {
        (operation_req_default kind1) with
        force_reveal = Some false;
        counter = Some counter;
        gas_limit = Some (Custom_gas gas_limit);
      }
      infos
  in
  let counter = Z.succ counter in
  let operation2 gas_limit =
    select_op
      {
        (operation_req_default kind2) with
        force_reveal = Some false;
        counter = Some counter;
        gas_limit = Some (Custom_gas gas_limit);
      }
      infos
  in
  let* op_case1 = operation g_limit in
  let* op2_case1 = operation2 Gas.Arith.zero in
  let* op_case2 = operation half_limit in
  let* op2_case2 = operation2 g_limit in
  let* op_case3 = operation half_limit in
  let* op2_case3 = operation2 half_limit in
  let* case1 =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op_case1; op2_case1]
  in
  let* case3 =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op_case3; op2_case3]
  in
  let* case2 =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op_case2; op2_case2]
  in
  let* i = Incremental.begin_construction infos.ctxt.block ~mempool_mode in
  let expect_failure errs =
    match errs with
    | [Environment.Ecoproto_error Gas.Block_quota_exceeded]
      when not mempool_mode ->
        return_unit
    | [
     Environment.Ecoproto_error Gas.Gas_limit_too_high;
     Environment.Ecoproto_error Gas.Block_quota_exceeded;
    ]
      when mempool_mode ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  let* _ = Incremental.add_operation i case1 ~expect_failure in
  let* _ = Incremental.add_operation i case3 ~expect_failure in
  let* _ = Incremental.add_operation i case2 ~expect_failure in
  return_unit

let generate_batches_exceeding_block_gas () =
  create_Tztest_batches
    (test_batch_exceeding_block_gas ~mempool_mode:false)
    "Too much gas consumption."
    revealed_subjects

let generate_batches_exceeding_block_gas_mp_mode () =
  create_Tztest_batches
    (test_batch_exceeding_block_gas ~mempool_mode:true)
    "Too much gas consumption in mempool mode."
    revealed_subjects

(** A batch that consumes all the balance for fees only at the end of
   the batch passes validate.*)
let test_batch_balance_just_enough kind1 kind2 () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  let source = contract_of infos.accounts.source in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source in
  let* init_bal = Context.Contract.balance (B infos.ctxt.block) source in
  let*? half_init_bal = Environment.wrap_tzresult @@ Tez.(init_bal /? 2L) in
  let* reveal =
    mk_reveal
      {(operation_req_default K_Reveal) with counter = Some counter}
      infos
  in
  let counter = Z.succ counter in
  let operation fee =
    select_op
      {
        (operation_req_default kind1) with
        force_reveal = Some false;
        counter = Some counter;
        fee = Some fee;
      }
      infos
  in
  let counter = Z.succ counter in
  let operation2 fee =
    select_op
      {
        (operation_req_default kind2) with
        force_reveal = Some false;
        counter = Some counter;
        fee = Some fee;
      }
      infos
  in
  let* op_case2 = operation Tez.zero in
  let* op2_case2 = operation2 init_bal in
  let* op_case3 = operation half_init_bal in
  let* op2_case3 = operation2 half_init_bal in
  let* case3 =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op_case3; op2_case3]
  in
  let* case2 =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; op_case2; op2_case2]
  in
  let* _ = validate_diagnostic infos [case2] in
  let* _ = validate_diagnostic infos [case3] in
  return_unit

let generate_batches_balance_just_enough () =
  create_Tztest_batches
    test_batch_balance_just_enough
    "Fee payment emptying balance in a batch."
    revealed_subjects

(** Simple reveal followed by a transaction. *)
let test_batch_reveal_transaction_ok () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  let source = contract_of infos.accounts.source in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source in
  let counter = counter in
  let fee = Tez.one_mutez in
  let* reveal =
    mk_reveal
      {
        (operation_req_default K_Reveal) with
        fee = Some fee;
        counter = Some counter;
      }
      infos
  in
  let counter = Z.succ counter in
  let* transaction =
    mk_transaction
      {
        (operation_req_default K_Reveal) with
        counter = Some counter;
        force_reveal = Some false;
      }
      infos
  in
  let* batch =
    Op.batch_operations
      ~recompute_counters:false
      ~source
      (Context.B infos.ctxt.block)
      [reveal; transaction]
  in
  let* _i = Incremental.begin_construction infos.ctxt.block in
  let* _ = validate_diagnostic infos [batch] in
  return_unit

let contract_tests =
  generate_batches_reveal_in_the_middle ()
  @ generate_tests_batches_two_reveals ()
  @ generate_batches_two_sources ()
  @ generate_batches_inconsistent_counters ()
  @ [
      Tztest.tztest
        "Validate a batch with a reveal and a transaction."
        `Quick
        test_batch_reveal_transaction_ok;
    ]

let gas_tests =
  generate_batches_exceeding_block_gas ()
  @ generate_batches_exceeding_block_gas_mp_mode ()

let fee_tests =
  generate_batches_emptying_balance_in_the_middle ()
  @ generate_batches_balance_just_enough ()
