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
    Component:  Protocol (precheck manager)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                -- test "^precheck manager$"
    Subject:    Precheck manager operation.
*)

open Protocol
open Alpha_context
open Manager_operation_helpers

(* The goal of this test is to ensure that [select_op] generate the
   wanted kind of manager operation

   Note: if a new manager operation kind is added in the protocol,
   [Manager_operation_helpers.manager_operation_kind] should be
   extended. You will also have to extend
   [Manager_operation_helpers.select_op] with a new `mk` for this new
   operation. Finally the list [Manager_operation_helpers.subjects]
   should also be extended to run the precheck test on the new manager
   operation kind. *)
let ensure_kind infos kind =
  let open Lwt_result_syntax in
  let* op = select_op kind infos ~force_reveal:false ~source:infos.contract1 in
  let (Operation_data {contents; _}) = op.protocol_data in
  match contents with
  | Single (Manager_operation {operation; _}) -> (
      match (operation, kind) with
      | Transaction _, K_Transaction
      | Reveal _, K_Reveal
      | Origination _, K_Origination
      | Delegation _, K_Delegation
      | Delegation _, K_Undelegation
      | Delegation _, K_Self_delegation
      | Register_global_constant _, K_Register_global_constant
      | Set_deposits_limit _, K_Set_deposits_limit
      | Tx_rollup_origination, K_Tx_rollup_origination
      | Tx_rollup_submit_batch _, K_Tx_rollup_submit_batch
      | Tx_rollup_commit _, K_Tx_rollup_commit
      | Tx_rollup_return_bond _, K_Tx_rollup_return_bond
      | Tx_rollup_finalize_commitment _, K_Tx_rollup_finalize
      | Tx_rollup_remove_commitment _, K_Tx_rollup_remove_commitment
      | Tx_rollup_rejection _, K_Tx_rollup_reject
      | Tx_rollup_dispatch_tickets _, K_Tx_rollup_dispatch_tickets
      | Transfer_ticket _, K_Transfer_ticket
      | Sc_rollup_originate _, K_Sc_rollup_origination
      | Sc_rollup_add_messages _, K_Sc_rollup_add_messages
      | Sc_rollup_cement _, K_Sc_rollup_cement
      | Sc_rollup_publish _, K_Sc_rollup_publish
      | Sc_rollup_refute _, K_Sc_rollup_refute
      | Sc_rollup_timeout _, K_Sc_rollup_timeout
      | Sc_rollup_execute_outbox_message _, K_Sc_rollup_execute_outbox_message
      | Sc_rollup_recover_bond _, K_Sc_rollup_recover_bond
      | Dal_publish_slot_header _, K_Dal_publish_slot_header ->
          return_unit
      | ( ( Transaction _ | Origination _ | Register_global_constant _
          | Delegation _ | Set_deposits_limit _ | Reveal _
          | Tx_rollup_origination | Tx_rollup_submit_batch _
          | Tx_rollup_commit _ | Tx_rollup_return_bond _
          | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _
          | Tx_rollup_dispatch_tickets _ | Transfer_ticket _
          | Tx_rollup_rejection _ | Sc_rollup_originate _ | Sc_rollup_publish _
          | Sc_rollup_cement _ | Sc_rollup_add_messages _ | Sc_rollup_refute _
          | Sc_rollup_timeout _ | Sc_rollup_execute_outbox_message _
          | Sc_rollup_recover_bond _ | Dal_publish_slot_header _ ),
          _ ) ->
          assert false)
  | Single _ -> assert false
  | Cons _ -> assert false

let ensure_manager_operation_coverage () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  List.iter_es (fun kind -> ensure_kind infos kind) subjects

let test_ensure_manager_operation_coverage () =
  Tztest.tztest
    (Format.sprintf "Ensure manager_operation coverage")
    `Quick
    (fun () -> ensure_manager_operation_coverage ())

(* Too low gas limit. *)
let low_gas_limit_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [
     Environment.Ecoproto_error Apply.Gas_quota_exceeded_init_deserialize;
     Environment.Ecoproto_error Raw_context.Operation_quota_exceeded;
    ] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  precheck_ko_diagnostic infos op expect_failure

let test_low_gas_limit kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let gas_limit = Op.Low in
  let* op =
    select_op ~gas_limit ~force_reveal:true ~source:infos.contract1 kind infos
  in
  low_gas_limit_diagnostic infos op

let generate_low_gas_limit () =
  create_Tztest
    test_low_gas_limit
    "Gas_limit too low."
    except_not_consumer_in_precheck_subjects

(* Too high gas limit. *)
let high_gas_limit_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [Environment.Ecoproto_error Gas.Gas_limit_too_high] -> return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  precheck_ko_diagnostic infos op expect_failure

let test_high_gas_limit kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let gas_limit = Op.Custom_gas (Gas.Arith.integral_of_int_exn 10_000_000) in
  let* op =
    select_op ~gas_limit ~force_reveal:true ~source:infos.contract1 kind infos
  in
  high_gas_limit_diagnostic infos op

let generate_high_gas_limit () =
  create_Tztest test_high_gas_limit "Gas_limit too high." subjects

(* Too high storage limit. *)
let high_storage_limit_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [Environment.Ecoproto_error Fees_storage.Storage_limit_too_high] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  precheck_ko_diagnostic infos op expect_failure

let test_high_storage_limit kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let storage_limit = Z.of_int max_int in
  let* op =
    select_op
      ~storage_limit
      ~force_reveal:true
      ~source:infos.contract1
      kind
      infos
  in
  high_storage_limit_diagnostic infos op

let generate_high_storage_limit () =
  create_Tztest test_high_gas_limit "Storage_limit too high." subjects

(* Counter in the future. *)
let high_counter_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [Environment.Ecoproto_error (Contract_storage.Counter_in_the_future _)] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  precheck_ko_diagnostic infos op expect_failure

let test_high_counter kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let counter = Z.of_int max_int in
  let* op =
    select_op ~counter ~force_reveal:true ~source:infos.contract1 kind infos
  in
  high_counter_diagnostic infos op

let generate_high_counter () =
  create_Tztest test_high_counter "Counter too high." subjects

(* Counter in the past. *)
let low_counter_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [Environment.Ecoproto_error (Contract_storage.Counter_in_the_past _)] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  precheck_ko_diagnostic infos op expect_failure

let test_low_counter kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let* current_counter =
    Context.Contract.counter (B infos.block) infos.contract1
  in
  let counter = Z.sub current_counter Z.one in
  let* op =
    select_op ~counter ~force_reveal:true ~source:infos.contract1 kind infos
  in
  low_counter_diagnostic infos op

let generate_low_counter () =
  create_Tztest test_low_counter "Counter too low." subjects

(* Not allocated source. *)
let not_allocated_diagnostic (infos : infos) op =
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
  precheck_ko_diagnostic infos op expect_failure

let test_not_allocated kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let* op =
    select_op ~force_reveal:false ~source:(mk_fresh_contract ()) kind infos
  in
  not_allocated_diagnostic infos op

let generate_not_allocated () =
  create_Tztest test_not_allocated "not allocated source." subjects

(* Unrevealed source. *)
let unrevealed_key_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [
     Environment.Ecoproto_error
       (Contract_manager_storage.Unrevealed_manager_key _);
    ] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  precheck_ko_diagnostic infos op expect_failure

let test_unrevealed_key kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let* op = select_op ~force_reveal:false ~source:infos.contract1 kind infos in
  unrevealed_key_diagnostic infos op

let generate_unrevealed_key () =
  create_Tztest
    test_unrevealed_key
    "unrevealed source (find_manager_public_key)."
    revealed_subjects

(* Not enough balance to pay fees. *)
let high_fee_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [Environment.Ecoproto_error (Contract_storage.Balance_too_low _)] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  precheck_ko_diagnostic infos op expect_failure

let test_high_fee kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let*? fee = Tez.(one +? one) |> Environment.wrap_tzresult in
  let* op =
    select_op ~fee ~force_reveal:true ~source:infos.contract1 kind infos
  in
  high_fee_diagnostic infos op

let generate_tests_high_fee () =
  create_Tztest test_high_fee "not enough for fee payment." subjects

(* Emptying delegated implicit contract. *)
let emptying_delegated_implicit_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [
     Environment.Ecoproto_error
       (Contract_storage.Empty_implicit_delegated_contract _);
    ] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  precheck_ko_diagnostic infos op expect_failure

let test_emptying_delegated_implicit kind () =
  let open Lwt_result_syntax in
  let* infos = init_delegated_implicit () in
  let fee = Tez.one in
  let* op =
    select_op ~fee ~force_reveal:false ~source:infos.contract1 kind infos
  in
  emptying_delegated_implicit_diagnostic infos op

let generate_tests_emptying_delegated_implicit () =
  create_Tztest
    test_emptying_delegated_implicit
    "just enough to empty a delegated source."
    revealed_subjects

(* Exceeding block gas. *)
let exceeding_block_gas_diagnostic ~mempool_mode (infos : infos) op =
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
        (* In mempool_mode, batch that exceed [operation_gas_limit] needs
           to be refused. [Gas.Block_quota_exceeded] only return a
           temporary error. [Gas.Gas_limit_too_high], which is a
           permanent error, is added to the error trace to ensure that
           the batch is refused. *)
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  precheck_ko_diagnostic infos op expect_failure ~mempool_mode

let test_exceeding_block_gas ~mempool_mode kind () =
  let open Lwt_result_syntax in
  let* infos = init_context ~hard_gas_limit_per_block:gb_limit () in
  let gas_limit =
    Op.Custom_gas (Gas.Arith.add gb_limit Gas.Arith.(integral_of_int_exn 1))
  in
  let* operation =
    select_op ~force_reveal:true ~source:infos.contract1 ~gas_limit kind infos
  in
  exceeding_block_gas_diagnostic ~mempool_mode infos operation

let generate_tests_exceeding_block_gas () =
  create_Tztest
    (test_exceeding_block_gas ~mempool_mode:false)
    "too much gas consumption."
    subjects

let generate_tests_exceeding_block_gas_mp_mode () =
  create_Tztest
    (test_exceeding_block_gas ~mempool_mode:true)
    "too much gas consumption in mempool mode."
    subjects

(* Positive tests. *)

(* Fee payment but emptying an self_delegated implicit. *)
let test_emptying_self_delegated_implicit kind () =
  let open Lwt_result_syntax in
  let* infos = init_self_delegated_implicit () in
  let fee = Tez.one in
  let* op =
    select_op ~fee ~force_reveal:false ~source:infos.contract1 kind infos
  in
  apply_ko_diagnostic infos op (fun _ -> return_unit)

let test_emptying_self_delegated_implicit2 kind () =
  let open Lwt_result_syntax in
  let* infos = init_self_delegated_implicit () in
  let fee = Tez.one in
  let* op =
    select_op ~fee ~force_reveal:false ~source:infos.contract1 kind infos
  in
  apply_ok_diagnostic infos op

let generate_tests_emptying_self_delegated_implicit () =
  create_Tztest
    test_emptying_self_delegated_implicit
    "fee payment and just enough to empty a self-delegated source."
    revealed_except_set_deposits_limit_and_submit_batch_subjects
  @ create_Tztest
      test_emptying_self_delegated_implicit2
      "fee payment and just enough to empty a self-delegated source."
      revealed_only_set_deposits_limit_and_submit_batch_subjects

(* Fee payment but emptying an undelegated implicit, test positive. *)
let emptying_undelegated_implicit_diagnostic (infos : infos) op =
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
  apply_ko_diagnostic infos op expect_failure

(* Minimum gas cost to pass the precheck:
   - cost_of_manager_operation for the generic part
   - 100 (empiric) for the specific part (script decoding or hash costs) *)
let empiric_minimal_gas_cost_for_precheck =
  Gas.Arith.integral_of_int_exn
    (Michelson_v1_gas.Internal_for_tests.int_cost_of_manager_operation + 100)

let test_emptying_undelegated_implicit kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let fee = Tez.one in
  let gas_limit = Op.Custom_gas empiric_minimal_gas_cost_for_precheck in
  let* op =
    select_op
      ~fee
      ~gas_limit
      ~force_reveal:true
      ~source:infos.contract1
      kind
      infos
  in
  emptying_undelegated_implicit_diagnostic infos op

let generate_tests_emptying_undelegated_implicit () =
  create_Tztest
    test_emptying_undelegated_implicit
    "(Positive test) fee payment and just enough to empty an undelegated \
     source."
    subjects

let tests =
  (test_ensure_manager_operation_coverage () :: generate_low_gas_limit ())
  @ generate_high_gas_limit ()
  @ generate_tests_exceeding_block_gas ()
  @ generate_tests_exceeding_block_gas_mp_mode ()
  @ generate_high_storage_limit ()
  @ generate_high_counter () @ generate_low_counter ()
  @ generate_not_allocated () @ generate_tests_high_fee ()
  @ generate_tests_emptying_delegated_implicit ()
  @ generate_tests_emptying_self_delegated_implicit ()
  @ generate_unrevealed_key ()
  @ generate_tests_emptying_undelegated_implicit ()
