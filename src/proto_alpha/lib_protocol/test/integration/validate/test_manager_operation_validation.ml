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
                -- test "^Single"
    Subject:    Validation of manager operation.
*)

open Protocol
open Alpha_context
open Manager_operation_helpers

(** The goal of this test is to ensure that [select_op] generate the
   wanted kind of manager operation

    Note: if a new manager operation kind is added in the protocol,
   [Manager_operation_helpers.manager_operation_kind] should be
   extended. You will also have to extend
   [Manager_operation_helpers.select_op] with a new `mk` for this new
   operation. Finally the list [Manager_operation_helpers.subjects]
   should also be extended to run the validate test on the new manager
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
      | Increase_paid_storage _, K_Increase_paid_storage
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
          | Delegation _ | Set_deposits_limit _ | Increase_paid_storage _
          | Reveal _ | Tx_rollup_origination | Tx_rollup_submit_batch _
          | Tx_rollup_commit _ | Tx_rollup_return_bond _
          | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _
          | Tx_rollup_dispatch_tickets _ | Transfer_ticket _
          | Tx_rollup_rejection _ | Sc_rollup_originate _ | Sc_rollup_publish _
          | Sc_rollup_cement _ | Sc_rollup_add_messages _ | Sc_rollup_refute _
          | Sc_rollup_timeout _ | Sc_rollup_execute_outbox_message _
          | Sc_rollup_recover_bond _ | Dal_publish_slot_header _
          | Sc_rollup_dal_slot_subscribe _ ),
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

(** {2 Negative tests assert the case where validate must fail} *)

(** Validate fails if the gas limit is too low.

    This test asserts that the validation of a manager operation
    with a too low gas limit fails at validate with an
    [Gas_quota_exceeded_init_deserialize] error.
    This test applies on manager operations that do not
    consume gas in their specific part of validate. *)
let low_gas_limit_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [
     Environment.Ecoproto_error
       Validate_operation.Manager.Gas_quota_exceeded_init_deserialize;
     Environment.Ecoproto_error Raw_context.Operation_quota_exceeded;
    ] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  validate_ko_diagnostic infos op expect_failure

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
    gas_consumer_in_validate_subjects

(** Validate fails if the gas limit is too high.

    This test asserts that the validation of a manager operation with
    a gas limit too high fails at validate with an [Gas_limit_too_high]
    error. It applies on every kind of manager operation. *)
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
  validate_ko_diagnostic infos op expect_failure

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

(** Validate fails if the storage limit is too high.

    This test asserts that a manager operation with a storage limit
    too high fails at validation with [Storage_limit_too_high] error.
    It applies to every kind of manager operation. *)
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
  validate_ko_diagnostic infos op expect_failure

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

(** Validate fails if the counter is in the future.

    This test asserts that the validation of
    a manager operation with a counter in the
    future -- aka greater than the successor of the manager counter
    stored in the current context -- fails with [Counter_in_the_future] error.
    It applies to every kind of manager operation. *)
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
  validate_ko_diagnostic infos op expect_failure

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

(** Validate fails if the counter is in the past.

    This test asserts that the validation of a manager operation with a
    counter in the past -- aka smaller than the successor of the
    manager counter stored in the current context -- fails with
    [Counter_in_the_past] error. It applies to every kind of manager
    operation. *)
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
  validate_ko_diagnostic infos op expect_failure

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

(** Validate fails if the source is not allocated.

    This test asserts that the validation of a manager operation which
    manager contract is not allocated fails with
    [Empty_implicit_contract] error. It applies on every kind of
    manager operation. *)
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
  validate_ko_diagnostic infos op expect_failure

let test_not_allocated kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let* op =
    select_op ~force_reveal:false ~source:(mk_fresh_contract ()) kind infos
  in
  not_allocated_diagnostic infos op

let generate_not_allocated () =
  create_Tztest test_not_allocated "Not allocated source." subjects

(** Validate fails if the source is unrevealed.

    This test asserts that a manager operation with an unrevealed source
    contract fails at validation with [Unrevealed_manager_key].
    It applies on every kind of manager operation except [Revelation]. *)
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
  validate_ko_diagnostic infos op expect_failure

let test_unrevealed_key kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let* op = select_op ~force_reveal:false ~source:infos.contract1 kind infos in
  unrevealed_key_diagnostic infos op

let generate_unrevealed_key () =
  create_Tztest
    test_unrevealed_key
    "Unrevealed source (find_manager_public_key)."
    revealed_subjects

(** Validate fails if the source balance is not enough to pay the fees.

    This test asserts that validation of a manager operation fails if the
    source balance is lesser than the manager operation fee.
    It applies on every kind of manager operation. *)
let high_fee_diagnostic (infos : infos) op =
  let expect_failure errs =
    match errs with
    | [
     Environment.Ecoproto_error (Contract_storage.Balance_too_low _);
     Environment.Ecoproto_error (Tez_repr.Subtraction_underflow _);
    ] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  validate_ko_diagnostic infos op expect_failure

let test_high_fee kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let*? fee = Tez.(one +? one) |> Environment.wrap_tzresult in
  let* op =
    select_op ~fee ~force_reveal:true ~source:infos.contract1 kind infos
  in
  high_fee_diagnostic infos op

let generate_tests_high_fee () =
  create_Tztest test_high_fee "Balance too low for fee payment." subjects

(** Validate fails if the fee payment empties the balance of a
    delegated implicit contract.

    This test asserts that in case that:
    - the source is a delegated implicit contract, and
    - the fee is the exact balance of source.
    then, validate fails with [Empty_implicit_delegated_contract] error.
    It applies to every kind of manager operation except [Revelation].*)
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
  validate_ko_diagnostic infos op expect_failure

let test_emptying_delegated_implicit kind () =
  let open Lwt_result_syntax in
  let* infos = init_delegated_implicit () in
  let* fee = Context.Contract.balance (B infos.block) infos.contract1 in
  let* op =
    select_op ~fee ~force_reveal:false ~source:infos.contract1 kind infos
  in
  emptying_delegated_implicit_diagnostic infos op

let generate_tests_emptying_delegated_implicit () =
  create_Tztest
    test_emptying_delegated_implicit
    "Just enough funds to empty a delegated source."
    revealed_subjects

(** Validate fails if there is not enough available gas in the block.

    This test asserts that validate fails with:
    - [Gas_limit_too_high;Block_quota_exceeded] in mempool mode,
    - [Block_quota_exceeded] in other mode
    with gas limit exceeds the available gas in the block.
    It applies to every kind of manager operation. *)
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
  validate_ko_diagnostic infos op expect_failure ~mempool_mode

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
    "Too much gas consumption."
    subjects

let generate_tests_exceeding_block_gas_mp_mode () =
  create_Tztest
    (test_exceeding_block_gas ~mempool_mode:true)
    "Too much gas consumption in mempool mode."
    subjects

(** {2 Positive tests} *)

(** Tests that validate succeeds when:
     - it empties the balance of a self_delegated implicit source,
     - it empties the balance of an undelegated implicit source, and
     - in case:
          - the counter is the successor of the one stored in the context,
          - the fee is lesser than the balance,
          - the storage limit is lesser than the maximum authorized storage,
          - the gas limit is:
                - lesser than the available gas in the block,
                - less than the maximum gas consumable by an operation, and
                - greater than the minimum gas consumable by an operation.

    Notice that in the first two cases only validate succeeds while
    in the last case, the full application also succeeds.
    In the first 2 case, we observe in the output context that:
     - the counter is the successor of the one stored in the initial context,
     - the balance decreased by fee,
     - the available gas in the block decreased by gas limit.
    In the last case, we observe in the output context that:
     - the counter is the successor of the one stored in the initial context,
     - the balance is at least decreased by fee,
     - the available gas in the block decreased by gas limit. *)

(** Fee payment that emptying a self_delegated implicit. *)
let test_emptying_self_delegated_implicit kind () =
  let open Lwt_result_syntax in
  let* infos = init_self_delegated_implicit () in
  let* fee = Context.Contract.balance (B infos.block) infos.contract1 in
  let* op =
    select_op ~fee ~force_reveal:false ~source:infos.contract1 kind infos
  in
  only_validate_diagnostic infos op

let generate_tests_emptying_self_delegated_implicit () =
  create_Tztest
    test_emptying_self_delegated_implicit
    "Validate and empties a self-delegated source."
    subjects

(** Minimum gas cost to pass the validation:
    - cost_of_manager_operation for the generic part
    - 100 (empiric) for the specific part (script decoding or hash costs) *)
let empiric_minimal_gas_cost_for_validate =
  Gas.Arith.integral_of_int_exn
    (Michelson_v1_gas.Internal_for_tests.int_cost_of_manager_operation + 100)

let test_emptying_undelegated_implicit kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let gas_limit = Op.Custom_gas empiric_minimal_gas_cost_for_validate in
  let* fee = Context.Contract.balance (B infos.block) infos.contract1 in
  let* op =
    select_op
      ~fee
      ~gas_limit
      ~force_reveal:true
      ~source:infos.contract1
      kind
      infos
  in
  only_validate_diagnostic infos op

let generate_tests_emptying_undelegated_implicit () =
  create_Tztest
    test_emptying_undelegated_implicit
    "Validate and empties an undelegated source."
    subjects

(** No gas consumer with the minimal gas limit for manager operations
   passes validate. *)
let test_low_gas_limit_no_consumer kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let gas_limit = Op.Low in
  let* op =
    select_op ~gas_limit ~force_reveal:true ~source:infos.contract1 kind infos
  in
  validate_diagnostic infos op

let generate_low_gas_limit_no_consumer () =
  create_Tztest
    test_low_gas_limit
    "passes validate with minimal gas limit for manager operations."
    gas_consumer_in_validate_subjects

(** Fee payment.*)
let test_validate kind () =
  let open Lwt_result_syntax in
  let* infos = init_context () in
  let* counter = Context.Contract.counter (B infos.block) infos.contract1 in
  let source = infos.contract1 in
  let* operation = select_op ~counter ~force_reveal:true ~source kind infos in
  validate_diagnostic infos operation

let generate_tests_validate () =
  create_Tztest test_validate "Validate." subjects

let sanity_tests =
  test_ensure_manager_operation_coverage () :: generate_tests_validate ()

let gas_tests =
  generate_low_gas_limit () @ generate_high_gas_limit ()
  @ generate_tests_exceeding_block_gas ()
  @ generate_tests_exceeding_block_gas_mp_mode ()
  @ generate_low_gas_limit_no_consumer ()

let storage_tests = generate_high_storage_limit ()

let fee_tests =
  generate_tests_high_fee ()
  @ generate_tests_emptying_delegated_implicit ()
  @ generate_tests_emptying_self_delegated_implicit ()
  @ generate_tests_emptying_undelegated_implicit ()

let contract_tests =
  generate_high_counter () @ generate_low_counter () @ generate_not_allocated ()
  @ generate_unrevealed_key ()
