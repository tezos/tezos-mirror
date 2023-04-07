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
    Invocation: dune exec src/proto_017_PtNairob/lib_protocol/test/integration/validate/main.exe \
                  -- --file test_manager_operation_validation.ml
    Subject:    Validation of manager operation.
*)

open Protocol
open Alpha_context
open Manager_operation_helpers

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
       Validate_errors.Manager.Gas_quota_exceeded_init_deserialize;
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

let test_low_gas_limit infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        gas_limit = Some Op.Low;
        force_reveal = Some true;
      }
      infos
  in
  low_gas_limit_diagnostic infos [op]

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

let test_high_gas_limit infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        gas_limit =
          Some (Op.Custom_gas (Gas.Arith.integral_of_int_exn 10_000_000));
      }
      infos
  in
  high_gas_limit_diagnostic infos [op]

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

let test_high_storage_limit infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        storage_limit = Some (Z.of_int max_int);
      }
      infos
  in
  high_storage_limit_diagnostic infos [op]

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

let test_high_counter infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        counter = Some (Manager_counter.Internal_for_tests.of_int max_int);
      }
      infos
  in
  high_counter_diagnostic infos [op]

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

let test_low_counter infos kind =
  let open Lwt_result_syntax in
  let* current_counter =
    Context.Contract.counter
      (B infos.ctxt.block)
      (contract_of (get_source infos))
  in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        counter =
          Some (Manager_counter.Internal_for_tests.add current_counter (-1));
      }
      infos
  in
  low_counter_diagnostic infos [op]

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

let test_not_allocated infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {(operation_req_default kind) with force_reveal = Some false}
      {
        infos with
        accounts = {infos.accounts with sources = [Account.(new_account ())]};
      }
  in
  not_allocated_diagnostic infos [op]

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

let test_unrevealed_key infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {(operation_req_default kind) with force_reveal = Some false}
      infos
  in
  unrevealed_key_diagnostic infos [op]

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

let test_high_fee infos kind =
  let open Lwt_result_syntax in
  let*? fee = Tez.(one +? default_fund) |> Environment.wrap_tzresult in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        fee = Some fee;
      }
      infos
  in
  high_fee_diagnostic infos [op]

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

let test_empty_implicit infos kind =
  let open Lwt_result_syntax in
  let* fee =
    Context.Contract.balance
      (B infos.ctxt.block)
      (contract_of (get_source infos))
  in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some false;
        fee = Some fee;
      }
      infos
  in
  emptying_delegated_implicit_diagnostic infos [op]

(** Validate fails if there is not enough available gas in the block.

    This test asserts that validate fails with:
    - [Gas_limit_too_high;Block_quota_exceeded] in mempool mode,
    - [Block_quota_exceeded] in other mode
    with gas limit exceeds the available gas in the block.
    It applies to every kind of manager operation. *)
let exceeding_block_gas_diagnostic ~mode (infos : infos) op =
  let expect_failure errs =
    match (errs, mode) with
    | ( [Environment.Ecoproto_error Gas.Block_quota_exceeded],
        (Construction | Application) ) ->
        return_unit
    | ( [
          Environment.Ecoproto_error Gas.Gas_limit_too_high;
          Environment.Ecoproto_error Gas.Block_quota_exceeded;
        ],
        Mempool ) ->
        (* In mempool_mode, batch that exceed [operation_gas_limit] needs
           to be refused. [Gas.Block_quota_exceeded] only return a
           temporary error. [Gas.Gas_limit_too_high], which is a
           permanent error, is added to the error trace to ensure that
           the batch is refused. *)
        return_unit
    | err, _ ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  validate_ko_diagnostic infos op expect_failure ~mode

let test_exceeding_block_gas ~mode infos kind =
  let open Lwt_result_syntax in
  let* operation =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        gas_limit =
          Some
            (Op.Custom_gas
               (Gas.Arith.add gb_limit Gas.Arith.(integral_of_int_exn 1)));
      }
      infos
  in
  exceeding_block_gas_diagnostic ~mode infos [operation]

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
    In the case of emptying the balance of an undelegated implicit source,
    we observe in the output context that the source is deallocated.

    Otherwise, we observe in the output context that:
     - the counter is the successor of the one stored in the initial context,
     - the balance is at least decreased by fee,
     - the available gas in the block decreased at least gas limit. *)

(** Fee payment*)
let test_validate infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        amount = Some Tez.one;
      }
      infos
  in
  let* (_ : infos) = validate_diagnostic infos [op] in
  return_unit

(** Fee payment that emptying a self_delegated implicit. *)
let test_emptying_self_delegate infos kind =
  let open Lwt_result_syntax in
  let* fee =
    Context.Contract.balance
      (B infos.ctxt.block)
      (contract_of (get_source infos))
  in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some false;
        fee = Some fee;
      }
      infos
  in
  let* (_ : infos) = validate_diagnostic infos [op] in
  return_unit

let test_empty_undelegate infos kind =
  let open Lwt_result_syntax in
  let* fee =
    Context.Contract.balance
      (B infos.ctxt.block)
      (contract_of (get_source infos))
  in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        fee = Some fee;
        gas_limit = Some Op.High;
      }
      infos
  in
  let* (_ : infos) = validate_diagnostic ~deallocated:true infos [op] in
  return_unit

(** No gas consumer with the minimal gas limit for manager operations
   passes validate. *)
let test_low_gas_limit_no_consumer infos kind =
  let open Lwt_result_syntax in
  let* op =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        gas_limit = Some Op.Low;
      }
      infos
  in
  let* (_ : infos) = validate_diagnostic infos [op] in
  return_unit

(* Feature flags.*)

(* Select the error according to the positionned flag.
   We assume that only one feature is disabled. *)
let flag_expect_failure flags errs =
  match errs with
  | [
   Environment.Ecoproto_error Validate_errors.Manager.Sc_rollup_feature_disabled;
  ]
    when flags.scoru = false ->
      return_unit
  | [
   Environment.Ecoproto_error Validate_errors.Manager.Tx_rollup_feature_disabled;
  ]
    when flags.toru = false ->
      return_unit
  | [Environment.Ecoproto_error Dal_errors.Dal_feature_disabled]
    when flags.dal = false ->
      return_unit
  | [
   Environment.Ecoproto_error Validate_errors.Manager.Zk_rollup_feature_disabled;
  ]
    when flags.zkru = false ->
      return_unit
  | err ->
      failwith
        "Error trace:@, %a does not match the expected one"
        Error_monad.pp_print_trace
        err

(* Tests that operations depending on feature flags are not valid
   when the flag is set as disable.

   See [is_disabled] and the [flags] in `manager_operation_helpers`.
   We assume that only one flag is set at false in flag.

   In order to forge Toru, Scoru or Dal operation when the correspondong
   feature is disable, we use a [infos_op] with default requirements,
   so that we have a Tx_rollup.t and a Sc_rollup.t. *)
let test_feature_flags infos kind =
  let open Lwt_result_syntax in
  let* counter =
    Context.Contract.counter
      (B infos.ctxt.block)
      (contract_of (get_source infos))
  in
  let* op =
    select_op
      {
        {(operation_req_default kind) with force_reveal = Some true} with
        counter = Some counter;
      }
      infos
  in
  let flags = infos.flags in
  if is_disabled flags kind then
    validate_ko_diagnostic infos [op] (flag_expect_failure flags)
  else
    let* (_ : infos) = validate_diagnostic infos [op] in
    return_unit

let tests =
  let mk_default () = default_init_ctxt () in
  let mk_reveal () =
    init_ctxt {ctxt_req_default with reveal_accounts = false}
  in
  let mk_deleg () = default_ctxt_with_delegation () in
  let mk_gas () =
    init_ctxt {ctxt_req_default with hard_gas_limit_per_block = Some gb_limit}
  in
  let mk_self_deleg () = default_ctxt_with_self_delegation () in
  let mk_flags flags () =
    let open Lwt_result_syntax in
    let* infos_op = default_init_ctxt () in
    let* infos = default_init_with_flags flags in
    let infos =
      {
        infos with
        ctxt =
          {
            infos.ctxt with
            sc_rollup = infos_op.ctxt.sc_rollup;
            zk_rollup = infos_op.ctxt.zk_rollup;
          };
      }
    in
    return infos
  in
  let all = subjects in
  let gas_consum = gas_consumer_in_validate_subjects in
  let not_gas_consum = not_gas_consumer_in_validate_subjects in
  let revealed = revealed_subjects in
  List.map
    (fun (name, f, subjects, info_builder) ->
      make_tztest name f subjects info_builder)
    [
      (* Expected validation failure *)
      ("gas limit too low", test_low_gas_limit, gas_consum, mk_default);
      ("gas limit too high", test_high_gas_limit, all, mk_default);
      ("storage limit too high", test_high_storage_limit, all, mk_default);
      ("counter too high", test_high_counter, all, mk_default);
      ("counter too low", test_low_counter, all, mk_default);
      ("unallocated source", test_not_allocated, all, mk_default);
      ("unrevealed source", test_unrevealed_key, revealed, mk_reveal);
      ("balance too low for fee payment", test_high_fee, all, mk_default);
      ("empty delegate source", test_empty_implicit, revealed, mk_deleg);
      ( "too much gas consumption in block",
        test_exceeding_block_gas ~mode:Construction,
        all,
        mk_gas );
      (* Expected validation success *)
      ("fees are taken when valid", test_validate, all, mk_default);
      ("empty self-delegate", test_emptying_self_delegate, all, mk_self_deleg);
      ( "too much gas consumption in mempool",
        test_exceeding_block_gas ~mode:Mempool,
        all,
        mk_gas );
      ("empty undelegated source", test_empty_undelegate, all, mk_default);
      ( "minimal gas for manager",
        test_low_gas_limit_no_consumer,
        not_gas_consum,
        mk_default );
      ("dal disabled", test_feature_flags, all, mk_flags disabled_dal);
      ("toru disabled", test_feature_flags, all, mk_flags disabled_toru);
      ("scoru disabled", test_feature_flags, all, mk_flags disabled_scoru);
      ("zkru disabled", test_feature_flags, all, mk_flags disabled_zkru);
    ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("single manager validation", tests)]
  |> Lwt_main.run
