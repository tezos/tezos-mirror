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
    Invocation: dune exec src/proto_015_PtLimaPt/lib_protocol/test/integration/validate/main.exe
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

let batch_in_the_middle infos kind1 kind2 =
  let open Lwt_result_syntax in
  let* counter =
    Context.Contract.counter
      (B infos.ctxt.block)
      (contract_of (get_source infos))
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
      ~source:(contract_of (get_source infos))
      (Context.B infos.ctxt.block)
      [operation1; reveal; operation2]
  in
  batch_reveal_in_the_middle_diagnostic infos [batch]

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

let batch_two_reveals infos kind =
  let open Lwt_result_syntax in
  let* counter =
    Context.Contract.counter
      (B infos.ctxt.block)
      (contract_of (get_source infos))
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
      ~source:(contract_of (get_source infos))
      (Context.B infos.ctxt.block)
      [reveal; reveal1; operation]
  in
  batch_two_reveals_diagnostic infos [batch]

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

let batch_two_sources infos kind1 kind2 =
  let open Lwt_result_syntax in
  let source = contract_of (get_source infos) in
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
    {infos with accounts = {infos.accounts with sources = [source2]}}
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

(** Counters in a batch should be a sequence from the successor of
   the stored counter associated to source in the initial context. *)
let batch_incons_counters infos kind1 kind2 =
  let open Lwt_result_syntax in
  let source = contract_of (get_source infos) in
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

(** A batch that consumes all the balance for fees can only face the total
   consumption at the end of the batch. *)
let batch_emptying_balance_in_the_middle infos kind1 kind2 =
  let open Lwt_result_syntax in
  let source = contract_of (get_source infos) in
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

(** A batch that consumes all the balance for fees only at the end of
   the batch passes validate.*)
let batch_empty_at_end infos kind1 kind2 =
  let open Lwt_result_syntax in
  let source = contract_of (get_source infos) in
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
  let* _ = validate_diagnostic ~deallocated:true infos [case2] in
  let* _ = validate_diagnostic ~deallocated:true infos [case3] in
  return_unit

(** Simple reveal followed by a transaction. *)
let batch_reveal_transaction infos =
  let open Lwt_result_syntax in
  let source = contract_of (get_source infos) in
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

(** A batch of manager operation must not exceed the initial available gas in the block. *)
let batch_exceeding_block_gas ~mempool_mode infos kind1 kind2 =
  let open Lwt_result_syntax in
  let source = contract_of (get_source infos) in
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

let make_tztest_batched ?(fmt = Format.std_formatter) name test subjects
    info_builder =
  let open Lwt_result_syntax in
  Tztest.tztest name `Quick (fun () ->
      let* infos = info_builder () in
      List.iter_es
        (fun kind1 ->
          let k1s = kind_to_string kind1 in
          List.iter_es
            (fun kind2 ->
              Format.fprintf
                fmt
                "%s: [%s ; %s]@."
                name
                k1s
                (kind_to_string kind2) ;
              test infos kind1 kind2)
            subjects)
        subjects)

let tests =
  let open Lwt_result_syntax in
  let mk_default () = default_init_ctxt () in
  let mk_high_gas_limit () =
    init_ctxt {ctxt_req_default with hard_gas_limit_per_block = Some gb_limit}
  in
  let revealed = revealed_subjects in
  [
    ( Tztest.tztest "batch reveal and transaction" `Quick @@ fun () ->
      let* infos = mk_default () in
      batch_reveal_transaction infos );
  ]
  @ List.map
      (fun (name, f, subjects, info_builder) ->
        make_tztest name f subjects info_builder)
      [("batch two reveals", batch_two_reveals, revealed, mk_default)]
  @ List.map
      (fun (name, f, subjects, info_builder) ->
        make_tztest_batched name f subjects info_builder)
      [
        ("reveal in the middle", batch_in_the_middle, revealed, mk_default);
        ("batch two sources", batch_two_sources, revealed, mk_default);
        ("batch incons. counters", batch_incons_counters, revealed, mk_default);
        ( "empty balance in middle of batch",
          batch_emptying_balance_in_the_middle,
          revealed,
          mk_default );
        ( "empty balance at end of batch",
          batch_empty_at_end,
          revealed,
          mk_default );
        ( "too much gas consumption",
          batch_exceeding_block_gas ~mempool_mode:false,
          revealed,
          mk_high_gas_limit );
        ( "too much gas consumption (mempool)",
          batch_exceeding_block_gas ~mempool_mode:true,
          revealed,
          mk_high_gas_limit );
      ]
