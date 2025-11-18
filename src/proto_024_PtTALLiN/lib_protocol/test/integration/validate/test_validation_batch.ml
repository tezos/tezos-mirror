(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (validate manager)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/validate/main.exe \
                  -- --file test_validation_batch.ml
    Subject:    Validation of batched manager operation.

                There may be overlap with
                [lib_protocol/test/integration/operations/test_combined_operations.ml].
*)

open Protocol
open Alpha_context
open Manager_operation_helpers
open Error_helpers

let register_test =
  Tezt_helpers.register_test_es ~__FILE__ ~file_tags:["validation"; "batch"]

let make_test = make_test ~register_test

let make_test_batched = make_test_batched ~register_test

(** {2 Tests on operation batches} *)

let batch_in_the_middle infos kind1 kind2 =
  let open Lwt_result_syntax in
  let* counter =
    Context.Contract.counter
      (B infos.ctxt.block)
      (contract_of (get_source infos))
  in
  let counter = Manager_counter.succ counter in
  let* operation1 =
    select_op
      {
        (operation_req_default kind1) with
        force_reveal = Some false;
        counter = Some counter;
      }
      infos
  in
  let counter = Manager_counter.succ counter in
  let* reveal =
    mk_reveal
      {
        (operation_req_default K_Reveal) with
        fee = Some Tez.one_mutez;
        counter = Some counter;
      }
      infos
  in
  let counter = Manager_counter.succ counter in
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
  let expect_failure = expect_incorrect_reveal_position ~loc:__LOC__ in
  validate_ko_diagnostic infos [batch] expect_failure

let batch_two_reveals infos kind =
  let open Lwt_result_syntax in
  let* counter =
    Context.Contract.counter
      (B infos.ctxt.block)
      (contract_of (get_source infos))
  in
  let counter = Manager_counter.succ counter in
  let* reveal =
    mk_reveal
      {
        (operation_req_default K_Reveal) with
        fee = Some Tez.one_mutez;
        counter = Some counter;
      }
      infos
  in
  let counter = Manager_counter.succ counter in
  let* reveal1 =
    mk_reveal
      {
        (operation_req_default K_Reveal) with
        fee = Some Tez.one_mutez;
        counter = Some counter;
      }
      infos
  in
  let counter = Manager_counter.succ counter in
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
  let expect_failure = expect_incorrect_reveal_position ~loc:__LOC__ in
  validate_ko_diagnostic infos [batch] expect_failure

let batch_two_sources infos kind1 kind2 =
  let open Lwt_result_syntax in
  let source = contract_of (get_source infos) in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source in
  let counter = Manager_counter.succ counter in
  let* operation1 =
    select_op
      {
        (operation_req_default kind1) with
        force_reveal = Some true;
        counter = Some counter;
      }
      infos
  in
  let source2 =
    match infos.accounts.del with None -> assert false | Some s -> s
  in
  let infos =
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
  let expect_failure =
    Error_helpers.expect_inconsistent_sources
      ~loc:__LOC__
      ~first_source:source
      ~source:(contract_of source2)
  in
  validate_ko_diagnostic infos [batch] expect_failure

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
  let counter = Manager_counter.succ counter in
  let counter2 = Manager_counter.succ counter in
  let counter3 = Manager_counter.succ counter2 in
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
  let* i = Incremental.begin_construction infos.ctxt.block in
  let test_inconsistent_counters ~__LOC__ ~before_wrong_counter ~wrong_counter
      op =
    let expect_failure =
      (* Remember that [select_op] actually builds an operation
         using the successor of the counter argument. *)
      Error_helpers.expect_inconsistent_counters
        ~loc:__LOC__
        ~source
        ~previous_counter:(Manager_counter.succ before_wrong_counter)
        ~counter:(Manager_counter.succ wrong_counter)
    in
    let* (_ : Incremental.t) = Incremental.add_operation ~expect_failure i op in
    return_unit
  in
  let* () =
    test_inconsistent_counters
      ~__LOC__
      ~before_wrong_counter:counter
      ~wrong_counter:counter
      batch_same
  in
  let* () =
    test_inconsistent_counters
      ~__LOC__
      ~before_wrong_counter:counter0
      ~wrong_counter:counter2
      batch_in_the_future
  in
  let* () =
    test_inconsistent_counters
      ~__LOC__
      ~before_wrong_counter:counter
      ~wrong_counter:counter3
      batch_missing_one
  in
  let* () =
    test_inconsistent_counters
      ~__LOC__
      ~before_wrong_counter:counter0
      ~wrong_counter:counter2
      batch_inverse
  in
  test_inconsistent_counters
    ~__LOC__
    ~before_wrong_counter:counter0
    ~wrong_counter:counter0
    batch_in_the_past

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
  let counter = Manager_counter.succ counter in
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
  let counter = Manager_counter.succ counter in
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
  let* (_ : Incremental.t) =
    Incremental.add_operation i case1 ~expect_failure
  in
  return_unit

(** A batch that consumes all the balance for fees only at the end of
   the batch passes validate.*)
let batch_empty_at_end infos kind1 kind2 =
  let open Lwt_result_wrap_syntax in
  let source = contract_of (get_source infos) in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source in
  let* init_bal = Context.Contract.balance (B infos.ctxt.block) source in
  let half_init_bal = Tez_helpers.(init_bal /! 2L) in
  let* reveal =
    mk_reveal
      {(operation_req_default K_Reveal) with counter = Some counter}
      infos
  in
  let counter = Manager_counter.succ counter in
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
  let counter = Manager_counter.succ counter in
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
  let* (_ : infos) = validate_diagnostic ~deallocated:true infos [case2] in
  let* (_ : infos) = validate_diagnostic ~deallocated:true infos [case3] in
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
  let counter = Manager_counter.succ counter in
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
  let* (_ : Incremental.t) = Incremental.begin_construction infos.ctxt.block in
  let* (_ : infos) = validate_diagnostic infos [batch] in
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
  let counter = Manager_counter.succ counter in
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
  let counter = Manager_counter.succ counter in
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
  let* (_ : Incremental.t) =
    Incremental.add_operation i case1 ~expect_failure
  in
  let* (_ : Incremental.t) =
    Incremental.add_operation i case3 ~expect_failure
  in
  let* (_ : Incremental.t) =
    Incremental.add_operation i case2 ~expect_failure
  in
  return_unit

let () =
  let mk_default () = default_init_ctxt () in
  let mk_high_gas_limit () =
    init_ctxt {ctxt_req_default with hard_gas_limit_per_block = Some gb_limit}
  in
  let revealed = revealed_subjects in
  let () =
    register_test ~title:"batch reveal and transaction" @@ fun () ->
    let* infos = default_init_ctxt () in
    let infos =
      match infos with
      | Error errs -> Tezt.Test.fail "Error: %a" Error_monad.pp_print_trace errs
      | Ok infos -> infos
    in
    batch_reveal_transaction infos
  in
  List.iter
    (fun (name, f, subjects, info_builder) ->
      make_test name f subjects info_builder)
    [("batch two reveals", batch_two_reveals, revealed, mk_default)] ;
  List.iter
    (fun (name, f, subjects, info_builder) ->
      make_test_batched name f subjects info_builder)
    [
      ("reveal in the middle", batch_in_the_middle, revealed, mk_default);
      ("batch two sources", batch_two_sources, revealed, mk_default);
      ("batch incons. counters", batch_incons_counters, revealed, mk_default);
      ( "empty balance in middle of batch",
        batch_emptying_balance_in_the_middle,
        revealed,
        mk_default );
      ("empty balance at end of batch", batch_empty_at_end, revealed, mk_default);
      ( "too much gas consumption",
        batch_exceeding_block_gas ~mempool_mode:false,
        revealed,
        mk_high_gas_limit );
      ( "too much gas consumption (mempool)",
        batch_exceeding_block_gas ~mempool_mode:true,
        revealed,
        mk_high_gas_limit );
    ]
