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
                -- test "^1M"
    Subject:    1M restriction in validation of manager operation.
*)

open Protocol
open Alpha_context
open Manager_operation_helpers

(* Temporary local helpers to be clean up. *)
let create_Tztest ?hd_msg test tests_msg =
  let hd_msg k =
    let sk = kind_to_string k in
    match hd_msg with None -> sk | Some hd -> Format.sprintf "%s, %s" sk hd
  in
  let kind = K_Register_global_constant in
  Tztest.tztest
    (Format.sprintf "%s [%s]" tests_msg (hd_msg kind))
    `Quick
    (fun () -> test kind ())

let generate_op ~fee ~reverse:_ kind infos =
  let open Lwt_result_syntax in
  let* counter =
    Context.Contract.counter
      (B infos.ctxt.block)
      (contract_of infos.accounts.source)
  in
  let* operation =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        fee = Some fee;
        counter = Some counter;
      }
      infos
  in
  let+ operation2 =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some false;
        fee = Some fee;
        counter = Some (Z.succ (Z.succ counter));
      }
      infos
  in
  (operation, operation2)

let generate_op_diff_man ~fee ~reverse:_ kind infos =
  let open Lwt_result_syntax in
  let source = contract_of infos.accounts.source in
  let source2_account =
    match infos.accounts.del with None -> assert false | Some s -> s
  in
  let source2 = contract_of source2_account in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source in
  let* operation =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        fee = Some fee;
        counter = Some counter;
      }
      infos
  in
  let* counter = Context.Contract.counter (B infos.ctxt.block) source2 in
  let+ operation2 =
    select_op
      {
        (operation_req_default kind) with
        force_reveal = Some true;
        fee = Some fee;
        counter = Some counter;
      }
      {infos with accounts = {infos.accounts with source = source2_account}}
  in
  (operation, operation2)

(** Under 1M restriction, neither a block nor a prevalidator's valid
    pool should contain two operations with the same manager. It raises
    a Manager_restriction error. *)
let test_two_op_with_same_manager ~mempool_mode kind () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  let* op1, op2 = generate_op ~fee:Tez.zero ~reverse:false kind infos in
  let* inc = Incremental.begin_construction ~mempool_mode infos.ctxt.block in
  let* inc = Incremental.validate_operation inc op1 in
  let* _inc =
    Incremental.validate_operation
      inc
      ~expect_failure:(function
        | [
            Environment.Ecoproto_error
              (Validate_operation.Manager.Manager_restriction _);
          ] ->
            return_unit
        | err ->
            failwith
              "Error trace:@,\
              \ %a does not match the \
               [Validate_operation.Manager.Manager_restriction]"
              Error_monad.pp_print_trace
              err)
      op2
  in
  return_unit

(** Under 1M restriction, a batch of two operations cannot be replaced
    by two single operations. *)
let test_batch_of_two_not_be_two_singles ~mempool_mode kind () =
  let open Lwt_result_syntax in
  let mode = if mempool_mode then Mempool else Construction in
  let* infos = default_init_ctxt () in
  let source = contract_of infos.accounts.source in
  let* inc = Incremental.begin_construction ~mempool_mode infos.ctxt.block in
  let* op1, op2 = generate_op ~fee:Tez.one_mutez ~reverse:false kind infos in
  let* batch = Op.batch_operations ~source (B infos.ctxt.block) [op1; op2] in
  let* inc_batch = Incremental.validate_operation inc batch in
  let* () = observe ~only_validate:false ~mode (I inc) (I inc_batch) batch in
  let* inc1 = Incremental.validate_operation inc op1 in
  let* () = observe ~only_validate:false ~mode (I inc) (I inc1) op1 in
  let* _inc2 =
    Incremental.validate_operation
      ~expect_failure:(fun _ -> return_unit)
      inc
      op2
  in
  let* b1 = Incremental.finalize_block inc1 in
  let* inc1' = Incremental.begin_construction ~mempool_mode b1 in
  let* inc1_op2 = Incremental.validate_operation inc1' op2 in
  let* () = observe ~only_validate:false ~mode (I inc1') (I inc1_op2) op2 in
  return_unit

(** The application of a valid operation succeeds, at least, to perform
    the fee payment. *)
let valid_validate ~mempool_mode kind () =
  let open Lwt_result_syntax in
  let* infos = default_init_ctxt () in
  let* inc = Incremental.begin_construction ~mempool_mode infos.ctxt.block in
  let* op, _ = generate_op ~fee:Tez.one_mutez ~reverse:false kind infos in
  let {shell; protocol_data = Operation_data protocol_data} = op in
  let operation : _ Alpha_context.operation = {shell; protocol_data} in
  let oph = Alpha_context.Operation.hash operation in
  let init_infos, init_state =
    Validate_operation.init_info_and_state
      (Incremental.alpha_ctxt inc)
      Validate_operation.Mempool
      Chain_id.zero
  in
  let _res1 =
    Validate_operation.validate_operation init_infos init_state oph operation
  in
  let* _ = Incremental.validate_operation inc op in
  return_unit

(** The applications of two covalid operations in a certain context
    succeed, at least, to perform the fee payment of both, in whatever
    application order.

    The application of a manager operation has two step: the fees
    payment guarded by the validation and the rest of its application.
    Two manager operations that are valid in a context, will succeed to
    pass the first step of their application -- aka fee payment -- in
    whatever application order.

    By construction they have distinct manager thanks to
    [generate_op_diff_man]. *)
let valid_context_free ~mempool_mode kind () =
  let open Lwt_result_syntax in
  let mode = if mempool_mode then Mempool else Construction in

  let* infos = default_init_ctxt () in
  let* inc = Incremental.begin_construction ~mempool_mode infos.ctxt.block in
  let* op1, op2 =
    generate_op_diff_man ~fee:Tez.one_mutez ~reverse:false kind infos
  in
  let {shell; protocol_data = Operation_data protocol_data} = op1 in
  let operation1 : _ Alpha_context.operation = {shell; protocol_data} in
  let oph1 = Alpha_context.Operation.hash operation1 in
  let {shell; protocol_data = Operation_data protocol_data} = op2 in
  let operation2 : _ Alpha_context.operation = {shell; protocol_data} in
  let oph2 = Alpha_context.Operation.hash operation2 in
  let init_infos, init_state =
    Validate_operation.init_info_and_state
      (Incremental.alpha_ctxt inc)
      Validate_operation.Mempool
      Chain_id.zero
  in
  let* _res1 =
    let*! res =
      Validate_operation.validate_operation
        init_infos
        init_state
        oph1
        operation1
    in
    Lwt.return (Environment.wrap_tzresult res)
  in
  let* _res2 =
    let*! res =
      Validate_operation.validate_operation
        init_infos
        init_state
        oph2
        operation2
    in
    Lwt.return (Environment.wrap_tzresult res)
  in
  let* inc1 = Incremental.validate_operation inc op1 in
  let* () = observe ~only_validate:false ~mode (I inc) (I inc1) op1 in
  let* inc1' = Incremental.validate_operation inc1 op2 in
  let* () = observe ~only_validate:false ~mode (I inc1) (I inc1') op2 in
  let* inc2 = Incremental.validate_operation inc op2 in
  let* () = observe ~only_validate:false ~mode (I inc) (I inc2) op2 in
  let* inc2' = Incremental.validate_operation inc2 op1 in
  let* () = observe ~only_validate:false ~mode (I inc2) (I inc2') op1 in
  return_unit

let generate_1m_conflit_mempool_mode () =
  create_Tztest
    (test_two_op_with_same_manager ~mempool_mode:true)
    "At most one operation per manager in mempool mode"

let generate_1m_conflit_construction_mode () =
  create_Tztest
    (test_two_op_with_same_manager ~mempool_mode:false)
    "At most one operation per manager in construction mode"

let generate_batch_of_two_not_be_two_singles_construction_mode () =
  create_Tztest
    (test_batch_of_two_not_be_two_singles ~mempool_mode:false)
    "A batch differs from a sequence in construction mode"

let generate_batch_of_two_not_be_two_singles_mempool_mode () =
  create_Tztest
    (test_batch_of_two_not_be_two_singles ~mempool_mode:true)
    "A batch differs from a sequence in mempool mode"

let generate_valid_validate_mempool_mode () =
  create_Tztest
    (valid_validate ~mempool_mode:true)
    "Valid implies fee payment in mempool mode"

let generate_valid_validate_construction_mode () =
  create_Tztest
    (valid_validate ~mempool_mode:false)
    "Valid implies fee payment in construction mode"

let generate_valid_context_free_mempool_mode () =
  create_Tztest
    (valid_context_free ~mempool_mode:true)
    "Fee payment of two covalid operations commute in mempool mode"

let generate_valid_context_free_construction_mode () =
  create_Tztest
    (valid_context_free ~mempool_mode:false)
    "Fee payment of two covalid operations commute in construction mode"

let tests =
  [
    generate_1m_conflit_construction_mode ();
    generate_batch_of_two_not_be_two_singles_construction_mode ();
    generate_valid_validate_construction_mode ();
    generate_valid_context_free_construction_mode ();
    generate_1m_conflit_mempool_mode ();
    generate_batch_of_two_not_be_two_singles_mempool_mode ();
    generate_valid_validate_mempool_mode ();
    generate_valid_context_free_mempool_mode ();
  ]

open Generators

let contract_of (account : Account.t) = Contract.Implicit account.pkh

let positive_validated_op () =
  let op_cstrs =
    {
      default_operation_cstrs with
      fee = Range {min = 0; max = 1_000; origin = 1_000};
      force_reveal = Some true;
      amount = Range {min = 0; max = 1_000; origin = 10_000};
    }
  in
  let ctxt_cstrs =
    {
      default_ctxt_cstrs with
      src_cstrs = Greater {n = 15_000; origin = 15_000};
      dest_cstrs = Pure 15000;
      del_cstrs = Pure 15000;
      tx_cstrs = Pure 15000;
      sc_cstrs = Pure 15000;
    }
  in
  let gen =
    QCheck2.Gen.triple
      (Generators.gen_ctxt_req ctxt_cstrs)
      (Generators.gen_operation_req op_cstrs subjects)
      Generators.gen_mode
  in
  let print (ctxt_req, op_req, mode) =
    Format.asprintf
      "@[<v 2>Generator printer:@,%a@,%a@,%a@]"
      pp_ctxt_req
      ctxt_req
      pp_operation_req
      op_req
      pp_mode
      mode
  in
  wrap
    ~count:1000
    ~print
    ~name:"Positive validated op"
    ~gen
    (fun (ctxt_req, operation_req, mode) ->
      let open Lwt_result_syntax in
      let* infos = init_ctxt ctxt_req in
      let* op = select_op operation_req infos in
      let* _infos = wrap_mode infos [op] mode in
      return_true)

open Lib_test.Qcheck2_helpers

let positive_tests () = qcheck_wrap [positive_validated_op ()]

let qcheck_tests () = ("Positive tests", positive_tests ())

let () = Alcotest.run "1M QCheck" [qcheck_tests ()]
