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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/validate/main.exe \
                  -- --file test_1m_restriction.ml
    Subject:    1M restriction in validation of manager operation.
*)

open Protocol
open Manager_operation_helpers
open Generators

let count = 100

(** Local default values for the tests. *)
let ctxt_cstrs_default =
  {
    default_ctxt_cstrs with
    src_cstrs = Pure 1500000;
    dest_cstrs = Pure 15000;
    del_cstrs = Pure 150000;
    sc_cstrs = Pure 15000;
    zk_cstrs = Pure 15000;
  }

let op_cstrs_default b =
  {
    default_operation_cstrs with
    fee = Range {min = 0; max = 1_000; origin = 1_000};
    force_reveal = Some b;
    amount = Range {min = 0; max = 10_000; origin = 10_000};
  }

let print_one_op (ctxt_req, op_req, mode) =
  Format.asprintf
    "@[<v 2>Generator printer:@,%a@,%a@,%a@]"
    pp_ctxt_req
    ctxt_req
    pp_operation_req
    op_req
    pp_mode
    mode

let print_two_ops (ctxt_req, op_req, op_req', mode) =
  Format.asprintf
    "@[<v 2>Generator printer:@,%a@,%a@,%a@,%a@]"
    pp_ctxt_req
    ctxt_req
    pp_operation_req
    op_req
    pp_operation_req
    op_req'
    pp_mode
    mode

let print_ops_pair (ctxt_req, op_req, mode) =
  Format.asprintf
    "@[<v 2>Generator printer:@,%a@,%a@,%a@]"
    pp_ctxt_req
    ctxt_req
    pp_2_operation_req
    op_req
    pp_mode
    mode

(** The application of a valid operation succeeds, at least, to perform
   the fee payment. *)
let positive_tests =
  let gen =
    QCheck2.Gen.triple
      (Generators.gen_ctxt_req ctxt_cstrs_default)
      (Generators.gen_operation_req (op_cstrs_default true) subjects)
      Generators.gen_mode
  in
  wrap
    ~count
    ~print:print_one_op
    ~name:"positive validated op"
    ~gen
    (fun (ctxt_req, operation_req, mode) ->
      let open Lwt_result_syntax in
      let* infos = init_ctxt ctxt_req in
      let* op = select_op operation_req infos in
      let* (_ : infos) = wrap_mode infos [op] mode in
      return_true)

(** Under 1M restriction, neither a block nor a prevalidator's valid
    pool should contain two operations with the same manager. It
    raises a Manager_restriction error. *)
let two_op_from_same_manager_tests =
  let gen =
    QCheck2.Gen.quad
      (Generators.gen_ctxt_req ctxt_cstrs_default)
      (Generators.gen_operation_req (op_cstrs_default true) subjects)
      (Generators.gen_operation_req (op_cstrs_default false) revealed_subjects)
      Generators.gen_mode
  in
  let expect_failure = function
    | [
        Environment.Ecoproto_error
          (Validate_errors.Manager.Manager_restriction _);
      ] ->
        return_unit
    | err ->
        failwith
          "Error trace:@,\
          \ %a does not match the \
           [Validate_errors.Manager.Manager_restriction] error"
          Error_monad.pp_print_trace
          err
  in
  wrap
    ~count
    ~print:print_two_ops
    ~name:"check conflicts between managers."
    ~gen
    (fun (ctxt_req, operation_req, operation_req2, mode) ->
      let open Lwt_result_syntax in
      let* infos = init_ctxt ctxt_req in
      let* op1 = select_op operation_req infos in
      let* op2 = select_op operation_req2 infos in
      let* () = validate_ko_diagnostic ~mode infos [op1; op2] expect_failure in
      return_true)

(** Under 1M restriction, a batch of two operations cannot be replaced
   by two single operations. *)
let batch_is_not_singles_tests =
  let gen =
    QCheck2.Gen.triple
      (Generators.gen_ctxt_req ctxt_cstrs_default)
      (Generators.gen_2_operation_req
         (op_cstrs_default false)
         revealed_subjects)
      Generators.gen_mode
  in
  let expect_failure _ = return_unit in
  wrap
    ~count
    ~print:print_ops_pair
    ~name:"batch is not sequence of Single"
    ~gen
    (fun (ctxt_req, operation_req, mode) ->
      let open Lwt_result_syntax in
      let* infos = init_ctxt ctxt_req in
      let* op1 = select_op (fst operation_req) infos in
      let* op2 = select_op (snd operation_req) infos in
      let source = contract_of (get_source infos) in
      let* batch =
        Op.batch_operations ~source (B infos.ctxt.block) [op1; op2]
      in
      let* (_ : infos) = validate_diagnostic ~mode infos [batch] in
      let* () = validate_ko_diagnostic ~mode infos [op1; op2] expect_failure in
      return_true)

(** The applications of two covalid operations in a certain context
   succeed, at least, to perform the fee payment of both, in whatever
   application order. *)
let conflict_free_tests =
  let gen =
    QCheck2.Gen.quad
      (Generators.gen_ctxt_req ctxt_cstrs_default)
      (Generators.gen_operation_req (op_cstrs_default true) revealed_subjects)
      (Generators.gen_operation_req (op_cstrs_default true) revealed_subjects)
      Generators.gen_mode
  in
  wrap
    ~count
    ~print:print_two_ops
    ~name:"under 1M, co-valid ops commute"
    ~gen
    (fun (ctxt_req, operation_req, operation_req', mode) ->
      let open Lwt_result_syntax in
      let* infos = init_ctxt ctxt_req in
      let* op1 = select_op operation_req infos in
      let infos2 =
        {
          infos with
          accounts =
            {
              infos.accounts with
              sources =
                (match infos.accounts.del with
                | None -> assert false
                | Some s -> [s]);
            };
        }
      in
      let* op2 = select_op operation_req' infos2 in
      let* (_ : infos) = validate_diagnostic ~mode infos [op1; op2] in
      let* (_ : infos) = validate_diagnostic ~mode infos [op2; op1] in
      return_true)

open Qcheck2_helpers

let tests : (string * [`Quick | `Slow] * (unit -> unit Lwt.t)) trace =
  qcheck_wrap_lwt
    [
      positive_tests;
      two_op_from_same_manager_tests;
      batch_is_not_singles_tests;
      conflict_free_tests;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("one-manager restriction", tests)]
  |> Lwt_main.run
