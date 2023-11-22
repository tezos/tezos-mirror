(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <team@marigold.dev>                           *)
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
open Protocol
open Alpha_context
open Transfers

(** Testing
    -------
    Component:  Protocol (global table of constants)
    Invocation: dune exec src/proto_018_Proxford/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_global_constants_storage.ml
    Subject:  This module tests that the global table of constants
              can be written to and read from across blocks.
*)

let get_next_context b =
  let open Lwt_result_syntax in
  let* b = Incremental.begin_construction b in
  return (Incremental.alpha_ctxt b)

let assert_proto_error_id loc id result =
  let test err =
    (Error_monad.find_info_of_error err).id
    = "proto." ^ Protocol.name ^ "." ^ id
  in
  Assert.error ~loc result test

let expr_to_hash expr =
  let open Result_syntax in
  let lexpr = Script_repr.lazy_expr @@ Expr.from_string expr in
  let+ b = Script_repr.force_bytes lexpr in
  Script_expr_hash.hash_bytes [b]

(* This test has a long wind-up, but is very simple: it just asserts
   that values written to the global table of constants persist across
   blocks. *)
let get_happy_path () =
  let open Lwt_result_wrap_syntax in
  let* b, (alice, bob) = Context.init2 ~consensus_threshold:0 () in
  let* b = Incremental.begin_construction b in
  let expr_str = "Pair 3 7" in
  let expr = Expr.from_string expr_str in
  let*?@ hash = expr_to_hash expr_str in
  let* op =
    Op.register_global_constant
      (I b)
      ~source:alice
      ~value:(Script_repr.lazy_expr expr)
  in
  let* b = Incremental.add_operation b op in
  let* b = Incremental.finalize_block b in
  let assert_unchanged b =
    let* context = get_next_context b in
    let*! result = Global_constants_storage.get context hash in
    let*?@ _, result_expr = result in
    let+ () =
      Test_global_constants.assert_expr_equal __LOC__ expr result_expr
    in
    b
  in
  let* b = assert_unchanged b in
  let do_many_transfers b =
    let* b = Incremental.begin_construction b in
    let* b = n_transactions 10 b alice bob (Tez.of_mutez_exn 1000L) in
    let* b = Incremental.finalize_block b in
    assert_unchanged b
  in
  let* (_ : Block.t) =
    let* b = do_many_transfers b in
    let* b = do_many_transfers b in
    do_many_transfers b
  in
  return_unit

(* Blocks that include a registration of a bad expression should
   fail. *)
let test_registration_of_bad_expr_fails () =
  let open Lwt_result_syntax in
  let* b, alice = Context.init1 () in
  let* b = Incremental.begin_construction b in
  (* To produce the failure, we attempt to register an expression with
     a malformed hash. *)
  let expr = Expr.from_string "Pair 1 (constant \"foo\")" in
  let* op =
    Op.register_global_constant
      (I b)
      ~source:alice
      ~value:(Script_repr.lazy_expr expr)
  in
  let*! result = Incremental.add_operation b op in
  assert_proto_error_id __LOC__ "Badly_formed_constant_expression" result

(* You cannot register the same expression twice. *)
let test_no_double_register () =
  let open Lwt_result_syntax in
  let* b, alice = Context.init1 ~consensus_threshold:0 () in
  let expr = Expr.from_string "Pair 1 2" in
  let* operation =
    Op.register_global_constant
      (B b)
      ~source:alice
      ~value:(Script_repr.lazy_expr expr)
  in
  let* b = Block.bake ~operation b in
  (* Register the same expression again *)
  let* op =
    Op.register_global_constant
      (B b)
      ~source:alice
      ~value:(Script_repr.lazy_expr expr)
  in
  let* i = Incremental.begin_construction b in
  let*! result = Incremental.add_operation i op in
  assert_proto_error_id __LOC__ "Expression_already_registered" result

let tests =
  [
    Tztest.tztest "Multiple blocks happy path" `Quick get_happy_path;
    Tztest.tztest
      "Bad register global operations fail when added to the block"
      `Quick
      test_registration_of_bad_expr_fails;
    Tztest.tztest
      "You cannot register the same expression twice."
      `Quick
      test_no_double_register;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("global table of constants", tests)]
  |> Lwt_main.run
