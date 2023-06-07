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
    Invocation: dune exec src/proto_017_PtNairob/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_global_constants_storage.ml
    Subject:  This module tests that the global table of constants
              can be written to and read from across blocks.
*)

let get_next_context b =
  Incremental.begin_construction b >>=? fun b ->
  return (Incremental.alpha_ctxt b)

let assert_proto_error_id loc id result =
  let test err =
    (Error_monad.find_info_of_error err).id
    = "proto." ^ Protocol.name ^ "." ^ id
  in
  Assert.error ~loc result test

let expr_to_hash expr =
  let lexpr = Script_repr.lazy_expr @@ Expr.from_string expr in
  Script_repr.force_bytes lexpr >|? fun b -> Script_expr_hash.hash_bytes [b]

(* This test has a long wind-up, but is very simple: it just asserts
   that values written to the global table of constants persist across
   blocks. *)
let get_happy_path () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (b, (alice, bob)) ->
  Incremental.begin_construction b >>=? fun b ->
  let expr_str = "Pair 3 7" in
  let expr = Expr.from_string expr_str in
  Environment.wrap_tzresult @@ expr_to_hash expr_str >>?= fun hash ->
  Op.register_global_constant
    (I b)
    ~source:alice
    ~value:(Script_repr.lazy_expr expr)
  >>=? fun op ->
  Incremental.add_operation b op >>=? fun b ->
  Incremental.finalize_block b >>=? fun b ->
  let assert_unchanged b =
    get_next_context b >>=? fun context ->
    Global_constants_storage.get context hash >|= Environment.wrap_tzresult
    >>=? fun (_, result_expr) ->
    Test_global_constants.assert_expr_equal __LOC__ expr result_expr
    >|=? fun () -> b
  in
  assert_unchanged b >>=? fun b ->
  let do_many_transfers b =
    Incremental.begin_construction b >>=? fun b ->
    n_transactions 10 b alice bob (Tez.of_mutez_exn 1000L) >>=? fun b ->
    Incremental.finalize_block b >>=? fun b -> assert_unchanged b
  in
  do_many_transfers b >>=? do_many_transfers >>=? do_many_transfers
  >>=? fun (_ : Block.t) -> Lwt.return_ok ()

(* Blocks that include a registration of a bad expression should
   fail. *)
let test_registration_of_bad_expr_fails () =
  Context.init1 () >>=? fun (b, alice) ->
  Incremental.begin_construction b >>=? fun b ->
  (* To produce the failure, we attempt to register an expression with
     a malformed hash. *)
  let expr = Expr.from_string "Pair 1 (constant \"foo\")" in
  Op.register_global_constant
    (I b)
    ~source:alice
    ~value:(Script_repr.lazy_expr expr)
  >>=? fun op ->
  Incremental.add_operation b op
  >>= assert_proto_error_id __LOC__ "Badly_formed_constant_expression"

(* You cannot register the same expression twice. *)
let test_no_double_register () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, alice) ->
  let expr = Expr.from_string "Pair 1 2" in
  Op.register_global_constant
    (B b)
    ~source:alice
    ~value:(Script_repr.lazy_expr expr)
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  (* Register the same expression again *)
  Op.register_global_constant
    (B b)
    ~source:alice
    ~value:(Script_repr.lazy_expr expr)
  >>=? fun op ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation i op
  >>= assert_proto_error_id __LOC__ "Expression_already_registered"

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
