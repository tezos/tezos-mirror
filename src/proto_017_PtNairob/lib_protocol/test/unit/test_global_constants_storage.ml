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

(** Testing
    -------
    Component:    Global table of constants
    Invocation:   dune exec src/proto_017_PtNairob/lib_protocol/test/unit/main.exe \
                  -- --file test_global_constants_storage.ml
    Dependencies: contract_hash.ml
    Subject:      Test the global table of constants
*)

open Protocol
open Alpha_context
open Tztest
open Micheline
open QCheck2
open Qcheck2_helpers
open Michelson_v1_primitives
open Michelson_v1_printer
open Test_global_constants

(** [get] on a nonexistent global constant
    returns an error. *)
let test_get_on_nonexistent_fails =
  tztest_qcheck2
    ~name:"get on a nonexistent global constants fails"
    (Gen.pair
       (Generators.context_gen ())
       (Generators.canonical_without_constant_gen ()))
    (fun (context, expr) ->
      expr_to_hash expr |> Environment.wrap_tzresult >>?= fun hash ->
      Global_constants_storage.get context hash
      >|= Environment.wrap_tzresult
      >>= assert_proto_error_id __LOC__ "Nonexistent_global")

(** If registering an expression yields a hash [h] and context [c],
    then [get c h] should yield the original expression. *)
let test_get_always_returns_registered_expr =
  tztest_qcheck2
    ~name:"get always returned the registered constant"
    (Gen.pair
       (Generators.context_gen ())
       (Generators.canonical_without_constant_gen ()))
    (fun (context, expr) ->
      Global_constants_storage.register context expr
      >|= Environment.wrap_tzresult
      >>=? fun (context, hash, _cost) ->
      Global_constants_storage.get context hash >|= Environment.wrap_tzresult
      >|=? fun (_context, actual_expr) ->
      qcheck_eq ~pp:print_expr actual_expr expr)

(* Attempts to register an expression that contains references
   to expressions not already registered should fail. *)
let test_register_fails_with_unregistered_references =
  tztest "register: fails with unregistered references" `Quick (fun () ->
      let prim_with_constant =
        Expr.from_string
          {| Pair 1
          (constant "exprubuoE4JFvkSpxsZJXAvhTdozCNZpgfCnyg6WsiAYX79q4z3bXu")|}
      in
      create_context () >>=? fun context ->
      Global_constants_storage.register context prim_with_constant
      >|= Environment.wrap_tzresult
      >>= assert_proto_error_id __LOC__ "Nonexistent_global")

(** Same test as [test_register_fails_with_unregistered_references]
    but with random values. *)
let test_register_fails_with_unregistered_references_pbt =
  tztest_qcheck2
    ~name:"register: fails with unregistered references pbt"
    (Gen.pair
       (Generators.context_gen ())
       (Generators.canonical_with_constant_gen ()))
    (fun (context, (_, expr, _)) ->
      assume_expr_not_too_large expr ;
      Global_constants_storage.register context expr
      >|= Environment.wrap_tzresult
      >>= assert_proto_error_id __LOC__ "Nonexistent_global")

let rec grow n node =
  match n with n when n <= 0 -> node | n -> grow (n - 1) (Seq ((), [node]))

(* Any expression with a depth that exceeds
   [Global_constants_storage.max_allowed_global_constant_depth]
   should be rejected. *)
let test_register_fails_if_too_deep =
  tztest "register: fails if expression too deep" `Quick (fun () ->
      let vdeep_expr =
        grow
          (Constants_repr.max_allowed_global_constant_depth + 1)
          (Int ((), Z.of_int 1))
        |> Micheline.strip_locations
      in
      create_context () >>=? fun context ->
      Global_constants_storage.register context vdeep_expr
      >|= Environment.wrap_tzresult
      >>= assert_proto_error_id __LOC__ "Expression_too_deep")

(** [expand] on an expression containing a nonexistent global
    constant returns an error. *)
let test_expand_nonexistent_fails =
  tztest_qcheck2
    ~name:
      "expand on an expression containing a nonexistent global constant fails"
    (Gen.pair
       (Generators.context_gen ())
       (Generators.canonical_with_constant_gen ()))
  @@ fun (context, (_, expr, _)) ->
  assume_expr_not_too_large expr ;
  Global_constants_storage.expand context expr
  >|= Environment.wrap_tzresult
  >>= assert_proto_error_id __LOC__ "Nonexistent_global"

(** Expanding an expression without constants should yield the same expression. *)
let test_expand_no_constants =
  tztest "expand: no constants case" `Quick (fun () ->
      create_context () >>=? fun context ->
      let expected = Expr.from_string "Pair 1 (Pair 2 3)" in
      Global_constants_storage.expand context expected
      >|= Environment.wrap_tzresult
      >>=? fun (_, result_expr) ->
      assert_expr_equal __LOC__ expected result_expr)

(** Similar to [test_expand_no_constants], but random. *)
let test_register_and_expand_orthogonal =
  tztest_qcheck2
    ~name:"register and expand are orthogonal"
    (Gen.triple
       (Generators.context_gen ())
       (Generators.canonical_without_constant_gen ())
       (Generators.canonical_without_constant_gen ()))
    (fun (context, expr1, expr2) ->
      assume_expr_not_too_large expr1 ;
      assume_expr_not_too_large expr2 ;
      let open Michelson_v1_printer in
      Global_constants_storage.register context expr1
      >|= Environment.wrap_tzresult
      >>=? fun (context, _hash, _cost) ->
      Global_constants_storage.expand context expr2
      >|= Environment.wrap_tzresult
      >|=? fun (_, expr2_result) -> qcheck_eq ~pp:print_expr expr2 expr2_result)

(** Expanding should expand constants in the given
    expression, then expand any new constants, etc.
    recursively until no constants remain.  *)
let test_expand_deep_constants =
  tztest "expand: deep constants" `Quick (fun () ->
      (* Should hold for any n, but this test is very slow,
         hence we don't do QCheck2. *)
      let n = 1000 in
      let expr1 = Expr.from_string "{}" in
      create_context () >>=? fun context ->
      let rec n_constants_deep context node n =
        Global_constants_storage.register context (strip_locations node)
        >|= Environment.wrap_tzresult
        >>=? fun (context, hash, _) ->
        if n <= 1 then return (context, node, hash)
        else
          let new_node =
            Seq
              ( -1,
                [
                  Prim
                    ( -1,
                      H_constant,
                      [String (-1, Script_expr_hash.to_b58check hash)],
                      [] );
                ] )
          in
          n_constants_deep context new_node (n - 1)
      in
      n_constants_deep context (root expr1) n >>=? fun (context, _, hash) ->
      let deep_expr =
        Expr.from_string
        @@ Format.sprintf
             "{constant \"%s\"; CDR; NIL operation; PAIR}"
             (Script_expr_hash.to_b58check hash)
      in
      Global_constants_storage.expand context deep_expr
      >|= Environment.wrap_tzresult
      >>=? fun (_, result) ->
      let seq_n_deep n =
        let rec advance n acc =
          match n with 0 -> acc | _ -> advance (n - 1) (Seq (-1, [acc]))
        in
        advance (n - 1) (Seq (-1, []))
      in
      let seq_str = Expr.to_string @@ strip_locations @@ seq_n_deep n in
      let expected =
        Expr.from_string
        @@ Format.sprintf "{ %s; CDR; NIL operation; PAIR; }"
        @@ seq_str
      in
      assert_expr_equal __LOC__ expected result)

(** The [constant] prim is permitted only to have a
    single string argument, representing a valid
    Script_repr.expr hash, with no annotations *)
let test_expand_reject_ill_formed =
  tztest "expand: ill formed constants are rejected" `Quick (fun () ->
      (* first, create a context, register a constant and check
         that its expansion works well. *)
      create_context () >>=? fun context ->
      let some_expr = Expr.from_string "0" in
      Global_constants_storage.register context some_expr
      >|= Environment.wrap_tzresult
      >>=? fun (context, hash, _) ->
      let hash = Script_expr_hash.to_b58check hash in
      (* check that expansion of the registered constant works *)
      Global_constants_storage.expand
        context
        (Expr.from_string @@ Format.sprintf "constant \"%s\"" hash)
      >|= Environment.wrap_tzresult
      >>=? fun (context, result) ->
      assert_expr_equal __LOC__ some_expr result >>=? fun () ->
      let test expr =
        let expected = Expr.from_string expr in
        Global_constants_storage.expand context expected
        >|= Environment.wrap_tzresult
        >>= assert_proto_error_id __LOC__ "Badly_formed_constant_expression"
      in
      (* constant with an argument other than String fails *)
      test "constant 9" >>=? fun () ->
      (* same as above but nested *)
      test "Pair 1 (constant (Pair 2 3))" >>=? fun () ->
      (* constant with bad hash fails *)
      test "constant \"foobar\"" >>=? fun () ->
      (* constant with type annot *)
      test @@ Format.sprintf "(constant :a \"%s\")" hash >>=? fun () ->
      (* constant with var annot *)
      test @@ Format.sprintf "(constant @a \"%s\")" hash >>=? fun () ->
      (* constant with field annot *)
      test @@ Format.sprintf "(constant %%a \"%s\")" hash)

(** The [constant] prim is not permitted to have a
    [constant] child argument. 

    The idea is to have expansion like this:

    constant (constant <hash of hash>) -> constant hash -> value
        
    But we want to forbid this as a badly formed constant.
    Asserting that every constant must be a *static* string 
    makes it easier to see which constants are used where, because
    you can just traverse the AST (no expansion necessary). *)
let test_reject_use_of_inner_constant =
  tztest
    "expand: use of 'constant (constant ...)' is rejected"
    `Quick
    (fun () ->
      (* First, create a context, register a constant and check
         that its expansion works well. *)
      create_context () >>=? fun context ->
      let some_expr = Expr.from_string "0" in
      Global_constants_storage.register context some_expr
      >|= Environment.wrap_tzresult
      >>=? fun (context, hash, _) ->
      let hash = Script_expr_hash.to_b58check hash in
      (* Next, register the hash itself as a constant. *)
      Global_constants_storage.register
        context
        (strip_locations (Micheline.String (-1, hash)))
      >|= Environment.wrap_tzresult
      >>=? fun (context, hash, _) ->
      let hash = Script_expr_hash.to_b58check hash in
      Global_constants_storage.expand
        context
        (Expr.from_string
        @@ Format.sprintf "{ constant (constant \"%s\") } " hash)
      >|= Environment.wrap_tzresult
      >>= assert_proto_error_id __LOC__ "Badly_formed_constant_expression")

(** [test_expand] accepts an expression [stored] to be
    registered in the store, an expression [expr] that includes a template slot for
    the hash of [stored], and an [expected] expression, and generates a test that
    asserts the value of [expr] after expansion matches [expected]. *)
let make_expand_test ~stored ~expr ~expected () =
  create_context () >>=? fun context ->
  let stored_expr = Expr.from_string stored in
  Global_constants_storage.register context stored_expr
  >|= Environment.wrap_tzresult
  >>=? fun (context, hash, _) ->
  let expected = Expr.from_string expected in
  let expr_with_constant =
    Format.sprintf expr (Script_expr_hash.to_b58check hash) |> Expr.from_string
  in
  Global_constants_storage.expand context expr_with_constant
  >|= Environment.wrap_tzresult
  >>=? fun (_, result_expr) -> assert_expr_equal __LOC__ expected result_expr

let test_expand_data_example =
  tztest
    "expand: data"
    `Quick
    (make_expand_test
       ~stored:"3"
       ~expr:"Pair 1 (Pair 2 (constant \"%s\"))"
       ~expected:"Pair 1 (Pair 2 3)")

let test_expand_types_example =
  tztest
    "expand: types"
    `Quick
    (make_expand_test
       ~stored:"big_map string string"
       ~expr:"PUSH (constant \"%s\") {}"
       ~expected:"PUSH (big_map string string) {}")

let test_expand_instr_example =
  tztest
    "expand: instr"
    `Quick
    (make_expand_test
       ~stored:"PUSH int 3"
       ~expr:"{ DROP; constant \"%s\"; DROP }"
       ~expected:"{ DROP; PUSH int 3 ; DROP }")

(** For any expression [e], when replacing any subexpression
    [e'] with a constant hash and registering [e'], calling
    [expand] on the new expression yields the
    original expression [e]*)
let test_expand_pbt =
  let open Michelson_v1_printer in
  tztest_qcheck2
    ~name:"expand: random"
    (Gen.pair
       (Generators.context_gen ())
       (Generators.canonical_with_constant_gen ()))
    (fun (context, (full_expr, expr_with_constant, sub_expr)) ->
      assume_expr_not_too_large full_expr ;
      assume_expr_not_too_large expr_with_constant ;
      assume_expr_not_too_large sub_expr ;
      Global_constants_storage.register context sub_expr
      >|= Environment.wrap_tzresult
      >>=? fun (context, _, _) ->
      Global_constants_storage.expand context expr_with_constant
      >|= Environment.wrap_tzresult
      >|=? fun (_, result_expr) ->
      qcheck_eq ~pp:print_expr full_expr result_expr)

let test_expand_is_idempotent =
  tztest_qcheck2
    ~name:"expand is idempotent"
    (Gen.pair
       (Generators.context_gen ())
       (Generators.canonical_with_constant_gen ()))
    (fun (context, (full_expr, expr_with_constant, sub_expr)) ->
      assume_expr_not_too_large full_expr ;
      Global_constants_storage.register context sub_expr
      >|= Environment.wrap_tzresult
      >>=? fun (context, _, _) ->
      Global_constants_storage.expand context expr_with_constant
      >|= Environment.wrap_tzresult
      >>=? fun (context, result1) ->
      Global_constants_storage.expand context full_expr
      >|= Environment.wrap_tzresult
      >|=? fun (_, result2) -> qcheck_eq ~pp:print_expr result1 result2)

(** [bottom_up_fold_cps] does not stack overflow even when
    given large values. *)
let test_fold_does_not_stack_overflow =
  tztest "bottom_up_fold_cps: does not stack overflow" `Quick (fun () ->
      let node = grow 1_000_000 @@ Int ((), Z.zero) in
      return @@ ignore
      @@ Global_constants_storage.Internal_for_tests.bottom_up_fold_cps
           ()
           node
           (fun _ _ -> ())
           (fun _ node k -> k () node))

let tests =
  [
    test_get_on_nonexistent_fails;
    test_get_always_returns_registered_expr;
    test_register_fails_with_unregistered_references;
    test_register_fails_with_unregistered_references_pbt;
    test_register_fails_if_too_deep;
    test_expand_nonexistent_fails;
    test_expand_no_constants;
    test_register_and_expand_orthogonal;
    test_expand_deep_constants;
    test_expand_reject_ill_formed;
    test_reject_use_of_inner_constant;
    test_expand_data_example;
    test_expand_types_example;
    test_expand_instr_example;
    test_expand_pbt;
    test_expand_is_idempotent;
    test_fold_does_not_stack_overflow;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("Global_constants_storage.ml", tests)]
  |> Lwt_main.run
