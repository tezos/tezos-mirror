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
open Tztest
open Micheline
open QCheck
open Lib_test.Qcheck_helpers
open Michelson_v1_primitives
open Michelson_v1_printer

let create_context () =
  let accounts = Account.generate_accounts 2 in
  Block.alpha_context accounts

let expr_to_hash expr =
  let lexpr = Script_repr.lazy_expr expr in
  Script_repr.force_bytes lexpr >|? fun b -> Script_expr_hash.hash_bytes [b]

let assert_expr_equal loc =
  Assert.equal
    ~loc
    ( = )
    "Michelson Expressions Not Equal"
    Michelson_v1_printer.print_expr

let assert_error_id loc id result =
  let test err = (Error_monad.find_info_of_error err).id = id in
  Assert.error ~loc result test

let assert_ok_lwt x =
  match Lwt_main.run x with
  | Ok x -> x
  | Error _ -> raise @@ Failure "Called assert_ok_lwt on Error"

let assert_ok = function
  | Ok x -> x
  | Error _ -> raise @@ Failure "Called assert_ok on Error"

module Generators = struct
  let context_arbitrary =
    QCheck.make @@ QCheck.Gen.return (create_context () |> assert_ok_lwt)

  let prims =
    [
      K_parameter;
      K_storage;
      K_code;
      D_False;
      D_Elt;
      D_Left;
      D_None;
      D_Pair;
      D_Right;
      D_Some;
      D_True;
      D_Unit;
      I_PACK;
      I_UNPACK;
      I_BLAKE2B;
      I_SHA256;
      I_SHA512;
      I_ABS;
      I_ADD;
      I_AMOUNT;
      I_AND;
      I_BALANCE;
      I_CAR;
      I_CDR;
      I_CHAIN_ID;
      I_CHECK_SIGNATURE;
      I_COMPARE;
      I_CONCAT;
      I_CONS;
      I_CREATE_ACCOUNT;
      I_CREATE_CONTRACT;
      I_IMPLICIT_ACCOUNT;
      I_DIP;
      I_DROP;
      I_DUP;
      I_EDIV;
      I_EMPTY_BIG_MAP;
      I_EMPTY_MAP;
      I_EMPTY_SET;
      I_EQ;
      I_EXEC;
      I_APPLY;
      I_FAILWITH;
      I_GE;
      I_GET;
      I_GET_AND_UPDATE;
      I_GT;
      I_HASH_KEY;
      I_IF;
      I_IF_CONS;
      I_IF_LEFT;
      I_IF_NONE;
      I_INT;
      I_LAMBDA;
      I_LE;
      I_LEFT;
      I_LEVEL;
      I_LOOP;
      I_LSL;
      I_LSR;
      I_LT;
      I_MAP;
      I_MEM;
      I_MUL;
      I_NEG;
      I_NEQ;
      I_NIL;
      I_NONE;
      I_NOT;
      I_NOW;
      I_OR;
      I_PAIR;
      I_UNPAIR;
      I_PUSH;
      I_RIGHT;
      I_SIZE;
      I_SOME;
      I_SOURCE;
      I_SENDER;
      I_SELF;
      I_SELF_ADDRESS;
      I_SLICE;
      I_STEPS_TO_QUOTA;
      I_SUB;
      I_SWAP;
      I_TRANSFER_TOKENS;
      I_SET_DELEGATE;
      I_UNIT;
      I_UPDATE;
      I_XOR;
      I_ITER;
      I_LOOP_LEFT;
      I_ADDRESS;
      I_CONTRACT;
      I_ISNAT;
      I_CAST;
      I_RENAME;
      I_SAPLING_EMPTY_STATE;
      I_SAPLING_VERIFY_UPDATE;
      I_DIG;
      I_DUG;
      I_NEVER;
      I_VOTING_POWER;
      I_TOTAL_VOTING_POWER;
      I_KECCAK;
      I_SHA3;
      I_PAIRING_CHECK;
      I_TICKET;
      I_READ_TICKET;
      I_SPLIT_TICKET;
      I_JOIN_TICKETS;
      T_bool;
      T_contract;
      T_int;
      T_key;
      T_key_hash;
      T_lambda;
      T_list;
      T_map;
      T_big_map;
      T_nat;
      T_option;
      T_or;
      T_pair;
      T_set;
      T_signature;
      T_string;
      T_bytes;
      T_mutez;
      T_timestamp;
      T_unit;
      T_operation;
      T_address;
      T_sapling_transaction;
      T_sapling_state;
      T_chain_id;
      T_never;
      T_bls12_381_g1;
      T_bls12_381_g2;
      T_bls12_381_fr;
      T_ticket;
      H_constant;
    ]

  let prim_gen = QCheck.Gen.oneofl prims

  let prims_without_constants_gen =
    QCheck.Gen.oneofl (List.filter (fun x -> x != H_constant) prims)

  let z_gen = QCheck.Gen.map Z.of_int QCheck.Gen.int

  let micheline_node_gen l_gen p_gen annot_gen :
      ('l, 'p) Micheline.node QCheck.Gen.t =
    let open Micheline in
    let open QCheck.Gen in
    fix
      (fun self () ->
        frequency
          [
            (3, map (fun (l, x) -> Int (l, x)) (pair l_gen z_gen));
            (3, map (fun (l, x) -> String (l, x)) (pair l_gen string));
            ( 3,
              map
                (fun (l, x) -> Bytes (l, Bytes.of_string x))
                (pair l_gen string) );
            ( 1,
              map
                (fun (l, p, args, annot) -> Prim (l, p, args, annot))
                (quad
                   l_gen
                   p_gen
                   (list_size (int_bound 10) (self ()))
                   annot_gen) );
            ( 1,
              map
                (fun (l, args) -> Seq (l, args))
                (pair l_gen (list_size (int_bound 10) (self ()))) );
          ])
      ()

  let rec replace_with_constant :
      Script.node -> int -> Script.node * Script.node option =
   fun node loc ->
    let open Michelson_v1_primitives in
    let open Micheline in
    let rec loop : Script.node list -> Script.node list * Script.node option =
     fun node_list ->
      match node_list with
      | [] -> ([], None)
      | hd :: tl -> (
          match replace_with_constant hd loc with
          | (node, Some x) -> (node :: tl, Some x)
          | (_, None) ->
              let (l, x) = loop tl in
              (hd :: l, x))
    in
    match node with
    | (Int (l, _) | String (l, _) | Bytes (l, _)) as node ->
        if l = loc then
          let hash =
            node |> strip_locations |> expr_to_hash |> assert_ok
            |> Script_expr_hash.to_b58check
          in
          (Prim (-1, H_constant, [String (-1, hash)], []), Some node)
        else (node, None)
    | Prim (l, prim, args, annot) as node ->
        if l = loc then
          let hash =
            node |> strip_locations |> expr_to_hash |> assert_ok
            |> Script_expr_hash.to_b58check
          in
          (Prim (-1, H_constant, [String (-1, hash)], []), Some node)
        else
          let (result, x) = loop args in
          (Prim (l, prim, result, annot), x)
    | Seq (l, args) as node ->
        if l = loc then
          let hash =
            node |> strip_locations |> expr_to_hash |> assert_ok
            |> Script_expr_hash.to_b58check
          in
          (Prim (-1, H_constant, [String (-1, hash)], []), Some node)
        else
          let (result, x) = loop args in
          (Seq (l, result), x)

  let micheline_gen p_gen annot_gen =
    QCheck.Gen.map
      Micheline.strip_locations
      (micheline_node_gen (QCheck.Gen.return (-1)) p_gen annot_gen)

  let canonical_without_constant_gen =
    QCheck.Gen.map
      strip_locations
      (micheline_node_gen
         (QCheck.Gen.return (-1))
         prims_without_constants_gen
         (QCheck.Gen.return []))

  let canonical_without_constant_arbitrary =
    QCheck.make canonical_without_constant_gen

  let canonical_with_constant_gen =
    let open QCheck.Gen in
    canonical_without_constant_gen >>= fun expr ->
    let size = Script_repr.micheline_nodes (root expr) in
    0 -- (size - 1) >|= fun loc ->
    match replace_with_constant (root expr) loc with
    | (_, None) -> assert false
    | (node, Some replaced_node) ->
        (expr, strip_locations node, strip_locations replaced_node)

  let canonical_with_constant_arbitrary =
    QCheck.make canonical_with_constant_gen
end

(** Filters out values that would cause [register] *)
let assume_expr_not_too_large expr =
  let node = root expr in
  assume @@ not
  @@ Global_constants_storage.Internal_for_tests.node_too_large node

(** [get] on a nonexistent global constant
    returns an error. *)
let test_get_on_nonexistent_fails =
  tztest_qcheck
    ~name:"get on a nonexistent global constants fails"
    (pair
       Generators.context_arbitrary
       Generators.canonical_without_constant_arbitrary)
    (fun (context, expr) ->
      expr_to_hash expr |> Environment.wrap_tzresult >>?= fun hash ->
      Global_constants_storage.get context hash
      >|= Environment.wrap_tzresult
      >>= assert_error_id __LOC__ "proto.alpha.Nonexistent_global")

(** If registering an expression yields a hash [h] and context [c],
    then [get c h] should yield the original expression. *)
let test_get_always_returns_registered_expr =
  tztest_qcheck
    ~name:"get always returned the registered constant"
    (pair
       Generators.context_arbitrary
       Generators.canonical_without_constant_arbitrary)
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
      >>= assert_error_id __LOC__ "proto.alpha.Nonexistent_global")

(** Same test as [test_register_fails_with_unregistered_references]
    but with random values. *)
let test_register_fails_with_unregistered_references_pbt =
  tztest_qcheck
    ~name:"register: fails with unregistered references pbt"
    (pair
       Generators.context_arbitrary
       Generators.canonical_with_constant_arbitrary)
    (fun (context, (_, expr, _)) ->
      assume_expr_not_too_large expr ;
      Global_constants_storage.register context expr
      >|= Environment.wrap_tzresult
      >>= assert_error_id __LOC__ "proto.alpha.Nonexistent_global")

let rec grow n node =
  match n with n when n <= 0 -> node | n -> grow (n - 1) (Seq (-1, [node]))

(* Any expression with a depth that exceeds
   [Global_constants_storage.max_allowed_global_constant_depth]
   should be rejected. *)
let test_register_fails_if_too_deep =
  tztest "register: fails if expression too deep" `Quick (fun () ->
      let vdeep_expr =
        grow
          (Constants_repr.max_allowed_global_constant_depth + 1)
          (Int (-1, Z.of_int 1))
        |> Micheline.strip_locations
      in
      create_context () >>=? fun context ->
      Global_constants_storage.register context vdeep_expr
      >|= Environment.wrap_tzresult
      >>= assert_error_id __LOC__ "proto.alpha.Expression_too_deep")

(** [substitute] on an expression containing a nonexistent global
    constant returns an error. *)
let test_substitute_nonexistent_fails =
  tztest_qcheck
    ~name:
      "substitute on an expression containing a nonexistent global constant \
       fails"
    (pair
       Generators.context_arbitrary
       Generators.canonical_with_constant_arbitrary)
  @@ fun (context, (_, expr, _)) ->
  assume_expr_not_too_large expr ;
  Global_constants_storage.substitute context expr
  >|= Environment.wrap_tzresult
  >>= assert_error_id __LOC__ "proto.alpha.Nonexistent_global"

(** Substituting an expression without constants should yield the same expression. *)
let test_substitute_no_constants =
  tztest "substitute: no constants case" `Quick (fun () ->
      create_context () >>=? fun context ->
      let expected = Expr.from_string "Pair 1 (Pair 2 3)" in
      Global_constants_storage.substitute context expected
      >|= Environment.wrap_tzresult
      >>=? fun (_, result_expr) ->
      assert_expr_equal __LOC__ expected result_expr)

(** Similar to [test_substitute_no_constants], but random. *)
let test_register_and_substitute_orthogonal =
  tztest_qcheck
    ~name:"register and substitute are orthogonal"
    (triple
       Generators.context_arbitrary
       Generators.canonical_without_constant_arbitrary
       Generators.canonical_without_constant_arbitrary)
    (fun (context, expr1, expr2) ->
      assume_expr_not_too_large expr1 ;
      assume_expr_not_too_large expr2 ;
      let open Michelson_v1_printer in
      Global_constants_storage.register context expr1
      >|= Environment.wrap_tzresult
      >>=? fun (context, _hash, _cost) ->
      Global_constants_storage.substitute context expr2
      >|= Environment.wrap_tzresult
      >|=? fun (_, expr2_result) -> qcheck_eq ~pp:print_expr expr2 expr2_result)

(** Substitution should expand constants in the given
    expression, then expand any new constants, etc.
    recursively until no constants remain.  *)
let test_substitute_deep_constants =
  tztest "substitute: deep constants" `Quick (fun () ->
      (* Should hold for any n, but this test is very slow,
         hence we don't do QCheck. *)
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
      Global_constants_storage.substitute context deep_expr
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
    Script_repr.expr hash. *)
let test_substitute_reject_ill_formed =
  tztest "substitute: ill formed constants are rejected" `Quick (fun () ->
      let test expr =
        create_context () >>=? fun context ->
        let expected = Expr.from_string expr in
        Global_constants_storage.substitute context expected
        >|= Environment.wrap_tzresult
        >>= assert_error_id
              __LOC__
              "proto.alpha.Badly_formed_constant_expression"
      in
      (* constant with an argument other than String fails *)
      test "constant 9" >>=? fun _ ->
      (* same as above but nested *)
      test "Pair 1 (constant (Pair 2 3))"
      (* constant with bad hash fails *)
      >>=? fun _ -> test "constant \"foobar\"")

(** [test_substitute] accepts an expression [stored] to be
    registered in the store, an expression [expr] that includes a template slot for
    the hash of [stored], and an [expected] expression, and generates a test that
    asserts the value of [expr] after substitution matches [expected]. *)
let make_substitute_test ~stored ~expr ~expected () =
  create_context () >>=? fun context ->
  let stored_expr = Expr.from_string stored in
  Global_constants_storage.register context stored_expr
  >|= Environment.wrap_tzresult
  >>=? fun (context, hash, _) ->
  let expected = Expr.from_string expected in
  let expr_with_constant =
    Format.sprintf expr (Script_expr_hash.to_b58check hash) |> Expr.from_string
  in
  Global_constants_storage.substitute context expr_with_constant
  >|= Environment.wrap_tzresult
  >>=? fun (_, result_expr) -> assert_expr_equal __LOC__ expected result_expr

let test_substitute_data_example =
  tztest
    "substitute: data"
    `Quick
    (make_substitute_test
       ~stored:"3"
       ~expr:"Pair 1 (Pair 2 (constant \"%s\"))"
       ~expected:"Pair 1 (Pair 2 3)")

let test_substitute_types_example =
  tztest
    "substitute: types"
    `Quick
    (make_substitute_test
       ~stored:"big_map string string"
       ~expr:"PUSH (constant \"%s\") {}"
       ~expected:"PUSH (big_map string string) {}")

let test_substitute_instr_example =
  tztest
    "substitute: instr"
    `Quick
    (make_substitute_test
       ~stored:"PUSH int 3"
       ~expr:"{ DROP; constant \"%s\"; DROP }"
       ~expected:"{ DROP; PUSH int 3 ; DROP }")

(** For any expression [e], when replacing any subexpression
    [e'] with a constant hash and registering [e'], calling
    [substitute] on the new expression yields the
    original expression [e]*)
let test_substitute_pbt =
  let open Michelson_v1_printer in
  tztest_qcheck
    ~name:"substitute: random"
    (pair
       Generators.context_arbitrary
       Generators.canonical_with_constant_arbitrary)
    (fun (context, (full_expr, expr_with_constant, sub_expr)) ->
      assume_expr_not_too_large full_expr ;
      assume_expr_not_too_large expr_with_constant ;
      assume_expr_not_too_large sub_expr ;
      Global_constants_storage.register context sub_expr
      >|= Environment.wrap_tzresult
      >>=? fun (context, _, _) ->
      Global_constants_storage.substitute context expr_with_constant
      >|= Environment.wrap_tzresult
      >|=? fun (_, result_expr) ->
      qcheck_eq ~pp:print_expr full_expr result_expr)

let test_substitute_is_idempotent =
  tztest_qcheck
    ~name:"substitute is idempotent"
    (pair
       Generators.context_arbitrary
       Generators.canonical_with_constant_arbitrary)
    (fun (context, (full_expr, expr_with_constant, sub_expr)) ->
      assume_expr_not_too_large full_expr ;
      Global_constants_storage.register context sub_expr
      >|= Environment.wrap_tzresult
      >>=? fun (context, _, _) ->
      Global_constants_storage.substitute context expr_with_constant
      >|= Environment.wrap_tzresult
      >>=? fun (context, result1) ->
      Global_constants_storage.substitute context full_expr
      >|= Environment.wrap_tzresult
      >|=? fun (_, result2) -> qcheck_eq ~pp:print_expr result1 result2)

(** [bottom_up_fold_cps] does not stack overflow even when
    given large values. *)
let test_fold_does_not_stack_overflow =
  tztest "bottom_up_fold_cps: does not stack overflow" `Quick (fun () ->
      let node = grow 1_000_000 @@ Int (-1, Z.zero) in
      return @@ ignore
      @@ Global_constants_storage.Internal_for_tests.bottom_up_fold_cps
           ()
           node
           (fun _ _ -> ())
           (fun k _ node -> k () node))

let tests =
  [
    test_get_on_nonexistent_fails;
    test_get_always_returns_registered_expr;
    test_register_fails_with_unregistered_references;
    test_register_fails_with_unregistered_references_pbt;
    test_register_fails_if_too_deep;
    test_substitute_nonexistent_fails;
    test_substitute_no_constants;
    test_register_and_substitute_orthogonal;
    test_substitute_deep_constants;
    test_substitute_reject_ill_formed;
    test_substitute_data_example;
    test_substitute_types_example;
    test_substitute_instr_example;
    test_substitute_pbt;
    test_substitute_is_idempotent;
    test_fold_does_not_stack_overflow;
  ]
