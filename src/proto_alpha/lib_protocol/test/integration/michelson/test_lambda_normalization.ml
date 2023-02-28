(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:  Protocol (Michelson)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                -- test "^lambda normalization"
    Subject:    Test that lambdas are normalized to optimized format at elaboration
*)

open Protocol
open Alpha_context
open Script_typed_ir

let new_ctxt () =
  let open Lwt_result_wrap_syntax in
  let* block, _contract = Context.init1 () in
  let+ incr = Incremental.begin_construction block in
  Incremental.alpha_ctxt incr

let parse_and_project (ty : ((_, _) lambda, _) ty) (node : Script.node) =
  let open Lwt_result_wrap_syntax in
  let* ctxt = new_ctxt () in
  let elab_conf = Script_ir_translator_config.make ~legacy:false () in
  let*@ lam, _ctxt =
    Script_ir_translator.parse_data ~elab_conf ctxt ~allow_forged:false ty node
  in
  match lam with
  | Lam (_kdescr, node) -> return node
  | LamRec (_kdescr, node) ->
      return
        Micheline.(
          Prim (dummy_location, Michelson_v1_primitives.D_Lambda_rec, [node], []))

let node_of_string str =
  let open Lwt_result_wrap_syntax in
  let*? parsed =
    Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_expression ~check:false str
  in
  return @@ Micheline.root parsed.expanded

let node_to_string node =
  Format.asprintf
    "%a"
    Micheline_printer.print_expr
    ((Micheline_printer.printable Michelson_v1_primitives.string_of_prim)
       (Micheline.strip_locations node))

let assert_lambda_normalizes_to ~loc ty str expected =
  let open Lwt_result_wrap_syntax in
  let* node = node_of_string str in
  let* node_normalized = parse_and_project ty node in
  let str_normalized = node_to_string node_normalized in
  let* expected_node = node_of_string expected in
  let expected = node_to_string expected_node in
  Assert.equal_string ~loc expected str_normalized

let assert_normalizes_to ~loc ty str expected =
  let open Lwt_result_wrap_syntax in
  let* () = assert_lambda_normalizes_to ~loc ty str expected in
  let* () =
    assert_lambda_normalizes_to
      ~loc
      ty
      ("Lambda_rec " ^ str)
      ("Lambda_rec " ^ expected)
  in
  return_unit

let test_lambda_normalization () =
  let open Lwt_result_wrap_syntax in
  let*?@ ty =
    Script_typed_ir.(lambda_t Micheline.dummy_location unit_t never_t)
  in
  let*?@ lam_unit_unit =
    Script_typed_ir.(lambda_t Micheline.dummy_location unit_t unit_t)
  in
  let* () =
    (* Empty sequence normalizes to itself. *)
    assert_lambda_normalizes_to ~loc:__LOC__ lam_unit_unit "{}" "{}"
  in
  let* () =
    (* Another example normalizing to itself. *)
    assert_normalizes_to ~loc:__LOC__ ty "{FAILWITH}" "{FAILWITH}"
  in
  let* () =
    (* Readable address normalizes to optimized. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{PUSH address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"; FAILWITH}|}
      {|{PUSH address 0x000002298c03ed7d454a101eb7022bc95f7e5f41ac78; FAILWITH}|}
  in
  let* () =
    (* Binary pair normalizes to itself. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{PUSH (pair nat nat) (Pair 0 0); FAILWITH}|}
      {|{PUSH (pair nat nat) (Pair 0 0); FAILWITH}|}
  in
  let* () =
    (* Ternary pair normalizes to nested binary pairs. Type is unchanged. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{PUSH (pair nat nat nat) (Pair 0 0 0); FAILWITH}|}
      {|{PUSH (pair nat nat nat) (Pair 0 (Pair 0 0)); FAILWITH}|}
  in
  let* () =
    (* Same with nested pairs in type. Type is still unchanged. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{PUSH (pair nat (pair nat nat)) (Pair 0 0 0); FAILWITH}|}
      {|{PUSH (pair nat (pair nat nat)) (Pair 0 (Pair 0 0)); FAILWITH}|}
  in
  let* () =
    (* Quadrary pair normalizes to sequence. Type is unchanged. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{PUSH (pair nat nat nat nat) (Pair 0 0 0 0); FAILWITH}|}
      {|{PUSH (pair nat nat nat nat) {0; 0; 0; 0}; FAILWITH}|}
  in
  let* () =
    (* Code inside LAMBDA is normalized too. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{LAMBDA unit never
           {PUSH address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"; FAILWITH};
         FAILWITH}|}
      {|{LAMBDA unit never
           {PUSH address 0x000002298c03ed7d454a101eb7022bc95f7e5f41ac78; FAILWITH};
         FAILWITH}|}
  in
  let* () =
    (* Same with LAMBDA replaced by PUSH. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{PUSH (lambda unit never)
           {PUSH address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"; FAILWITH};
         FAILWITH}|}
      {|{PUSH (lambda unit never)
           {PUSH address 0x000002298c03ed7d454a101eb7022bc95f7e5f41ac78; FAILWITH};
         FAILWITH}|}
  in
  let* () =
    (* Code inside LAMBDA_REC is normalized too. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{LAMBDA_REC unit never
           {PUSH address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
            FAILWITH};
         FAILWITH}|}
      {|{LAMBDA_REC unit never
           {PUSH address 0x000002298c03ed7d454a101eb7022bc95f7e5f41ac78;
            FAILWITH};
         FAILWITH}|}
  in
  let* () =
    (* Same with LAMBDA_REC replaced by PUSH. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{PUSH (lambda unit never)
           (Lambda_rec
              {PUSH address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
               FAILWITH});
         FAILWITH}|}
      {|{PUSH (lambda unit never)
           (Lambda_rec
              {PUSH address 0x000002298c03ed7d454a101eb7022bc95f7e5f41ac78;
               FAILWITH});
         FAILWITH}|}
  in
  let* () =
    (* Code inside CREATE_CONTRACT is normalized too. *)
    assert_normalizes_to
      ~loc:__LOC__
      ty
      {|{PUSH mutez 0;
         NONE key_hash;
         CREATE_CONTRACT
           {parameter unit;
            storage unit;
            code { PUSH address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"; FAILWITH}};
         DROP;
         FAILWITH}|}
      {|{PUSH mutez 0;
         NONE key_hash;
         CREATE_CONTRACT
           {parameter unit;
            storage unit;
            code { PUSH address 0x000002298c03ed7d454a101eb7022bc95f7e5f41ac78; FAILWITH}};
         DROP;
         FAILWITH}|}
  in
  return_unit

let tests =
  [
    Tztest.tztest
      "Test that lambdas are normalized to optimized format during elaboration"
      `Quick
      test_lambda_normalization;
  ]
