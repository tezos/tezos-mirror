(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Micheline
    Invocation:   dune build @src/lib_micheline/runtest
    Dependencies: src/lib_micheline/test/assert.ml
    Subject:      Diffing Micheline expressions
*)

open Alcotest
open Micheline
open Micheline_diff

module Expr : TESTABLE with type t = Micheline_printer.node = struct
  type t = Micheline_printer.node

  let pp = Micheline_printer.print_expr

  let rec equal l r =
    match (l, r) with
    | (Int (locl, il), Int (locr, ir)) -> locl = locr && il = ir
    | (String (locl, sl), String (locr, sr)) -> locl = locr && sl = sr
    | (Bytes (locl, bl), Bytes (locr, br)) -> locl = locr && bl = br
    | (Prim (locl, pl, nodesl, annotl), Prim (locr, pr, nodesr, annotr)) ->
        locl = locr && pl = pr
        && List.equal equal nodesl nodesr
        && annotl = annotr
    | (Seq (locl, nodesl), Seq (locr, nodesr)) ->
        locl = locr && List.equal equal nodesl nodesr
    | _ -> false
end

let no_comment = Micheline_printer.{comment = None}

let comment c = Micheline_printer.{comment = Some c}

let expr : Micheline_printer.node testable = (module Expr)

let int i = Int ((), Z.of_int i)

let str s = String ((), s)

let bytes bs = Bytes ((), bs)

let prim ?(args = []) ?(annots = []) p = Prim ((), p, args, annots)

let seq els = Seq ((), els)

let test_identical _ =
  let (different, d) = diff (seq []) (seq []) in
  check bool "Empty Seqs are not identical!" different false ;
  check expr "Diff not identical to input!" (Seq (no_comment, [])) d ;

  let (different, d) = diff (int 12) (int 12) in
  check bool "Same Integers are not identical!" different false ;
  check expr "Diff not identical to input!" (Int (no_comment, Z.of_int 12)) d ;

  let (different, d) = diff (str "xxx") (str "xxx") in
  check bool "Same Strings are not identical!" different false ;
  check expr "Diff not identical to input!" (String (no_comment, "xxx")) d ;

  let (different, d) = diff (prim "prim") (prim "prim") in
  check bool "Prims are not identical!" different false ;
  check
    expr
    "Diff not identical to input!"
    (Prim (no_comment, "prim", [], []))
    d

let test_different_ints _ =
  let (different, d) = diff (int 23) (int 32) in
  check bool "Different numbers don't differ!" different true ;
  check expr "Unexpected diff!" (Int (comment "32", Z.of_int 23)) d

let test_different_strings _ =
  let (different, d) = diff (str "tezos") (str "texos") in
  check bool "Different strings don't differ!" different true ;
  check expr "Unexpected diff!" (String (comment "texos", "tezos")) d

let test_different_prims _ =
  let (different, d) = diff (prim "ADD") (prim "SUB") in
  check bool "Different prims don't differ!" different true ;
  check expr "Unexpected diff!" (Prim (comment "SUB", "ADD", [], [])) d

let test_different_singleton_seqs _ =
  let (different, d) = diff (seq [str "ADD"]) (seq [int 32]) in
  check bool "Different seqs don't differ!" different true ;
  check
    expr
    "Unexpected diff!"
    (Seq (no_comment, [String (comment "32", "ADD")]))
    d

let test_left_seq_missing_element _ =
  let (different, d) =
    diff
      (seq [str "ADD"; str "MUL"; str "SUB"])
      (seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
  in
  check bool "Different seqs don't differ!" different true ;
  check
    expr
    "Unexpected diff!"
    (Seq
       ( no_comment,
         [
           String (no_comment, "ADD");
           String (no_comment, "MUL");
           String (no_comment, "SUB");
           String (comment "+", "DIV");
         ] ))
    d

let test_right_seq_missing_element _ =
  let (different, d) =
    diff
      (seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
      (seq [str "ADD"; str "MUL"; str "SUB"])
  in
  check bool "Different seqs don't differ!" different true ;
  check
    expr
    "Unexpected diff!"
    (Seq
       ( no_comment,
         [
           String (no_comment, "ADD");
           String (no_comment, "MUL");
           String (no_comment, "SUB");
           String (comment "-", "DIV");
         ] ))
    d

let test_seq_reordered_elements _ =
  let (different, d) =
    diff
      (seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
      (seq [str "ADD"; str "SUB"; str "DIV"; str "MUL"])
  in
  check bool "Different seqs don't differ!" different true ;
  check
    expr
    "Unexpected diff!"
    (Seq
       ( no_comment,
         [
           String (no_comment, "ADD");
           String (comment "SUB", "MUL");
           String (comment "DIV", "SUB");
           String (comment "MUL", "DIV");
         ] ))
    d

let test_seq_replaced_elements _ =
  let (different, d) =
    diff
      (seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
      (seq [int 1; int 2; int 3; int 0])
  in
  check bool "Different seqs don't differ!" different true ;
  check
    expr
    "Unexpected diff!"
    (Seq
       ( no_comment,
         [
           String (comment "1", "ADD");
           String (comment "2", "MUL");
           String (comment "3", "SUB");
           String (comment "0", "DIV");
         ] ))
    d

let test_missing_prim_argument _ =
  let (different, d) = diff (prim "TEST" ~args:[str "TRUE"]) (prim "TEST") in
  check bool "Different seqs don't differ!" different true ;
  check
    expr
    "Unexpected diff!"
    (Prim (no_comment, "TEST", [String (comment "-", "TRUE")], []))
    d

let test_additional_prim_argument _ =
  let (different, d) =
    diff
      (prim "TEST" ~args:[int 1; int 3])
      (prim "TEST" ~args:[int 1; int 2; int 3])
  in
  check bool "Different seqs don't differ!" different true ;
  check
    expr
    "Unexpected diff!"
    (Prim
       ( no_comment,
         "TEST",
         [
           Int (no_comment, Z.of_int 1);
           Int (comment "2", Z.of_int 3);
           Int (comment "+", Z.of_int 3);
         ],
         [] ))
    d

(****************************************************************************)

let tests =
  [
    test_case "test_diff_identical" `Quick test_identical;
    test_case "test_diff_ints" `Quick test_different_ints;
    test_case "test_diff_strings" `Quick test_different_strings;
    test_case "test_diff_prims" `Quick test_different_prims;
    test_case "test_diff_singleton_seqs" `Quick test_different_singleton_seqs;
    test_case
      "test_diff_left_seq_missing_element"
      `Quick
      test_left_seq_missing_element;
    test_case "test_diff_reordered_elements" `Quick test_seq_reordered_elements;
    test_case "test_diff_replaced_elements" `Quick test_seq_replaced_elements;
    test_case "test_diff_missing_prim_arg" `Quick test_missing_prim_argument;
    test_case
      "test_diff_additional_prim_arg"
      `Quick
      test_additional_prim_argument;
  ]

let () = run ~argv:[|""|] "tezos-lib-micheline" [("micheline", tests)]
