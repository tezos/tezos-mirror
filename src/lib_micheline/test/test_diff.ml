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

let check_diff ~descr ~expected actual =
  check' (option expr) ~msg:descr ~expected ~actual

let test_identical _ =
  let descr = "For identical expressions, the diff is None." in
  let expected : Micheline_printer.node option = None in
  let actual = diff ~prev:(seq []) ~current:(seq []) () in
  check_diff ~descr actual ~expected ;

  let actual = diff ~prev:(int 12) ~current:(int 12) () in
  check_diff ~descr actual ~expected ;

  let actual = diff ~prev:(str "xxx") ~current:(str "xxx") () in
  check_diff ~descr actual ~expected ;

  let actual = diff ~prev:(prim "prim") ~current:(prim "prim") () in
  check_diff ~descr actual ~expected

let test_different_ints _ =
  check_diff
    ~descr:"For ints the comment contains changed value."
    ~expected:(Some (Int (comment "-> 32", Z.of_int 23)))
    (diff ~prev:(int 23) ~current:(int 32) ())

let test_different_strings _ =
  check_diff
    ~descr:"For strings the comment contains changed value."
    ~expected:(Some (String (comment "-> \"texos\"", "tezos")))
    (diff ~prev:(str "tezos") ~current:(str "texos") ())

let test_different_prims _ =
  check_diff
    ~descr:"For prims the comment contains changed prim name."
    ~expected:(Some (Prim (comment "-> SUB", "ADD", [], [])))
    (diff ~prev:(prim "ADD") ~current:(prim "SUB") ())

let test_different_singleton_seqs _ =
  check_diff
    ~descr:"Changed Seq elements are put in comments."
    ~expected:(Some (Seq (no_comment, [String (comment "-> 32", "ADD")])))
    (diff ~prev:(seq [str "ADD"]) ~current:(seq [int 32]) ())

let test_prev_seq_missing_element _ =
  check_diff
    ~descr:"Elements added in current are marked with '+'."
    ~expected:
      (Some
         (Seq
            ( no_comment,
              [
                String (no_comment, "ADD");
                String (no_comment, "MUL");
                String (no_comment, "SUB");
                String (comment "+", "DIV");
              ] )))
    (diff
       ()
       ~prev:(seq [str "ADD"; str "MUL"; str "SUB"])
       ~current:(seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"]))

let test_current_seq_missing_element _ =
  check_diff
    ~descr:"Elements removed in current are marked with '-'."
    ~expected:
      (Some
         (Seq
            ( no_comment,
              [
                String (no_comment, "ADD");
                String (no_comment, "MUL");
                String (no_comment, "SUB");
                String (comment "-", "DIV");
              ] )))
    (diff
       ()
       ~prev:(seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
       ~current:(seq [str "ADD"; str "MUL"; str "SUB"]))

let test_seq_reordered_elements _ =
  check_diff
    ~descr:"Reordered elements appear as just changed."
    ~expected:
      (Some
         (Seq
            ( no_comment,
              [
                String (no_comment, "ADD");
                String (comment "-> \"SUB\"", "MUL");
                String (comment "-> \"DIV\"", "SUB");
                String (comment "-> \"MUL\"", "DIV");
              ] )))
    (diff
       ()
       ~prev:(seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
       ~current:(seq [str "ADD"; str "SUB"; str "DIV"; str "MUL"]))

let test_seq_replaced_elements _ =
  check_diff
    ~descr:"Changed values are marked with '->'."
    ~expected:
      (Some
         (Seq
            ( no_comment,
              [
                String (comment "-> 1", "ADD");
                String (comment "-> 2", "MUL");
                String (comment "-> 3", "SUB");
                String (comment "-> 0", "DIV");
              ] )))
    (diff
       ()
       ~prev:(seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
       ~current:(seq [int 1; int 2; int 3; int 0]))

let test_missing_prim_argument _ =
  check_diff
    ~descr:"Prim arguments removed in current are marked with '-'."
    ~expected:
      (Some (Prim (no_comment, "TEST", [String (comment "-", "TRUE")], [])))
    (diff ~prev:(prim "TEST" ~args:[str "TRUE"]) ~current:(prim "TEST") ())

let test_additional_prim_argument _ =
  check_diff
    ~descr:"Prim arguments added in current are marked with '+'."
    ~expected:
      (Some
         (Prim
            ( no_comment,
              "TEST",
              [
                Int (no_comment, Z.of_int 1);
                Int (comment "-> 2", Z.of_int 3);
                Int (comment "+", Z.of_int 3);
              ],
              [] )))
    (diff
       ()
       ~prev:(prim "TEST" ~args:[int 1; int 3])
       ~current:(prim "TEST" ~args:[int 1; int 2; int 3]))

let test_annots_are_preserved _ =
  check_diff
    ~descr:"Annotations should be preserved as in previous version."
    ~expected:(Some (Prim (comment "-> SUB", "ADD", [], ["@annot"])))
    (diff
       ()
       ~prev:(prim "ADD" ~annots:["@annot"])
       ~current:(prim "SUB" ~annots:["@annot"]))

let test_different_annotations_are_ignored _ =
  check_diff
    ~descr:"Differences in annotations are ignored."
    ~expected:None
    (diff () ~prev:(prim "ADD" ~annots:["@annot"]) ~current:(prim "ADD"))

(****************************************************************************)

let tests =
  [
    test_case "test_diff_identical" `Quick test_identical;
    test_case "test_diff_ints" `Quick test_different_ints;
    test_case "test_diff_strings" `Quick test_different_strings;
    test_case "test_diff_prims" `Quick test_different_prims;
    test_case "test_diff_singleton_seqs" `Quick test_different_singleton_seqs;
    test_case
      "test_diff_prev_seq_missing_element"
      `Quick
      test_prev_seq_missing_element;
    test_case
      "test_diff_current_seq_missing_element"
      `Quick
      test_current_seq_missing_element;
    test_case "test_diff_reordered_elements" `Quick test_seq_reordered_elements;
    test_case "test_diff_replaced_elements" `Quick test_seq_replaced_elements;
    test_case "test_diff_missing_prim_arg" `Quick test_missing_prim_argument;
    test_case
      "test_diff_additional_prim_arg"
      `Quick
      test_additional_prim_argument;
    test_case
      "test_annots_are_preserved_as_in_prev"
      `Quick
      test_annots_are_preserved;
    test_case
      "test_different_annotations_are_ignored"
      `Quick
      test_different_annotations_are_ignored;
  ]

let () = run ~argv:[|""|] "tezos-lib-micheline" [("micheline", tests)]
