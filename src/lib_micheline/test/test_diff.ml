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

(* Testing
   -------
   Component:    Micheline
   Invocation:   dune build @src/lib_micheline/runtest
   Subject:      Diffing Micheline expressions
*)

open Micheline
open Micheline_diff

let int i = Int ((), Z.of_int i)

let str s = String ((), s)

let prim ?(args = []) ?(annots = []) p = Prim ((), p, args, annots)

let seq els = Seq ((), els)

let print_diff diff : unit =
  Option.iter (Micheline_printer.print_expr Format.std_formatter) diff

let%expect_test "For identical expressions, the diff is None." =
  print_diff (diff ~prev:(seq []) ~current:(seq []) ()) ;
  [%expect {||}] ;
  print_diff (diff ~prev:(int 12) ~current:(int 12) ()) ;
  [%expect {||}] ;
  print_diff (diff ~prev:(str "xxx") ~current:(str "xxx") ()) ;
  [%expect {||}] ;
  print_diff (diff ~prev:(prim "prim") ~current:(prim "prim") ()) ;
  [%expect {||}]

let%expect_test "For ints the comment contains changed value." =
  print_diff (diff ~prev:(int 23) ~current:(int 32) ()) ;
  [%expect {|
    23
    /* -> 32 */ |}]

let%expect_test "For strings the comment contains changed value." =
  print_diff (diff ~prev:(str "tezos") ~current:(str "texos") ()) ;
  [%expect {|
    "tezos"
    /* -> "texos" */ |}]

let%expect_test "For prims the comment contains changed prim name." =
  print_diff (diff ~prev:(prim "ADD") ~current:(prim "SUB") ()) ;
  [%expect {|
    ADD
    /* -> SUB */ |}]

let%expect_test "Changed Seq elements are put in comments." =
  print_diff (diff ~prev:(seq [str "ADD"]) ~current:(seq [int 32]) ()) ;
  [%expect {| { "ADD" /* -> 32 */ } |}]

let%expect_test "Elements added in current are marked with '+'." =
  print_diff
    (diff
       ()
       ~prev:(seq [str "ADD"; str "MUL"; str "SUB"])
       ~current:(seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])) ;
  [%expect {| { "ADD" ; "MUL" ; "SUB" ; "DIV" /* + */ } |}]

let%expect_test "Elements removed in current are marked with '-'." =
  print_diff
    (diff
       ()
       ~prev:(seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
       ~current:(seq [str "ADD"; str "MUL"; str "SUB"])) ;
  [%expect {| { "ADD" ; "MUL" ; "SUB" ; "DIV" /* - */ } |}]

let%expect_test "Reordered elements appear as just changed." =
  print_diff
    (diff
       ()
       ~prev:(seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
       ~current:(seq [str "ADD"; str "SUB"; str "DIV"; str "MUL"])) ;
  [%expect
    {| { "ADD" ; "MUL" /* -> "SUB" */ ; "SUB" /* -> "DIV" */ ; "DIV" /* -> "MUL" */ } |}]

let%expect_test "Changed values are marked with '->'." =
  print_diff
    (diff
       ()
       ~prev:(seq [str "ADD"; str "MUL"; str "SUB"; str "DIV"])
       ~current:(seq [int 1; int 2; int 3; int 0])) ;
  [%expect
    {| { "ADD" /* -> 1 */ ; "MUL" /* -> 2 */ ; "SUB" /* -> 3 */ ; "DIV" /* -> 0 */ } |}]

let%expect_test "Prim arguments removed in current are marked with '-'." =
  print_diff
    (diff ~prev:(prim "TEST" ~args:[str "TRUE"]) ~current:(prim "TEST") ()) ;
  [%expect {| (TEST "TRUE" /* - */) |}]

let%expect_test "Prim arguments added in current are marked with '+'." =
  print_diff
    (diff
       ()
       ~prev:(prim "TEST" ~args:[int 1; int 3])
       ~current:(prim "TEST" ~args:[int 1; int 2; int 3])) ;
  [%expect {| (TEST 1 3 /* -> 2 */ 3 /* + */) |}]

let%expect_test "Annotations should be preserved as in previous version." =
  print_diff
    (diff
       ()
       ~prev:(prim "ADD" ~annots:["@annot"])
       ~current:(prim "SUB" ~annots:["@annot"])) ;
  [%expect {|
    (ADD @annot
    /* -> SUB */) |}]

let%expect_test "Differences in annotations are ignored." =
  print_diff
    (diff () ~prev:(prim "ADD" ~annots:["@annot"]) ~current:(prim "ADD")) ;
  [%expect {||}]
