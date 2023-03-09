(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Client
    Invocation:   dune build @src/proto_alpha/lib_client/runtest
    Dependencies: src/proto_alpha/lib_client/test/assert.ml
    Subject:      Expansion and unexpansion of Micheline terms.
*)

open Protocol
module Assert = Assert

let pp ppf expr =
  expr
  |> Micheline_printer.printable (fun s -> s)
  |> Format.fprintf ppf "%a" Micheline_printer.print_expr

let to_string e = Format.asprintf "%a" pp e
(* expands : expression with macros fully expanded *)

let assert_expands
    (original : (Micheline_parser.location, string) Micheline.node)
    (expanded : (Micheline_parser.location, string) Micheline.node) =
  let {Michelson_v1_parser.expanded = expansion; _}, errors =
    let source = to_string (Micheline.strip_locations original) in
    Michelson_v1_parser.expand_all ~source ~original
  in
  match errors with
  | [] ->
      Assert.equal
        ~pp
        (Michelson_v1_primitives.strings_of_prims expansion)
        (Micheline.strip_locations expanded) ;
      ok ()
  | errors -> Error errors

(****************************************************************************)

open Micheline

let zero_loc = Micheline_parser.location_zero

let left_branch = Seq (zero_loc, [Prim (zero_loc, "SWAP", [], [])])

let right_branch = Seq (zero_loc, [])

(***************************************************************************)
(* Test expands *)
(***************************************************************************)

(** [prim_name] is the syntactic sugar to be expanded, while [compare_name]
    is syntactic atom. *)
let assert_compare_macro prim_name compare_name =
  assert_expands
    (Prim (zero_loc, prim_name, [], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "COMPARE", [], []);
           Prim (zero_loc, compare_name, [], []);
         ] ))

(** Expand "COMP{EQ|NEQ|LT|GT|LE|GE}"
    into   "COMPARE ; {EQ|NEQ|LT|GT|LE|GE}".
*)
let test_compare_marco_expansion () =
  assert_compare_macro "CMPEQ" "EQ" >>? fun () ->
  assert_compare_macro "CMPNEQ" "NEQ" >>? fun () ->
  assert_compare_macro "CMPLT" "LT" >>? fun () ->
  assert_compare_macro "CMPGT" "GT" >>? fun () ->
  assert_compare_macro "CMPLE" "LE" >>? fun () ->
  assert_compare_macro "CMPGE" "GE"

let assert_if_macro prim_name compare_name =
  assert_expands
    (Prim (zero_loc, prim_name, [left_branch; right_branch], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, compare_name, [], []);
           Prim (zero_loc, "IF", [left_branch; right_branch], []);
         ] ))

(** Expand "IF{EQ|NEQ|LT|GT|LE|GE}"
    into   "{EQ|NEQ|LT|GT|LE|GE} ; IF"
*)
let test_if_compare_macros_expansion () =
  assert_if_macro "IFEQ" "EQ" >>? fun () ->
  assert_if_macro "IFNEQ" "NEQ" >>? fun () ->
  assert_if_macro "IFLT" "LT" >>? fun () ->
  assert_if_macro "IFGT" "GT" >>? fun () ->
  assert_if_macro "IFLE" "LE" >>? fun () -> assert_if_macro "IFGE" "GE"

let assert_if_cmp_macros prim_name compare_name =
  assert_expands
    (Prim (zero_loc, prim_name, [left_branch; right_branch], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "COMPARE", [], []);
           Prim (zero_loc, compare_name, [], []);
           Prim (zero_loc, "IF", [left_branch; right_branch], []);
         ] ))

(** Expand "IF{EQ|NEQ|LT|GT|LE|GE}"
    into   "{EQ|NEQ|LT|GT|LE|GE} ; IF"
*)
let test_if_cmp_macros_expansion () =
  assert_if_cmp_macros "IFCMPEQ" "EQ" >>? fun () ->
  assert_if_cmp_macros "IFCMPNEQ" "NEQ" >>? fun () ->
  assert_if_cmp_macros "IFCMPLT" "LT" >>? fun () ->
  assert_if_cmp_macros "IFCMPGT" "GT" >>? fun () ->
  assert_if_cmp_macros "IFCMPLE" "LE" >>? fun () ->
  assert_if_cmp_macros "IFCMPGE" "GE"

(****************************************************************************)
(* Fail *)

(** Expand "FAIL"
    into   "UNIT ; FAILWITH"
*)
let test_fail_expansion () =
  assert_expands
    (Prim (zero_loc, "FAIL", [], []))
    (Seq
       ( zero_loc,
         [Prim (zero_loc, "UNIT", [], []); Prim (zero_loc, "FAILWITH", [], [])]
       ))

(**********************************************************************)
(* assertion *)

let seq_unit_failwith =
  Seq
    ( zero_loc,
      [Prim (zero_loc, "UNIT", [], []); Prim (zero_loc, "FAILWITH", [], [])] )

(* {} {FAIL} *)
let fail_false = [Seq (zero_loc, []); Seq (zero_loc, [seq_unit_failwith])]

(* {FAIL} {} *)
let fail_true = [Seq (zero_loc, [seq_unit_failwith]); Seq (zero_loc, [])]

(** Expand "ASSERT"
    into   "IF {} {FAIL}"
*)
let test_assert_expansion () =
  assert_expands
    (Prim (zero_loc, "ASSERT", [], []))
    (Seq (zero_loc, [Prim (zero_loc, "IF", fail_false, [])]))

let assert_assert_if_compare prim_name compare_name =
  assert_expands
    (Prim (zero_loc, prim_name, [], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, compare_name, [], []);
           Prim (zero_loc, "IF", fail_false, []);
         ] ))

(** Expand "ASSERT_{EQ|NEQ|LT|GT|LE|GE}"
    into   "{EQ|NEQ|LT|GT|LE|GE} ; IF {} {FAIL}"
*)
let test_assert_if () =
  assert_assert_if_compare "ASSERT_EQ" "EQ" >>? fun () ->
  assert_assert_if_compare "ASSERT_NEQ" "NEQ" >>? fun () ->
  assert_assert_if_compare "ASSERT_LT" "LT" >>? fun () ->
  assert_assert_if_compare "ASSERT_LE" "LE" >>? fun () ->
  assert_assert_if_compare "ASSERT_GT" "GT" >>? fun () ->
  assert_assert_if_compare "ASSERT_GE" "GE"

let assert_cmp_if prim_name compare_name =
  assert_expands
    (Prim (zero_loc, prim_name, [], []))
    (Seq
       ( zero_loc,
         [
           Seq
             ( zero_loc,
               [
                 Prim (zero_loc, "COMPARE", [], []);
                 Prim (zero_loc, compare_name, [], []);
               ] );
           Prim (zero_loc, "IF", fail_false, []);
         ] ))

(** Expand "ASSERT_CMP{EQ|NEQ|LT|GT|LE|GE}"
    into   "COMPARE ; {EQ|NEQ|LT|GT|LE|GE} ; IF {} {FAIL}"
*)
let test_assert_cmp_if () =
  assert_cmp_if "ASSERT_CMPEQ" "EQ" >>? fun () ->
  assert_cmp_if "ASSERT_CMPNEQ" "NEQ" >>? fun () ->
  assert_cmp_if "ASSERT_CMPLT" "LT" >>? fun () ->
  assert_cmp_if "ASSERT_CMPLE" "LE" >>? fun () ->
  assert_cmp_if "ASSERT_CMPGT" "GT" >>? fun () ->
  assert_cmp_if "ASSERT_CMPGE" "GE"

(* The work of merge request !628
   > ASSERT_LEFT @x  =>  IF_LEFT {RENAME @x} {FAIL}
   > ASSERT_RIGHT @x  =>  IF_LEFT {FAIL} {RENAME @x}
   > ASSERT_SOME @x  =>  IF_NONE {FAIL} {RENAME @x}
*)

let may_rename annot = Seq (zero_loc, [Prim (zero_loc, "RENAME", [], annot)])

let fail_false_may_rename =
  [
    may_rename ["@annot"];
    Seq
      ( zero_loc,
        [
          Seq
            ( zero_loc,
              [
                Prim (zero_loc, "UNIT", [], []);
                Prim (zero_loc, "FAILWITH", [], []);
              ] );
        ] );
  ]

let fail_true_may_rename =
  [
    Seq
      ( zero_loc,
        [
          Seq
            ( zero_loc,
              [
                Prim (zero_loc, "UNIT", [], []);
                Prim (zero_loc, "FAILWITH", [], []);
              ] );
        ] );
    may_rename ["@annot"];
  ]

(** Expand "ASSERT_SOME @annot"
    into   "IF_NONE { } {UNIT;FAILWITH}"
   using variable annotation "@annot"
*)
let test_assert_some_annot () =
  assert_expands
    (Prim (zero_loc, "ASSERT_SOME", [], ["@annot"]))
    (Seq (zero_loc, [Prim (zero_loc, "IF_NONE", fail_true_may_rename, [])]))

(** Expand "ASSERT_SOME"
    into   "IF_NONE { UNIT;FAILWITH } { }"
*)
let test_assert_some () =
  assert_expands
    (Prim (zero_loc, "ASSERT_SOME", [], []))
    (Seq (zero_loc, [Prim (zero_loc, "IF_NONE", fail_true, [])]))

(** Expand "ASSERT_LEFT @annot"
    into   "IF_LEFT { } {UNIT;FAILWITH}"
   using variable annotation "@annot"
*)
let test_assert_left_annot () =
  assert_expands
    (Prim (zero_loc, "ASSERT_LEFT", [], ["@annot"]))
    (Seq (zero_loc, [Prim (zero_loc, "IF_LEFT", fail_false_may_rename, [])]))

(** Expand "ASSERT_LEFT"
    into   "IF_LEFT { } {UNIT;FAILWITH}"
*)
let test_assert_left () =
  assert_expands
    (Prim (zero_loc, "ASSERT_LEFT", [], []))
    (Seq (zero_loc, [Prim (zero_loc, "IF_LEFT", fail_false, [])]))

(** Expand "ASSERT_RIGHT @annot"
    into   "IF_LEFT {UNIT;FAILWITH} { }"
   using variable annotation "@annot"
*)
let test_assert_right_annot () =
  assert_expands
    (Prim (zero_loc, "ASSERT_RIGHT", [], ["@annot"]))
    (Seq (zero_loc, [Prim (zero_loc, "IF_LEFT", fail_true_may_rename, [])]))

(** Expand "ASSERT_RIGHT"
    into   "IF_LEFT {UNIT;FAILWITH} { }"
*)
let test_assert_right () =
  assert_expands
    (Prim (zero_loc, "ASSERT_RIGHT", [], []))
    (Seq (zero_loc, [Prim (zero_loc, "IF_LEFT", fail_true, [])]))

(** Expand "ASSERT_NONE"
    into   "IF_NONE { } { UNIT;FAILWITH }"
*)
let test_assert_none () =
  assert_expands
    (Prim (zero_loc, "ASSERT_NONE", [], []))
    (Seq (zero_loc, [Prim (zero_loc, "IF_NONE", fail_false, [])]))

(***********************************************************************)
(*Syntactic Conveniences*)

(* diip *)

(** Expand "DIP" into "DIP".
   Expand "DIIIIIIIIP" into "DIP 8".
   Expand "DIIP" into "DIP 2".
*)
let test_diip () =
  let code = Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]) in
  assert_expands
    (Prim (zero_loc, "DIP", [code], []))
    (Prim (zero_loc, "DIP", [code], []))
  >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DIIIIIIIIP", [code], []))
    (Prim (zero_loc, "DIP", [Int (zero_loc, Z.of_int 8); code], []))
  >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DIIP", [code], []))
    (Prim (zero_loc, "DIP", [Int (zero_loc, Z.of_int 2); code], []))

(* pair *)

(** Expand "PAIR"
    into   "PAIR"
*)
let test_pair () =
  assert_expands
    (Prim (zero_loc, "PAIR", [], []))
    (Prim (zero_loc, "PAIR", [], []))

(** Expand "PAPPAIIR"
    into   "DIP {PAIR}; DIP {PAIR}; PAIR"
*)
let test_pappaiir () =
  let pair = Prim (zero_loc, "PAIR", [], []) in
  assert_expands
    (Prim (zero_loc, "PAPPAIIR", [], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DIP", [Seq (zero_loc, [pair])], []);
           Prim (zero_loc, "DIP", [Seq (zero_loc, [pair])], []);
           pair;
         ] ))

(* unpair *)

(** Expand "UNPAIR"
    into   "DUP ; CAR ; DIP {CDR}"
*)
let test_unpair () =
  assert_expands
    (Prim (zero_loc, "UNPAIR", [], []))
    (Prim (zero_loc, "UNPAIR", [], []))

(* duup *)

(** Expand "DUUP"
    into   "DIP {DUP} ; SWAP"
*)
let test_duup () =
  assert_expands
    (Prim (zero_loc, "DUUP", [], []))
    (Prim (zero_loc, "DUP", [Int (zero_loc, Z.of_int 2)], []))

(* car/cdr *)

(** Expand "CAR" into "CAR"
   Expand "CDR" into "CDR"
   Expand "CADR" into "CAR ; CDR"
   Expand "CDAR" into "CDR ; CAR"
*)
let test_caddadr_expansion () =
  let car = Prim (zero_loc, "CAR", [], []) in
  assert_expands (Prim (zero_loc, "CAR", [], [])) car >>? fun () ->
  let cdr = Prim (zero_loc, "CDR", [], []) in
  assert_expands (Prim (zero_loc, "CDR", [], [])) cdr >>? fun () ->
  assert_expands (Prim (zero_loc, "CADR", [], [])) (Seq (zero_loc, [car; cdr]))
  >>? fun () ->
  assert_expands (Prim (zero_loc, "CDAR", [], [])) (Seq (zero_loc, [cdr; car]))

let test_carn_cdrn_expansion () =
  let car n = Prim (zero_loc, "CAR", [Int (zero_loc, Z.of_int n)], []) in
  let cdr n = Prim (zero_loc, "CDR", [Int (zero_loc, Z.of_int n)], []) in
  let get n =
    Seq (zero_loc, [Prim (zero_loc, "GET", [Int (zero_loc, Z.of_int n)], [])])
  in
  assert_expands (cdr 0) (get 0) >>? fun () ->
  assert_expands (car 0) (get 1) >>? fun () ->
  assert_expands (cdr 1) (get 2) >>? fun () ->
  assert_expands (car 1) (get 3) >>? fun () -> assert_expands (cdr 2) (get 4)

(* if_some *)

(** Expand "IF_SOME { 1 } { 2 }"
    into   "IF_NONE { 2 } { 1 }"
*)
let test_if_some () =
  assert_expands
    (Prim (zero_loc, "IF_SOME", [right_branch; left_branch], []))
    (Seq
       (zero_loc, [Prim (zero_loc, "IF_NONE", [left_branch; right_branch], [])]))

(*set_caddadr*)

(** Expand "SET_CAR"
    into   "CDR; SWAP; PAIR"
*)
let test_set_car_expansion () =
  assert_expands
    (Prim (zero_loc, "SET_CAR", [], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "CDR", [], ["@%%"]);
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "PAIR", [], ["%"; "%@"]);
         ] ))

(** Expand "SET_CDR"
    into   "CAR; PAIR"
*)
let test_set_cdr_expansion () =
  assert_expands
    (Prim (zero_loc, "SET_CDR", [], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "CAR", [], ["@%%"]);
           Prim (zero_loc, "PAIR", [], ["%@"; "%"]);
         ] ))

(** Expand "SET_CADR"
    into   "DUP; DIP {CAR; { CAR; PAIR }}; CDR; SWAP; PAIR"
*)
let test_set_cadr_expansion () =
  let set_car =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "CAR", [], ["@%%"]);
          Prim (zero_loc, "PAIR", [], ["%@"; "%"]);
        ] )
  in
  assert_expands
    (Prim (zero_loc, "SET_CADR", [], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "CAR", [], ["@%%"]); set_car])],
               [] );
           Prim (zero_loc, "CDR", [], ["@%%"]);
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
         ] ))

(** Expand "SET_CDAR"
    into   "DUP; DIP {CDR; { CDR; SWAP; PAIR }}; CAR; PAIR"
*)
let test_set_cdar_expansion () =
  let set_cdr =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "CDR", [], ["@%%"]);
          Prim (zero_loc, "SWAP", [], []);
          Prim (zero_loc, "PAIR", [], ["%"; "%@"]);
        ] )
  in
  assert_expands
    (Prim (zero_loc, "SET_CDAR", [], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "CDR", [], ["@%%"]); set_cdr])],
               [] );
           Prim (zero_loc, "CAR", [], ["@%%"]);
           Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
         ] ))

(* TO BE CHANGE IN THE DOCUMENTATION: @MR!791
   FROM:
   > MAP_CAR code  =>  DUP ; CDR ; DIP { CAR ; code } ; SWAP ; PAIR
   TO:
   > MAP_CAR code  =>  DUP ; CDR ; DIP { CAR ; {code} } ; SWAP ; PAIR
*)

(** Expand "MAP_CAR {CAR}"
    into   "DUP; CDR; DIP {CAR; CAR}; SWAP; PAIR"
*)
let test_map_car () =
  (* code is a sequence *)
  let code = Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]) in
  assert_expands
    (Prim (zero_loc, "MAP_CAR", [code], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim (zero_loc, "CDR", [], ["@%%"]);
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "CAR", [], []); code])],
               [] );
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "PAIR", [], ["%"; "%@"]);
         ] ))

(** Expand "MAP_CDR {CAR}"
    into   "DUP; CDR; CAR; SWAP; CAR; PAIR"
*)
let test_map_cdr () =
  let code = Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]) in
  assert_expands
    (Prim (zero_loc, "MAP_CDR", [code], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim (zero_loc, "CDR", [], []);
           code;
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "CAR", [], ["@%%"]);
           Prim (zero_loc, "PAIR", [], ["%@"; "%"]);
         ] ))

(** Expand "MAP_CAADR {CAR}"
    into   "DUP;
           DIP { CAR;
                 DUP;
                 DIP { CAR;
                       DUP;
                       CDR;
                       CAR;
                       SWAP;
                       CAR;
                       PAIR
                     }
                 CDR;
                 SWAP;
                 PAIR
               };
           CDR;
           SWAP;
           PAIR"
*)
let test_map_caadr () =
  let code = Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]) in
  let map_cdr =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "DUP", [], []);
          Prim (zero_loc, "CDR", [], []);
          code;
          Prim (zero_loc, "SWAP", [], []);
          Prim (zero_loc, "CAR", [], ["@%%"]);
          Prim (zero_loc, "PAIR", [], ["%@"; "%"]);
        ] )
  in
  let map_cadr =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "DUP", [], []);
          Prim
            ( zero_loc,
              "DIP",
              [Seq (zero_loc, [Prim (zero_loc, "CAR", [], ["@%%"]); map_cdr])],
              [] );
          Prim (zero_loc, "CDR", [], ["@%%"]);
          Prim (zero_loc, "SWAP", [], []);
          Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
        ] )
  in
  assert_expands
    (Prim (zero_loc, "MAP_CAADR", [code], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "CAR", [], ["@%%"]); map_cadr])],
               [] );
           Prim (zero_loc, "CDR", [], ["@%%"]);
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
         ] ))

(** Expand "MAP_CDADR"
    into   "DUP;
           DIP { CDR;
                 DUP;
                 DIP { CAR;
                       DUP;
                       CDR;
                       CAR;
                       SWAP;
                       CAR;
                       PAIR
                     };
                 CDR;
                 CAR;
                 PAIR
               };
           CAR;
           PAIR"
*)
let test_map_cdadr () =
  let code = Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]) in
  let map_cdr =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "DUP", [], []);
          Prim (zero_loc, "CDR", [], []);
          code;
          Prim (zero_loc, "SWAP", [], []);
          Prim (zero_loc, "CAR", [], ["@%%"]);
          Prim (zero_loc, "PAIR", [], ["%@"; "%"]);
        ] )
  in
  let map_cadr =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "DUP", [], []);
          Prim
            ( zero_loc,
              "DIP",
              [Seq (zero_loc, [Prim (zero_loc, "CAR", [], ["@%%"]); map_cdr])],
              [] );
          Prim (zero_loc, "CDR", [], ["@%%"]);
          Prim (zero_loc, "SWAP", [], []);
          Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
        ] )
  in
  assert_expands
    (Prim (zero_loc, "MAP_CDADR", [code], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "CDR", [], ["@%%"]); map_cadr])],
               [] );
           Prim (zero_loc, "CAR", [], ["@%%"]);
           Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
         ] ))

(****************************************************************************)
(* Unexpand tests *)
(****************************************************************************)

(** Asserts that unexpanding the expression [original] conforms with
    the canonical form of [ex].
    [unparse.Michelson_v1_parser.unexpanded] contains the original
    expression with macros *)
let assert_unexpansion original ex =
  let {Michelson_v1_parser.expanded; _}, errors =
    let source = to_string (Micheline.strip_locations original) in
    Michelson_v1_parser.expand_all ~source ~original
  in
  let unparse = Michelson_v1_printer.unparse_expression expanded in
  match errors with
  | [] ->
      Assert.equal
        ~pp
        unparse.Michelson_v1_parser.unexpanded
        (Micheline.strip_locations ex) ;
      ok ()
  | _ :: _ -> Error errors

(** Unexpanding "UNIT; FAILWITH"
    yields      "FAIL"
*)
let test_unexpand_fail () =
  assert_unexpansion
    (Seq
       ( zero_loc,
         [Prim (zero_loc, "UNIT", [], []); Prim (zero_loc, "FAILWITH", [], [])]
       ))
    (Prim (zero_loc, "FAIL", [], []))

(** Unexpanding "IF_LEFT { 1 } { 2 }"
    yields      "IF_RIGHT { 2 } { 1 }"
*)
let test_unexpand_if_right () =
  assert_unexpansion
    (Seq
       (zero_loc, [Prim (zero_loc, "IF_LEFT", [left_branch; right_branch], [])]))
    (Prim (zero_loc, "IF_RIGHT", [right_branch; left_branch], []))

(** IF_NONE
   Unexpanding "IF_NONE { 1 } { 2 }"
    yields      "IF_SOME { 2 } { 1 }"
*)
let test_unexpand_if_some () =
  assert_unexpansion
    (Seq
       (zero_loc, [Prim (zero_loc, "IF_NONE", [left_branch; right_branch], [])]))
    (Prim (zero_loc, "IF_SOME", [right_branch; left_branch], []))

(** Unexpanding "IF {} { UNIT; FAILWITH }"
    yields      "ASSERT"
*)
let test_unexpand_assert () =
  assert_unexpansion
    (Seq (zero_loc, [Prim (zero_loc, "IF", fail_false, [])]))
    (Prim (zero_loc, "ASSERT", [], []))

let assert_unexpansion_assert_if_compare compare_name prim_name =
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, compare_name, [], []);
           Prim (zero_loc, "IF", fail_false, []);
         ] ))
    (Prim (zero_loc, prim_name, [], []))

(** Unexpanding "{EQ|NEQ|LT|LE|GT|GE} ; IF {} {FAIL}"
    yields      "ASSERT_{EQ|NEQ|LT|LE|GT|GE}"
*)
let test_unexpand_assert_if () =
  assert_unexpansion_assert_if_compare "EQ" "ASSERT_EQ" >>? fun () ->
  assert_unexpansion_assert_if_compare "NEQ" "ASSERT_NEQ" >>? fun () ->
  assert_unexpansion_assert_if_compare "LT" "ASSERT_LT" >>? fun () ->
  assert_unexpansion_assert_if_compare "LE" "ASSERT_LE" >>? fun () ->
  assert_unexpansion_assert_if_compare "GT" "ASSERT_GT" >>? fun () ->
  assert_unexpansion_assert_if_compare "GE" "ASSERT_GE"

let assert_unexpansion_assert_cmp_if_compare compare_name prim_name =
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Seq
             ( zero_loc,
               [
                 Prim (zero_loc, "COMPARE", [], []);
                 Prim (zero_loc, compare_name, [], []);
               ] );
           Prim (zero_loc, "IF", fail_false, []);
         ] ))
    (Prim (zero_loc, prim_name, [], []))

(** Unexpanding "COMPARE; {EQ|NEQ|LT|LE|GT|GE}; IF {} {FAIL}"
    yields      "ASSERT_CMP{EQ|NEQ|LT|LE|GT|GE}"
*)
let test_unexpansion_assert_cmp_if () =
  assert_unexpansion_assert_cmp_if_compare "EQ" "ASSERT_CMPEQ" >>? fun () ->
  assert_unexpansion_assert_cmp_if_compare "NEQ" "ASSERT_CMPNEQ" >>? fun () ->
  assert_unexpansion_assert_cmp_if_compare "LT" "ASSERT_CMPLT" >>? fun () ->
  assert_unexpansion_assert_cmp_if_compare "LE" "ASSERT_CMPLE" >>? fun () ->
  assert_unexpansion_assert_cmp_if_compare "GT" "ASSERT_CMPGT" >>? fun () ->
  assert_unexpansion_assert_cmp_if_compare "GE" "ASSERT_CMPGE"

(** Unexpanding "IF_NONE { FAIL } { RENAME @annot }"
    yields      "ASSERT_SOME @annot"
*)
let test_unexpand_assert_some_annot () =
  assert_unexpansion
    (Seq (zero_loc, [Prim (zero_loc, "IF_NONE", fail_true_may_rename, [])]))
    (Prim (zero_loc, "ASSERT_SOME", [], ["@annot"]))

(** Unexpanding "IF_LEFT { RENAME @annot } { FAIL }"
    yields      "ASSERT_LEFT @annot"
*)
let test_unexpand_assert_left_annot () =
  assert_unexpansion
    (Seq (zero_loc, [Prim (zero_loc, "IF_LEFT", fail_false_may_rename, [])]))
    (Prim (zero_loc, "ASSERT_LEFT", [], ["@annot"]))

(** Unexpanding "IF_LEFT { FAIL } { RENAME @annot }"
    yields      "ASSERT_RIGHT @annot"
*)
let test_unexpand_assert_right_annot () =
  assert_unexpansion
    (Seq (zero_loc, [Prim (zero_loc, "IF_LEFT", fail_true_may_rename, [])]))
    (Prim (zero_loc, "ASSERT_RIGHT", [], ["@annot"]))

(** Unexpanding "IF_NONE {} { FAIL }"
    yields      "ASSERT_NONE"
*)
let test_unexpand_assert_none () =
  assert_unexpansion
    (Seq (zero_loc, [Prim (zero_loc, "IF_NONE", fail_false, [])]))
    (Prim (zero_loc, "ASSERT_NONE", [], []))

(** Unexpanding "IF_NONE { FAIL } {}"
    yields      "ASSERT_SOME"
*)
let test_unexpand_assert_some () =
  assert_unexpansion
    (Seq (zero_loc, [Prim (zero_loc, "IF_NONE", fail_true, [])]))
    (Prim (zero_loc, "ASSERT_SOME", [], []))

(** Unexpanding "IF_LEFT {} { FAIL }"
    yields      "ASSERT_LEFT"
*)
let test_unexpand_assert_left () =
  assert_unexpansion
    (Seq (zero_loc, [Prim (zero_loc, "IF_LEFT", fail_false, [])]))
    (Prim (zero_loc, "ASSERT_LEFT", [], []))

(** Unexpanding "IF_LEFT { FAIL } {}"
    yields      "ASSERT_RIGHT"
*)
let test_unexpand_assert_right () =
  assert_unexpansion
    (Seq (zero_loc, [Prim (zero_loc, "IF_LEFT", fail_true, [])]))
    (Prim (zero_loc, "ASSERT_RIGHT", [], []))

(** Unexpanding "DUP; CAR; DIP { CDR }"
    yields      "UNPAIR"
*)
let test_unexpand_unpair () =
  assert_unexpansion
    (Prim (zero_loc, "UNPAIR", [], []))
    (Prim (zero_loc, "UNPAIR", [], []))

(** Unexpanding "PAIR"
    yields      "PAIR"
*)
let test_unexpand_pair () =
  assert_unexpansion
    (Prim (zero_loc, "PAIR", [], []))
    (Prim (zero_loc, "PAIR", [], []))

(** Unexpanding "DIP { PAIR }; DIP { PAIR }; PAIR"
    yields      "PAPPAIIR"
*)
let test_unexpand_pappaiir () =
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "PAIR", [], [])])],
               [] );
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "PAIR", [], [])])],
               [] );
           Prim (zero_loc, "PAIR", [], []);
         ] ))
    (Prim (zero_loc, "PAPPAIIR", [], []))

(** Unexpanding "DIP { DUP }; SWAP"
    yields      "DUP 2"
*)
let test_unexpand_duup () =
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "DUP", [], [])])],
               [] );
           Prim (zero_loc, "SWAP", [], []);
         ] ))
    (Prim (zero_loc, "DUP", [Int (zero_loc, Z.of_int 2)], []))

(** Unexpanding "CAR" yields "CAR"
   Unexpanding "CDR" yields "CDR"
   Unexpanding "CAR; CDR" yields "CADR"
   Unexpanding "CDR; CAR" yields "CDAR"
*)
let test_unexpand_caddadr () =
  let car = Prim (zero_loc, "CAR", [], []) in
  let cdr = Prim (zero_loc, "CDR", [], []) in
  assert_unexpansion (Seq (zero_loc, [car])) car >>? fun () ->
  assert_unexpansion (Seq (zero_loc, [cdr])) cdr >>? fun () ->
  assert_unexpansion
    (Seq (zero_loc, [car; cdr]))
    (Prim (zero_loc, "CADR", [], []))
  >>? fun () ->
  assert_unexpansion
    (Seq (zero_loc, [cdr; car]))
    (Prim (zero_loc, "CDAR", [], []))

let test_unexpand_carn_cdrn () =
  let car n = Prim (zero_loc, "CAR", [Int (zero_loc, Z.of_int n)], []) in
  let cdr n = Prim (zero_loc, "CDR", [Int (zero_loc, Z.of_int n)], []) in
  let get n =
    Seq (zero_loc, [Prim (zero_loc, "GET", [Int (zero_loc, Z.of_int n)], [])])
  in
  assert_unexpansion (get 0) (cdr 0) >>? fun () ->
  assert_unexpansion (get 1) (car 0) >>? fun () ->
  assert_unexpansion (get 2) (cdr 1) >>? fun () ->
  assert_unexpansion (get 3) (car 1) >>? fun () ->
  assert_unexpansion (get 4) (cdr 2)

(** Unexpanding "CDR; SWAP; PAIR"
    yields      "SET_CAR"
*)
let test_unexpand_set_car () =
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "CDR", [], ["@%%"]);
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "PAIR", [], ["%"; "%@"]);
         ] ))
    (Prim (zero_loc, "SET_CAR", [], []))

(** Unexpanding "CAR; PAIR"
    yields      "SET_CDR"
*)
let test_unexpand_set_cdr () =
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "CAR", [], ["@%%"]);
           Prim (zero_loc, "PAIR", [], ["%@"; "%"]);
         ] ))
    (Prim (zero_loc, "SET_CDR", [], []))

(** Unexpanding "DUP; CAR; DROP; CDR; SWAP; PAIR"
    yields      "SET_CAR"
*)
let test_unexpand_set_car_annot () =
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim (zero_loc, "CAR", [], ["%@"]);
           Prim (zero_loc, "DROP", [], []);
           Prim (zero_loc, "CDR", [], []);
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "PAIR", [], []);
         ] ))
    (Prim (zero_loc, "SET_CAR", [], ["%@"]))

(** Unexpanding "DUP; CDR; DROP; CAR; PAIR"
    yields      "SET_CDR"
*)
let test_unexpand_set_cdr_annot () =
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim (zero_loc, "CDR", [], ["%@"]);
           Prim (zero_loc, "DROP", [], []);
           Prim (zero_loc, "CAR", [], []);
           Prim (zero_loc, "PAIR", [], []);
         ] ))
    (Prim (zero_loc, "SET_CDR", [], ["%@"]))

(** Unexpanding "DUP; DIP { CAR; CAR; PAIR }; CDR; SWAP; PAIR"
    yields      "SET_CADR"
*)
let test_unexpand_set_cadr () =
  let set_car =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "CAR", [], ["@%%"]);
          Prim (zero_loc, "PAIR", [], ["%@"; "%"]);
        ] )
  in
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "CAR", [], ["@%%"]); set_car])],
               [] );
           Prim (zero_loc, "CDR", [], ["@%%"]);
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
         ] ))
    (Prim (zero_loc, "SET_CADR", [], []))

let test_unexpand_set_cdar () =
  let set_cdr =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "CDR", [], ["@%%"]);
          Prim (zero_loc, "SWAP", [], []);
          Prim (zero_loc, "PAIR", [], ["%"; "%@"]);
        ] )
  in
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "CDR", [], ["@%%"]); set_cdr])],
               [] );
           Prim (zero_loc, "CAR", [], ["@%%"]);
           Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
         ] ))
    (Prim (zero_loc, "SET_CDAR", [], []))

(* FIXME: Seq()(Prim): does not parse, raise an error unparse *)
let test_unexpand_map_car () =
  let code = Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]) in
  assert_unexpansion
    (Prim (zero_loc, "MAP_CAR", [code], []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim (zero_loc, "CDR", [], ["@%%"]);
           Prim
             ( zero_loc,
               "DIP",
               [
                 Seq
                   ( zero_loc,
                     [
                       Prim (zero_loc, "CAR", [], []);
                       Prim (zero_loc, "CAR", [], []);
                     ] );
               ],
               [] );
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "PAIR", [], ["%"; "%@"]);
         ] ))

(***********************************************************************)
(*BUG: the test with MAP_CDR or any map with "D" inside fail *)

let _test_unexpand_map_cdr () =
  let code = Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]) in
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim (zero_loc, "CDR", [], []);
           code;
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "CAR", [], []);
           Prim (zero_loc, "PAIR", [], []);
         ] ))
    (Prim (zero_loc, "MAP_CDR", [code], []))

let _test_unexpand_map_caadr () =
  let code = [Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])])] in
  let map_cdr =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "DUP", [], []);
          Prim
            ( zero_loc,
              "DIP",
              [
                Seq
                  ( zero_loc,
                    [
                      Prim (zero_loc, "CAR", [], ["@%%"]);
                      Seq
                        ( zero_loc,
                          [
                            Prim (zero_loc, "DUP", [], []);
                            Prim (zero_loc, "CDR", [], []);
                            Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]);
                            Prim (zero_loc, "SWAP", [], []);
                            Prim (zero_loc, "CAR", [], ["@%%"]);
                            Prim (zero_loc, "PAIR", [], ["%@"; "%"]);
                          ] );
                    ] );
              ],
              [] );
          Prim (zero_loc, "CDR", [], ["@%%"]);
          Prim (zero_loc, "SWAP", [], []);
          Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
        ] )
  in
  assert_unexpansion
    (Prim (zero_loc, "MAP_CAAR", code, []))
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "CAR", [], ["@%%"]); map_cdr])],
               [] );
           Prim (zero_loc, "CDR", [], ["@%%"]);
           Prim (zero_loc, "SWAP", [], []);
           Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
         ] ))

let _test_unexpand_map_cdadr () =
  let code = [Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])])] in
  let map_cdr =
    Seq
      ( zero_loc,
        [
          Prim (zero_loc, "DUP", [], []);
          Prim
            ( zero_loc,
              "DIP",
              [
                Seq
                  ( zero_loc,
                    [
                      Prim (zero_loc, "CAR", [], ["@%%"]);
                      Seq
                        ( zero_loc,
                          [
                            Prim (zero_loc, "DUP", [], []);
                            Prim (zero_loc, "CDR", [], []);
                            Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]);
                            Prim (zero_loc, "SWAP", [], []);
                            Prim (zero_loc, "CAR", [], ["@%%"]);
                            Prim (zero_loc, "PAIR", [], ["%@"; "%"]);
                          ] );
                    ] );
              ],
              [] );
          Prim (zero_loc, "CDR", [], ["@%%"]);
          Prim (zero_loc, "SWAP", [], []);
          Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
        ] )
  in
  assert_unexpansion
    (Seq
       ( zero_loc,
         [
           Prim (zero_loc, "DUP", [], []);
           Prim
             ( zero_loc,
               "DIP",
               [Seq (zero_loc, [Prim (zero_loc, "CDR", [], ["@%%"]); map_cdr])],
               [] );
           Prim (zero_loc, "CAR", [], ["@%%"]);
           Prim (zero_loc, "PAIR", [], ["%@"; "%@"]);
         ] ))
    (Prim (zero_loc, "MAP_CDADR", code, []))

let test_unexpand_diip () =
  let code = Seq (zero_loc, [Prim (zero_loc, "CAR", [], [])]) in
  assert_unexpansion
    (Prim (zero_loc, "DIIP", [code], []))
    (Prim (zero_loc, "DIP", [Int (zero_loc, Z.of_int 2); code], []))

(** Unexpanding "DIP { DIP { DIP { DUP }; SWAP"
    yields      "DIIP { DIP { DUP }; SWAP }"
*)
let test_unexpand_diip_duup1 () =
  let single code = Seq (zero_loc, [code]) in
  let cst str = Prim (zero_loc, str, [], []) in
  let app str code = Prim (zero_loc, str, [code], []) in
  let dip = app "DIP" in
  let diip code =
    Prim (zero_loc, "DIP", [Int (zero_loc, Z.of_int 2); code], [])
  in
  let dup = cst "DUP" in
  let swap = cst "SWAP" in
  let dip_dup_swap = Seq (zero_loc, [dip (single dup); swap]) in
  assert_unexpansion
    (* { DIP { DIP { DIP { DUP }; SWAP }}} *)
    (single (dip (single (dip dip_dup_swap))))
    (* DIIP { DIP { DUP }; SWAP } *)
    (diip dip_dup_swap)

(** Unexpanding "DIP { DIP {{ DIP { DUP }; SWAP"
    yields      "DIIP { DUUP }"
*)
let test_unexpand_diip_duup2 () =
  let single code = Seq (zero_loc, [code]) in
  let cst str = Prim (zero_loc, str, [], []) in
  let app str code = Prim (zero_loc, str, [code], []) in
  let dip = app "DIP" in
  let diip code =
    Prim (zero_loc, "DIP", [Int (zero_loc, Z.of_int 2); code], [])
  in
  let dup = cst "DUP" in
  let duup = Prim (zero_loc, "DUP", [Int (zero_loc, Z.of_int 2)], []) in
  let swap = cst "SWAP" in
  let dip_dup_swap = Seq (zero_loc, [dip (single dup); swap]) in
  assert_unexpansion
    (* { DIP { DIP {{ DIP { DUP }; SWAP }}}} *)
    (single (dip (single (dip (single dip_dup_swap)))))
    (* DIIP { DUUP } *)
    (diip (single duup))

(*****************************************************************************)
(* Test           *)
(*****************************************************************************)

let tests =
  [
    (*compare*)
    ("compare expansion", fun _ -> Lwt.return (test_compare_marco_expansion ()));
    ( "if compare expansion",
      fun _ -> Lwt.return (test_if_compare_macros_expansion ()) );
    ( "if compare expansion: IFCMP",
      fun _ -> Lwt.return (test_if_cmp_macros_expansion ()) );
    (*fail*)
    ("fail expansion", fun _ -> Lwt.return (test_fail_expansion ()));
    (*assertion*)
    ("assert expansion", fun _ -> Lwt.return (test_assert_expansion ()));
    ("assert if expansion", fun _ -> Lwt.return (test_assert_if ()));
    ("assert cmpif expansion", fun _ -> Lwt.return (test_assert_cmp_if ()));
    ("assert none expansion", fun _ -> Lwt.return (test_assert_none ()));
    ("assert some expansion", fun _ -> Lwt.return (test_assert_some ()));
    ("assert left expansion", fun _ -> Lwt.return (test_assert_left ()));
    ("assert right expansion", fun _ -> Lwt.return (test_assert_right ()));
    ( "assert some annot expansion",
      fun _ -> Lwt.return (test_assert_some_annot ()) );
    ( "assert left annot expansion",
      fun _ -> Lwt.return (test_assert_left_annot ()) );
    ( "assert right annot expansion",
      fun _ -> Lwt.return (test_assert_right_annot ()) );
    (*syntactic conveniences*)
    ("diip expansion", fun _ -> Lwt.return (test_diip ()));
    ("duup expansion", fun _ -> Lwt.return (test_duup ()));
    ("pair expansion", fun _ -> Lwt.return (test_pair ()));
    ("pappaiir expansion", fun _ -> Lwt.return (test_pappaiir ()));
    ("unpair expansion", fun _ -> Lwt.return (test_unpair ()));
    ("caddadr expansion", fun _ -> Lwt.return (test_caddadr_expansion ()));
    ( "carn and cdrn expansion",
      fun _ -> Lwt.return (test_carn_cdrn_expansion ()) );
    ("if_some expansion", fun _ -> Lwt.return (test_if_some ()));
    ("set_car expansion", fun _ -> Lwt.return (test_set_car_expansion ()));
    ("set_cdr expansion", fun _ -> Lwt.return (test_set_cdr_expansion ()));
    ("set_cadr expansion", fun _ -> Lwt.return (test_set_cadr_expansion ()));
    ("set_cdar expansion", fun _ -> Lwt.return (test_set_cdar_expansion ()));
    ("map_car expansion", fun _ -> Lwt.return (test_map_car ()));
    ("map_cdr expansion", fun _ -> Lwt.return (test_map_cdr ()));
    ("map_caadr expansion", fun _ -> Lwt.return (test_map_caadr ()));
    ("map_cdadr expansion", fun _ -> Lwt.return (test_map_cdadr ()));
    (*Unexpand*)
    ("fail unexpansion", fun _ -> Lwt.return (test_unexpand_fail ()));
    ("if_right unexpansion", fun _ -> Lwt.return (test_unexpand_if_right ()));
    ("if_some unexpansion", fun _ -> Lwt.return (test_unexpand_if_some ()));
    ("assert unexpansion", fun _ -> Lwt.return (test_unexpand_assert ()));
    ("assert_if unexpansion", fun _ -> Lwt.return (test_unexpand_assert_if ()));
    ( "assert_cmp_if unexpansion",
      fun _ -> Lwt.return (test_unexpansion_assert_cmp_if ()) );
    ( "assert_none unexpansion",
      fun _ -> Lwt.return (test_unexpand_assert_none ()) );
    ( "assert_some unexpansion",
      fun _ -> Lwt.return (test_unexpand_assert_some ()) );
    ( "assert_left unexpansion",
      fun _ -> Lwt.return (test_unexpand_assert_left ()) );
    ( "assert_right unexpansion",
      fun _ -> Lwt.return (test_unexpand_assert_right ()) );
    ( "assert_some annot unexpansion",
      fun _ -> Lwt.return (test_unexpand_assert_some_annot ()) );
    ( "assert_left annot unexpansion",
      fun _ -> Lwt.return (test_unexpand_assert_left_annot ()) );
    ( "assert_right annot unexpansion",
      fun _ -> Lwt.return (test_unexpand_assert_right_annot ()) );
    ("unpair unexpansion", fun _ -> Lwt.return (test_unexpand_unpair ()));
    ("pair unexpansion", fun _ -> Lwt.return (test_unexpand_pair ()));
    ("pappaiir unexpansion", fun _ -> Lwt.return (test_unexpand_pappaiir ()));
    ("duup unexpansion", fun _ -> Lwt.return (test_unexpand_duup ()));
    ("caddadr unexpansion", fun _ -> Lwt.return (test_unexpand_caddadr ()));
    ( "carn and cdrn unexpansion",
      fun _ -> Lwt.return (test_unexpand_carn_cdrn ()) );
    ("set_car unexpansion", fun _ -> Lwt.return (test_unexpand_set_car ()));
    ("set_cdr unexpansion", fun _ -> Lwt.return (test_unexpand_set_cdr ()));
    ("set_cdar unexpansion", fun _ -> Lwt.return (test_unexpand_set_cdar ()));
    ("set_cadr unexpansion", fun _ -> Lwt.return (test_unexpand_set_cadr ()));
    ( "set_car annot unexpansion",
      fun _ -> Lwt.return (test_unexpand_set_car_annot ()) );
    ( "set_cdr annot unexpansion",
      fun _ -> Lwt.return (test_unexpand_set_cdr_annot ()) );
    ("map_car unexpansion", fun _ -> Lwt.return (test_unexpand_map_car ()));
    ("diip unexpansion", fun _ -> Lwt.return (test_unexpand_diip ()));
    ("diip_duup1 unexpansion", fun _ -> Lwt.return (test_unexpand_diip_duup1 ()));
    ("diip_duup2 unexpansion", fun _ -> Lwt.return (test_unexpand_diip_duup2 ()))
    (***********************************************************************)
    (*BUG
      the function in Michelson_v1_macros.unexpand_map_caddadr
      failed to test the case with the character "D".
      It returns an empty {} for the expand *)
    (*"diip unexpansion",  (fun _ -> Lwt.return (test_unexpand_diip ())) ;*)
    (*"map_cdr unexpansion",  (fun _ -> Lwt.return (test_unexpand_map_cdr ())) ;*)
    (*"map_caadr unexpansion",  (fun _ -> Lwt.return (test_unexpand_map_caadr ())) ;*)
    (*"map_cdadr unexpansion",  (fun _ -> Lwt.return (test_unexpand_map_cdadr ())) ;*);
  ]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      f () >>= function
      | Ok () -> Lwt.return_unit
      | Error error ->
          Format.kasprintf Stdlib.failwith "%a" pp_print_trace error)

let () =
  Alcotest_lwt.run
    ~__FILE__
    "tezos-lib-client"
    [(Protocol.name ^ ": micheline v1 macros", List.map wrap tests)]
  |> Lwt_main.run
