(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Subject:      Lexing and parsing functions for Micheline terms.
*)

open Tezos_micheline

(****************************************************************************)
(* Token value   *)
(****************************************************************************)

let pp_tokens fmt tokens =
  let token_value_printer fmt token_value =
    Format.fprintf
      fmt
      "@[%s@]"
      (let open Micheline_parser in
      match token_value with
      | String s -> Format.sprintf "String %S" s
      | Bytes s -> Format.sprintf "Bytes %S" s
      | Int s -> Format.sprintf "Int %S" s
      | Ident s -> Format.sprintf "Ident %S" s
      | Annot s -> Format.sprintf "Annot %S" s
      | Comment s -> Format.sprintf "Comment %S" s
      | Eol_comment s -> Format.sprintf "Eol_comment %S" s
      | Semi -> Format.sprintf "Semi"
      | Open_paren -> Format.sprintf "Open_paren"
      | Close_paren -> Format.sprintf "Close_paren"
      | Open_brace -> Format.sprintf "Open_brace"
      | Close_brace -> Format.sprintf "Close_brace")
  in
  Format.fprintf
    fmt
    "@[<hv>%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@;")
       token_value_printer)
    tokens

let tokenize given =
  match Micheline_parser.tokenize given with
  | tokens, [] ->
      let tokens_got = List.map (fun x -> x.Micheline_parser.token) tokens in
      pp_tokens Format.std_formatter tokens_got
  | _, _ -> Format.printf "Cannot tokenize %s" given

(* Basic tokenizing of strings, bytes, integers, identifiers,
    annotations, comments. *)
let%expect_test "Basic tokenizing" =
  (* String *)
  tokenize "\"abc\"" ;
  [%expect {| String "abc" |}] ;
  tokenize "\"abc\t\"" ;
  [%expect {| String "abc\t" |}] ;
  tokenize "\"abc\b\"" ;
  [%expect {| String "abc\b" |}] ;
  tokenize "\"abc\\n\"" ;
  [%expect {| String "abc\n" |}] ;
  tokenize "\"abc\\r\"" ;
  [%expect {| String "abc\r" |}] ;
  (*fail*)
  tokenize "\"abc\n\"" ;
  [%expect {|
    Cannot tokenize "abc
    " |}] ;
  tokenize "\"abc\\\"" ;
  [%expect {| Cannot tokenize "abc\" |}] ;
  tokenize "\"abc\"" ;
  [%expect {| String "abc" |}] ;
  tokenize "\"abc\r\"" ;
  [%expect {| Cannot tokenize "abc" |}] ;
  tokenize "abc\r" ;
  [%expect {| Cannot tokenize abc |}] ;
  tokenize "\"abc\"\r" ;
  [%expect {| Cannot tokenize "abc" |}] ;
  tokenize "\"abc" ;
  [%expect {| Cannot tokenize "abc |}] ;
  tokenize "abc\"" ;
  [%expect {| Cannot tokenize abc" |}] ;
  tokenize "\"\"\"" ;
  [%expect {| Cannot tokenize """ |}] ;
  (* Bytes *)
  tokenize "0xabc" ;
  [%expect {| Bytes "0xabc" |}] ;
  tokenize "0x" ;
  [%expect {| Bytes "0x" |}] ;
  tokenize "0x1" ;
  [%expect {| Bytes "0x1" |}] ;
  tokenize "xabc" ;
  [%expect {| Ident "xabc" |}] ;
  tokenize "1xabc" ;
  [%expect {| Cannot tokenize 1xabc |}] ;
  tokenize "1c" ;
  [%expect {| Cannot tokenize 1c |}] ;
  tokenize "0c" ;
  [%expect {| Cannot tokenize 0c |}] ;
  tokenize "0xx" ;
  [%expect {| Cannot tokenize 0xx |}] ;
  tokenize "0b" ;
  [%expect {| Cannot tokenize 0b |}] ;
  tokenize "0xg" ;
  [%expect {| Cannot tokenize 0xg |}] ;
  tokenize "0X" ;
  [%expect {| Cannot tokenize 0X |}] ;
  tokenize "1x" ;
  [%expect {| Cannot tokenize 1x |}] ;
  (* Int *)
  tokenize "10" ;
  [%expect {| Int "10" |}] ;
  tokenize "0" ;
  [%expect {| Int "0" |}] ;
  tokenize "00" ;
  [%expect {| Int "00" |}] ;
  tokenize "001" ;
  [%expect {| Int "001" |}] ;
  tokenize "-0" ;
  [%expect {| Int "0" |}] ;
  tokenize "-1" ;
  [%expect {| Int "-1" |}] ;
  tokenize "1" ;
  [%expect {| Int "1" |}] ;
  tokenize "-10" ;
  [%expect {| Int "-10" |}] ;
  tokenize ".1000" ;
  [%expect {| Cannot tokenize .1000 |}] ;
  tokenize "10_00" ;
  [%expect {| Cannot tokenize 10_00 |}] ;
  tokenize "1,000" ;
  [%expect {| Cannot tokenize 1,000 |}] ;
  tokenize "1000.000" ;
  [%expect {| Cannot tokenize 1000.000 |}] ;
  tokenize "-0" ;
  [%expect {| Int "0" |}] ;
  tokenize "--0" ;
  [%expect {| Cannot tokenize --0 |}] ;
  tokenize "+0" ;
  [%expect {| Cannot tokenize +0 |}] ;
  (* Ident *)
  tokenize "a" ;
  [%expect {| Ident "a" |}] ;
  tokenize "0a" ;
  [%expect {| Cannot tokenize 0a |}] ;
  tokenize "_" ;
  [%expect {| Ident "_" |}] ;
  tokenize "string" ;
  [%expect {| Ident "string" |}] ;
  tokenize "_string" ;
  [%expect {| Ident "_string" |}] ;
  tokenize "string_42" ;
  [%expect {| Ident "string_42" |}] ;
  tokenize "_string_42" ;
  [%expect {| Ident "_string_42" |}] ;
  tokenize "STRING" ;
  [%expect {| Ident "STRING" |}] ;
  tokenize "STRING_42" ;
  [%expect {| Ident "STRING_42" |}] ;
  tokenize "_STRING_42" ;
  [%expect {| Ident "_STRING_42" |}] ;
  tokenize "STRING42" ;
  [%expect {| Ident "STRING42" |}] ;
  tokenize "_STRING42" ;
  [%expect {| Ident "_STRING42" |}] ;
  (* Annotation *)
  tokenize "@my_pair" ;
  [%expect {| Annot "@my_pair" |}] ;
  tokenize "@@my_pair" ;
  [%expect {| Annot "@@my_pair" |}] ;
  tokenize "$t" ;
  [%expect {| Annot "$t" |}] ;
  tokenize "&t" ;
  [%expect {| Annot "&t" |}] ;
  tokenize ":t" ;
  [%expect {| Annot ":t" |}] ;
  tokenize ":_" ;
  [%expect {| Annot ":_" |}] ;
  tokenize ":0" ;
  [%expect {| Annot ":0" |}] ;
  tokenize ":%" ;
  [%expect {| Annot ":%" |}] ;
  tokenize ":%%" ;
  [%expect {| Annot ":%%" |}] ;
  tokenize ":%@" ;
  [%expect {| Annot ":%@" |}] ;
  tokenize ":%@_" ;
  [%expect {| Annot ":%@_" |}] ;
  tokenize ":%@_0" ;
  [%expect {| Annot ":%@_0" |}] ;
  tokenize "%from" ;
  [%expect {| Annot "%from" |}] ;
  tokenize "%@from" ;
  [%expect {| Annot "%@from" |}] ;
  tokenize "%from_a" ;
  [%expect {| Annot "%from_a" |}] ;
  tokenize "%from.a" ;
  [%expect {| Annot "%from.a" |}] ;
  tokenize "%From.a" ;
  [%expect {| Annot "%From.a" |}] ;
  tokenize "%0From.a" ;
  [%expect {| Annot "%0From.a" |}] ;
  tokenize "?t" ;
  [%expect {| Annot "?t" |}] ;
  (*fail*)
  tokenize "??t" ;
  [%expect {|
    Annot "?"; Annot "?t" |}] ;
  tokenize "&&t" ;
  [%expect {|
    Annot "&"; Annot "&t" |}] ;
  tokenize "$$t" ;
  [%expect {|
    Annot "$"; Annot "$t" |}] ;
  tokenize ".from" ;
  [%expect {| Cannot tokenize .from |}] ;
  (*NOTE: the cases below fail because ':' is used in the middle of the
    annotation. *)
  tokenize "%:from" ;
  [%expect {|
    Annot "%"; Annot ":from" |}] ;
  tokenize "%:@from" ;
  [%expect {|
    Annot "%"; Annot ":@from" |}] ;
  tokenize "::t" ;
  [%expect {|
    Annot ":"; Annot ":t" |}] ;
  (* Comment *)
  tokenize "/*\"/**/\"*/" ;
  [%expect {| Comment "/*\"/**/\"*/" |}] ;
  tokenize "/* /* /* */ */ */" ;
  [%expect {| Comment "/* /* /* */ */ */" |}] ;
  tokenize "/*parse 1" ;
  [%expect {| Cannot tokenize /*parse 1 |}] ;
  tokenize "parse 1*/" ;
  [%expect {| Cannot tokenize parse 1*/ |}] ;
  tokenize "/* */*/" ;
  [%expect {| Cannot tokenize /* */*/ |}] ;
  tokenize "/*/* */" ;
  [%expect {| Cannot tokenize /*/* */ |}] ;
  (* EOL *)
  tokenize "#Access" ;
  [%expect {| Eol_comment "#Access" |}] ;
  tokenize "##Access" ;
  [%expect {| Eol_comment "##Access" |}] ;
  tokenize "?Access" ;
  [%expect {| Annot "?Access" |}] ;
  (* SKIP *)
  tokenize ";" ;
  [%expect {| Semi |}] ;
  tokenize "{" ;
  [%expect {| Open_brace |}] ;
  tokenize "}" ;
  [%expect {| Close_brace |}] ;
  tokenize "(" ;
  [%expect {| Open_paren |}] ;
  tokenize ")" ;
  [%expect {| Close_paren |}] ;
  (*fail*)
  tokenize "{" ;
  [%expect {| Open_brace |}] ;
  tokenize ";" ;
  [%expect {| Semi |}] ;
  tokenize "}" ;
  [%expect {| Close_brace |}] ;
  tokenize "(" ;
  [%expect {| Open_paren |}] ;
  tokenize ")" ;
  [%expect {| Close_paren |}]

(* Tokenizing simple one-line contracts (not including parsing). *)
let%expect_test "test_one_line_contract" =
  tokenize "(option int)" ;
  [%expect {|
    Open_paren; Ident "option"; Ident "int"; Close_paren |}] ;
  tokenize "DIP {ADD}" ;
  [%expect {|
    Ident "DIP"; Open_brace; Ident "ADD"; Close_brace |}] ;
  tokenize "parameter int;" ;
  [%expect {|
    Ident "parameter"; Ident "int"; Semi |}] ;
  tokenize "PUSH string \"abc\";" ;
  [%expect {|
    Ident "PUSH"; Ident "string"; String "abc"; Semi |}] ;
  tokenize "DROP; SWAP" ;
  [%expect {|
    Ident "DROP"; Semi; Ident "SWAP" |}] ;
  (* NOTE: the cases below do not fail because we only do tokenization. *)
  tokenize "DIP {ADD" ;
  [%expect {|
    Ident "DIP"; Open_brace; Ident "ADD" |}] ;
  tokenize "(option int" ;
  [%expect {|
    Open_paren; Ident "option"; Ident "int" |}] ;
  tokenize "parameter int}" ;
  [%expect {|
    Ident "parameter"; Ident "int"; Close_brace |}] ;
  tokenize "}{}{}{" ;
  [%expect
    {|
    Close_brace; Open_brace; Close_brace; Open_brace; Close_brace; Open_brace |}]

(* Tokenizing conditional contracts (not including parsing). *)
let%expect_test "test_condition_contract" =
  tokenize
    "parameter (or string (option int));storage unit;return string;code \
     {CAR;IF_LEFT{}{IF_NONE {FAIL}{PUSH int 0; CMPGT; IF {FAIL}{PUSH string \
     \"\"}}};UNIT; SWAP; PAIR}" ;
  [%expect
    {|
    Ident "parameter";
    Open_paren;
    Ident "or";
    Ident "string";
    Open_paren;
    Ident "option";
    Ident "int";
    Close_paren;
    Close_paren;
    Semi;
    Ident "storage";
    Ident "unit";
    Semi;
    Ident "return";
    Ident "string";
    Semi;
    Ident "code";
    Open_brace;
    Ident "CAR";
    Semi;
    Ident "IF_LEFT";
    Open_brace;
    Close_brace;
    Open_brace;
    Ident "IF_NONE";
    Open_brace;
    Ident "FAIL";
    Close_brace;
    Open_brace;
    Ident "PUSH";
    Ident "int";
    Int "0";
    Semi;
    Ident "CMPGT";
    Semi;
    Ident "IF";
    Open_brace;
    Ident "FAIL";
    Close_brace;
    Open_brace;
    Ident "PUSH";
    Ident "string";
    String "";
    Close_brace;
    Close_brace;
    Close_brace;
    Semi;
    Ident "UNIT";
    Semi;
    Ident "SWAP";
    Semi;
    Ident "PAIR";
    Close_brace |}] ;
  (* NOTE: the cases below do not fail because we only do tokenization. *)
  tokenize "parameter (or string (option int);" ;
  [%expect
    {|
    Ident "parameter";
    Open_paren;
    Ident "or";
    Ident "string";
    Open_paren;
    Ident "option";
    Ident "int";
    Close_paren;
    Semi |}] ;
  tokenize "parameter (or)" ;
  [%expect {|
    Ident "parameter"; Open_paren; Ident "or"; Close_paren |}] ;
  tokenize "parameter (or" ;
  [%expect {|
    Ident "parameter"; Open_paren; Ident "or" |}]

(****************************************************************************)
(* Top-level parsing tests *)
(****************************************************************************)

let toplevel_parsing source =
  match Micheline_parser.tokenize source with
  | _, _ :: _ -> Printf.printf "Cannot tokenize %s" source
  | tokens, [] -> (
      match Micheline_parser.parse_toplevel tokens with
      | _, _ :: _ -> Printf.printf "Cannot parse_toplevel %s" source
      | ast, [] ->
          let ast =
            List.map
              (fun x ->
                Micheline_printer.printable
                  (fun x -> x)
                  (Micheline.strip_locations x))
              ast
          in
          Format.fprintf
            Format.std_formatter
            "%a"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
               Micheline_printer.print_expr)
            ast)

(* Parse basic terms (not including type-checking). *)
let%expect_test "test_basic_parsing" =
  toplevel_parsing "parameter unit;" ;
  [%expect {| (parameter unit) |}] ;
  (* Sequence *)
  toplevel_parsing "code {}" ;
  [%expect {| (code {}) |}] ;
  (* Int *)
  toplevel_parsing "PUSH int 100" ;
  [%expect {| (PUSH int 100) |}] ;
  (*NOTE: this case doesn't fail because we don't type check *)
  toplevel_parsing "PUSH string 100" ;
  [%expect {| (PUSH string 100) |}] ;
  toplevel_parsing "PUSH int 100_000" ;
  [%expect {| Cannot tokenize PUSH int 100_000 |}] ;
  toplevel_parsing "PUSH int 100" ;
  [%expect {| (PUSH int 100) |}] ;
  toplevel_parsing "PUSH int 100" ;
  [%expect {| (PUSH int 100) |}] ;
  toplevel_parsing "PUSH int \"100\"" ;
  [%expect {| (PUSH int "100") |}] ;
  (* String *)
  toplevel_parsing "Pair False \"abc\"" ;
  [%expect {| (Pair False "abc") |}] ;
  toplevel_parsing "Pair False \"ab\"" ;
  [%expect {| (Pair False "ab") |}] ;
  toplevel_parsing "Pair False abc\"" ;
  [%expect {| Cannot tokenize Pair False abc" |}] ;
  (* annotations *)
  toplevel_parsing "NIL @annot string; #comment\n" ;
  [%expect {| (NIL @annot string) |}] ;
  toplevel_parsing "NIL @annot string; #comment\n" ;
  [%expect {| (NIL @annot string) |}] ;
  toplevel_parsing "IF_NONE {FAIL} {}" ;
  [%expect {| (IF_NONE { FAIL } {}) |}] ;
  toplevel_parsing "PUSH (map int bool) (Map (Item 100 False))" ;
  [%expect {| (PUSH (map int bool) (Map (Item 100 False))) |}] ;
  toplevel_parsing "LAMBDA @name int int {}" ;
  [%expect {| (LAMBDA @name int int {}) |}] ;
  toplevel_parsing "code {DUP @test; DROP}" ;
  [%expect {| (code { DUP @test ; DROP }) |}]

(* Parses a contract with conditional IF. *)
let%expect_test "test_condition_contract_parsing" =
  toplevel_parsing
    "parameter unit;return unit;storage tez; #How much you have to send me \n\
     code {CDR; DUP;AMOUNT; CMPLT;IF {FAIL}}" ;
  [%expect
    {| (parameter unit); (return unit); (storage tez); (code { CDR ; DUP ; AMOUNT ; CMPLT ; IF { FAIL } }) |}]

(* Parses a contract which appends two lists. *)
let%expect_test "test_list_append_parsing" =
  toplevel_parsing
    "parameter (pair (list int)(list int));return (list int);storage unit;code \
     { CAR; DUP; DIP{CDR}; CAR;NIL int; SWAP;LAMBDA (pair int (list int))(list \
     int){DUP; CAR; DIP {CDR}; CONS};REDUCE;LAMBDA (pair int (list int))(list \
     int){DUP; CAR; DIP{CDR}; CONS};UNIT; SWAP; PAIR}" ;
  [%expect
    {|
    (parameter (pair (list int) (list int))); (return (list int)); (storage unit); (code { CAR ;
            DUP ;
            DIP { CDR } ;
            CAR ;
            NIL int ;
            SWAP ;
            LAMBDA (pair int (list int)) (list int) { DUP ; CAR ; DIP { CDR } ; CONS } ;
            REDUCE ;
            LAMBDA (pair int (list int)) (list int) { DUP ; CAR ; DIP { CDR } ; CONS } ;
            UNIT ;
            SWAP ;
            PAIR }) |}]

(****************************************************************************)
(* Expression parsing tests *)
(****************************************************************************)

let expression_parsing source =
  match Micheline_parser.tokenize source with
  | _, _ :: _ -> Printf.printf "Cannot tokenize %s" source
  | tokens, [] -> (
      match Micheline_parser.parse_expression tokens with
      | _, _ :: _ -> Printf.printf "Cannot parse_expression %s" source
      | ast, [] ->
          let ast = Micheline.strip_locations ast in
          let ast = Micheline_printer.printable (fun x -> x) ast in
          Format.fprintf
            Format.std_formatter
            "%a"
            Micheline_printer.print_expr
            ast)

(* Parses Michelson-expressions. *)
let%expect_test "test_parses_expression" =
  (* String *)
  expression_parsing "Pair False \"abc\"" ;
  [%expect {| (Pair False "abc") |}] ;
  (* Int *)
  expression_parsing "Item 100" ;
  [%expect {| (Item 100) |}] ;
  (* Sequence *)
  expression_parsing "{}" ;
  [%expect {| {} |}]
