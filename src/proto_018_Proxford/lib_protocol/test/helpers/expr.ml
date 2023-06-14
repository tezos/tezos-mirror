(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

exception Expression_from_string

(** Parse a Michelson expression from string, raising an exception on error. *)
let from_string ?(check_micheline_indentation = false) str : Script.expr =
  let ast, errs =
    Michelson_v1_parser.parse_expression ~check:check_micheline_indentation str
  in
  (match errs with
  | [] -> ()
  | lst ->
      Format.printf "expr_from_string: %a\n" Error_monad.pp_print_trace lst ;
      raise Expression_from_string) ;
  ast.expanded

(** Parses a Michelson contract from string, raising an exception on error. *)
let toplevel_from_string ?(check_micheline_indentation = false) str =
  let ast, errs =
    Michelson_v1_parser.parse_toplevel ~check:check_micheline_indentation str
  in
  match errs with [] -> ast.expanded | _ -> Stdlib.failwith "parse toplevel"

let to_string c = Format.asprintf "%a" Michelson_v1_printer.print_expr c
