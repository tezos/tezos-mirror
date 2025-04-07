(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Tezos_micheline

val print_expr : Format.formatter -> Script_repr.expr -> unit

val print_expr_unwrapped : Format.formatter -> Script_repr.expr -> unit

val unparse_stack :
  'location ->
  (Script_repr.expr * Script_repr.expr) list ->
  ('location, string) Micheline.node

val print_typed_stack :
  Format.formatter -> (Script_repr.expr * Script_repr.expr) list -> unit

val print_execution_trace :
  Format.formatter -> Script_typed_ir.execution_trace -> unit

val print_big_map_diff : Format.formatter -> Lazy_storage.diffs -> unit

(** Insert the type map returned by the typechecker as comments in a
    printable Micheline AST. *)
val inject_types :
  Script_tc_errors.type_map ->
  Michelson_v1_parser.parsed ->
  Micheline_printer.node

(** Unexpand the macros and produce the result of parsing an
    intermediate pretty printed source. Useful when working with
    contracts extracted from the blockchain and not local files. *)
val unparse_toplevel :
  ?type_map:Script_tc_errors.type_map ->
  Script.expr ->
  Michelson_v1_parser.parsed

val unparse_expression : Script.expr -> Michelson_v1_parser.parsed

(** Unexpand the macros and produce the result of parsing an
    intermediate pretty printed source. Works on generic trees,for
    programs that fail to be converted to a specific script version. *)
val unparse_invalid :
  string Micheline.canonical -> string Michelson_v1_parser.parser_result

val ocaml_constructor_of_prim : Michelson_v1_primitives.prim -> string

val micheline_string_of_expression : zero_loc:bool -> Script.expr -> string
