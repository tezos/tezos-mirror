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
open Tezos_micheline

(** The result of parsing and expanding a Michelson V1 script or data. *)
type 'prim parser_result = {
  source : string;  (** The original source code. *)
  unexpanded : string Micheline.canonical;
      (** Original expression with macros. *)
  expanded : 'prim Micheline.canonical;
      (** Expression with macros fully expanded. *)
  expansion_table : (int * (Micheline_parser.location * int list)) list;
      (** Associates unexpanded nodes to their parsing locations and
        the nodes expanded from it in the expanded expression. *)
  unexpansion_table : (int * int) list;
      (** Associates an expanded node to its source in the unexpanded
        expression. *)
}

type parsed = Michelson_v1_primitives.prim parser_result

val compare_parsed : parsed -> parsed -> int

val parse_toplevel :
  ?check:bool -> string -> parsed Micheline_parser.parsing_result

(** Same as [parse_toplevel] but skips the final step (recognizing the
    primitives). *)
val expand_toplevel :
  ?check:bool -> string -> string parser_result Micheline_parser.parsing_result

val parse_expression :
  ?check:bool -> string -> parsed Micheline_parser.parsing_result

(** Same as [parse_expression] but skips the final step (recognizing the
    primitives). *)
val expand_expression :
  ?check:bool -> string -> string parser_result Micheline_parser.parsing_result

val expand_all_and_recognize_prims :
  source:string ->
  original:Micheline_parser.node ->
  parsed Micheline_parser.parsing_result

val unrecognize_prims : parsed -> string parser_result
