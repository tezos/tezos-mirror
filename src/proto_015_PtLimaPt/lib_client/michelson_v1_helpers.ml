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

(* Generic Michelson building functions *)

open Tezos_micheline.Micheline
open Protocol.Alpha_context.Script

let seq ~loc l = Seq (loc, l)

let pair ~loc a b = Prim (loc, D_Pair, [a; b], [])

let none ~loc () = Prim (loc, D_None, [], [])

let some ~loc a = Prim (loc, D_Some, [a], [])

let left ~loc a = Prim (loc, D_Left, [a], [])

let right ~loc b = Prim (loc, D_Right, [b], [])

let int ~loc i = Int (loc, i)

let bytes ~loc s = Bytes (loc, s)

let unit_t ~loc = Prim (loc, T_unit, [], [])

let unit ~loc = Prim (loc, D_Unit, [], [])

let lambda_from_string code =
  Tezos_micheline.Micheline_parser.no_parsing_error
  @@ Michelson_v1_parser.parse_expression code
  >|? fun parsed -> root Michelson_v1_parser.(parsed.expanded)

let lambda_t ~loc param res = Prim (loc, T_lambda, [param; res], [])

let operation_t ~loc = Prim (loc, T_operation, [], [])

let operations_t ~loc = Prim (loc, T_list, [operation_t ~loc], [])
