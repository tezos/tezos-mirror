(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Script_typed_ir

type in_lambda = bool

type callsite =
  | Toplevel : {
      storage_type : ('sto, _) ty;
      param_type : ('param, _) ty;
      entrypoints : 'param Script_typed_ir.entrypoints;
    }
      -> callsite
  | View : callsite
  | Data : callsite

type t = {callsite : callsite; in_lambda : in_lambda}

let init callsite = {callsite; in_lambda = false}

let toplevel ~storage_type ~param_type ~entrypoints =
  init (Toplevel {storage_type; param_type; entrypoints})

let view = init View

(* [data] is prefered over [toplevel] outside [Script_ir_translator], because
   [toplevel] needs to setup a lot of information. *)
let data = init Data

let add_lambda tc_context = {tc_context with in_lambda = true}

let is_in_lambda {callsite = _; in_lambda} = in_lambda

let check_not_in_view loc ~legacy tc_context prim =
  let open Result_syntax in
  match tc_context.callsite with
  (* The forbidden (stateful) instructions in views are in facts allowed in
     lambdas in views, because they could be returned to the caller, and then
     executed on his responsibility. *)
  | Toplevel _ | Data -> return_unit
  | View
    when is_in_lambda tc_context
         || legacy (* Legacy check introduced in Jakarta *) ->
      return_unit
  | View ->
      tzfail Script_tc_errors.(Forbidden_instr_in_context (loc, View, prim))
