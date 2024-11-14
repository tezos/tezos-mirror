(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

module Config = Config
module Engine = Engine
module Store = Store
module Module = Module
module Ref = Ref
module Memory = Memory
module Exports = Exports
module Instance = Instance

exception Trap = Trap.Trap

type 'a typ = 'a Value_type.t

let i32 = Value_type.I32

let i64 = Value_type.I64

let f32 = Value_type.F32

let f64 = Value_type.F64

let anyref = Value_type.AnyRef

let funcref = Value_type.FuncRef

type 'a fn = 'a Function_type.t

let ( @-> ) param (Function_type.Function (params, results)) =
  Function_type.(Function (Cons_param (param, params), results))

type 'a ret = 'a Function_type.results

let ret1 x = Function_type.One_result x

let returning1 typ = Function_type.(Function (End_param, ret1 typ))

let ( @** ) lhs rhs = Function_type.Cons_result (lhs, ret1 rhs)

let ( @* ) lhs results = Function_type.Cons_result (lhs, results)

let returning r = Function_type.(Function (End_param, r))

let producer results =
  Function_type.(Function (Trigger_param End_param, results))

let nothing = Function_type.No_result

type extern = Extern.t

let fn typ f = Extern.Function (typ, f)
