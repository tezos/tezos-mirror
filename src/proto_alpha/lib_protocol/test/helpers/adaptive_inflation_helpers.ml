(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

let get_launch_cycle ~loc blk =
  let open Lwt_result_syntax in
  let* launch_cycle_opt = Context.get_adaptive_inflation_launch_cycle (B blk) in
  Assert.get_some ~loc launch_cycle_opt

let stake ctxt contract amount =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
    ~fee:Protocol.Alpha_context.Tez.zero
    contract
    contract
    amount

let set_delegate_parameters ctxt delegate ~staking_over_baking_limit
    ~baking_over_staking_edge_billionth =
  let entrypoint = Protocol.Alpha_context.Entrypoint.set_delegate_parameters in
  let parameters =
    Protocol.Alpha_context.Script.lazy_expr
      (Expr.from_string
         (Printf.sprintf
            "Pair %d (Pair %d Unit)"
            staking_over_baking_limit
            baking_over_staking_edge_billionth))
  in
  Op.transaction
    ctxt
    ~entrypoint
    ~parameters
    ~fee:Protocol.Alpha_context.Tez.zero
    delegate
    delegate
    Protocol.Alpha_context.Tez.zero

let unstake ctxt contract amount =
  let open Protocol.Alpha_context.Tez in
  let parameters =
    Protocol.Alpha_context.Script.lazy_expr
      (Expr.from_string (Printf.sprintf "%Ld" (to_mutez amount)))
  in
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.unstake
    ~parameters
    ~fee:zero
    contract
    contract
    zero

let finalize_unstake ctxt ?(amount = Protocol.Alpha_context.Tez.zero) contract =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.finalize_unstake
    ~fee:Protocol.Alpha_context.Tez.zero
    contract
    contract
    amount
