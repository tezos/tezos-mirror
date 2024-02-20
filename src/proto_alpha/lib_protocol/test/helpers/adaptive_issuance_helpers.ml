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

(** Abstraction of the staking parameters for tests *)
type staking_parameters = {
  limit_of_staking_over_baking : Q.t;
  edge_of_baking_over_staking : Q.t;
}

let default_params =
  let Protocol.Staking_parameters_repr.
        {
          limit_of_staking_over_baking_millionth;
          edge_of_baking_over_staking_billionth;
        } =
    Protocol.Staking_parameters_repr.default
  in
  {
    limit_of_staking_over_baking =
      Q.(Int32.to_int limit_of_staking_over_baking_millionth // 1_000_000);
    edge_of_baking_over_staking =
      Q.(Int32.to_int edge_of_baking_over_staking_billionth // 1_000_000_000);
  }

let get_launch_cycle ~loc blk =
  let open Lwt_result_syntax in
  let* launch_cycle_opt = Context.get_adaptive_issuance_launch_cycle (B blk) in
  Assert.get_some ~loc launch_cycle_opt

(** AI operations *)

let stake ctxt contract amount =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
    ~fee:Test_tez.zero
    contract
    contract
    amount

let set_delegate_parameters ctxt delegate
    ~parameters:{limit_of_staking_over_baking; edge_of_baking_over_staking} =
  let entrypoint = Protocol.Alpha_context.Entrypoint.set_delegate_parameters in
  let limit_of_staking_over_baking_millionth =
    Q.mul limit_of_staking_over_baking (Q.of_int 1_000_000) |> Q.to_int
  in
  let edge_of_baking_over_staking_billionth =
    Q.mul edge_of_baking_over_staking (Q.of_int 1_000_000_000) |> Q.to_int
  in
  let parameters =
    Protocol.Alpha_context.Script.lazy_expr
      (Expr.from_string
         (Printf.sprintf
            "Pair %d (Pair %d Unit)"
            limit_of_staking_over_baking_millionth
            edge_of_baking_over_staking_billionth))
  in
  Op.transaction
    ctxt
    ~entrypoint
    ~parameters
    ~fee:Test_tez.zero
    delegate
    delegate
    Test_tez.zero

let unstake ctxt contract amount =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.unstake
    ~fee:Test_tez.zero
    contract
    contract
    amount

let finalize_unstake ctxt ?(amount = Test_tez.zero) contract =
  Op.transaction
    ctxt
    ~entrypoint:Protocol.Alpha_context.Entrypoint.finalize_unstake
    ~fee:Test_tez.zero
    contract
    contract
    amount

let portion_of_rewards_to_liquid_for_cycle ?policy ctxt cycle pkh rewards =
  let open Lwt_result_syntax in
  let* {frozen; weighted_delegated} =
    Context.Delegate.stake_for_cycle ?policy ctxt cycle pkh
  in
  let portion =
    Test_tez.(ratio weighted_delegated (frozen +! weighted_delegated))
  in
  let to_liquid = Test_tez.mul_q rewards portion in
  return (Test_tez.of_q ~round:`Down to_liquid)
