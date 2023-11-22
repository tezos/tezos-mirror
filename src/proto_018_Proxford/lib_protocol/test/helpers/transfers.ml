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

open Protocol
open Alpha_context
open Test_tez

let transfer_and_check_balances ?(with_burn = false) ~loc b ?(fee = Tez.zero)
    ?expect_apply_failure src dst amount =
  let open Lwt_result_syntax in
  let*? amount_fee = fee +? amount in
  let* bal_src = Context.Contract.balance (I b) src in
  let* bal_dst = Context.Contract.balance (I b) dst in
  let* op =
    Op.transaction
      ~force_reveal:true
      ~gas_limit:(Custom_gas (Alpha_context.Gas.Arith.integral_of_int_exn 3000))
      (I b)
      ~fee
      src
      dst
      amount
  in
  let* b = Incremental.add_operation ?expect_apply_failure b op in
  let* {parametric = {origination_size; cost_per_byte; _}; _} =
    Context.get_constants (I b)
  in
  let*? origination_burn = cost_per_byte *? Int64.of_int origination_size in
  let*? amount_fee_burn = amount_fee +? origination_burn in
  let amount_fee_maybe_burn =
    if with_burn then amount_fee_burn else amount_fee
  in
  let* () =
    Assert.balance_was_debited ~loc (I b) src bal_src amount_fee_maybe_burn
  in
  let+ () = Assert.balance_was_credited ~loc (I b) dst bal_dst amount in
  (b, op)

let n_transactions n b ?fee source dest amount =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun b _ ->
      let* i, _ =
        transfer_and_check_balances ~loc:__LOC__ b ?fee source dest amount
      in
      let* b = Incremental.finalize_block i in
      Incremental.begin_construction b)
    b
    (1 -- n)
