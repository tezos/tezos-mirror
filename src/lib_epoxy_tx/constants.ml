(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let two = Z.of_int 2

let accounts_depth = 2

let tickets_depth = 2

let max_nb_accounts = Int.shift_left 1 accounts_depth

let max_nb_tickets = Int.shift_left 1 tickets_depth

let tez_id = Bls12_381.Fr.zero

type position

type balance

type amount

type fee

type counter

type op_code

module Bound : sig
  type 'a t = private Z.t

  val max_nb_leaves : position t

  val max_balance : balance t

  val max_amount : amount t

  val max_fee : fee t

  val max_counter : counter t

  val max_op_code : op_code t

  val v : 'a t -> Z.t
end = struct
  type 'a t = Z.t

  let max_nb_leaves = Z.(shift_left one) (accounts_depth + tickets_depth)

  let max_balance = Z.(shift_left one 20)

  let max_amount = Z.(shift_left one 20)

  let max_fee = Z.(shift_left one 10)

  let max_counter = Z.(shift_left one 20)

  let max_op_code = Z.(shift_left one 2)

  let v x = x
end

(* Fee for creating a new L2 account. This needs to be fixed,
   as the create always goes through L1 and thus the L2 operator
   is forced to include it.
*)
let create_fee = Z.of_int 5
