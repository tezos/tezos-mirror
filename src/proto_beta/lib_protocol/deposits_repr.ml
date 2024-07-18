(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type t = {initial_amount : Tez_repr.t; current_amount : Tez_repr.t}

let encoding =
  let open Data_encoding in
  conv
    (fun {initial_amount; current_amount} -> (initial_amount, current_amount))
    (fun (initial_amount, current_amount) -> {initial_amount; current_amount})
    (obj2
       (req "initial_amount" Tez_repr.encoding)
       (req "actual_amount" Tez_repr.encoding))

let zero = {initial_amount = Tez_repr.zero; current_amount = Tez_repr.zero}

let ( +? ) {initial_amount; current_amount} inc =
  let open Result_syntax in
  let* initial_amount = Tez_repr.(initial_amount +? inc) in
  let+ current_amount = Tez_repr.(current_amount +? inc) in
  {initial_amount; current_amount}

let ( ++? ) {initial_amount = i1; current_amount = c1}
    {initial_amount = i2; current_amount = c2} =
  let open Result_syntax in
  let* initial_amount = Tez_repr.(i1 +? i2) in
  let+ current_amount = Tez_repr.(c1 +? c2) in
  {initial_amount; current_amount}
