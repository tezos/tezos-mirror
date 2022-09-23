(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

include Compare.Int64

let zero = 0L

let one = 1L

let of_int64 q = if q < 0L then None else Some q

let of_int64_exn q =
  match of_int64 q with
  | Some q -> q
  | None -> invalid_arg "Tx_rollup_l2_qty.of_int64_exn"

let to_int64 q = q

let to_z = Z.of_int64

let to_string q = Int64.to_string q

let of_string q = Option.bind (Int64.of_string_opt q) of_int64

let pp fmt q = Format.pp_print_string fmt (to_string q)

let compact_encoding = Data_encoding.Compact.(conv to_int64 of_int64_exn int64)

let encoding = Data_encoding.Compact.(make ~tag_size:`Uint8 compact_encoding)

let sub q1 q2 = if q2 <= q1 then Some (Int64.sub q1 q2) else None

let add q1 q2 =
  let q = Int64.add q1 q2 in
  if q < q1 then None else Some q

let succ q = add q one

let ( - ) = sub

let ( + ) = add
