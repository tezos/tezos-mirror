(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

include Z

let initial = zero

let next = succ

let jump tick z = max initial (add tick z)

let pp = pp_print

let encoding = Data_encoding.n

let distance tick1 tick2 = Z.abs (Z.sub tick1 tick2)

let of_int x = if Compare.Int.(x < 0) then None else Some (Z.of_int x)

let to_int x = if Z.fits_int x then Some (Z.to_int x) else None

let of_z x = x

let to_z x = x

let of_number_of_ticks x =
  Z.of_int64 (Sc_rollup_repr.Number_of_ticks.to_value x)

let ( <= ) = leq

let ( < ) = lt

let ( >= ) = geq

let ( > ) = gt

let ( = ) = equal

let ( <> ) x y = not (x = y)

let size_in_bytes tick =
  (* Same definition as in {!Michelson_v1_gas}. *)
  let bits = numbits tick in
  (7 + bits) / 8

module Map = Map.Make (Z)
