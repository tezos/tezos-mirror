(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = int

let of_int_guarded i =
  if Compare.Int.(i >= 0 && i <= 100) then Ok i
  else Error "Value must be between 0 and 100"

let of_int_bounded i = Compare.Int.(max 0 (min 100 i))

let encoding =
  let open Data_encoding in
  conv_with_guard (fun i -> i) of_int_guarded uint8

let of_ratio_bounded Ratio_repr.{numerator; denominator} =
  of_int_bounded (100 * numerator / denominator)

let neg p = 100 - p

let add_bounded p1 p2 = Compare.Int.min 100 (p1 + p2)

let sub_bounded p1 p2 = Compare.Int.max 0 (p1 - p2)

let p0 = 0

let p7 = 7

let p50 = 50
