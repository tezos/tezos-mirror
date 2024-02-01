(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = int

(* The factor by which to multiply the smallest non-zero representation in
   order to obtain 1%. A factor of 100 means that the precision is 0.01%. *)
let precision_factor = 100

let one_hundred_percent = 100 * precision_factor

(* Remove after P *)
let convert_from_o_to_p x = x * precision_factor

let of_int_guarded i =
  if Compare.Int.(i >= 0 && i <= one_hundred_percent) then Ok i
  else Error "Value must be between 0 and 10000"

let of_int_bounded i = Compare.Int.(max 0 (min one_hundred_percent i))

let encoding =
  let open Data_encoding in
  conv_with_guard (fun i -> i) of_int_guarded uint16

let of_ratio_bounded Ratio_repr.{numerator; denominator} =
  of_int_bounded (one_hundred_percent * numerator / denominator)

let to_q x = Q.of_ints x one_hundred_percent

let neg p = one_hundred_percent - p

let add_bounded p1 p2 = Compare.Int.min one_hundred_percent (p1 + p2)

let sub_bounded p1 p2 = Compare.Int.max 0 (p1 - p2)

let p0 = 0

let p5 = 5 * precision_factor

let p50 = 50 * precision_factor

let p51 = 51 * precision_factor

let p100 = one_hundred_percent

module Compare = struct
  include Compare.Int
end
