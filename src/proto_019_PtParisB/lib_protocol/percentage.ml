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

(* TODO #6918: Remove after P *)
let convert_from_o_to_p x = x * precision_factor

(* TODO #6918: Remove after P *)
let of_int_guarded_legacy_in_o i =
  if Compare.Int.(i >= 0 && i <= 100) then Ok i
  else Error "Value must be between 0 and 100"

let of_int_guarded i =
  if Compare.Int.(i >= 0 && i <= one_hundred_percent) then Ok i
  else
    Error (Format.asprintf "Value must be between 0 and %d" one_hundred_percent)

let of_int_bounded i = Compare.Int.(max 0 (min one_hundred_percent i))

(* TODO #6918: Remove after P *)
let encoding_legacy_in_o =
  let open Data_encoding in
  conv_with_guard (fun i -> i) of_int_guarded_legacy_in_o uint8

let encoding =
  let open Data_encoding in
  conv_with_guard (fun i -> i) of_int_guarded uint16

let of_ratio_bounded Ratio_repr.{numerator; denominator} =
  of_int_bounded (one_hundred_percent * numerator / denominator)

let of_q_bounded ~round (Q.{num; den} as q) =
  if Compare.Q.(q >= Q.one) then one_hundred_percent
  else
    (* Ensures that [to_int] doesn't overflow *)
    let div = match round with `Down -> Z.div | `Up -> Z.cdiv in
    of_int_bounded
      (Z.to_int (div (Z.mul (Z.of_int one_hundred_percent) num) den))

let to_q x = Q.of_ints x one_hundred_percent

let neg p = one_hundred_percent - p

let add_bounded p1 p2 = Compare.Int.min one_hundred_percent (p1 + p2)

let sub_bounded p1 p2 = Compare.Int.max 0 (p1 - p2)

let mul ~round a b = Q.mul (to_q a) (to_q b) |> of_q_bounded ~round

let mul_q_bounded ~round a q = Q.mul (to_q a) q |> of_q_bounded ~round

let p0 = 0

let p5 = 5 * precision_factor

let p50 = 50 * precision_factor

let p51 = 51 * precision_factor

let p100 = one_hundred_percent

module Compare = struct
  include Compare.Int
end
