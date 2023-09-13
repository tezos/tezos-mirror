(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = Q.t

(* Bonus will represent a fraction of the total supply, so its precision
   should be in the order of magnitude of the total supply. *)
(* Order of magnitude of the total supply in mutez
   Approximately 2^50 *)
let bonus_unit = Q.of_int64 1_000_000_000_000_000L

let zero = Q.zero

let of_int64_repr i = Q.(div (of_int64 i) bonus_unit)

let to_int64_repr q = Q.(mul q bonus_unit |> to_int64)

let encoding =
  Data_encoding.conv to_int64_repr of_int64_repr Data_encoding.int64

let check_bounds (constants : Constants_parametric_repr.adaptive_rewards_params)
    q =
  Q.(q >= zero && q <= of_int64_repr constants.max_bonus)
  [@@inline always]

type error += Out_of_bounds_bonus

let of_Q ~(constants : Constants_parametric_repr.adaptive_rewards_params) q =
  if check_bounds constants q then error Out_of_bounds_bonus else ok q
  [@@inline always]
