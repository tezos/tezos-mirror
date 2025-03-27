(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Shanghai | Cancun

let to_string = function Shanghai -> "shanghai" | Cancun -> "cancun"

let number = function Shanghai -> 0 | Cancun -> 1

let compare a b = number a - number b

let ( < ) a b = compare a b < 0

let ( >= ) a b = compare a b >= 0

let max a b = if a >= b then a else b

let is_pre_cancun a = a < Cancun
