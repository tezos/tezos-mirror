(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = Shanghai | Cancun | Prague | Osaka

let to_string = function
  | Shanghai -> "shanghai"
  | Cancun -> "cancun"
  | Prague -> "prague"
  | Osaka -> "osaka"

let to_solidity_evm_version = function
  | Shanghai -> "shanghai"
  | Cancun -> "cancun"
  | Prague -> "prague"
  | Osaka ->
      (* TODO: L2-562
         Replace by "osaka" once the solidity compiler
         supports this evm version (pending 0.8.31 release). *)
      "prague"

let number = function Shanghai -> 0 | Cancun -> 1 | Prague -> 2 | Osaka -> 3

let compare a b = number a - number b

let ( < ) a b = compare a b < 0

let ( >= ) a b = compare a b >= 0

let max a b = if a >= b then a else b

let is_pre_cancun a = a < Cancun
