(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = Tezos

let to_string = function Tezos -> "tezos"

let feature_flag = function Tezos -> "/evm/feature_flags/enable_tezos_runtime"

let tag t = to_string t ^ "_runtime"
