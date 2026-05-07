(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = Tezos

let to_string = function Tezos -> "tezos"

let feature_flag kernel = function
  | Tezos -> (
      match kernel with
      | Kernel.Latest | Kernel.Previewnet ->
          "/base/feature_flags/enable_tezos_runtime"
      | Kernel.Mainnet | Kernel.Tezlink_shadownet ->
          "/evm/feature_flags/enable_tezos_runtime")

let target_sunrise_level = function
  | Tezos -> "/evm/michelson_runtime/target_sunrise_level"

let sunrise_level = function Tezos -> "/evm/michelson_runtime/sunrise_level"

let tag t = to_string t ^ "_runtime"

let mem r with_runtimes = List.mem r @@ Option.value ~default:[] with_runtimes
