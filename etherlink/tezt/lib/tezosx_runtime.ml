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

(* V57 moved both paths under /tez/world_state/. Dispatch on the kernel's
   baked-in storage version (rather than its variant) so this stays correct
   when production kernels are rebaked past V57 — Kernel.storage_version is
   the single source of truth, and bumping it there flips this automatically. *)
let michelson_runtime_paths_in_world_state kernel =
  Kernel.storage_version kernel >= 57

let target_sunrise_level kernel = function
  | Tezos ->
      if michelson_runtime_paths_in_world_state kernel then
        "/tez/world_state/michelson_runtime/target_sunrise_level"
      else "/evm/michelson_runtime/target_sunrise_level"

let sunrise_level kernel = function
  | Tezos ->
      if michelson_runtime_paths_in_world_state kernel then
        "/tez/world_state/michelson_runtime/sunrise_level"
      else "/evm/michelson_runtime/sunrise_level"

let tag t = to_string t ^ "_runtime"

let mem r with_runtimes = List.mem r @@ Option.value ~default:[] with_runtimes
