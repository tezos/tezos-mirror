(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** This module mimics the behavior of the [gas_price] Rust module (in
    [kernel/src/gas_price]). It needs to take into account the storage version
    to be able to be backward compatible. *)

type constants = {speed_limit : Z.t; alpha : float}

let ticks_constants =
  {speed_limit = Z.of_int (40_000_000 * 50); alpha = 0.000_000_000_007}

let gas_constants =
  {speed_limit = Z.of_int (40_000 * 50); alpha = 0.000_000_007}

let tolerance {speed_limit; _} = Z.(mul speed_limit (of_int 10))

let constants_from_storage_version version =
  (* See migrations V32 in [migration.rs] *)
  if version < 32 then ticks_constants else gas_constants

let price_from_backlog ~version ~minimum backlog =
  let constants = constants_from_storage_version version in
  let tolerance = tolerance constants in
  if Compare.Z.(backlog < tolerance) then minimum
  else
    let min = Z.to_float minimum in
    let price =
      min *. Float.exp (constants.alpha *. Z.(to_float (sub backlog tolerance)))
    in
    Z.of_float price
