(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** This module mimics the behavior of the [gas_price] Rust module (in
    [kernel/src/gas_price]). It needs to take into account the storage version
    to be able to be backward compatible. *)

type constants = {target : Z.t; alpha : float}

let ticks_constants =
  {target = Z.of_int (40_000_000 * 50); alpha = 0.000_000_000_007}

let gas_constants version =
  let capacity, alpha =
    if version < 35 (* Calypso or before *) then (4_000_000, 0.000_000_007)
    else if version < 38 then (* Dionysus *) (8_000_000, 0.000_000_004)
    else if version < 44 then (* Ebisu *) (14_000_000, 0.000_000_0019)
    else (27_000_000, 0.000_000_000_99)
  in
  {target = Z.of_int (capacity / 2); alpha}

let tolerance {target; _} = Z.(mul target (of_int 10))

let constants_from_storage_version version =
  (* See migrations V32 in [migration.rs] *)
  if version < 32 then ticks_constants else gas_constants version

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
