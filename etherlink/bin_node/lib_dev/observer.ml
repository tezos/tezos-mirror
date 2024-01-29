(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

let on_new_blueprint (blueprint : Blueprint_types.t) =
  let open Lwt_result_syntax in
  let (Qty level) = blueprint.number in
  let*! () = Blueprint_event.blueprint_applied level in
  return_unit
