(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc

val get_blueprint_service :
  ([`GET], unit, unit * int64, unit, unit, Blueprint_types.t) Service.service

val register : Sequencer_context.t -> unit Directory.t -> unit Directory.t
