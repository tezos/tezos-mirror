(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_micheline
open Protocol
open Alpha_context

type localized_node

type error +=
  | Wrong_stack_item of localized_node
  | Wrong_stack of localized_node
  | Wrong_other_contracts_item of localized_node
  | Wrong_other_contracts of localized_node
  | Wrong_extra_big_maps_item of localized_node
  | Wrong_extra_big_maps of localized_node
  | Invalid_address_for_smart_contract of string

val print_localized_node_location : Format.formatter -> localized_node -> unit

val print_localized_node : Format.formatter -> localized_node -> unit

val parse_stack :
  source:string ->
  Micheline_parser.node ->
  (Script.expr * Script.expr) list tzresult

val parse_other_contracts :
  source:string ->
  Micheline_parser.node ->
  RPC.Scripts.S.other_contract_description list tzresult

val parse_extra_big_maps :
  source:string ->
  Micheline_parser.node ->
  RPC.Scripts.S.extra_big_map_description list tzresult
