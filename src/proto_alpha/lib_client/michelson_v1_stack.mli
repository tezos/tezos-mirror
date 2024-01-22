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
  | Duplicated_tzt_top_prim of string * localized_node
  | Wrong_tzt_top_prim_arity of string * localized_node * int
  | Unknown_tzt_top_prim of string * localized_node
  | Missing_mandatory_tzt_top_prim of string

val print_localized_node_location : Format.formatter -> localized_node -> unit

val print_localized_node : Format.formatter -> localized_node -> unit

val parse_stack :
  ?node:(Micheline.canonical_location, string) Micheline.node ->
  string Michelson_v1_parser.parser_result ->
  (Script.expr * Script.expr) list tzresult

val parse_other_contracts :
  ?node:(Micheline.canonical_location, string) Micheline.node ->
  string Michelson_v1_parser.parser_result ->
  RPC.Scripts.S.other_contract_description list tzresult

val parse_extra_big_maps :
  ?node:(Micheline.canonical_location, string) Micheline.node ->
  string Michelson_v1_parser.parser_result ->
  RPC.Scripts.S.extra_big_map_description list tzresult

type unit_test = {
  input : (Script.expr * Script.expr) list;
  code : Script.expr;
  output : (Script.expr * Script.expr) list;
}

val parse_unit_test :
  string Michelson_v1_parser.parser_result -> unit_test tzresult
