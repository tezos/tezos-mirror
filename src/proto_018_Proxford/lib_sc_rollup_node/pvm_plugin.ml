(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Protocol
open Alpha_context

let get_tick kind state =
  let open Lwt_syntax in
  let module PVM = (val Pvm.of_kind kind) in
  let+ tick = PVM.get_tick state in
  Sc_rollup.Tick.to_z tick

let state_hash kind state =
  let open Lwt_syntax in
  let module PVM = (val Pvm.of_kind kind) in
  let+ hash = PVM.state_hash state in
  Sc_rollup_proto_types.State_hash.to_octez hash

let initial_state kind =
  let module PVM = (val Pvm.of_kind kind) in
  PVM.initial_state ~empty:(PVM.State.empty ())

let parse_boot_sector kind =
  let module PVM = (val Pvm.of_kind kind) in
  PVM.parse_boot_sector

let install_boot_sector kind state boot_sector =
  let module PVM = (val Pvm.of_kind kind) in
  PVM.install_boot_sector state boot_sector

let get_status (node_ctxt : _ Node_context.t) state =
  let open Lwt_syntax in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5871
     Use constants for correct protocol. *)
  let is_reveal_enabled =
    node_ctxt.current_protocol.constants.sc_rollup.reveal_activation_level
    |> WithExceptions.Option.get ~loc:__LOC__
    |> Sc_rollup_proto_types.Constants.reveal_activation_level_of_octez
    |> Protocol.Alpha_context.Sc_rollup.is_reveal_enabled_predicate
  in
  let+ status = PVM.get_status ~is_reveal_enabled state in
  PVM.string_of_status status

let get_current_level kind state =
  let open Lwt_option_syntax in
  let module PVM = (val Pvm.of_kind kind) in
  let+ current_level = PVM.get_current_level state in
  Raw_level.to_int32 current_level

module Fueled = Fueled_pvm

let start_of_level_serialized =
  let open Sc_rollup_inbox_message_repr in
  unsafe_to_string start_of_level_serialized

let end_of_level_serialized =
  let open Sc_rollup_inbox_message_repr in
  unsafe_to_string end_of_level_serialized

let protocol_migration_serialized =
  let open Sc_rollup_inbox_message_repr in
  Some (unsafe_to_string Raw_context.protocol_migration_serialized_message)

let info_per_level_serialized ~predecessor ~predecessor_timestamp =
  let open Sc_rollup_inbox_message_repr in
  unsafe_to_string
    (info_per_level_serialized ~predecessor ~predecessor_timestamp)
