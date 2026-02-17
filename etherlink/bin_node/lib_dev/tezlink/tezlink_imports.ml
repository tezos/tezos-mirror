(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module SeouLo_protocol = Tezos_protocol_023_PtSeouLo.Protocol
module SeouLo_context = SeouLo_protocol.Alpha_context
module SeouLo_parameter = Tezos_protocol_023_PtSeouLo_parameters
module SeouLo_env = Tezos_protocol_environment_023_PtSeouLo
module SeouLo_plugin = Tezos_protocol_plugin_023_PtSeouLo
module SeouLo_test_helpers = Tezos_023_PtSeouLo_test_helpers

(* Current Tezlink protocol *)

module Imported_protocol = SeouLo_protocol
module Imported_protocol_plugin = SeouLo_plugin
module Imported_protocol_parameters = SeouLo_parameter
module Imported_env = SeouLo_env
module Imported_context = SeouLo_context

(* This is code only intended for testing. So this import is
   to be used sparingly, for temporary code. We import it to implement a
   temporary quick and dirty `list_entrypoints`. *)
module Imported_protocol_test_helpers = SeouLo_test_helpers
