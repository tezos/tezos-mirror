(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Zero_protocol = Tezos_shell_services.Block_services.Fake_protocol
module Imported_protocol = Tezos_protocol_023_PtSeouLo.Protocol
module Imported_protocol_plugin = Tezos_protocol_plugin_023_PtSeouLo
module Imported_protocol_parameters = Tezos_protocol_023_PtSeouLo_parameters
module Imported_env = Tezos_protocol_environment_023_PtSeouLo
module Alpha_context = Imported_protocol.Alpha_context

(* This is code only intended for testing. So this import is
   to be used sparingly, for temporary code. We import it to implement a
   temporary quick and dirty `list_entrypoints`. *)
module Imported_protocol_test_helpers = Tezos_023_PtSeouLo_test_helpers
