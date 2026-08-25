(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* TALLiN modules *)
module TALLiN_protocol = Tezos_protocol_024_PtTALLiN.Protocol
module TALLiN_context = TALLiN_protocol.Alpha_context
module TALLiN_parameter = Tezos_protocol_024_PtTALLiN_parameters
module TALLiN_env = Tezos_protocol_environment_024_PtTALLiN
module TALLiN_plugin = Tezos_protocol_plugin_024_PtTALLiN
module TALLiN_test_helpers = Tezos_024_PtTALLiN_test_helpers

(* Ushuai modules *)
module Ushuai_protocol = Tezos_protocol_025_PsUshuai.Protocol
module Ushuai_context = Ushuai_protocol.Alpha_context
module Ushuai_parameter = Tezos_protocol_025_PsUshuai_parameters
module Ushuai_env = Tezos_protocol_environment_025_PsUshuai
module Ushuai_plugin = Tezos_protocol_plugin_025_PsUshuai
module Ushuai_test_helpers = Tezos_025_PsUshuai_test_helpers

(* Current Tezlink protocol *)
module Imported_protocol = Ushuai_protocol
module Imported_protocol_plugin = Ushuai_plugin
module Imported_protocol_parameters = Ushuai_parameter
module Imported_env = Ushuai_env
module Imported_context = Ushuai_context

(* This is code only intended for testing. So this import is
   to be used sparingly, for temporary code. We import it to implement a
   temporary quick and dirty `list_entrypoints`. *)
module Imported_protocol_test_helpers = Ushuai_test_helpers
