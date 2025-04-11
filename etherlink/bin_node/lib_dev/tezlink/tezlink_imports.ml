(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Imported_protocol = Tezos_protocol_021_PsQuebec.Protocol
module Imported_protocol_plugin = Tezos_protocol_plugin_021_PsQuebec
module Imported_protocol_parameters = Tezos_protocol_021_PsQuebec_parameters
module Imported_env = Tezos_protocol_environment_021_PsQuebec
module Alpha_context = Imported_protocol.Alpha_context
module Block_services =
  Tezos_shell_services.Block_services.Make
    (Imported_protocol)
    (Imported_protocol)
