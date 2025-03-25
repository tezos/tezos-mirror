(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

val baker_commands :
  ?plugin:(module Protocol_plugin_sig.S) ->
  unit ->
  Tezos_client_base.Client_context.full Tezos_clic.command list
