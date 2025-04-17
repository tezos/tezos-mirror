(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** [baker_commands ?plugin ()] returns a list of CLI commands available for
    controlling a baker process in the Tezos client context.

    - If [?plugin] is provided, the returned commands are fully functional and use
      the protocol-specific baking implementation defined in the plugin.
    - If [?plugin] is omitted, the function returns only the command specifications,
      which can be used for documentation without actual execution. *)
val baker_commands :
  ?plugin:(module Protocol_plugin_sig.S) ->
  unit ->
  Tezos_client_base.Client_context.full Tezos_clic.command list

(** [accuser_commands ?plugin ()] behaves similarly to [baker_commands], but
    for the accuser process. *)
val accuser_commands :
  ?plugin:(module Protocol_plugin_sig.S) ->
  unit ->
  Tezos_client_base.Client_context.full Tezos_clic.command list
