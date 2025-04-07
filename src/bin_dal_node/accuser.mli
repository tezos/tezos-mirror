(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** [inject_entrapment_evidences plugin node_ctxt rpc_ctxt block]
    processes and injects trap evidence retrieving traps from a
    specific published level according to the [block]'s level,
    filtering them to identify injectable ones, and then injecting
    entrapment evidence for each injectable trap that the delegate
    actually attested.

    Guarded by [proto_parameters.incentives_enable]. *)
val inject_entrapment_evidences :
  (module Dal_plugin.T with type block_info = 'block) ->
  Node_context.t ->
  Rpc_context.t ->
  'block ->
  unit tzresult Lwt.t
