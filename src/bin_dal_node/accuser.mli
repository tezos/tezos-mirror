(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** [inject_entrapment_evidences plugin node_ctxt rpc_ctxt block]
    processes entrapment evidence for slot indices monitored by the
    DAL node.

    More notably, it:
    - fetches attestation operations for the attested [block],
    - processes potential entrapments for each monitored slot,
    - injects entrapment evidence when found.
*)
val inject_entrapment_evidences :
  (module Dal_plugin.T with type block_info = 'block) ->
  Node_context.t ->
  Rpc_context.t ->
  'block ->
  unit tzresult Lwt.t
