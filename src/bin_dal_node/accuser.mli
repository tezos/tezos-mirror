(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** [inject_entrapment_evidences plugin attestations node_ctxt rpc_ctxt block]
    processes and injects trap evidence retrieving traps from a
    specific published level according to the [block]'s level,
    filtering them to identify injectable ones.
    It then injects an entrapment evidence for each injectable trap
    that is actually attested in [attestations].

    Guarded by [proto_parameters.incentives_enable]. *)
val inject_entrapment_evidences :
  (module Dal_plugin.T
     with type block_info = 'block
      and type attestation_operation = 'attestation_operation
      and type dal_attestation = 'dal_attestation) ->
  (int
  * Signature.public_key_hash option
  * 'attestation_operation
  * 'dal_attestation option)
  list ->
  Node_context.t ->
  Rpc_context.t ->
  'block ->
  unit tzresult Lwt.t
