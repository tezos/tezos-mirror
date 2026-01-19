(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** [inject_entrapment_evidences plugin attestations node_ctxt rpc_ctxt attested_level]
    processes and injects trap evidence retrieving traps from a
    specific published level according to the [attested_level],
    filtering them to identify injectable ones.
    It then injects an entrapment evidence for each injectable trap
    that is actually attested in [attestations].

    Guarded by [proto_parameters.incentives_enable]. *)
val inject_entrapment_evidences :
  (module Dal_plugin.T
     with type attestation_operation = 'attestation_operation
      and type dal_attestations = 'dal_attestations
      and type tb_slot = 'tb_slot) ->
  ('tb_slot * 'attestation_operation * 'dal_attestations option) list ->
  (int * (Signature.public_key_hash * int trace)) trace ->
  Node_context.t ->
  Rpc_context.t ->
  attested_level:int32 ->
  ('tb_slot -> int) ->
  unit tzresult Lwt.t
