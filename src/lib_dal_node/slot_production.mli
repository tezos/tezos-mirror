(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Cryptobox_proof_error of string
  | Post_slot_too_large of {expected : int; got : int}
  | No_prover_profile

(** [produce_commitment_and_proof ctxt padding msg] computes the commitment and
    proof associated to the slot containing [msg] padded with character
    [padding]. *)
val produce_commitment_and_proof :
  Node_context.t ->
  char ->
  string ->
  (Cryptobox.commitment * Cryptobox.commitment_proof, [> Errors.other]) result
  Lwt.t

module Tests : sig
  (** [publish_slot_using_client ctxt rpc_ctxt block_level slot_index secret_key
      msg Plugin] uses the layer 1 node accessible through [rpc_ctxt] to inject a
      dal_publish operation of [msg] on [slot_index] signed with [secret_key]. *)
  val publish_slot_using_client :
    Node_context.t ->
    Tezos_rpc.Context.generic ->
    int32 ->
    int ->
    Signature.secret_key ->
    string ->
    (module Dal_plugin.T) ->
    (unit, [> Errors.other]) result Lwt.t
end
