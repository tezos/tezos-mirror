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
