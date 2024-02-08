(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Returns the pending denunciations list of the given delegate.
    It returns an empty list if none are registered.
  *)
val find :
  Raw_context.t ->
  Signature.public_key_hash ->
  Denunciations_repr.item list tzresult Lwt.t

(** Add a denunciation in the list of the given delegate  *)
val add_denunciation :
  Raw_context.t ->
  misbehaving_delegate:Signature.public_key_hash ->
  Operation_hash.t ->
  rewarded_delegate:Signature.public_key_hash ->
  Misbehaviour_repr.t ->
  Raw_context.t tzresult Lwt.t
