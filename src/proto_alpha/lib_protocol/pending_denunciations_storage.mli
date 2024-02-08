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
