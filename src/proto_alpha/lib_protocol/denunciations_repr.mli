(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Internal representation of a pending denunciation, meaning that a
    denunciation operation has been observed in an applied block, but
    the corresponding slashing has not happened yet.

    Note: the public key hash of the culprit doesn't appear in this
    type because it is used as key to store the list of a culprit's
    items (see type [t] below) in the context. *)
type item = {
  operation_hash : Operation_hash.t;
  rewarded : Signature.public_key_hash;
  misbehaviour : Misbehaviour_repr.t;
}

(** List of all pending denunciations about the same culprit. *)
type t = item list

val item_encoding : item Data_encoding.t

val encoding : t Data_encoding.t

(** Append a new pending denunciation to the end of the given list. *)
val add :
  Operation_hash.t -> Signature.public_key_hash -> Misbehaviour_repr.t -> t -> t
