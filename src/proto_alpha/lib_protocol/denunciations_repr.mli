(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Given that [max_slashing_period = 2] (see {!Constants_repr.check_constants}),
   a misbehaviour can only have happened during the same cycle as the
   denunciation or the preceding one. *)
type misbehaviour_cycle = Current | Previous

type item = {
  operation_hash : Operation_hash.t;
  rewarded : Signature.public_key_hash;
  misbehaviour : Misbehaviour_repr.t;
  misbehaviour_cycle : misbehaviour_cycle;
}

type t = item list

val item_encoding : item Data_encoding.t

val encoding : t Data_encoding.t

val add :
  Operation_hash.t ->
  Signature.public_key_hash ->
  Misbehaviour_repr.t ->
  misbehaviour_cycle ->
  t ->
  t
