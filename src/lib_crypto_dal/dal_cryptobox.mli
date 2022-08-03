(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Dal_cryptobox_intf

(** Encapsulates parameters required to use the cryptographic primitives exported
    by this module. *)
type t

(** A trusted setup. *)
type srs

(** Because of the shell/protocol separation, cryptographic primitives
   need to be splitted. An interface, called the {!module:Verifier}
   aims to be provided for the economic protocol. The other interface,
   called the {!module:Builder} is for the shell.

    A [Verifier], has hinted by the name, mainly needs to check
   proofs:

    1. A proof that a commitment is valid

    2. A proof that a segment is valid

   A technicality is that the economic protocol is able to configure
   those cryptographic primitives via several constants.  Also, an SRS
   (aka trusted setup) is required.

   It is the responsibility of the shell and the protocol to ensure
   that both the [Verifier] and the [Builder] are instantiated with the
   same parameters and use the same trusted setup. *)

module Verifier : VERIFIER

include VERIFIER with type srs := srs and type t := t

(** FIXME https://gitlab.com/tezos/tezos/-/issues/3390

     This is unsafe but can be used for testing. *)
val srs : t -> srs

module Commitment : sig
  include Dal_cryptobox_intf.COMMITMENT with type t = commitment

  val rpc_arg : commitment Resto.Arg.t
end

module IntMap : Tezos_error_monad.TzLwtreslib.Map.S with type key = int

(** A slot is a byte sequence corresponding to some data. *)
type slot = bytes

(** The finited field used by the polynomial. *)
type scalar

(** A polynomial is another representation for a slot. One advantage
     of this representation is that a commitment can be computed from
     a polynomial. A commitment has nice properties:

      1. A commitment ensures that the size of the [slot] has a
     bounded size (typically [slot_size]).

      2. A commitment can be used to verify that a segment of fixed size
      (typically [segment_size]) is part of the original slot. *)
type polynomial

(** [polynomial_degree polynomial] returns the degree of the
     polynomial. *)
val polynomial_degree : polynomial -> int

(** [polynomial_evaluate polynomial x] evaluates [polynomial(x)]. *)
val polynomial_evaluate : polynomial -> scalar -> scalar

(** [polynomial_from_slot t slot] returns a polynomial from the a slot [slot].

      Fails with [`Slot_wrong_size] when the slot size is different from
      [CONFIGURATION.slot_size]. *)
val polynomial_from_slot :
  t -> bytes -> (polynomial, [> `Slot_wrong_size of string]) Result.t

(** [polynomial_to_slot t polynomial] returns a slot from a [polynomial]. *)
val polynomial_to_bytes : t -> polynomial -> bytes

(** [commit polynomial] returns the commitment associated to a
     polynomial [p].

      Fails with [`Degree_exceeds_srs_length] if the degree of [p]
     exceeds the SRS size. *)
val commit :
  srs ->
  polynomial ->
  (commitment, [> `Degree_exceeds_srs_length of string]) Result.t

(** A portion of the data represented by a polynomial. *)
type share

(** Encoding of a share. *)
val share_encoding : share Data_encoding.t

(** A shard is share with its index (see
     {!val:shards_from_polynomial}). *)
type shard = {index : int; share : share}

(** An encoding of a share. *)
val shard_encoding : shard Data_encoding.t

(** An encoding for a map of shares. *)
val shards_encoding : share IntMap.t Data_encoding.t

(** [polynomial_from_shards t shares] computes the original polynomial
     from [shares]. The proportion of shares needed is [1] over
     [C.redundancy_factor] the total number of shards. It is
     guaranteed that for any share with different indices, if there is
     more than the number of required shards, then the original data
     can be recomputed. *)
val polynomial_from_shards :
  t ->
  share IntMap.t ->
  (polynomial, [> `Invert_zero of string | `Not_enough_shards of string]) result

(** [shards_from_polynomial t polynomial] computes all the shards
     encoding the original [polynomial]. *)
val shards_from_polynomial : t -> polynomial -> share IntMap.t

(** A proof that a shard belongs to some commitment. *)
type shard_proof

(** [verify_shard t srs commitment shard proof] allows to check
     whether [shard] is a portion of the data corresponding to the
     [commitment] using [proof]. The verification time is
     constant. The [srs] should be the same as the one used to produce
     the commitment. *)
val verify_shard :
  t ->
  srs ->
  commitment ->
  shard ->
  shard_proof ->
  (bool, [> `Degree_exceeds_srs_length of string]) result

(** [prove_commitment srs polynomial] produces a proof that the
     slot represented by [polynomial] has its size bounded by the
     maximum slot size. *)
val prove_commitment :
  srs ->
  polynomial ->
  (commitment_proof, [> `Degree_exceeds_srs_length of string]) result

(** [prove_segment] produces a proof that the [n]th segment computed
     is part of a commitment. This segment corresponds to the original
     data and are split into [C.segment_size]. *)
val prove_segment :
  t ->
  srs ->
  polynomial ->
  int ->
  ( segment_proof,
    [> `Degree_exceeds_srs_length of string | `Segment_index_out_of_range] )
  result

(** [prove_shards] computes the proofs for all the [shards] that
     each [shard] is a valid piece of data associated to a polynomial
     and its commitment. Only the commitment is needed to check the
     proof. *)
val prove_shards : t -> srs -> polynomial -> shard_proof array
