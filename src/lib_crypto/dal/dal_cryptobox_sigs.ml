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

(** Parameters of the DAL relevant to the cryptographic primitives. *)
module type CONSTANTS = sig
  (** Redundancy factor of the erasure code. *)
  val redundancy_factor : int

  (** Size in bytes of a slot, must be a power of two. *)
  val slot_size : int

  (** Size in bytes of a segment, must be a power of two. *)
  val segment_size : int

  (** Each erasure-encoded slot splits evenly into the given amount of shards. *)
  val shards_amount : int
end

module type SRS = sig
  (** A trusted setup. *)
  type srs

  (** FIXME https://gitlab.com/tezos/tezos/-/issues/3390

     This is unsafe but can be used for testing. *)
  val srs :
    redundancy_factor:int ->
    segment_size:int ->
    slot_size:int ->
    shards_amount:int ->
    srs

  (** [load_srs ()] loads a trusted [srs]. If the [srs] is already
     loaded, it is given directly. Otherwise, the trusted [srs] is
     read from some files. The [srs] depends on the [slot_size]
     parameters. Loading the first time an srs is consequently costly
     while the other times would be cheap.

      We assume the [srs] won't change many times. The shell ensures
     that a bounded and small number of [srs] can be loaded at the
     same time. *)
  val load_srs : unit -> srs Error_monad.tzresult
end

module type COMMITMENT = sig
  (** A trusted setup. *)
  type srs

  (** Commitment to a polynomial. *)
  type commitment

  (** An encoding for a commitment. *)
  val commitment_encoding : commitment Data_encoding.t

  (** [commitment_to_bytes commitment] returns a byte representation
     of [commitment]. *)
  val commitment_to_bytes : commitment -> Bytes.t

  (** [commitment_size] is the size in bytes of the commitment. *)
  val commitment_size : int

  (** [commitment_of_bytes_opt bytes] computes a commitment from its
     bytes representation. Returns [None] if [bytes] is not a valid
     representation. *)
  val commitment_of_bytes_opt : Bytes.t -> commitment option

  (** A proof that the polynomial associated to some commitment is
     bounded by a constant. *)
  type commitment_proof

  (** An encoding for the proof of a commitment. *)
  val commitment_proof_encoding : commitment_proof Data_encoding.t

  (** [verify_commitment srs commitment proof] checks whether
     [commitment] is a valid [commitment]. In particular, it check
     that the size of the data committed via [commitment] do not
     exceed [C.slot_size]. The verification time is constant. *)
  val verify_commitment :
    srs ->
    commitment ->
    commitment_proof ->
    (bool, [> `Degree_exceeds_srs_length of string]) Result.t
end

module type POLYNOMIAL = sig
  module IntMap : Tezos_error_monad.TzLwtreslib.Map.S with type key = int

  (** A trusted setup. *)
  type srs

  (** A slot is just some data represented as bytes. *)
  type slot = bytes

  (** The field used by the polynomial. *)
  type scalar

  (** A polynomial is another representation for a slot. One advantage
     of this representation is that a commitment can be computed from
     a polynomial. A commitment has nice properties:

      1. A commitment ensures that the size of the [slot] has a
     bounded size (typically [slot_size]).

      2. A commitment ensures that a segment of fixed size (typically
     [segment_size]) is part of the original slot. *)
  type polynomial

  (** Commitment to a polynomial. *)
  type commitment

  (** [polynomial_degree polynomial] returns the degree of the
     polynomial. *)
  val polynomial_degree : polynomial -> int

  (** [polynomial_evaluate polynomial x] evaluates [polynomial(x)]. *)
  val polynomial_evaluate : polynomial -> scalar -> scalar

  (** [polynomial_from_slot slot] returns a polynomial from the a slot [slot].

      Fail with [`Slot_wrong_size] when the slot size is different from
      [CONFIGURATION.slot_size]. *)
  val polynomial_from_slot :
    bytes -> (polynomial, [> `Slot_wrong_size of string]) Result.t

  (** [polynomial_to_slot polynomial] returns a slot from a [polynomial]. *)
  val polynomial_to_bytes : polynomial -> bytes

  (** [commit polynomial] returns the commitment associated to a
     polynomial [p].

      Fail with [`Degree_exceeds_srs_length] if the degree of [p]
     exceeds the SRS size. *)
  val commit :
    srs ->
    polynomial ->
    (commitment, [> `Degree_exceeds_srs_length of string]) Result.t
end

module type SEGMENT = sig
  (** A trusted setup. *)
  type srs

  (** Commitment to a polynomial. *)
  type commitment

  (** The original slot can be split into a list of segments of size
     [segment_size]. A segment is consequently encoded as a pair of an
     [index] and the content of this segment. *)
  type segment = {index : int; content : bytes}

  (** A proof that the evaluation of points of a polynomial is part of
     a commitment. *)
  type segment_proof

  (** An encoding for the proof of a segment. *)
  val segment_proof_encoding : segment_proof Data_encoding.t

  (** [verify_segment commitment segment segment_proof] returns [Ok
     true] if the [proof] certifies that the [slot_segment] is indeed
     included in the slot committed with commitment
     [comitment]. Returns [Ok false] otherwise.

      Fails if the index of the segment is out of range. *)
  val verify_segment :
    srs ->
    commitment ->
    segment ->
    segment_proof ->
    (bool, [> `Slot_segment_index_out_of_range]) Result.t
end

module type SHARD = sig
  module IntMap : Tezos_error_monad.TzLwtreslib.Map.S with type key = int

  (** A trusted setup. *)
  type srs

  (** A polynomial encoding some data. *)
  type polynomial

  (** Commitment to a polynomial. *)
  type commitment

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

  (** [polynomial_from_shards shares] computes the original polynomial
     from [shares]. The proportion of shares needed is [1] over
     [C.redundancy_factor] the total number of shards. It is
     guaranteed that for any share with different indices, if there is
     more than the number of required shards, then the original data
     can be recomputed. *)
  val polynomial_from_shards :
    share IntMap.t ->
    ( polynomial,
      [> `Invert_zero of string | `Not_enough_shards of string] )
    result

  (** [shards_from_polynomial polynomial] compute all the shards
     encoding the original [polynomial]. *)
  val shards_from_polynomial : polynomial -> share IntMap.t

  (** A proof that a shard belong to some commitment. *)
  type shard_proof

  (** [verify_shard srs commitment shard proof] allows to check
     whether [shard] is a porition of the data corresopding to the
     [commitment] using [proof]. The verification time is
     constant. The [srs] should be the same as the one used to produce
     the commitment. *)
  val verify_shard : srs -> commitment -> shard -> shard_proof -> bool
end

module type PROOF = sig
  (** A trusted setup. *)
  type srs

  (** A polynomial encoding some data. *)
  type polynomial

  (** A segment of the original slot. *)
  type segment

  (** A proof of a segment. *)
  type segment_proof

  (** Commitment to a polynomial. *)
  type commitment

  (** A proof that the polynomial associated to some commitment is
     bounded by a constant. *)
  type commitment_proof

  (** A proof that a shard belong to some commitment. *)
  type shard_proof

  (** [prove_commitment srs polynomial] produces a proof that the
     commitment produced by the function [commit] is indeed a
     commitment of the polynomial. *)
  val prove_commitment :
    srs ->
    polynomial ->
    (commitment_proof, [> `Degree_exceeds_srs_length of string]) result

  (** [prove_segment] produces a proof that the [n]th segment computed
     is part of a commitment. This segment corresponds to the original
     data and are split into [C.segment_size]. *)
  val prove_segment :
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
  val prove_shards : srs -> polynomial -> shard_proof array
end
