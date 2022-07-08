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

  (** Size in bytes of a slot segment, must be a power of two. *)
  val slot_segment_size : int

  (** Each erasure-encoded slot splits evenly into the given amount of shards. *)
  val shards_amount : int
end

module type COMMITMENT = sig
  (** Commitment to a polynomial. *)
  type commitment

  val commitment_encoding : commitment Data_encoding.t

  (** A proof that the polynomial associated to some commitment is
     bounded by a constant. *)
  type commitment_proof

  val commitment_proof_encoding : commitment_proof Data_encoding.t

  val verify_commitment_proof :
    commitment ->
    commitment_proof ->
    (bool, [> `Degree_exceeds_srs_length of string]) Result.t
end

module type POLYNOMIAL = sig
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
     [slot_segment_size]) is part of the original slot. *)
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
    polynomial ->
    (commitment, [> `Degree_exceeds_srs_length of string]) Result.t
end

module type SEGMENT = sig
  (** Commitment to a polynomial. *)
  type commitment

  (** The original slot can be split into a list of segments of size
     [slot_segment_size]. A segment is consequently encoded as a pair
     of an [index] and the content of this segment. *)
  type segment = {index : int; content : bytes}

  (** A proof that the evaluation of points of a polynomial is part of
     a commitment. *)
  type segment_proof

  val segment_proof_encoding : segment_proof Data_encoding.t

  (** [verify_segment commitment segment segment_proof] returns [Ok
     true] if the [proof] certifies that the [slot_segment] is indeed
     included in the slot committed with commitment
     [comitment]. Returns [Ok false] otherwise.

      Fails if the index of the segment is out of range. *)
  val verify_segment :
    commitment ->
    segment ->
    segment_proof ->
    (bool, [> `Slot_segment_index_out_of_range]) Result.t
end

module type SHARD = sig
  type commitment

  type shard

  type shard_proof

  val verify_shard : commitment -> shard -> shard_proof -> bool
end

module type PROOF = sig
  type polynomial

  type segment

  type segment_proof

  type commitment

  type commitment_proof

  type proof_shard

  val prove_commitment :
    polynomial ->
    int ->
    (commitment_proof, [> `Degree_exceeds_srs_length of string]) result

  val prove_slot_segment :
    polynomial ->
    int ->
    ( segment_proof,
      [> `Degree_exceeds_srs_length of string | `Slot_segment_index_out_of_range]
    )
    result

  val prove_shards : polynomial -> proof_shard array
end

(* module Verifier : functor (C : CONSTANTS) -> sig
 *   val check_initialisation : unit -> unit
 * 
 *   include COMMITMENT
 * 
 *   include SEGMENT with type commitment := commitment
 * end
 * 
 * module type Builder (C : CONSTANTS) : sig
 *   val initialisation :
 *     srs_g1_file:string -> srs_g2_file:string -> logarithm_size:int -> unit
 * 
 *   include module type of Verifier (C)
 * 
 *   include POLYNOMIAL with type commitment := commitment
 * end *)

(** The cryptographic primitives for the data availability layer (DAL). *)
module type S = sig
  module Scalar : Ff_sig.PRIME with type t = Bls12_381.Fr.t

  module IntMap : Tezos_error_monad.TzLwtreslib.Map.S with type key = int

  type polynomial

  (** Commitment to a polynomial. *)
  type commitment = Kate_amortized.Kate_amortized.commitment

  (** Proof of evaluations of a shard. *)
  type proof_shard

  (** Proof that a polynomial has degree less than some given bound. *)
  type proof_degree

  (** Proof of evaluation at a single point. *)
  type proof_single

  (** Proof of a slot segment. *)
  type proof_slot_segment

  (** A slot segment is defined by its index and the associated part of the
      slot. *)
  type slot_segment = int * bytes

  (** A share is a part of the encoded data. *)
  type share = Scalar.t array

  (** A shard is defined by its index and the part of the encoded data it
      contains. *)
  type shard = int * share

  (** Collection of shards. *)
  type shards_map = share IntMap.t

  (** Preprocessing to compute shards' proofs that depends on the DAL cryptobox
      parameters. *)
  type shards_proofs_precomputation

  type srs

  (** The path to the files of the SRS on G1 and G2 and the log of their size. *)
  val srs :
    redundancy_factor:int ->
    slot_segment_size:int ->
    shards_amount:int ->
    slot_size:int ->
    srs

  module Encoding : sig
    val commitment_encoding : commitment Data_encoding.t

    val proof_shards_encoding : proof_shard Data_encoding.t

    val proof_degree_encoding : proof_degree Data_encoding.t

    val proof_single_encoding : proof_single Data_encoding.t

    val share_encoding : share Data_encoding.t

    val shard_encoding : shard Data_encoding.t

    val shards_encoding : shards_map Data_encoding.t

    val shards_proofs_precomputation_encoding :
      shards_proofs_precomputation Data_encoding.t
  end

  (** Length of the erasure-encoded slot in terms of scalar elements. *)
  val erasure_encoding_length : int

  val polynomial_degree : polynomial -> int

  (** [polynomial_evaluate p z] evaluates [p] in [z]. *)
  val polynomial_evaluate : polynomial -> Scalar.t -> Scalar.t

  (** [polynomial_from_bytes slot] returns a polynomial from the input [slot].
      Errors with [`Slot_wrong_size] when the slot size is different from
      [CONFIGURATION.slot_size]. *)
  val polynomial_from_bytes :
    bytes -> (polynomial, [> `Slot_wrong_size of string]) Result.t

  (** [polynomial_to_bytes polynomial] returns a slot from a [polynomial]. *)
  val polynomial_to_bytes : polynomial -> bytes

  (** [to_shards polynomial] returns the Reed-Solomon-encoded data in shards. *)
  val to_shards : polynomial -> shards_map

  (** [from_shards shards] returns the Reed-Solomon-decoded polynomial. *)
  val from_shards :
    shards_map ->
    ( polynomial,
      [> `Invert_zero of string | `Not_enough_shards of string] )
    Result.t

  (** [commit p] returns the commitment to [p]. Errors with
      [`Degree_exceeds_srs_length] if the degree of [p] exceeds the SRS size. *)
  val commit :
    srs ->
    polynomial ->
    (commitment, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [prove_degree p n] produces a proof that [p] has degree less
      than [n]. The function fails with [`Degree_exceeds_srs_length] if that is
        not the case. *)
  val prove_degree :
    srs ->
    polynomial ->
    int ->
    (proof_degree, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [verify_degree commitment ts proof n] returns true if and only if the
      committed polynomial has degree less than [n], using trusted setup
      [ts]. *)
  val verify_degree :
    srs ->
    commitment ->
    proof_degree ->
    int ->
    (bool, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [precompute_shards_proofs ts] returns the precomputation used to prove
      shards, using trusted setup [ts]. *)
  val precompute_shards_proofs : srs -> shards_proofs_precomputation

  (** [save_precompute_shards_proofs precomputation filename ()] saves to file
      [filename] the given [precomputation]. *)
  val save_precompute_shards_proofs :
    shards_proofs_precomputation -> string -> unit

  (** [load_precompute_shards_proofs filename] loads to memory the shards'
        proofs precomputation stored in file [filename]. *)
  val load_precompute_shards_proofs : string -> shards_proofs_precomputation

  (** [prove_shards p ~preprocess] creates a proof of evaluation for each
      shard. *)
  val prove_shards :
    polynomial -> preprocess:shards_proofs_precomputation -> proof_shard array

  (** [verify_shard ts cm shard proof] returns true if and only if the
      [proof] certifies that the [shard] is comming from the erasure encoding
      of the committed polynomial whose commitment is [cm]. *)
  val verify_shard : srs -> commitment -> shard -> proof_shard -> bool

  (** [prove_single ts p z] returns a proof of evaluation of [p] at [z], using
      trusted setup [ts]. *)
  val prove_single :
    srs ->
    polynomial ->
    Scalar.t ->
    (proof_single, [> `Degree_exceeds_srs_length of string]) Result.t

  (** [verify_single ts cm ~point ~evaluation pi] returns true if the proof [pi]
    is correct with regard to the opening ([cm], [point], [evaluation]), using
    the trusted setup [ts]. *)
  val verify_single :
    srs ->
    commitment ->
    point:Scalar.t ->
    evaluation:Scalar.t ->
    proof_single ->
    bool

  (** [prove_slot_segments ts p slot_segment_index] where [p] is the output of
      [polynomial_from_bytes slot], returns proofs for the slot segment] whose
      index is [slot_segment_index], using the trusted setup [ts]. *)
  val prove_slot_segment :
    srs ->
    polynomial ->
    int ->
    ( proof_slot_segment,
      [> `Degree_exceeds_srs_length of string | `Slot_segment_index_out_of_range]
    )
    result

  (** [verify_slot_segment cm slot_segment proof] returns true if the [proof]
      certifies that the [slot_segment] is indeed included in the slot committed
      with commitment [cm],  using the trusted setup [ts]. *)
  val verify_slot_segment :
    srs ->
    commitment ->
    slot_segment ->
    proof_slot_segment ->
    (bool, [> `Slot_segment_index_out_of_range]) result
end

(* module Make (C : CONSTANTS) : S *)
