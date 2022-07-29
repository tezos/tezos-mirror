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

(** A precomputed set of constants *)
type t

(** [make] precomputes the set of values needed by cryptographic primitives
    defined in this module and store them in a value of type [t] *)
val make :
  redundancy_factor:int ->
  slot_size:int ->
  segment_size:int ->
  number_of_shards:int ->
  t

(** A trusted setup. Namely Structured Reference String.

      Those are data necessary to make the cryptographic primitives
     secured. In particular, to prevent an attacker to forge two
     polynomials with the same commitment. *)
type srs

(** [load_srs ()] loads a trusted [srs]. If the [srs] is already
   loaded, it is given directly. Otherwise, the trusted [srs] is read
   from two dedicated files. The function assumes those files are
   located in some predetermined directories UNIX-compatible. The
   [srs] depends on the [slot_size] parameters. Loading the first time
   an srs is consequently costly while the other times would be cheap.

      We assume the [srs] won't change many times. The shell ensures
   that a bounded and small number of [srs] can be loaded at the same
   time. *)
val load_srs : t -> srs Error_monad.shell_tzresult

(** Commitment to a polynomial. *)
type commitment

module Commitment : sig
  (** An encoding for a commitment. *)
  val encoding : commitment Data_encoding.t

  (** [commitment_to_b58check commitment] returns a b58 representation
        of [commitment]. *)
  val to_b58check : commitment -> string

  (** [commitment_of_b58check_opt bytes] computes a commitment from
        its b58 representation. Returns [None] if it is not a valid
        representation. *)
  val of_b58check_opt : string -> commitment option

  val zero : commitment

  val pp : Format.formatter -> commitment -> unit
end

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

(** The original slot can be split into a list of segments of size
     [segment_size]. A segment is consequently encoded as a pair of an
     [index] and the content of this segment. *)
type segment = {index : int; content : bytes}

(** A proof that the evaluation of points of a polynomial is part of
     a commitment. *)
type segment_proof

(** An encoding for the proof of a segment. *)
val segment_proof_encoding : segment_proof Data_encoding.t

(** [verify_segment t commitment segment segment_proof] returns [Ok
     true] if the [proof] certifies that the [slot_segment] is indeed
     included in the slot committed with commitment
     [comitment]. Returns [Ok false] otherwise.

      Fails if the index of the segment is out of range. *)
val verify_segment :
  t ->
  srs ->
  commitment ->
  segment ->
  segment_proof ->
  ( bool,
    [> `Degree_exceeds_srs_length of string | `Slot_segment_index_out_of_range]
  )
  Result.t
