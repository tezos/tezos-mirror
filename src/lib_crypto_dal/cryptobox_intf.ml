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

module type COMMITMENT = sig
  (** Commitment to a polynomial. *)
  type t

  (** An encoding for a commitment. *)
  val encoding : t Data_encoding.t

  (** [commitment_to_b58check commitment] returns a b58 representation
        of [commitment]. *)
  val to_b58check : t -> string

  (** [commitment_of_b58check_opt bytes] computes a commitment from
        its b58 representation. Returns [None] if it is not a valid
        representation. *)
  val of_b58check_opt : string -> t option

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val zero : t
end

module type VERIFIER = sig
  (** A precomputed set of constants *)
  type t

  (** Parameters to build a value of type [t] *)
  type parameters = {
    redundancy_factor : int;
    page_size : int;
    slot_size : int;
    number_of_shards : int;
  }

  (** An encoding for values of type {!parameters}. *)
  val parameters_encoding : parameters Data_encoding.t

  (** [make] precomputes the set of values needed by the cryptographic
    primitives defined in this module and stores them in a value of type [t] *)
  val make : parameters -> (t, [> `Fail of string]) result

  (** [parameters t] returns the parameters given when [t] was
     initialised with the function {!val:make} *)
  val parameters : t -> parameters

  (** Commitment to a polynomial. *)
  type commitment

  module Commitment : COMMITMENT with type t := commitment

  (** A proof that the polynomial associated to some commitment is
     bounded by a constant. *)
  type commitment_proof

  (** An encoding for the proof of a commitment. *)
  val commitment_proof_encoding : commitment_proof Data_encoding.t

  (** [verify_commitment t commitment proof] checks whether
     [commitment] is valid. In particular, it checks
     that the size of the data committed via [commitment] does not
     exceed [t.slot_size]. The verification time is constant.

      Fails if the size of the srs on the group G2 is too small. *)
  val verify_commitment : t -> commitment -> commitment_proof -> bool

  (** The original slot can be split into a list of pages of fixed
     size. This size is given by the parameter [page_size] given to the
     function {!val:make}. *)
  type page = bytes

  (** A proof that the evaluation of points of a polynomial is part of
     a commitment. *)
  type page_proof

  (** An encoding for the proof of a page. *)
  val page_proof_encoding : page_proof Data_encoding.t

  (** [pages_per_slot t] returns the number of expected pages per slot. *)
  val pages_per_slot : parameters -> int

  (** [verify_page t srs commitment page page_proof] returns [Ok true]
     if the [proof] certifies that the [slot_page] is indeed included
     in the slot committed with commitment [commitment]. Returns [Ok
     false] otherwise.

      Fails if the index of the page is out of range or if the page is
     not of the expected length [page_size] given for the
     initialisation of [t]. *)
  val verify_page :
    t ->
    commitment ->
    page_index:int ->
    page ->
    page_proof ->
    (bool, [> `Segment_index_out_of_range | `Page_length_mismatch]) Result.t
end
