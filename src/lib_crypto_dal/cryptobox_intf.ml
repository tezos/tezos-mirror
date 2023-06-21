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

  (** [to_b58check commitment] returns a b58 representation
        of [commitment]. *)
  val to_b58check : t -> string

  (** [of_b58check_opt bytes] computes a commitment from
        its b58 representation. Returns [None] if it is not a valid
        representation. *)
  val of_b58check_opt : string -> t option

  (** [of_b58check bytes] computes a commitment from its b58
      representation. Returns [Error _] if it is not a valid representation. *)
  val of_b58check : string -> t Error_monad.tzresult

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val zero : t
end

module type COMMITMENT_PROOF = sig
  (** A commitment proof. *)
  type t

  (** An encoding for a commitment proof. This encoding is bounded. *)
  val encoding : t Data_encoding.t

  val zero : t
end

module type VERIFIER = sig
  (** A precomputed set of constants *)
  type t

  (** Parameters to build a value of type [t] *)
  type parameters = Dal_config.parameters = {
    redundancy_factor : int;
    page_size : int;
    slot_size : int;
    number_of_shards : int;
  }

  type ('a, 'b) error_container = {given : 'a; expected : 'b}

  (** An encoding for values of type {!type-parameters}. *)
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

  module Commitment_proof : COMMITMENT_PROOF with type t := commitment_proof

  (** [verify_commitment t commitment proof] returns [true] if and only if the
      size of the data committed via [commitment] does not exceed the
      [slot_size] declared in [t].

      The verification time is constant. *)
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

  (** [verify_page t commitment ~page_index page proof] returns [Ok ()]
      if the [proof] certifies that the [page] is the [page_index]-th page
      of the slot with the given [commitment].

      Fails with:
      - [Error `Invalid_page] if the verification Fails
      - [Error `Invalid_degree_strictly_less_than_expected _] if the SRS
      contained in [t] is too small to proceed with the verification
      - [Error `Page_length_mismatch] if the page is not of the expected
      length [page_size] given for the initialisation of [t]
      - [Error `Page_index_out_of_range] if [page_index] is out of the
      range [0, slot_size/page_size - 1] where [slot_size] and [page_size]
      are given for the initialisation of [t]

      Ensures:
      - [verify_page t commitment ~page_index page proof = Ok ()] if
      and only if
      [page = Bytes.sub slot (page_index * t.page_size) t.page_size]),
      [proof = prove_page t polynomial page_index],
      [p = polynomial_from_slot t slot],
      and [commitment = commit t p]. *)
  val verify_page :
    t ->
    commitment ->
    page_index:int ->
    page ->
    page_proof ->
    ( unit,
      [> `Invalid_degree_strictly_less_than_expected of
         (int, int) error_container
      | `Invalid_page
      | `Page_length_mismatch
      | `Page_index_out_of_range ] )
    Result.t
end
