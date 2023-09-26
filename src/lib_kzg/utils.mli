(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

(** Module used to handle transcripts, used for applying the Fiat-Shamir heuristic *)
module Transcript : sig
  type t [@@deriving repr]

  val empty : t

  val equal : t -> t -> bool

  (* [of_srs ~len1 ~len2 (srs1, srs2)] builds a transcript from the first
     [len1] elements of [srs1] & the first [len2] elements of [srs2]. This
     function will rise an error if [len1] (resp. [len2]) is greater than the
     size of [srs1] (resp. [srs2]) *)
  val of_srs : len1:int -> len2:int -> Srs.t -> t

  (* expand the transcript with a list of elements *)
  val list_expand : 'a Repr.ty -> 'a list -> t -> t

  (* expand the transcript with a single element *)
  val expand : 'a Repr.ty -> 'a -> t -> t
end

module Fr_generation : sig
  (* computes [| 1; x; x²; x³; ...; xᵈ⁻¹ |] *)
  val powers : int -> Bls.Scalar.t -> Bls.Scalar.t array

  (* [batch x l] adds the elements of l scaled by ascending powers of x *)
  val batch : Bls.Scalar.t -> Bls.Scalar.t list -> Bls.Scalar.t

  (* quadratic non-residues for Sid *)
  val build_quadratic_non_residues : int -> Bls.Scalar.t array

  (* generate several scalars based on seed transcript *)
  val random_fr_list : Transcript.t -> int -> Bls.Scalar.t list * Transcript.t

  (* generate a single scalars based on seed transcript *)
  val random_fr : Transcript.t -> Bls.Scalar.t * Transcript.t
end

module FFT : sig
  (* [select_fft_domain domain_size] selects a suitable domain for the FFT.

     The domain size [domain_size] is expected to be strictly positive.
     Return [(size, power_of_two, remainder)] such that:
     * If [domain_size > 1], then [size] is the smallest integer greater or
     equal to [domain_size] and is of the form 2^a * 3^b * 11^c * 19^d,
     where a ∈ ⟦0, 32⟧, b ∈ {0, 1}, c ∈ {0, 1}, d ∈ {0, 1}.
     * If [domain_size = 1], then [size = 2].
     * [size = power_of_two * remainder], [power_of_two] is a power of two,
     and [remainder] is not divisible by 2.*)
  val select_fft_domain : int -> int * int * int

  val fft : Domain.t -> Bls.Poly.t -> Evaluations.t

  val ifft_inplace : Domain.t -> Evaluations.t -> Bls.Poly.t
end

val diff_next_power_of_two : int -> int

val is_power_of_two : int -> bool

(* Pad array to given size with the last element of the array *)
val pad_array : 'a array -> int -> 'a array

(* Resize array: return the array, subarray or pad it with its last element *)
val resize_array : 'a array -> int -> 'a array
