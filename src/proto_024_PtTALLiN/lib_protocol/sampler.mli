(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
(** Efficient sampling from given finitely supported (nonzero, positive)
      measures using the alias method. Measures need not be normalized on input,
      but sampling proceeds from the normalized probability measure associated
      to the given measure.
  *)

(** [S] is the module type of a module allowing to construct samplers based
    on the alias method. *)
module type S = sig
  (** [mass] is the type in which finite measures take their values
        (see [Mass] module type). *)
  type mass

  (** ['a t] is the type of auxilliary data for sampling from
          a given distribution. *)
  type 'a t

  (** [create measure] constructs auxilliary data to sample from
          [measure] after normalization. Complexity: O(n).

          It is assumed that the measure is positive. [measure] can contain
          zero mass elements: those are removed in a pre-processing step.
          The total mass of the measure should be strictly positive.

          @raise Invalid_argument if [measure] contains negative mass elements
          or if it contains only zero mass elements. *)
  val create : ('a * mass) list -> 'a t

  (** [sample auxdata rand] creates a sampler from [auxdata] that follows
          the distribution associated to the measure specified when
          creating the [auxdata]. The parameter [rand] is a random sampler
          for the two random values used by the sampling method. The first
          bound is at most the length of the list passed to [create] when
          creating [auxdata]. The second bound is at most the sum of all
          items in the list passed to [create]. *)
  val sample : 'a t -> (int_bound:int -> mass_bound:mass -> int * mass) -> 'a

  (** [encoding e] constructs an encoding for ['a t] given an encoding for ['a]. *)
  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t

  (** [map f t] changes the return values of the sampler without changing the distribution. *)
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(**/**)

module Internal_for_tests : sig
  (** [Mass] is the module type describing the measure associated to points.

      The current signature reflects the need for efficiency for the arithmetic
      operators. As such, they do not error or add dynamic checks for
      over-/under-flow.

      One must make sure that the implementation of its arithmetic operators
      cannot over-/under-flow under the current usage.  *)
  module type SMass = sig
    (** [t] is the type describing the measure associated to points. *)
    type t

    val encoding : t Data_encoding.t

    val zero : t

    val of_int : int -> t

    val mul : t -> t -> t

    val add : t -> t -> t

    val sub : t -> t -> t

    val ( = ) : t -> t -> bool

    val ( <= ) : t -> t -> bool

    val ( < ) : t -> t -> bool
  end

  (** [Make(Mass)] instantiates a module allowing to creates
      samplers for [Mass]-valued finite measures. *)
  module Make : functor (Mass : SMass) -> S with type mass = Mass.t
end

(** Sampler based on int64. In the current state of the protocol, this should
    not ever over-/under-flow -- see the thought process in the .ml file.

   However, should the total stake increase a lot or the number of delegates get
   close to 10k, this might not be true anymore and this module should be
   revisited.  *)
include S with type mass = Int64.t
