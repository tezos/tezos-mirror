(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Lang_core
open Lang_stdlib

module type S = sig
  module L : LIB

  open L

  (** Represents a point on the curve in affine coordinates *)
  type point = scalar * scalar

  (** Returns a Plompiler representation of a point *)
  val input_point : ?kind:input_kind -> S.t * S.t -> point repr t

  (** [is_on_curve p] checks whether a point [p] is on the curve *)
  val is_on_curve : point repr -> bool repr t

  (** [assert_is_on_curve p] asserts that a point [p] is on the curve *)
  val assert_is_on_curve : point repr -> unit repr t

  (** [from_coordinates x y] constructs a point [p = (x, y)] from coordinates
      [x] and [y]. The function also checks whether the point is on the curve
      (but not necessarily in the subgroup) *)
  val from_coordinates : scalar repr -> scalar repr -> point repr t

  (** [unsafe_from_coordinates x y] is similar to {!from_coordinates} but
      does not verify the point is on the curve. It can be used to build a
      variable of type [point] without adding any constraint *)
  val unsafe_from_coordinates : scalar repr -> scalar repr -> point repr t

  (** [get_x_coordinate p] returns a first coordinate [x] of a point [p] *)
  val get_x_coordinate : point repr -> scalar repr

  (** [get_y_coordinate p] returns a second coordinate [y] of a point [p] *)
  val get_y_coordinate : point repr -> scalar repr

  (** [add p q] computes a point addition [p + q] *)
  val add : point repr -> point repr -> point repr t

  (** [double p] computes a point doubling [p + p] *)
  val double : point repr -> point repr t

  (** [scalar_mul s p] computes a point multiplication [p] by a scalar [s].
      The scalar [s] is encoded in little-endian order *)
  val scalar_mul : bool list repr -> point repr -> point repr t

  (** Returns the order of the prime-order subgroup of the elliptic curve group *)
  val scalar_order : Z.t

  (** Returns the prime number defining the underlying field *)
  val base_order : Z.t
end

module type S_Edwards = sig
  include S

  open L

  (** Returns the point at infinity of the curve (additive identity) *)
  val id : S.t * S.t

  (** [cond_add p q b] returns [p + b * q], i.e., either a point addition [p] and [q]
      or a point [p] based on the value [b] *)
  val cond_add : point repr -> point repr -> bool repr -> point repr t

  (** [multi_scalar_mul ls lp] computes the multi-scalar multiplication
      [s₁·p₁ + s₂·p₂ + … + sₖ·pₖ] *)
  val multi_scalar_mul : bool list list repr -> point list repr -> point repr t
end

module type WEIERSTRASS = functor (L : LIB) -> S with module L = L

module type EDWARDS = functor (L : LIB) -> S_Edwards with module L = L
