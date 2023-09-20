(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
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

type natural_with_infinity = Natural of int | Infinity

module type UNIVARIATE = sig
  (** The type of the polynomial coefficients. Can be a field or more generally
      a ring. For the moment, it is restricted to prime fields.
  *)
  type scalar

  (** Represents a polynomial *)
  type t

  (** Returns the polynomial [P(X) = 0] *)
  val zero : t

  (** Returns the polynomial [P(X) = 1] *)
  val one : t

  (** Returns the degree of the polynomial *)
  val degree : t -> natural_with_infinity

  val degree_int : t -> int

  (** [have_same_degree P Q] returns [true] if [P] and [Q] have the same
      degree
  *)
  val have_same_degree : t -> t -> bool

  (* (\** [shift_by_n P n] multiplies [P] by [X^n]. For instance,
   *     [P(X) = a_{0} + a_{1} X + ... + a_{m} X^m] will be transformed in
   *     [a_{0} X^{n} + a_{1} X^{n + 1} + ... a_{m} X^{n + m}].
   * *\)
   * val shift_by_n : t -> int -> t *)

  (** [get_dense_polynomial_coefficients P] returns the list of the
      coefficients of P, including the null coefficients, in decreasing order
      i.e. if [P(X) = a_{0} + a_{1} X + ... + a_{n - 1} X^{n - 1}], the function
      will return [a_{n - 1}, ..., a_{0}]
  *)
  val get_dense_polynomial_coefficients : t -> scalar list

  (** [get_dense_polynomial_coefficients_with_degree P] returns the list of the
      coefficients of P with the degree as a tuple, including the null
      coefficients, in decreasing order
      i.e. if [P(X) = a_{0} + a_{1} X + ... + a_{n - 1} X^{n - 1}], the function
      will return [(a_{n - 1}, n -1), ..., (a_{0}, 0)].
  *)
  val get_dense_polynomial_coefficients_with_degree : t -> (scalar * int) list

  (** [get_list_coefficients P] returns [(a_4,4), (a_2,2), (a_0,0)] if
      P = a_4 X^4 + a_2 X^2 + a_0*)
  val get_list_coefficients : t -> (scalar * int) list

  (** [evaluation P s] computes [P(s)]. Use Horner's method in O(n). *)
  val evaluation : t -> scalar -> scalar

  (** [constants s] returns the constant polynomial [P(X) = s] *)
  val constants : scalar -> t

  (** [add P Q] returns [P(X) + Q(X)] *)
  val add : t -> t -> t

  (** [sub P Q] returns [P(X) - Q(X)] *)
  val sub : t -> t -> t

  (** [mult_by_scalar s P] returns [s*P(X)] *)
  val mult_by_scalar : scalar -> t -> t

  (** [is_null P] returns [true] iff [P(X) = 0] *)
  val is_null : t -> bool

  (** [is_constant P] returns [true] iff [P(X) = s] for s scalar *)
  val is_constant : t -> bool

  (** [opposite P] returns [-P(X)] *)
  val opposite : t -> t

  (** [equal P Q] returns [true] iff [P(X) = Q(X)] on S *)
  val equal : t -> t -> bool

  (** [of_coefficients [(x_0, y_0) ; (x_1, y_1); ... ; (x_n ; y_n)]] builds the
      polynomial Σ(a_i * X^i) as a type [t].

      By default, the null coefficients will be removed as the internal
      representation of polynomials is sparsed. However, a version with null
      coefficients can be generated if required. It is not recommended to use
      this possibility as it breaks an invariant of the type [t].
  *)
  val of_coefficients : (scalar * int) list -> t

  (** [lagrange_interpolation [(x_0, y_0) ; (x_1, y_1); ... ; (x_n ; y_n)]]
      builds the unique polynomial P of degre n such that P(x_i) = y_i for i = 0...n
      using the intermediate lagrange polynomials. [lagrange_interpolation_fft] can
      be used in case of a FFT friendly scalar structure. It is supposed all x_i
      are different.
  *)
  val lagrange_interpolation : (scalar * scalar) list -> t

  (** [even_polynomial P] returns the polynomial P_even containing only the even
      coefficients of P *)
  val even_polynomial : t -> t

  (** [odd_polynomial P] returns the polynomial P_odd containing only the odd
      coefficients of P *)
  val odd_polynomial : t -> t

  (** [evaluate_fft_imperative ~domain P] evaluates P on the points given in the [domain].
      The domain should be of the form [g^{i}] where [g] is a principal root of
      unity. If the domain is of size [n], [g] must be a [n]-th principal root
      of unity.
      The degree of [P] can be smaller than the domain size. Larger polynomials
      can also be used but the implementation is not the most memory efficient
      yet and must be improved. The
      complexity is in [O(n log(m))] where [n] is the domain size and [m] the
      degree of the polynomial. When [m] is smaller than [n], the polynomial is
      padded with zeroes to reach [n] coefficients.
      The resulting list contains the evaluation points
      [P(1), P(w), ..., P(w^{n - 1})].
  *)
  val evaluation_fft : domain:scalar array -> t -> scalar list

  (** [generate_random_polynomial n] returns a random polynomial of degree [n] *)
  val generate_random_polynomial : natural_with_infinity -> t

  (** [get_highest_coefficient P] where [P(X) = a_n X^n + ... a_0] returns [a_n] *)
  val get_highest_coefficient : t -> scalar

  (** [interpolation_fft ~domain [y_{0} ; y_{1} ;
      ... y_{n}]] computes the interpolation at the points [y_{0}, ..., y_{n}]
      using FFT Cookey Tukey.
      The domain should be of the form [g^{i}] where [g] is a principal root of
      unity. If the domain is of size [n], [g] must be a [n]-th principal root
      of unity.
      The domain size must be exactly the same than the number of points. The
      complexity is [O(n log(n))] where [n] is the domain size.
  *)
  val interpolation_fft : domain:scalar array -> scalar list -> t

  (** [polynomial_multiplication P Q] computes the
      product P(X).Q(X) *)
  val polynomial_multiplication : t -> t -> t

  (** [polynomial_multiplication_fft ~domain P Q] computes the
      product [P(X).Q(X)] using FFT.
      The domain should be of the form [g^{i}] where [g] is a principal root of
      unity. If the domain is of size [n], [g] must be a [n]-th principal root
      of unity.
      The degrees of [P] and [Q] can be different. The only condition is
      [degree P + degree Q] should be smaller or equal to [n - 2] (i.e. the domain should
      be big enough to compute [n - 1] points of [P * Q]).
  *)
  val polynomial_multiplication_fft : domain:scalar array -> t -> t -> t

  val euclidian_division_opt : t -> t -> (t * t) option

  (** [extended_euclide P S] returns (GCD, U, V) the greatest common divisor of [P] and [S]
        and the Bezout's coefficient:
      [U P + V S = GCD] and [GCD] greatest coefficient is one
  *)
  val extended_euclide : t -> t -> t * t * t

  (** Infix operator for [equal] *)
  val ( = ) : t -> t -> bool

  (** Infix operator for [add] *)
  val ( + ) : t -> t -> t

  (** Infix operator for [polynomial_multiplication] *)
  val ( * ) : t -> t -> t

  (** Infix operator for [sub] *)
  val ( - ) : t -> t -> t

  val to_string : t -> string
end

(** [generate_evaluation_domain (module Fp) n generator] generates the domain
    [g^{i}] to be used in FFT related algorithms. [generator] must be a [n]-th
    principal root of unity in the finite field [Fp] *)
val generate_evaluation_domain :
  (module Bls12_381.Ff_sig.PRIME with type t = 'a) -> int -> 'a -> 'a array

(** [Make(Fp)] builds a module of type [T] where the coefficients are in the prime field Fp *)
module MakeUnivariate : functor (R : Bls12_381.Ff_sig.PRIME) ->
  UNIVARIATE with type scalar = R.t

module Utils : sig
  val bitreverse : int -> int -> int

  val reorg_coefficients : int -> int -> 'a array -> unit

  val next_power_of_two : int -> int
end
