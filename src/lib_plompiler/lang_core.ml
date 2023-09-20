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

module S = Csir.Scalar

(** Plompiler core language.

    The {!COMMON} module type defines the set of primitives needed to interpret
    a Plompiler program.
    This module type provides the monadic interface needed to deal with
    Plompier computations, and a number of sub-modules to handle inputs
    and basic types.
*)

(** Numeric operations over the native field. *)
module type NUM = sig
  (** Element of the native scalar field. *)
  type scalar

  (** Representation of values. *)
  type 'a repr

  (** Plompiler program. *)
  type 'a t

  (** [constant s] returns the constant value [s]. *)
  val constant : S.t -> scalar repr t

  (** [zero] returns the value 0. *)
  val zero : scalar repr t

  (** [one] returns the value 1. *)
  val one : scalar repr t

  (** [range_check ~nb_bits s] asserts that [s] is in the
     range \[0, 2^nb_bits). *)
  val range_check : nb_bits:int -> scalar repr -> unit repr t

  (** [custom ~qc ~ql ~qr ~qo ~qm ~qx2b ~qx5a a b] returns a value [c]
      for which the following arithmetic constraint is added:
      [qc + ql * a + qr * b + qo * c + qm * a * b +
       qx2b * b^2 + qx5a * a^5 = 0]

      Manually adding constraints can be error-prone. Handle with care.
  *)
  val custom :
    ?qc:S.t ->
    ?ql:S.t ->
    ?qr:S.t ->
    ?qo:S.t ->
    ?qm:S.t ->
    ?qx2b:S.t ->
    ?qx5a:S.t ->
    scalar repr ->
    scalar repr ->
    scalar repr t

  (** [assert_custom ~qc ~ql ~qr ~qo ~qm a b c] asserts the
      following arithmetic constraint:
      [qc + ql * a + qr * b + qo * c + qm * a * b +
       qx2b * b^2 + qx5a * a^5 = 0]

      Manually adding constraints can be error-prone. Handle with care.
  *)
  val assert_custom :
    ?qc:S.t ->
    ?ql:S.t ->
    ?qr:S.t ->
    ?qo:S.t ->
    ?qm:S.t ->
    scalar repr ->
    scalar repr ->
    scalar repr ->
    unit repr t

  (** [add ~qc ~ql ~qr a b] returns a value [c] such that
      [ql * a + qr * b + qc = c].
  *)
  val add :
    ?qc:S.t -> ?ql:S.t -> ?qr:S.t -> scalar repr -> scalar repr -> scalar repr t

  (** [add_constant ~ql k a] returns a value [c] such that
      [ql * a + k = c].
  *)
  val add_constant : ?ql:S.t -> S.t -> scalar repr -> scalar repr t

  (** [mul ~qm a b] returns a value [c] such that
      [qm * a * b = c].
  *)
  val mul : ?qm:S.t -> scalar repr -> scalar repr -> scalar repr t

  (** [div ~den_coeff a b] asserts [b] is non-zero and returns a
      value [c] such that [a / (b * den_coeff) = c].
  *)
  val div : ?den_coeff:S.t -> scalar repr -> scalar repr -> scalar repr t

  (** [pow5 a] returns a value [c] such that [a^5 = c]. *)
  val pow5 : scalar repr -> scalar repr t

  (** [is_zero a] returns a boolean [c] representing whether [a] is zero. *)
  val is_zero : scalar repr -> bool repr t

  (** [is_not_zero a] is the opposite of [is_zero a]. *)
  val is_not_zero : scalar repr -> bool repr t

  (** [assert_nonzero a] asserts that [a] is not zero. *)
  val assert_nonzero : scalar repr -> unit repr t

  (** [assert_bool a] asserts that [a] is either zero or one. *)
  val assert_bool : scalar repr -> unit repr t
end

module type BOOL = sig
  (** Element of the native scalar field. *)
  type scalar

  (** Representation of values. *)
  type 'a repr

  (** Plompiler program. *)
  type 'a t

  (** [band a b] returns the conjunction of [a] and [b]. *)
  val band : bool repr -> bool repr -> bool repr t

  (** [xor a b] returns the exclusive disjunction of [a] and [b]. *)
  val xor : bool repr -> bool repr -> bool repr t

  (** [bor a b] returns the disjunction of [a] and [b]. *)
  val bor : bool repr -> bool repr -> bool repr t

  (** [bnot a] returns the negation of [a]. *)
  val bnot : bool repr -> bool repr t

  (** [ifthenelse c t e] returns [t] if [c] is true and [e] otherwise. *)
  val ifthenelse : bool repr -> 'a repr -> 'a repr -> 'a repr t

  (** [swap c a b] returns the pair [(b, a)] if [c] is true and
      [(a, b)] otherwise. *)
  val swap : bool repr -> 'a repr -> 'a repr -> ('a * 'a) repr t

  (** [assert_true a] asserts that [a] is true. *)
  val assert_true : bool repr -> unit repr t

  (** [assert_false a] asserts that [a] is false. *)
  val assert_false : bool repr -> unit repr t

  (** [constant kb] returns the constant [kb] as a Plompiler value. *)
  val constant : bool -> bool repr t

  (** [band_list bs] returns the conjunction of the list of booleans
      [bs]. *)
  val band_list : bool repr list -> bool repr t

  module Internal : sig
    val bor_lookup : bool repr -> bool repr -> bool repr t

    val xor_lookup : bool repr -> bool repr -> bool repr t

    val band_lookup : bool repr -> bool repr -> bool repr t

    val bnot_lookup : bool repr -> bool repr t
  end
end

module type COMMON = sig
  (** Element of the native scalar field. *)
  type scalar

  (** Inputs to a plompiler program have three kinds:
      {ul
        {li Public: known by both the prover and verifier.}
        {li Private: known only by the prover.}
        {li InputCom: part of an Input Commitment.
            See {!Lib_plonk.Input_commitment}.}
      }
  *)
  type input_kind = [`InputCom | `Public | `Private]

  (** The trace is the sequence of scalar values used in a Plompiler
      program. It includes the inputs and the intermediary variables.
      Inputs have to be a prefix of the trace, and public inputs come
      before private ones.
  *)
  type trace_kind = [input_kind | `NoInput]

  (** Representation of values. *)
  type 'a repr

  (** Plompiler program. *)
  type 'a t

  (** Monadic return. *)
  val ret : 'a -> 'a t

  (** Monadic bind. *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  (** Monadic sequence operator. *)
  val ( >* ) : unit repr t -> 'a t -> 'a t

  (** Infix map operator. *)
  val ( <$> ) : 'a t -> ('a -> 'b) -> 'b t

  (* Add a boolean check *)

  (** [with_bool_check c] adds an implicit boolean check computed by [c]
      to the circuit.
      The computation of this check is delayed until the end of the circuit
      construction, which is useful for defining complex conditions while
      still processing inputs.
  *)
  val with_bool_check : bool repr t -> unit repr t

  (** [get_checks_wire] retrieves the boolean representing the conjunction
      of all previous implicit checks.

      WARNING: This will "reset" the implicit check accumulator.
  *)
  val get_checks_wire : bool repr t

  (** Module for describing inputs to Plompiler circuits. *)
  module Input : sig
    (** Input of type ['a] to a Plompiler program.
        These hold a value of type ['a] with some additional structure used
        for constructing the circuit.
        Often, after implementing a Plompiler program, one is left with a
        function of the shape:

        [val prog : 'a input -> 'b input -> unit repr t].

        If the user only wants to execute the program in order to compute
        the corresponding circuit, but without producing a proof, then
        the [input]s can be set to dummy values, as they will be ignored.
        That is, only the structure of the [input] is used for building
        the circuit.
    *)
    type 'a input

    (** [scalar s] describes a scalar input holding the value [s]. *)
    val scalar : S.t -> scalar input

    (** [to_scalar i] retrieves the value from a scalar input. *)
    val to_scalar : scalar input -> S.t

    (** [bool b] describes a boolean input holding the value [b]. *)
    val bool : bool -> bool input

    (** [to_bool i] retrieves the value from a boolean input. *)
    val to_bool : bool input -> bool

    (** [unit] describes a unit input. *)
    val unit : unit input

    (** [pair a b] describes the input tuple [(a, b)] made out
        of inputs [a] and [b]. *)
    val pair : 'a input -> 'b input -> ('a * 'b) input

    (** [to_pair p] retrieves the inputs [(a, b)] that make
        up the input tuple [p]. *)
    val to_pair : ('a * 'b) input -> 'a input * 'b input

    (** [list l] turns a list of inputs [l] into an list input. *)
    val list : 'a input list -> 'a list input

    (** [to_list li] turns a list input [li]  into a list of inputs. *)
    val to_list : 'a list input -> 'a input list

    (** [with_implicit_bool_check bc i] attaches an implicit bool
        check [bc] to the input [i]. *)
    val with_implicit_bool_check :
      ('a repr -> bool repr t) -> 'a input -> 'a input

    (** [with_assertion assrtn i] attaches an assertion [assrtn]
        to the input [i]. *)
    val with_assertion : ('a repr -> unit repr t) -> 'a input -> 'a input

    type 'a t = 'a input
  end

  (** Type that describes an open input commitment. *)
  type 'b open_input_com

  (** [serialize i] returns the array of scalars corresponding to its values. *)
  val serialize : 'a Input.t -> S.t array

  (** [input ~kind i] declares an input of a given [kind] to the Plompiler
      program. It returns a Plompiler representation of the inputted value. *)
  val input : ?kind:input_kind -> 'a Input.t -> 'a repr t

  (** [begin_input_com builder] starts a new input commitment.
      [builder] is a function that takes the inputs to be committed one
      by one and returns the composite commitment.

      An example of usage is

      {[
        let* x1, x2 =
          begin_input_com (fun a b -> (a, b))
          |: Input.scalar x1 |: Input.scalar x2 |> end_input_com
        in
      ]}
  *)
  val begin_input_com : 'b -> 'b open_input_com

  (** [ic |: i] adds [i] to the input commitment [ic]  *)
  val ( |: ) : ('c repr -> 'd) open_input_com -> 'c Input.t -> 'd open_input_com

  (** [end_input_com ic] ends the declaration of an input commitment. *)
  val end_input_com : 'a open_input_com -> 'a t

  (** [eq a b] returns the physical equality of [a] and [b].
      Handle with care. *)
  val eq : 'a repr -> 'a repr -> bool

  (** Monadic fold over a list. *)
  val foldM : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

  (** [pair x y] makes a pair value out of two values. *)
  val pair : 'a repr -> 'b repr -> ('a * 'b) repr

  (** [of_pair p] retrieves the values out of a pair value. *)
  val of_pair : ('a * 'b) repr -> 'a repr * 'b repr

  (** [to_list l] makes a list value out of a list of values. *)
  val to_list : 'a repr list -> 'a list repr

  (** [of_list v] retrieves a list of Plompiler values out of
      a list value. *)
  val of_list : 'a list repr -> 'a repr list

  (** [hd l] returns the head of list [l] *)
  val hd : 'a list repr -> 'a repr t

  (** [unit] is the unit value. *)
  val unit : unit repr

  (** [scalar_of_bool b] converts a boolean value into a scalar. *)
  val scalar_of_bool : bool repr -> scalar repr

  (** [unsafe_bool_of_scalar s] converts a scalar value into a bool.

      WARNING: [s] is expected to hold the values 0 or 1, but this
      is not checked. *)
  val unsafe_bool_of_scalar : scalar repr -> bool repr

  (** Assertion that two values are (structurally) equal. *)
  val assert_equal : 'a repr -> 'a repr -> unit repr t

  (** [equal a b] computes the structural equality between [a] and [b]. *)
  val equal : 'a repr -> 'a repr -> bool repr t

  val scalar_of_limbs : nb_bits:int -> scalar list repr -> scalar repr t

  (** Returns a list of Boolean variables representing the little endian
      bit decomposition of the given scalar (with the least significant bit
      on the head). *)
  val bits_of_scalar :
    ?shift:Z.t -> nb_bits:int -> scalar repr -> bool list repr t

  val limbs_of_scalar :
    ?shift:Z.t ->
    total_nb_bits:int ->
    nb_bits:int ->
    scalar repr ->
    scalar list repr t

  (** [with_label ~label c] adds a [label] to the Plompiler computation
      [c]. Useful for debugging and flamegraphs. *)
  val with_label : label:string -> 'a t -> 'a t

  (** Prints on stdout the prefix string and the repr. It works only when
      running the Result interpreter, it has no effect in the Circuit
      interpreter. *)
  val debug : string -> 'a repr -> unit repr t

  module Num :
    NUM
      with type scalar = scalar
       and type 'a repr = 'a repr
       and type 'a t = 'a t

  module Bool :
    BOOL
      with type scalar = scalar
       and type 'a repr = 'a repr
       and type 'a t = 'a t

  (** Module for describing operations over fixed size integers *)
  module Limb (N : sig
    val nb_bits : int
  end) : sig
    (** [xor_lookup a b] returns the exclusive disjunction of [a] and [b].
      This primitive uses a precomputed lookup table called "xor" ^ [nb_bits].
    *)
    val xor_lookup : scalar repr -> scalar repr -> scalar repr t

    (** [band_lookup a b] returns the conjunction of [a] and [b].
      This primitive uses a precomputed lookup table called "band" ^ [nb_bits].
    *)
    val band_lookup : scalar repr -> scalar repr -> scalar repr t

    (** [bnot_lookup b] returns the negation of [b].
      This primitive uses a precomputed lookup table called "bnot" ^ [nb_bits].
    *)
    val bnot_lookup : scalar repr -> scalar repr t

    (** [rotate_right_lookup x y i] returns the low [nb_bits] of
      [rotate_right (x + y * 2 ^ nb_bits) i] where [0 < i < nb_bits].
      This primitive uses a precomputed lookup table called
      "rotate_right" ^ [nb_bits] ^ "_" ^ [i].
    *)
    val rotate_right_lookup : scalar repr -> scalar repr -> int -> scalar repr t
  end

  (** Addition on ECC curves. *)
  module Ecc : sig
    (** [weierstrass_add (px, py) (qx, qy)] returns a pair [(rx, ry)]
        representing point addition over the Jubjub curve in Weierstrass
        coordinates of the given input points. Namely, it enforces constraints
        [rx = λ² - (px + qx)] and [ry = λ * (px - rx) - py],
        where [λ := (qy - py) / (qx - px)].
    *)
    val weierstrass_add :
      (scalar * scalar) repr ->
      (scalar * scalar) repr ->
      (scalar * scalar) repr t

    (** [edwards_add (px, py) (qx, qy)] returns a pair [(rx, ry)]
        representing point addition over the Jubjub curve in Edwards
        coordinates of the given input points. Namely, it enforces constraints
        [rx = (px * qy + qx * py) / (1 + d * px * py * qx * qy)] and
        [ry = (py * qy - a * px * qx) / (1 - d * px * py * qx * qy)]
        where [a := -1] and [d] are fixed parameters of the Jubjub curve
        in this representation. See {!Lib_plonk.Ecc_gates}.
    *)
    val edwards_add :
      (scalar * scalar) repr ->
      (scalar * scalar) repr ->
      (scalar * scalar) repr t

    (** [edwards_cond_add p q b] returns [edwards_add p q] if [b] is true and
        [p] otherwise.
    *)
    val edwards_cond_add :
      (scalar * scalar) repr ->
      (scalar * scalar) repr ->
      bool repr ->
      (scalar * scalar) repr t
  end

  (* See [lib_plompiler/gadget_mod_arith.ml] for documentation on mod_arith *)
  module Mod_arith : sig
    val add :
      ?subtraction:bool ->
      label:string ->
      modulus:Z.t ->
      nb_limbs:int ->
      base:Z.t ->
      moduli:Z.t list ->
      qm_bound:Z.t * Z.t ->
      ts_bounds:(Z.t * Z.t) list ->
      scalar list repr ->
      scalar list repr ->
      scalar list repr t

    val mul :
      ?division:bool ->
      label:string ->
      modulus:Z.t ->
      nb_limbs:int ->
      base:Z.t ->
      moduli:Z.t list ->
      qm_bound:Z.t * Z.t ->
      ts_bounds:(Z.t * Z.t) list ->
      scalar list repr ->
      scalar list repr ->
      scalar list repr t

    val assert_non_zero :
      label:string ->
      modulus:Z.t ->
      is_prime:bool ->
      nb_limbs:int ->
      base:Z.t ->
      moduli:Z.t list ->
      qm_bound:Z.t * Z.t ->
      ts_bounds:(Z.t * Z.t) list ->
      scalar list repr ->
      unit repr t

    val is_zero :
      label:string ->
      modulus:Z.t ->
      is_prime:bool ->
      nb_limbs:int ->
      base:Z.t ->
      moduli:Z.t list ->
      qm_bound:Z.t * Z.t ->
      ts_bounds:(Z.t * Z.t) list ->
      scalar list repr ->
      bool repr t
  end

  (** Helper functions for the Poseidon Hash defined over the scalar
      field of the BLS12-381 curve, using S-box function [x -> x^5]. *)
  module Poseidon : sig
    (** [poseidon128_full_round ~matrix ~k (x0, y0, z0)] returns
        [\[x1; y1; z1\]] where [(x1, y1, z1)] is the result of applying
        a (shifted) Poseidon full round (parametrized by [matrix] and [k]) to
        the 3-registers state [(x0, y0, z0)].

        Here, [matrix] is a 3 x 3 matrix and [k] is a vector of 3 elements.
        Note that this is a shifted round, that is, the S-box is applied first,
        followed by the linear layer. Namely:
        [(x1, y1, z1) = matrix * (x0^5, y0^5, z0^5) + k].
    *)
    val poseidon128_full_round :
      matrix:S.t array array ->
      k:S.t array ->
      scalar repr * scalar repr * scalar repr ->
      scalar list repr t

    (** [poseidon128_four_partial_rounds ~matrix ~k (x0, y0, z0)] returns
        [\[x4; y4; z4\]] where [(x4, y4, z4)] is the result of applying
        four (shifted) Poseidon partial round (parametrized by [matrix] and
        [ks]) to the 3-registers state [(x0, y0, z0)].

        Here, [matrix] is a 3 x 3 matrix and [ks] is an array of 4 vectors of
        3 elements each (one vector for each of the 4 rounds).

        In particular, for i = 1,...,4:
        [(xi, yi, zi) = matrix * (x_{i-1}, y_{i-1}, z_{i-1}^5) + ki].
    *)
    val poseidon128_four_partial_rounds :
      matrix:S.t array array ->
      ks:S.t array array ->
      scalar repr * scalar repr * scalar repr ->
      scalar list repr t
  end

  (** Helper functions for the Anemoi Hash defined over the scalar
      field of the BLS12-381 curve. *)
  module Anemoi : sig
    (** [anemoi_round ~kx ~ky (x0, y0)] returns [(x1, y1)], the result of
        applying an Anemoi round (parametrized by [kx] and [ky]) to the
        2-registers state [(x0, y0)].

        In particular,
        [x1 = u + kx + 7 * (v + ky)] and [y1 = 7 * (u + kx) + 50 * (v + ky)]
        where [(u, v) = S-BOX(x0, y0)] defined as:
        [u := t + beta * (y0 - t^(1/5))^2 + delta] and [v := y0 - t^(1/5)]
        where [t := x0 - beta * y0^2 - gamma] and [beta], [gamma], [delta]
        are system parameters.
    *)
    val anemoi_round :
      kx:S.t -> ky:S.t -> scalar repr * scalar repr -> (scalar * scalar) repr t

    (** [anemoi_double_round ~kx1 ~ky1 ~kx2 ~ky2 (x0, y0)] returns [(x2, y2)],
        the result of applying two Anemoi rounds.

        In particular, it is equivalent to
        [anemoi_round ~kx:kx2 ~ky:ky2 (anemoi_round ~kx:kx1 ~ky:ky1 (x0, y0))],
        but models the necessary constraints with 5 PlonK rows instead of 8.

        (Note that [anemoi_round] requires 4 PlonK rows.)
    *)
    val anemoi_double_round :
      kx1:S.t ->
      ky1:S.t ->
      kx2:S.t ->
      ky2:S.t ->
      scalar repr * scalar repr ->
      (scalar * scalar) repr t

    (** [anemoi_custom ~kx1 ~ky1 ~kx2 ~ky2 (x0, y0)] returns [(x2, y2)], the
        result of applying two Anemoi rounds.

        In particular, it is equivalent to
        [anemoi_round ~kx:kx2 ~ky:ky2 (anemoi_round ~kx:kx1 ~ky:ky1 (x0, y0))],
        but models the necessary constraints with 2 Plonk rows.

        This is possible thanks to our custom gate for Anemoi double rounds.

        See {!Lib_plonk.Hash_gates}. Furthermore, the second row is "compatible"
        with the one after if another Anemoi round follows this one.
        (Our optimizer would combine such rows in that case).
    *)
    val anemoi_custom :
      kx1:S.t ->
      ky1:S.t ->
      kx2:S.t ->
      ky2:S.t ->
      scalar repr * scalar repr ->
      (scalar * scalar) repr t
  end
end
