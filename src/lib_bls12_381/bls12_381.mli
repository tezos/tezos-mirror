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

module Ff_sig = Ff_sig

module Fr : sig
  include Ff_sig.PRIME

  (** Actual number of bytes allocated for a value of type t *)
  val size_in_memory : int

  (** Check if a point, represented as a byte array, is in the field **)
  val check_bytes : Bytes.t -> bool

  (** [add_inplace res a b] is the same than {!add} but writes the result in
      [res]. No allocation happens. *)
  val add_inplace : t -> t -> t -> unit

  (** [sub_inplace res a b] is the same than {!sub} but writes the result in
      [res]. No allocation happens. *)
  val sub_inplace : t -> t -> t -> unit

  (** [mul_inplace res a b] is the same than {!sub} but writes the result in
      [res]. No allocation happens. *)
  val mul_inplace : t -> t -> t -> unit

  (** [inverse_exn_inplace res a] is the same than {!inverse_exn} but writes
      the result in [res]. No allocation happens. *)
  val inverse_exn_inplace : t -> t -> unit

  (** [double_inplace res a] is the same than {!double} but writes the
      result in [res]. No allocation happens. *)
  val double_inplace : t -> t -> unit

  (** [square_inplace res a] is the same than {!square} but writes the
      result in [res]. No allocation happens. *)
  val square_inplace : t -> t -> unit

  (** [negate_inplace res a] is the same than {!negate} but writes the
      result in [res]. No allocation happens. *)
  val negate_inplace : t -> t -> unit

  (** [copy x] return a fresh copy of [x] *)
  val copy : t -> t

  (** [memcpy a b] overwrites the content of [a] with the content of [b]. *)
  val memcpy : t -> t -> unit

  (** [add_bulk xs] returns the sum of the elements of [xs] by performing only
      one allocation for the output. This method is recommended to save the
      allocation overhead of using [n] times {!add}. *)
  val add_bulk : t list -> t

  (** [mul_bulk xs] returns the product of the elements of [xs] by performing
      only one allocation for the output. This method is recommended to save the
      allocation overhead of using [n] times {!mul}. *)
  val mul_bulk : t list -> t

  (** [compare a b] compares the elements [a] and [b] based on their bytes
      representation *)
  val compare : t -> t -> int

  (** [inner_product_exn a b] returns the inner product of [a] and [b], i.e.
      [sum(a_i * b_i)]. Raise [Invalid_argument] if the arguments are not of the
      same length. Only two allocations are used. *)
  val inner_product_exn : t array -> t array -> t

  (** Same than {!inner_product_exn} but returns an option instead of raising an
      exception. *)
  val inner_product_opt : t array -> t array -> t option

  (** [of_int x] is equivalent to [of_z (Z.of_int x)]. If [x] is is negative,
      returns the element [order - |x|]. *)
  val of_int : int -> t
end

module type CURVE = sig
  exception Not_on_curve of Bytes.t

  (** The type of the element on the curve and in the prime subgroup. The point
      is given in jacobian coordinates *)
  type t

  (** An element on the curve and in the prime subgroup, in affine coordinates *)
  type affine

  (** [affine_of_jacobian p] creates a new value of type [affine] representing
      the point [p] in affine coordinates *)
  val affine_of_jacobian : t -> affine

  (** [jacobian_of_affine p] creates a new value of type [t] representing the
      point [p] in jacobian coordinates *)
  val jacobian_of_affine : affine -> t

  (** Contiguous C array containing points in affine coordinates *)
  type affine_array

  (** [to_affine_array pts] builds a contiguous C array and populate it with the
      points [pts] in affine coordinates. Use it with
      {!pippenger_with_affine_array} to get better performance. *)
  val to_affine_array : t array -> affine_array

  (** Build a OCaml array of [t] values from the contiguous C array *)
  val of_affine_array : affine_array -> t array

  (** [affine_array_of_compressed_bytes_opt pts] builds a contiguous C
      array and populate it with the points [pts] in affine
      coordinates from their compressed representation in bytes.

      If [subgroup_check] is set, the function also checks if the
      points are in the prime subgroup.

      Use it with {!affine_add_bulk} to get better performance *)
  val affine_array_of_compressed_bytes_opt :
    subgroup_check:bool -> Bytes.t array -> affine_array option

  (** Return the number of elements in the array *)
  val size_of_affine_array : affine_array -> int

  (** Actual number of bytes allocated for a value of type t *)
  val size_in_memory : int

  (** Size in bytes for the compressed representation *)
  val compressed_size_in_bytes : int

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module Scalar : Ff_sig.PRIME with type t = Fr.t

  (** Check if a point, represented as a byte array, is on the curve and in the
      prime subgroup.
      The bytes must be of length {!size_in_bytes}.
  *)
  val check_bytes : Bytes.t -> bool

  (** Attempt to construct a point from a byte array of length {!size_in_bytes}.
      Return [None] if the bytes do not represent a point on the curve and in
      the prime subgroup.
  *)
  val of_bytes_opt : Bytes.t -> t option

  (** Attempt to construct a point from a byte array of length {!size_in_bytes}.
      Raise {!Not_on_curve} if the point is not on the curve and in the prime
      subgroup.
  *)
  val of_bytes_exn : Bytes.t -> t

  (** Allocates a new point from a byte of length [size_in_bytes / 2] array
      representing a point in compressed form.
      Return [None] if the bytes do not represent a point on the curve and in
      the prime subgroup.
  *)
  val of_compressed_bytes_opt : Bytes.t -> t option

  (** Allocates a new point from a byte array of length [size_in_bytes / 2]
      representing a point in compressed form. Raise {!Not_on_curve} if the
      point is not on the curve and in the prime subgroup. *)
  val of_compressed_bytes_exn : Bytes.t -> t

  (** Return a representation in bytes *)
  val to_bytes : t -> Bytes.t

  (** Return a compressed bytes representation *)
  val to_compressed_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return [true] if the given element is zero *)
  val is_zero : t -> bool

  (** [copy x] return a fresh copy of [x] *)
  val copy : t -> t

  (** Generate a random element. The function ensures the element is on the
      curve and in the prime subgroup.

      The routines in the module [Random.State] are used to generate the
      elements. A state can be given to the function to be used. If no state is
      given, [Random.get_state] is used.

      To create a value of type [Random.State.t], you can use [Random.State.make
      [|42|]].
  *)
  val random : ?state:Random.State.t -> unit -> t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** [add_inplace res a b] is the same than {!add} but writes the
      result in [res]. No allocation happens. *)
  val add_inplace : t -> t -> t -> unit

  (** [add_bulk xs] returns the sum of the elements of [xs] by performing only
      one allocation for the output. This method is recommended to save the
      allocation overhead of using [n] times {!add}. *)
  val add_bulk : t list -> t

  (** [affine_add_bulk xs] returns the sum of the elements of [xs] by
      performing only one allocation for the output. *)
  val affine_add_bulk : affine_array -> t

  (** [double g] returns [2g] *)
  val double : t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return [true] if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> Scalar.t -> t

  (** [mul_inplace g x] is the same than {!mul} but writes the output
      in [res]. No allocation happens. *)
  val mul_inplace : t -> t -> Scalar.t -> unit

  (** [hash_to_curve msg dst] follows the standard {{:
      https://www.ietf.org/archive/id/draft-irtf-cfrg-hash-to-curve-14.txt } Hashing
      to Elliptic Curves} applied to BLS12-381 *)
  val hash_to_curve : Bytes.t -> Bytes.t -> t

  (** [pippenger ?start ?len pts scalars] computes the multi scalar
      exponentiation/multiplication. The scalars are given in [scalars] and the
      points in [pts]. If [pts] and [scalars] are not of the same length,
      perform the computation on the first [n] points where [n] is the smallest
      size. Arguments [start] and [len] can be used to take advantages of
      multicore OCaml. Default value for [start] (resp. [len]) is [0] (resp. the
      length of the array [scalars]).

      @raise Invalid_argument if [start] or [len] would infer out of bounds
      array access.

      Perform allocations on the C heap to convert scalars to bytes and to
      convert the points [pts] in affine coordinates as values of type [t] are
      in jacobian coordinates.

      {b Warning.} Undefined behavior if the point to infinity is in the array *)
  val pippenger : ?start:int -> ?len:int -> t array -> Scalar.t array -> t

  (** [pippenger_with_affine_array ?start ?len pts scalars] computes the multi
      scalar exponentiation/multiplication. The scalars are given in [scalars]
      and the points in [pts]. If [pts] and [scalars] are not of the same
      length, perform the computation on the first [n] points where [n] is the
      smallest length. The differences with {!pippenger} are 1. the points are
      loaded in a contiguous C array to speed up the access to the elements by
      relying on the CPU cache 2. and the points are in affine coordinates, the
      form expected by the algorithm implementation, avoiding new allocations
      and field inversions required to convert from jacobian (representation of
      a points of type [t], as expected by {!pippenger}) to affine coordinates.
      Expect a speed improvement around 20% compared to {!pippenger}, and less
      allocation on the C heap. A value of [affine_array] can be built using
      {!to_affine_array}. Arguments [start] and [len] can be used to take
      advantages of multicore OCaml. Default value for [start] (resp. [len]) is
      [0] (resp. the length of the array [scalars]).

      @raise Invalid_argument if [start] or [len] would infer out of bounds
      array access.

      Perform allocations on the C heap to convert scalars to bytes.

      {b Warning.} Undefined behavior if the point to infinity is in the array *)
  val pippenger_with_affine_array :
    ?start:int -> ?len:int -> affine_array -> Scalar.t array -> t

  (** [pippenger_with_compressed_bytes_array_opt points scalars]
      computes the multi-scalar multiplication, i.e., the sum of
      [scalars[i] * points[i]].

      If [subgroup_check] is set, the function also checks if the points
      are in the prime subgroup.

      Returns [None] if deserialization of points fails. *)
  val pippenger_with_compressed_bytes_array_opt :
    subgroup_check:bool -> Bytes.t array -> Scalar.t array -> Bytes.t option

  (** [add_bulk_with_compressed_bytes_array_opt points]
      computes the sum of [points[i]].

      If [subgroup_check] is set, the function also checks if the points
      are in the prime subgroup.

      Returns [None] if deserialization of points fails. *)
  val add_bulk_with_compressed_bytes_array_opt :
    subgroup_check:bool -> Bytes.t array -> Bytes.t option
end

(** Represents the field extension constructed as described {{:
    https://github.com/zcash/librustzcash/blob/6e0364cd42a2b3d2b958a54771ef51a8db79dd29/pairing/src/bls12_381/README.md}
    here}. The interface does not provide the full
    requirements to be a field (like the addition).

    In many applications, the field extension won't be used and {!GT} will be
    used instead. However, this library exposes the field extension to allow the
    user to use {!Pairing.miller_loop} and post-pone the call to
    {!Pairing.final_exponentiation_exn} follwing its taste.
*)
module Fq12 : sig
  exception Not_in_field of Bytes.t

  (** An element of the field extension. It is {b not} ensured the element are
      in the prime multiplicative subgroup. If you need inhabitants of the prime
      subgroup, use the module {!GT} *)
  type t

  (** The order of the field *)
  val order : Z.t

  (** Minimal number of bytes required to encode a value of the group *)
  val size_in_bytes : int

  (** Actual number of bytes allocated for a value of type t *)
  val size_in_memory : int

  (** The neutral element of the additive subgroup *)
  val zero : t

  (** The neutral element of the multiplicative subgroup *)
  val one : t

  (** [is_zero x] returns [true] if [x] is the neutral element of the additive
      subgroup *)
  val is_zero : t -> bool

  (** [is_one x] returns [true] if [x] is the neutral element for the
      multiplication *)
  val is_one : t -> bool

  (** [mul a b] returns the product of [a] and [b] *)
  val mul : t -> t -> t

  (** [inverse_exn x] returns [x^-1 mod order] if [x] is not [0], else raise
      [Division_by_zero]. Equivalently, [inverse_exn x] returns the unique [y]
      such that [x * y mod order = 1] *)
  val inverse_exn : t -> t

  (** [inverse_opt x] returns [x^-1 mod order] as an option if [x] is not [0],
      else returns [None]. Equivalently, [inverse_opt x] returns the unique [y]
      such that [x * y mod order = 1] *)
  val inverse_opt : t -> t option

  (** [eq a b] returns [true] if [a = b mod order], else [false] *)
  val eq : t -> t -> bool

  (** Generates a random element.

      The routines in the module [Random.State] are used to generate the
      elements. A state can be given to the function to be used. If no state is
      given, [Random.get_state] is used.

      To create a value of type [Random.State.t], you can use [Random.State.make
      [|42|]].
  *)
  val random : ?state:Random.State.t -> unit -> t

  val pow : t -> Z.t -> t

  (** [of_bytes_exn bs] builds a value of type t. Each coordinate is expected to
      be in little endian and the constant monomial is always encoded first.
      The size of [bs] is expected to be {!size_in_bytes}.
      If the element is not in the field or if [bs] is not of size
      {!size_in_bytes}, raises {!Not_in_field} with [bs] in parameter.
  *)
  val of_bytes_exn : Bytes.t -> t

  (** Same than {!of_bytes_exn} but returns an option instead of raising an
      exception *)
  val of_bytes_opt : Bytes.t -> t option

  (** [to_bytes p] encodes the point [p] following the encoding described by
      {!of_bytes_exn} *)
  val to_bytes : t -> Bytes.t

  (** Construct an element of Fq12 based on the following pattern:

      Fq12 =
        (Fq6 (Fq2(x: x0, y: x1)) Fq2(x: x2, y: x3)) Fq2(x: x4, y: x5)),
         Fq6 ( Fq2(x: x6, y: x7)) Fq2(x: x8, y: x9)) Fq2(x: x10, y: x11))

      [x0, ..., x11] are the parameters of the function. No check is applied.

      Example of usage (pairing result of the multiplicative neutre elements):
      ```OCaml
      Fq12.of_string
        "2819105605953691245277803056322684086884703000473961065716485506033588504203831029066448642358042597501014294104502"
        "1323968232986996742571315206151405965104242542339680722164220900812303524334628370163366153839984196298685227734799"
        "2987335049721312504428602988447616328830341722376962214011674875969052835043875658579425548512925634040144704192135"
        "3879723582452552452538684314479081967502111497413076598816163759028842927668327542875108457755966417881797966271311"
        "261508182517997003171385743374653339186059518494239543139839025878870012614975302676296704930880982238308326681253"
        "231488992246460459663813598342448669854473942105054381511346786719005883340876032043606739070883099647773793170614"
        "3993582095516422658773669068931361134188738159766715576187490305611759126554796569868053818105850661142222948198557"
        "1074773511698422344502264006159859710502164045911412750831641680783012525555872467108249271286757399121183508900634"
        "2727588299083545686739024317998512740561167011046940249988557419323068809019137624943703910267790601287073339193943"
        "493643299814437640914745677854369670041080344349607504656543355799077485536288866009245028091988146107059514546594"
        "734401332196641441839439105942623141234148957972407782257355060229193854324927417865401895596108124443575283868655"
        "2348330098288556420918672502923664952620152483128593484301759394583320358354186482723629999370241674973832318248497"
      ```
      {{: https://docs.rs/crate/pairing/0.16.0/source/src/bls12_381/tests/mod.rs} Source }.

      Undefined behaviours if the given elements are not in the field or any
      other representation than decimal is used. Use this function carefully.

      See https://docs.rs/crate/pairing/0.16.0/source/src/bls12_381/README.md
      for more information on the instances used by the library.

      FIXME: the function is not memory efficient because the elements are
      copied multiple times.
  *)
  val of_string :
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    String.t ->
    t

  (** Same than {!of_string}, using [Z.t] elements

      FIXME: the function is not memory efficient because the elements are
      copied multiple times.
  *)
  val of_z :
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    Z.t ->
    t
end

(** Elliptic curve built over the field [Fq] and the equation [y^2 = x^3 + 4] *)
module G1 : sig
  include CURVE

  (** Create a point from the coordinates. If the point is not on the curve and
      in the prime subgroup,returns [None].
      The points must be given modulo the order of [Fq]. To
      create the point at infinity, use {!zero}
  *)
  val of_z_opt : x:Z.t -> y:Z.t -> t option
end

(** Elliptic curve built over the field [Fq^2] and the equation [y^2 = x^3 + 4(u
    + 1)] *)
module G2 : sig
  include CURVE

  (** Create a point from the coordinates. If the point is not on the curve and
      in the prime subgroup, returns [None].
      The points must be given modulo the order of [Fq]. The
      points are in the form [(c0, c1)] where [x = c1 * X + c0] and [y = c1 * X
      + c0].
      To create the point at infinity, use {!zero}
  *)
  val of_z_opt : x:Z.t * Z.t -> y:Z.t * Z.t -> t option
end

(** Prime subgroup of {!Fq12}, of order {!Fr.order}, represented additively *)
module GT : sig
  exception Not_in_group of Bytes.t

  (** Represents an element in the prime subgroup *)
  type t

  (** The order of the group. It is the same than {!Fr.order} *)
  val order : Z.t

  (** Minimal number of bytes required to encode a value of the group *)
  val size_in_bytes : int

  (** Actual number of bytes allocated for a value of type t *)
  val size_in_memory : int

  (** Checks the bytes represent a point in the prime subgroup. The expected
  encoding is the same than {!Fq12.of_bytes_exn}. *)
  val check_bytes : Bytes.t -> bool

  (** Same than {!Fq12.of_bytes_exn} but also verifies the element is in the
      prime subgroup. Raise {!Not_in_group} if the element is not in the prime
      subgroup. *)
  val of_bytes_exn : Bytes.t -> t

  (** Same than {!of_bytes_exn} but returns an option instead of an
      exception. *)
  val of_bytes_opt : Bytes.t -> t option

  (** Same than {!Fq12.to_bytes}. *)
  val to_bytes : t -> Bytes.t

  val eq : t -> t -> bool

  (** The neutral element of the subgroup. It is equal to {!Fq12.one}. *)
  val zero : t

  (** [is_zero x] is equivalent to [eq x zero] *)
  val is_zero : t -> bool

  (** A generator of the group. It is set to the result of [Pairing.pairing
      G1.one G2.one]. *)
  val one : t

  (** [is_one x] is equivalent to [eq x one] *)
  val is_one : t -> bool

  (** Generate a random element. The function ensures the element is in the
      prime subgroup.

      The routines in the module [Random.State] are used to generate the
      elements. A state can be given to the function to be used. If no state is
      given, [Random.get_state] is used.

      To create a value of type [Random.State.t], you can use [Random.State.make
      [|42|]].
  *)
  val random : ?state:Random.State.t -> unit -> t

  (** [add x y] returns the sum of [x] and [y]. It is equivalent to [Fq12.mul x
      y]. *)
  val add : t -> t -> t

  (** [negate x] returns the opposite of [x]. It is equivalent to
      [Fq12.inverse_exn x]. *)
  val negate : t -> t

  (** [mul x n] returns [[n] x], i.e. the result of adding [x] n-times with
      itself. It is equivalent to [Fq12.pow x (Fr.to_z n)] *)
  val mul : t -> Fr.t -> t
end

(** Provides routines to compute the pairing over [G1 x G2 -> GT] *)
module Pairing : sig
  exception FailToComputeFinalExponentiation of Fq12.t

  (** Compute the miller loop on a list of points. Return {!Fq12.one} if the list
      is empty. *)
  val miller_loop : (G1.t * G2.t) list -> Fq12.t

  (** Compute the miller loop on a single tuple of point. *)
  val miller_loop_simple : G1.t -> G2.t -> Fq12.t

  (** Compute a pairing result of a list of points. *)
  val pairing : G1.t -> G2.t -> GT.t

  (** [pairing_check points] returns [true] if [pairing points = GT.one]. Return
      [true] if the empty list is given. *)
  val pairing_check : (G1.t * G2.t) list -> bool

  (** Compute the final exponentiation of the given point. Returns [None] if
      the point is equal to {!Fq12.zero} *)
  val final_exponentiation_opt : Fq12.t -> GT.t option

  (** Compute the final exponentiation of the given point.
      Raises {!FailToComputeFinalExponentiation} if the point is equal to
      {!Fq12.zero} *)
  val final_exponentiation_exn : Fq12.t -> GT.t
end

(** Return [true] if the environment variable `BLST_PORTABLE` was set when
    building the library, otherwise [false]. Can be used to detect if the
    backend blst has been optimised with ADX on ADX-supported platforms. *)
val built_with_blst_portable : bool
