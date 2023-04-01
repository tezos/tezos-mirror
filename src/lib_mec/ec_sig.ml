(** Basic interface for elliptic curves *)
module type BASE = sig
  exception Not_on_curve of Bytes.t

  (** Represents an element on the curve. In the case of a curve with a
      cofactor, the element is not necessarily in the prime subgroup.
  *)
  type t

  (** The size of a point representation, in bytes *)
  val size_in_bytes : int

  module Scalar : Bls12_381.Ff_sig.PRIME

  module Base : Bls12_381.Ff_sig.PRIME

  (** Check if a point, represented as a byte array, is on the curve **)
  val check_bytes : Bytes.t -> bool

  (** Attempt to construct a point from a byte array *)
  val of_bytes_opt : Bytes.t -> t option

  (** Attempt to construct a point from a byte array.
      Raise [Not_on_curve] if the point is not on the curve
  *)
  val of_bytes_exn : Bytes.t -> t

  (** Return a representation in bytes *)
  val to_bytes : t -> Bytes.t

  (** Zero of the elliptic curve *)
  val zero : t

  (** A fixed generator of the elliptic curve *)
  val one : t

  (** Return [true] if the given element is zero *)
  val is_zero : t -> bool

  (** Generate a random element *)
  val random : ?state:Random.State.t -> unit -> t

  (** Return the addition of two element *)
  val add : t -> t -> t

  (** Double the element *)
  val double : t -> t

  (** Return the opposite of the element *)
  val negate : t -> t

  (** Return [true] if the two elements are algebraically the same *)
  val eq : t -> t -> bool

  (** Multiply an element by a scalar *)
  val mul : t -> Scalar.t -> t
end

(** Curve in Weierstrass form with a and b. In affine, the curve has the
    equation form y² = x³ + ax + b *)
module type WeierstrassT = sig
  include BASE

  val a : Base.t

  val b : Base.t

  val cofactor : Z.t
end

module type AffineWeierstrassT = sig
  include WeierstrassT

  val get_x_coordinate : t -> Base.t

  val get_y_coordinate : t -> Base.t

  (* val to_montgomery_curve_parameters : unit -> (Base.t * Base.t * Z.t * (Base.t * Base.t)) option

     val to_montgomery : t -> (Base.t * Base.t) option *)

  (** [is_on_curve ~x ~y] returns [true] if the coordinates [(x, y)] represents
      a point on the curve. It does not check the point is in the prime subgroup.
  *)
  val is_on_curve : x:Base.t -> y:Base.t -> bool

  (** [is_in_prime_subgroup ~x ~y] returns [true] if the coordinates [(x, y)]
      represents a point in the prime subgroup. The coordinates must be a point
      on the curve
  *)
  val is_in_prime_subgroup : x:Base.t -> y:Base.t -> bool

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, returns [None]
  *)
  val from_coordinates_opt : x:Base.t -> y:Base.t -> t option

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, raise [Not_on_curve].
  *)
  val from_coordinates_exn : x:Base.t -> y:Base.t -> t

  (** Build a point from a compressed representation. It supposes the base field
      leaves at least a free bit in the last byte to encode the sign.
      Raise [Not_on_curve] if the bytes do not represent a point on the curve
      and in the prime subgroup.
  *)
  val of_compressed_bytes_exn : Bytes.t -> t

  (** Same than [of_compressed_bytes_exn] but returns an option instead of
      raising an exception
  *)
  val of_compressed_bytes_opt : Bytes.t -> t option

  (** Return the compressed representation of the point *)
  val to_compressed_bytes : t -> Bytes.t
end

module type ProjectiveWeierstrassT = sig
  include WeierstrassT

  (** [is_on_curve ~x ~y ~z] returns [true] if the coordinates [(x, y, z)]
      represents a point on the curve. It does not check the point is in the
      prime subgroup.
  *)
  val is_on_curve : x:Base.t -> y:Base.t -> z:Base.t -> bool

  (** [is_in_prime_subgroup ~x ~y ~z] returns [true] if the coordinates
      [(x, y, z)] represents a point in the prime subgroup. The coordinates must
      be a point on the curve.
  *)
  val is_in_prime_subgroup : x:Base.t -> y:Base.t -> z:Base.t -> bool

  val get_x_coordinate : t -> Base.t

  val get_y_coordinate : t -> Base.t

  val get_z_coordinate : t -> Base.t

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, returns [None]
  *)
  val from_coordinates_opt : x:Base.t -> y:Base.t -> z:Base.t -> t option

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, raise [Not_on_curve].
  *)
  val from_coordinates_exn : x:Base.t -> y:Base.t -> z:Base.t -> t

  val get_affine_x_coordinate : t -> Base.t

  val get_affine_y_coordinate : t -> Base.t

  val from_affine_coordinates_exn : x:Base.t -> y:Base.t -> t

  val from_affine_coordinates_opt : x:Base.t -> y:Base.t -> t
end

module type JacobianWeierstrassT = sig
  include WeierstrassT

  (** [is_on_curve ~x ~y ~z] returns [true] if the coordinates [(x, y, z)] represents
      a point on the curve. It does not check the point is in the prime subgroup.
  *)
  val is_on_curve : x:Base.t -> y:Base.t -> z:Base.t -> bool

  (** [is_in_prime_subgroup ~x ~y ~z] returns [true] if the coordinates [(x, y, z)]
      represents a point in the prime subgroup. The coordinates must be a point
      on the curve
  *)
  val is_in_prime_subgroup : x:Base.t -> y:Base.t -> z:Base.t -> bool

  val get_x_coordinate : t -> Base.t

  val get_y_coordinate : t -> Base.t

  val get_z_coordinate : t -> Base.t

  (** Build a point from the projective coordinates. If the point is not on the curve
      and in the subgroup, returns [None]
  *)
  val from_coordinates_opt : x:Base.t -> y:Base.t -> z:Base.t -> t option

  (** Build a point from the projective coordinates. If the point is not on the curve
      and in the subgroup, raise [Not_on_curve].
  *)
  val from_coordinates_exn : x:Base.t -> y:Base.t -> z:Base.t -> t

  val get_affine_x_coordinate : t -> Base.t

  val get_affine_y_coordinate : t -> Base.t

  val from_affine_coordinates_exn : x:Base.t -> y:Base.t -> t

  val from_affine_coordinates_opt : x:Base.t -> y:Base.t -> t
end

module type MontgomeryT = sig
  include BASE

  val a : Base.t

  val b : Base.t

  val cofactor : Z.t
end

module type AffineMontgomeryT = sig
  (** by^2 = x3 + ax^2 + x with b * (a^2 - 4) != 0*)
  include MontgomeryT

  (** [is_on_curve ~x ~y] returns [true] if the coordinates [(x, y)] represents
      a point on the curve. It does not check the point is in the prime subgroup.
  *)
  val is_on_curve : x:Base.t -> y:Base.t -> bool

  (** [is_in_prime_subgroup ~x ~y] returns [true] if the coordinates [(x, y)]
      represents a point in the prime subgroup. The coordinates must be a point
      on the curve
  *)
  val is_in_prime_subgroup : x:Base.t -> y:Base.t -> bool

  val get_x_coordinate : t -> Base.t

  val get_y_coordinate : t -> Base.t

  val to_twisted_curve_parameters :
    unit -> (Base.t * Base.t * Z.t * (Base.t * Base.t)) option

  val to_twisted : t -> (Base.t * Base.t) option

  val to_weierstrass_curve_parameters :
    unit -> (Base.t * Base.t * Z.t * (Base.t * Base.t)) option

  val to_weierstrass : t -> (Base.t * Base.t) option

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, returns [None]
  *)
  val from_coordinates_opt : x:Base.t -> y:Base.t -> t option

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, raise [Not_on_curve].
  *)
  val from_coordinates_exn : x:Base.t -> y:Base.t -> t

  (** Build a point from a compressed representation. It supposes the base field
      leaves at least a free bit in the last byte to encode the sign.
      Raise [Not_on_curve] if the bytes do not represent a point on the curve
      and in the prime subgroup.
  *)
  val of_compressed_bytes_exn : Bytes.t -> t

  (** Same than [of_compressed_bytes_exn] but returns an option instead of
      raising an exception
  *)
  val of_compressed_bytes_opt : Bytes.t -> t option

  (** Return the compressed representation of the point *)
  val to_compressed_bytes : t -> Bytes.t
end

module type AffineEdwardsT = sig
  (** au^2 + v^2 = 1 + du^2v^2 *)
  include BASE

  (** The parameter [a] of the curve, from the equation a * u^2 + v^2 = 1 + d * u^2 * v^2  *)
  val a : Base.t

  (** The parameter [d] of the curve, from the equation a * u^2 + v^2 = 1 + d * u^2 * v^2  *)
  val d : Base.t

  (** The cofactor of the curve. The parameter is used in [is_small_order] and
      in the random point generator.
  *)
  val cofactor : Z.t

  (** [is_on_curve ~u ~v] returns [true] if the coordinates [(u, v)] represents
      a point on the curve. It does not check the point is in the prime subgroup.
  *)
  val is_on_curve : u:Base.t -> v:Base.t -> bool

  (** [is_in_prime_subgroup ~u ~v] returns [true] if the coordinates [(u, v)]
      represents a point in the prime subgroup. The coordinates must be a point
      on the curve
  *)
  val is_in_prime_subgroup : u:Base.t -> v:Base.t -> bool

  (** Return the affine coordinate u (such that au^2 + v^2 = 1 + d u^2 v^2 *)
  val get_u_coordinate : t -> Base.t

  (** Return the affine coordinate u (such that au^2 + v^2 = 1 + d u^2 v^2 *)
  val get_v_coordinate : t -> Base.t

  val to_montgomery_curve_parameters :
    unit -> (Base.t * Base.t * Z.t * (Base.t * Base.t)) option

  val to_montgomery : t -> (Base.t * Base.t) option

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, returns [None]
  *)
  val from_coordinates_opt : u:Base.t -> v:Base.t -> t option

  (** Build a point from the affine coordinates. If the point is not on the curve
      and in the subgroup, raise [Not_on_curve].
  *)
  val from_coordinates_exn : u:Base.t -> v:Base.t -> t

  (** Build a point from the affine coordinates, without verifying the point is
      on the curve. Use with precaution.
  *)
  val unsafe_from_coordinates : u:Base.t -> v:Base.t -> t
end

module type PAIRING = sig
  module G1 : BASE

  module G2 : BASE

  module GT : Bls12_381.Ff_sig.BASE

  exception FailToComputeFinalExponentiation of GT.t

  val miller_loop : (G1.t * G2.t) list -> GT.t

  (** Compute the miller loop on a single tuple of point *)
  val miller_loop_simple : G1.t -> G2.t -> GT.t

  (** Compute a pairing result of a list of points *)
  val pairing : G1.t -> G2.t -> GT.t

  (** Compute the final exponentiation of the given point. Returns a [None] if
      the point is null *)
  val final_exponentiation_opt : GT.t -> GT.t option

  (** Compute the final exponentiation of the given point. Raise
      [FailToComputeFinalExponentiation] if the point is null *)
  val final_exponentiation_exn : GT.t -> GT.t
end
