module Uncompressed : sig
  include Elliptic_curve_sig.T with type Scalar.t = Fr.t

  (** Create a point from the coordinates. If the point is not on the curve,
      None is return. The points must be given modulo the order of Fq. The
      points are in the form (c0, c1) where x = c1 * X + c0 and y = c1 * X +
      c0. To create the point at infinity, use [zero ()] *)
  val of_z_opt : x:Z.t * Z.t -> y:Z.t * Z.t -> t option
end

module Compressed : sig
  include Elliptic_curve_sig.T with type Scalar.t = Fr.t

  val of_uncompressed : Uncompressed.t -> t

  val to_uncompressed : t -> Uncompressed.t
end
