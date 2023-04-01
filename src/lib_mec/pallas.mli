module Projective : Ec_sig.ProjectiveWeierstrassT

module Affine : Ec_sig.AffineWeierstrassT

module Jacobian : Ec_sig.JacobianWeierstrassT

val from_affine_weierstrass_to_jacobian_weierstrass : Affine.t -> Jacobian.t

val from_affine_weierstrass_to_projective_weierstrass : Affine.t -> Projective.t

val from_jacobian_weierstrass_to_affine_weierstrass : Jacobian.t -> Affine.t

val from_projective_weierstrass_to_affine_weierstrass : Projective.t -> Affine.t

module Iso : sig
  module Affine : Ec_sig.AffineWeierstrassT
end

val iso_map : Iso.Affine.t -> Affine.t

val hash_to_field : Bytes.t -> Bytes.t -> Affine.Base.t * Affine.Base.t
