module Projective : Ec_sig.ProjectiveWeierstrassT

module Affine : Ec_sig.AffineWeierstrassT

module Jacobian : Ec_sig.JacobianWeierstrassT

val from_affine_weierstrass_to_jacobian_weierstrass : Affine.t -> Jacobian.t

val from_affine_weierstrass_to_projective_weierstrass : Affine.t -> Projective.t

val from_jacobian_weierstrass_to_affine_weierstrass : Jacobian.t -> Affine.t

val from_projective_weierstrass_to_affine_weierstrass : Projective.t -> Affine.t
