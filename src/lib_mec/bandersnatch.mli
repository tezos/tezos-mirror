module AffineEdwards : Ec_sig.AffineEdwardsT

module AffineWeierstrass : Ec_sig.AffineWeierstrassT

module AffineMontgomery : Ec_sig.AffineMontgomeryT

val from_affine_edwards_to_affine_montgomery :
  AffineEdwards.t -> AffineMontgomery.t option

val from_affine_montgomery_to_affine_edwards :
  AffineMontgomery.t -> AffineEdwards.t option

val from_affine_montgomery_to_affine_weierstrass :
  AffineMontgomery.t -> AffineWeierstrass.t option

val from_affine_edwards_to_affine_weierstrass :
  AffineEdwards.t -> AffineWeierstrass.t option
