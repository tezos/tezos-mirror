module AffineEdwards : Ec_sig.AffineEdwardsT

module AffineMontgomery : Ec_sig.AffineMontgomeryT

val from_affine_edwards_to_affine_montgomery :
  AffineEdwards.t -> AffineMontgomery.t option

val from_affine_montgomery_to_affine_edwards :
  AffineMontgomery.t -> AffineEdwards.t option
