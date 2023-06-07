module G1 : sig
  module Projective : Ec_sig.ProjectiveWeierstrassT

  module Affine : Ec_sig.AffineWeierstrassT
end
