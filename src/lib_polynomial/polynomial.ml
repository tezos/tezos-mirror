module type S = sig
  type scalar

  module Domain : Domain.Domain_sig with type scalar = scalar

  module Polynomial : Polynomial_c.Polynomial_sig with type scalar = scalar

  module Evaluations :
    Evaluations_c.Evaluations_c_sig
      with type scalar = scalar
       and type domain = Domain.t
       and type polynomial = Polynomial.t
end

module M = struct
  type scalar = Bls12_381.Fr.t

  module Domain = Domain
  module Polynomial = Polynomial_c
  module Evaluations = Evaluations_c
end

include (M : S with type scalar = Bls12_381.Fr.t)

module Univariate = Univariate
module Multivariate = Multivariate
