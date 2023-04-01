module Constant : Poseidon_core.PARAMETERS

module Make (Scalar : Bls12_381.Ff_sig.PRIME) : sig
  module Strategy : Poseidon_core.STRATEGY with type scalar = Scalar.t

  module Hash : Poseidon_core.HASH with type scalar = Scalar.t
end
