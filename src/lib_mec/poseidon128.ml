module Constant : Poseidon_core.PARAMETERS = struct
  let width = 3

  let full_rounds = 8

  let partial_rounds = 56

  let mds_matrix = Mds_poseidon128.v

  let round_constants = Ark_poseidon128.v

  let partial_round_idx_to_permute = 2
end

module Make (Scalar : Bls12_381.Ff_sig.PRIME) =
  Poseidon_core.Make (Constant) (Scalar)
