module Constant : Poseidon_core.PARAMETERS = struct
  let width = 3

  let full_rounds = 8

  let partial_rounds = 58

  let mds_matrix = Mds_orchard.v

  let round_constants = Ark_orchard.v

  let partial_round_idx_to_permute = 0
end

module Make (Scalar : Bls12_381.Ff_sig.PRIME) =
  Poseidon_core.Make (Constant) (Scalar)
