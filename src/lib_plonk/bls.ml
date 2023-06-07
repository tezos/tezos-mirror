module Scalar = Plompiler.Csir.Scalar
module Scalar_map = Map.Make (Scalar)
module Poly = Bls12_381_polynomial.Polynomial
module Domain = Bls12_381_polynomial.Domain

(* Module to operate with polynomials in FFT evaluations form. *)
module Evaluations = Evaluations_map.Make (Bls12_381_polynomial.Evaluations)

module G1 = struct
  include Bls12_381.G1

  let t : t Repr.t =
    Repr.(
      map
        (bytes_of (`Fixed (size_in_bytes / 2)))
        of_compressed_bytes_exn
        to_compressed_bytes)
end

module G2 = struct
  include Bls12_381.G2

  let t : t Repr.t =
    Repr.(
      map
        (bytes_of (`Fixed (size_in_bytes / 2)))
        of_compressed_bytes_exn
        to_compressed_bytes)
end

module GT = struct
  include Bls12_381.GT

  let t : t Repr.t =
    Repr.(map (bytes_of (`Fixed size_in_bytes)) of_bytes_exn to_bytes)
end

module Pairing = Bls12_381.Pairing
module Srs_g1 = Bls12_381_polynomial.Srs.Srs_g1
module Srs_g2 = Bls12_381_polynomial.Srs.Srs_g2
