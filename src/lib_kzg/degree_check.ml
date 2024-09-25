(* This file implements the following protocol, that provides a way to build
   and verify a proof that a committed secret polynomial is of degree less that
   a given number d, with the bilinear groups (G1, G2, GT). In the following,
   we denote [P]₁ the commitment for a polynomial P in G1 and [P]₂ the
   commitment for the same polynomial in G2.

   In the DAL cryptobox, we take d = t.max_polynomial_length - 1 since we don't
   want to commit polynomials with degree greater than polynomials to be
   erasure-encoded.

   For a SRS of degree n₁ (>= d) in G1 (meaning that the maximal degree
   committable in G1 with this SRS is n₁-1), the protocol for a secret
   polynomial P of degree less than d is :
     - Prove: returns [P]₁, π = [P · X^{n₁-d}]₁ such that deg (P · X^{n₁-d}) <= n₁
     - Verify: checks if pairing([P]₁, [X^{n₁-d}]₂) = pairing(π, [1]₂)
               using [P]₁ and [P · X^{n₁-d}]₁ from the prover,
               and having [X^{n₁-d}]₂ from the SRS.

   Note that the proof can be committed in both G1 & G2. If π = [P · X^{n₂-d}]₂
   (n₂ being the maximal degree committable in G2 with the SRS), the verifier
   has to check that pairing([P]₁, [X^{n₂-d}]₂) = pairing([1]₁, π).
   This is needed for DAL where ZCash SRS is used (n₁ = 2²²-1, n₂ = 2²¹-1), and
   that’s why this file implements the two versions.

   /!\ n₁ & n₂ need to be the maximal sizes of the SRS computed from the
   trusted setup and available to the world. Using smaller values would allow
   producing false proofs.

   The code of the two versions of the protocol is factorized in a functor, as
   both are needed in different parts of the codebase.
   A batched version is also provided.
*)

open Bls
open Utils

module Pairing_G1 = struct
  module Proof = G1

  let check srs_n_d cm proof =
    Pairing.pairing_check [(G1.negate cm, srs_n_d); (proof, G2.one)]
end

module Pairing_G2 = struct
  module Proof = G2

  let check srs_n_d cm proof =
    Pairing.pairing_check [(G1.negate cm, srs_n_d); (G1.one, proof)]
end

module type Paring_G = sig
  module Proof : G_sig

  val check : G2.t -> G1.t -> Proof.t -> bool
end

module Make (Pairing : Paring_G) = struct
  module Cm = Commitment.Single (G1)
  module Proof = Commitment.Single (Pairing.Proof)

  type prover_public_parameters = Pairing.Proof.Srs.t

  type verifier_public_parameters = G2.t

  type secret = Poly.t

  (* Proves that degree(p) < t.max_polynomial_length *)
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4192
     Generalize this function to pass the slot_size in parameter. *)
  let prove ~max_commit ~max_degree srs p =
    let shift = max_commit - max_degree in
    (* Note: this reallocates a buffer of size (Srs_g1.size t.srs.raw.srs_g1)
       for the polynomial. Note that we could avoid this multiplication and some
       SRS storage by only committing [p] with the relevant section of the SRS *)
    Poly.mul_xn p shift Scalar.zero |> Proof.commit ~shift srs

  (* Verifies that the degree of the committed polynomial is < t.max_polynomial_length *)
  let verify srs_n_d cm proof =
    (* checking that cm * committed_offset_monomial = proof *)
    Pairing.check srs_n_d cm proof

  module Commit = Commitment.Commit
  module Commitment = Commitment.Make (G1)

  (* Batched version of [prove].
     For r random & P₀, P₁, P₂, … polynomials, deg(P₀ + r · P₁ + r² · P₂ + …) <= d
     implies with high probability over r that deg(P₀), deg(P₁), deg(P₂), … <= d
     The proof returned is [P₀ + r · P₁ + r² · P₂ + …], with r generated from
     the provided transcript expanded with [P₀], [P₁], [P₂], … *)
  let prove_multi ~max_commit ~max_degree srs transcript cm p =
    let transcript = Transcript.expand Commitment.t cm transcript in
    let r, transcript = Fr_generation.random_fr transcript in
    let rs = Fr_generation.powers (SMap.cardinal cm) r in
    let p, _ =
      SMap.fold
        (fun _ p (acc, i) -> (Poly.(acc + mul_by_scalar rs.(i) p), i + 1))
        p
        (Poly.zero, 0)
    in
    let proof = (prove ~max_commit ~max_degree srs) p in
    (proof, transcript)

  (* Batched version of [verify].
     Computes the commitment C = [P₀] + r · [P₁] + r² · [P₂] and checks that
     [proof] indeed proves that the P_i's are of degree less than d. *)
  let verify_multi pp transcript (cm : Commitment.t) proof =
    (* checking that cm * committed_offset_monomial = proof *)
    let transcript = Transcript.expand Commitment.t cm transcript in
    let r, transcript = Fr_generation.random_fr transcript in
    let rs = Fr_generation.powers (SMap.cardinal cm) r in
    let cm = Commit.with_affine_array_1 (SMap.values cm |> Array.of_list) rs in
    let check = verify pp cm proof in
    (check, transcript)
end

(* The main module is the one with the proof in G2 for the DAL *)
include Make (Pairing_G2)
module G1_proof = Make (Pairing_G1)
