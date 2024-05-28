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

  (* p(X) of degree n. Max degree that can be committed: d, which is also the
     SRS's length - 1. We take d = t.max_polynomial_length - 1 since we don't want to commit
     polynomials with degree greater than polynomials to be erasure-encoded.

     We consider the bilinear groups (G_1, G_2, G_T) with G_1=<g> and G_2=<h>.
     - Commit (p X^{d-n}) such that deg (p X^{d-n}) = d the max degree
     that can be committed
     - Verify: checks if e(commit(p), commit(X^{d-n})) = e(commit(p X^{d-n}), h)
     using the commitments for p and p X^{d-n}, and computing the commitment for
     X^{d-n} on G_2. *)

  (* Proves that degree(p) < t.max_polynomial_length *)
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4192
     Generalize this function to pass the slot_size in parameter. *)
  let prove ~max_commit ~max_degree srs p =
    (* Note: this reallocates a buffer of size (Srs_g1.size t.srs.raw.srs_g1)
       (2^21 elements in practice), so roughly 100MB. We can get rid of the
       allocation by giving an offset for the SRS in Pippenger. *)
    Poly.mul_xn p (max_commit - max_degree) Scalar.zero |> Proof.commit srs

  (* Verifies that the degree of the committed polynomial is < t.max_polynomial_length *)
  let verify srs_n_d cm proof =
    (* checking that cm * committed_offset_monomial = proof *)
    Pairing.check srs_n_d cm proof

  module Commit = Commitment.Commit
  module Commitment = Commitment.Make (G1)

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

  (* Verifies that the degree of the committed polynomial is < t.max_polynomial_length *)
  let verify_multi pp transcript (cm : Commitment.t) proof =
    (* checking that cm * committed_offset_monomial = proof *)
    let transcript = Transcript.expand Commitment.t cm transcript in
    let r, transcript = Fr_generation.random_fr transcript in
    let rs = Fr_generation.powers (SMap.cardinal cm) r in
    let cm = Commit.with_affine_array_1 (SMap.values cm |> Array.of_list) rs in
    let check = verify pp cm proof in
    (check, transcript)
end

include Make (Pairing_G2)
module G1_proof = Make (Pairing_G1)
