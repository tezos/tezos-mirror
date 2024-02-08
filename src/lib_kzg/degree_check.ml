open Bls
open Utils
module Proof = Commitment.Single

type prover_public_parameters = Srs_g1.t

type verifier_public_parameters = {srs_0 : G2.t; srs_n_d : G2.t}

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
let prove_g1 ~max_commit ~max_degree srs p =
  (* Note: this reallocates a buffer of size (Srs_g1.size t.srs.raw.srs_g1)
     (2^21 elements in practice), so roughly 100MB. We can get rid of the
     allocation by giving an offset for the SRS in Pippenger. *)
  Poly.mul_xn p (max_commit - max_degree) Scalar.zero
  |> Commitment.Single.commit srs

let prove ~max_commit ~max_degree srs p =
  (* Note: this reallocates a buffer of size (Srs_g1.size t.srs.raw.srs_g1)
     (2^21 elements in practice), so roughly 100MB. We can get rid of the
     allocation by giving an offset for the SRS in Pippenger. *)
  Poly.mul_xn p (max_commit - max_degree) Scalar.zero
  |> Commitment.Commit.with_srs2 srs

(* Verifies that the degree of the committed polynomial is < t.max_polynomial_length *)
let verify_g1 {srs_0; srs_n_d} (cm : Commitment.Single.t) proof =
  (* checking that cm * committed_offset_monomial = proof *)
  Pairing.pairing_check [(G1.negate cm, srs_n_d); (proof, srs_0)]

(* Verifies that the degree of the committed polynomial is < t.max_polynomial_length *)
let verify srs_0 srs_n_d (cm : Commitment.Single.t) proof =
  (* checking that cm * committed_offset_monomial = proof *)
  Pairing.pairing_check [(G1.negate cm, srs_n_d); (srs_0, proof)]

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
  let proof = (prove_g1 ~max_commit ~max_degree srs) p in
  (proof, transcript)

(* Verifies that the degree of the committed polynomial is < t.max_polynomial_length *)
let verify_multi pp transcript (cm : Commitment.t) proof =
  (* checking that cm * committed_offset_monomial = proof *)
  let transcript = Transcript.expand Commitment.t cm transcript in
  let r, transcript = Fr_generation.random_fr transcript in
  let rs = Fr_generation.powers (SMap.cardinal cm) r in
  let cm =
    Commitment.Commit.with_affine_array_1 (SMap.values cm |> Array.of_list) rs
  in
  let check = verify_g1 pp cm proof in
  (check, transcript)
