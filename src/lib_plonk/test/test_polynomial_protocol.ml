(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Plonk.Bls
open Plonk.Identities

module External (PP : Plonk.Polynomial_protocol.S) = struct
  module SMap = Plonk.SMap

  let pow5_mod_zh ~n p =
    let domain = Domain.build_power_of_two (Z.log2 @@ Z.of_int (8 * n)) in
    let p_e = Evaluations.evaluation_fft domain p in
    let p2_e = Evaluations.mul_c ~evaluations:[p_e; p_e] () in
    let p5_e = Evaluations.mul_c ~evaluations:[p2_e; p2_e; p_e] () in
    let p5 = Evaluations.interpolation_fft domain p5_e in
    snd @@ Poly.division_xn p5 n Scalar.(negate one)

  let sample_polys_and_identities n =
    let pA = Poly.random n in
    let pB = Poly.random n in
    let _, pC = Poly.(division_xn (mul pA pB) n Scalar.(negate one)) in
    let zh = Poly.of_coefficients Scalar.[(one, n); (negate one, 0)] in
    let pD = Poly.(sub pA @@ mul zh (Poly.random 2)) in
    let eD =
      let domain = Domain.build_power_of_two (Z.log2 @@ Z.of_int (8 * n)) in
      Evaluations.evaluation_fft domain pD
    in
    let pA5 = pow5_mod_zh ~n pA in
    let poly_map =
      SMap.of_list
        [
          ("A", pA);
          ("B", pB);
          ("C", pC);
          ("A5", pA5);
          ("D", pD);
          ("0", Poly.zero);
        ]
    in
    (* define prover identities *)
    let prover_id1 evaluations =
      let eA = SMap.find "A" evaluations in
      let eB = SMap.find "B" evaluations in
      let eC = SMap.find "C" evaluations in
      let eAB = Evaluations.mul_c ~evaluations:[eA; eB] () in
      Evaluations.linear_c
        ~evaluations:[eAB; eC]
        ~linear_coeffs:Scalar.[one; negate one]
        ()
    in

    let prover_id2 evaluations =
      let eA = SMap.find "A" evaluations in
      Evaluations.linear_c
        ~evaluations:[eA; eD]
        ~linear_coeffs:Scalar.[one; negate one]
        ()
    in
    let prover_id3 evaluations =
      Evaluations.create (Evaluations.length @@ snd (SMap.choose evaluations))
    in
    let prover_id4 evaluations =
      let eA = SMap.find "A" evaluations in
      let eA5 = SMap.find "A5" evaluations in
      let eA_5 = Evaluations.mul_c ~evaluations:[eA] ~powers:[5] () in
      Evaluations.linear_c
        ~evaluations:[eA_5; eA5]
        ~linear_coeffs:Scalar.[one; negate one]
        ()
    in
    let prover_identities : prover_identities =
     fun evaluations ->
      SMap.of_list
        [
          ("id1", prover_id1 evaluations);
          ("id2", prover_id2 evaluations);
          ("id3", prover_id3 evaluations);
          ("id4", prover_id4 evaluations);
        ]
    in
    (* define verifier identities *)
    let verifier_id1 answers =
      let x_evals = SMap.find "x" answers in
      let a = SMap.find "A" x_evals in
      let b = SMap.find "B" x_evals in
      let c = SMap.find "C" x_evals in
      Scalar.(sub (a * b) c)
    in
    let verifier_id2 answers =
      let x_evals = SMap.find "x" answers in
      let a = SMap.find "A" x_evals in
      let d = SMap.find "D" x_evals in
      Scalar.(sub a d)
    in
    let verifier_id3 answers =
      let x_evals = SMap.find "x" answers in
      SMap.find "0" x_evals
    in
    let verifier_id4 answers =
      let x_evals = SMap.find "x" answers in
      let a = SMap.find "A" x_evals in
      let a5 = SMap.find "A5" x_evals in
      Scalar.(sub (a * a * a * a * a) a5)
    in
    let verifier_identities : verifier_identities =
     fun _x answers ->
      SMap.of_list
        [
          ("id1", verifier_id1 answers);
          ("id2", verifier_id2 answers);
          ("id3", verifier_id3 answers);
          ("id4", verifier_id4 answers);
        ]
    in
    (poly_map, prover_identities, verifier_identities)

  let test_protocol () =
    let n = 1 lsl 5 in
    let nb_of_t_chunks = 5 in
    (* sample test *)
    let poly_map, prover_identities, verifier_identities =
      sample_polys_and_identities n
    in
    (* setup *)
    let prover_pp, verifier_pp =
      let next_pow2 k = Z.(pow (of_int 2) @@ log2up (of_int k) |> to_int) in
      PP.setup
        ~setup_params:(next_pow2 @@ max nb_of_t_chunks (SMap.cardinal poly_map))
        ~srs:Plonk_test.Helpers.srs
    in
    (* prover *)
    let cm, prover_aux = PP.PC.Commitment.commit prover_pp poly_map in
    let secrets = [(poly_map, prover_aux)] in
    let eval_points = [[X]] in
    (* The generator is not needed since we evaluate only at X *)
    let generator = Scalar.zero in
    let domain = Domain.build_power_of_two (Z.log2 @@ Z.of_int (8 * n)) in
    let evaluations = SMap.map (Evaluations.evaluation_fft domain) poly_map in
    let proof, transcript =
      PP.prove
        prover_pp
        Bytes.empty
        ~n
        ~generator
        ~secrets
        ~eval_points
        ~evaluations
        ~identities:prover_identities
        ~nb_of_t_chunks
    in
    (* verifier *)
    let ok, transcript' =
      PP.verify
        verifier_pp
        Bytes.empty
        ~n
        ~generator
        ~commitments:[cm]
        ~eval_points
        ~identities:verifier_identities
        proof
    in
    assert (Bytes.equal transcript transcript') ;
    assert ok
end

module External_Kzg = External (Plonk.Polynomial_protocol)
module External_Kzg_Pack =
  External (Plonk.Polynomial_protocol.Make (Aggregation.Polynomial_commitment))

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("Polynomial Protocol (Kzg)", External_Kzg.test_protocol);
      ("Polynomial Protocol (Kzg_Pack)", External_Kzg_Pack.test_protocol);
    ]
