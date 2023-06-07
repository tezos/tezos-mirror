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
open Plonk.Utils

module Internal = struct
  open Plonk.Polynomial_commitment.Kzg_impl

  let test_verifier_srs () =
    let n = 2 in
    let pp_prv, pp_vrf =
      Public_parameters.setup (n, 0) Plonk_test.Helpers.srs
    in
    assert (G2.eq pp_prv.encoding_1 pp_vrf.encoding_1) ;
    assert (G2.eq pp_prv.encoding_x pp_vrf.encoding_x)
end

module External (PC : Plonk.Polynomial_commitment.S) = struct
  module SMap = Plonk.SMap

  let generate_random_poly degree =
    Poly.of_coefficients (List.init degree (fun i -> (Scalar.random (), i)))

  let generate_f_map ~prefix degree n =
    let generate_f i =
      (prefix ^ "|" ^ string_of_int i, generate_random_poly degree)
    in
    SMap.of_list (List.init n generate_f)

  type instance = {
    pp_prover : PC.Public_parameters.prover;
    pp_verifier : PC.Public_parameters.verifier;
    f_map_list : Poly.t SMap.t list;
    cmt_list : PC.Commitment.t list;
    prover_aux_list : PC.Commitment.prover_aux list;
    transcript : Bytes.t;
    query_list : Scalar.t SMap.t list;
    answer_list : PC.answer list;
  }

  let generate_instance ~nb_batches ~nb_polys_per_batch =
    let max_degree = 20 in
    let pp_prover, pp_verifier =
      PC.Public_parameters.setup (2 * nb_polys_per_batch) Plonk_test.Helpers.srs
    in
    let f_map_list =
      List.init nb_batches (fun i ->
          generate_f_map ~prefix:(string_of_int i) max_degree nb_polys_per_batch)
    in
    let cmt_list, prover_aux_list =
      List.map (PC.Commitment.commit pp_prover) f_map_list |> List.split
    in
    let transcript =
      Transcript.list_expand PC.Commitment.t cmt_list Bytes.empty
    in
    let x1 = Scalar.random () in
    let x2 = Scalar.random () in
    let just_x1 = SMap.singleton "x1" x1 in
    let both_points = SMap.of_list [("x1", x1); ("x2", x2)] in
    let query_list =
      just_x1 :: List.init (nb_batches - 1) (fun _ -> both_points)
    in
    let answer_list = List.map2 PC.evaluate f_map_list query_list in
    {
      pp_prover;
      pp_verifier;
      f_map_list;
      cmt_list;
      prover_aux_list;
      transcript;
      query_list;
      answer_list;
    }

  let prove_and_verify_instance ?(wrong_transcript = false) instance =
    let proof, prover_final_transcript =
      PC.prove
        instance.pp_prover
        instance.transcript
        instance.f_map_list
        instance.prover_aux_list
        instance.query_list
        instance.answer_list
    in

    let b, verifier_final_transcript =
      PC.verify
        instance.pp_verifier
        (if wrong_transcript then Bytes.empty else instance.transcript)
        instance.cmt_list
        instance.query_list
        instance.answer_list
        proof
    in

    (* This should hold as long as they input the same query list,
       the same answer list and start from the same transcript (where
       we should typically have included the cmt list *)
    if not wrong_transcript then
      assert (Bytes.equal prover_final_transcript verifier_final_transcript) ;
    b

  let test_correctness () =
    let instance = generate_instance ~nb_batches:4 ~nb_polys_per_batch:16 in
    assert (prove_and_verify_instance instance)

  let test_wrong_answer () =
    let instance = generate_instance ~nb_batches:4 ~nb_polys_per_batch:8 in
    let first_answer = List.hd instance.answer_list in
    let key, outputs = SMap.choose first_answer in
    let first_answer' =
      SMap.add key (SMap.map (fun _ -> Scalar.random ()) outputs) first_answer
    in
    let instance' =
      {
        instance with
        answer_list = first_answer' :: List.tl instance.answer_list;
      }
    in
    assert (not @@ prove_and_verify_instance instance')

  let test_wrong_transcript () =
    let instance = generate_instance ~nb_batches:4 ~nb_polys_per_batch:8 in
    assert (not @@ prove_and_verify_instance ~wrong_transcript:true instance)
end

module KZG_Tests = External (Plonk.Polynomial_commitment)
module KZG_Pack_Tests = External (Aggregation.Polynomial_commitment)

let tests =
  [Alcotest.test_case "test_verifier_srs" `Quick Internal.test_verifier_srs]
  @ List.map
      (fun (name, f) -> Alcotest.test_case name `Quick f)
      [
        ("correctness (KZG)", KZG_Tests.test_correctness);
        ("wrong answer (KZG)", KZG_Tests.test_wrong_answer);
        ("wrong transcript (KZG)", KZG_Tests.test_wrong_transcript);
        ("correctness (KZG_Pack)", KZG_Pack_Tests.test_correctness);
        ("wrong answer (KZG_Pack)", KZG_Pack_Tests.test_wrong_answer);
        ("wrong transcript (KZG_Pack)", KZG_Pack_Tests.test_wrong_transcript);
      ]
