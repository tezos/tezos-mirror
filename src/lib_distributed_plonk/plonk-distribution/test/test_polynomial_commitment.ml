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

module External (PC : Plonk_for_distribution.Kzg.PC_for_distribution_sig) =
struct
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

  let prove_distributed instance =
    let main_secrets, worker_secrets =
      List.map
        (let b = Random.bool () in
         SMap.partition (fun _ _ -> b))
        instance.f_map_list
      |> List.split
    in
    let secrets1, secrets2 =
      List.map (SMap.partition (fun _ _ -> Random.bool ())) worker_secrets
      |> List.split
    in
    let transcript =
      PC.distributed_expand_transcript
        instance.transcript
        instance.query_list
        instance.answer_list
    in
    let worker_msg, state =
      PC.distributed_prove_main1
        instance.pp_prover
        transcript
        instance.query_list
        instance.answer_list
        main_secrets
        instance.prover_aux_list
    in
    let main_prover_msg1 =
      PC.distributed_prove_worker secrets1 instance.prover_aux_list worker_msg
    in
    let main_prover_msg2 =
      PC.distributed_prove_worker secrets2 instance.prover_aux_list worker_msg
    in
    PC.distributed_prove_main2 state [main_prover_msg1; main_prover_msg2]

  let prove_and_verify_instance ?(wrong_transcript = false)
      ?(distributed = false) instance =
    let proof, prover_final_transcript =
      if distributed then
        let proof, transcript, _aux = prove_distributed instance in
        (proof, transcript)
      else
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

  let test_correctness_distribution () =
    let instance = generate_instance ~nb_batches:4 ~nb_polys_per_batch:16 in
    assert (prove_and_verify_instance ~distributed:true instance)

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

module KZG_Tests = External (Plonk_for_distribution.Kzg.Kzg_impl)
module KZG_Pack_Tests = External (Plonk_for_distribution.Kzg_pack.Kzg_pack_impl)

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("distr. correctness (KZG)", KZG_Tests.test_correctness_distribution);
      ( "distr. correctness (KZG_Pack)",
        KZG_Pack_Tests.test_correctness_distribution );
    ]
