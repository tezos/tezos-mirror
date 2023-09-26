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

module External
    (MP : Plonk.Main_protocol.S
            with type public_inputs = Plompiler.S.t array list) =
struct
  module H = Plonk_test.Helpers.Make (MP)

  let test_encodings ~zero_knowledge () =
    let open Plonk.Circuit in
    let open Plonk_test.Cases in
    let module Main = H.Singleton in
    let {circuit; witness; _} = General.non_zero_values in
    let pp_prover, pp_verifier =
      Main.setup ~zero_knowledge circuit ~srs:Plonk_test.Helpers.srs
    in
    let inputs = [H.make_secret pp_prover circuit.input_com_sizes witness] in
    let verifier_inputs =
      Main.to_verifier_inputs pp_prover (Kzg.SMap.singleton "" inputs)
      |> Kzg.SMap.choose |> snd
    in
    let b_pp_verifier =
      Data_encoding.Binary.to_bytes_exn
        Main.verifier_public_parameters_encoding
        pp_verifier
    in
    let pp_verifier' =
      Data_encoding.Binary.of_bytes_exn
        Main.verifier_public_parameters_encoding
        b_pp_verifier
    in
    let proof = Main.prove pp_prover ~inputs in
    let b_proof = Data_encoding.Binary.to_bytes_exn Main.proof_encoding proof in
    let proof' =
      Data_encoding.Binary.of_bytes_exn Main.proof_encoding b_proof
    in
    assert (Main.verify pp_verifier' ~inputs:verifier_inputs proof') ;
    (* Json *)
    let b_pp_verifier =
      Data_encoding.Json.construct
        Main.verifier_public_parameters_encoding
        pp_verifier
    in
    let _pp_verifier' =
      Data_encoding.Json.destruct
        Main.verifier_public_parameters_encoding
        b_pp_verifier
    in
    let proof = Main.prove pp_prover ~inputs in
    let b_proof = Data_encoding.Json.construct Main.proof_encoding proof in
    let proof' = Data_encoding.Json.destruct Main.proof_encoding b_proof in
    assert (Main.verify pp_verifier ~inputs:verifier_inputs proof')

  let tests_quick pc_name =
    let open Plonk_test in
    let aggregated_cases =
      let open Cases.General in
      let open Cases.Unit_tests_for_each_selector in
      let ql = linear_selector_test 0 in
      let qr = linear_selector_test 1 in
      let qlg = next_linear_selector_test 0 in
      let qrg = next_linear_selector_test 1 in
      let qog = next_linear_selector_test 2 in
      [
        [zero_values; non_zero_values];
        [zero_values; zero_values];
        [ql; qr; qlg; qrg; qog; qm];
      ]
      @ Cases.Lookup.
          [
            [qplookup; lookup_zero_values];
            [lookup_non_zero_values; lookup_no_public_inputs];
            [lookup_wrong_arith_values; lookup_wrong_arith_values];
            [qplookup_two_tables; qplookup_two_tables];
          ]
    in
    List.map
      (fun case ->
        Cases.
          ( case.name,
            H.run_test_case {case with name = pc_name ^ "." ^ case.name} ))
      (Cases.list @ Cases.Lookup.list)
    @ List.map (H.test_aggregated_cases ~prefix:pc_name) aggregated_cases
    @ [("test_encodings", test_encodings)]

  let tests_slow pc_name =
    let open Plonk_test in
    List.map
      (fun case ->
        let case = Cases.{case with name = pc_name ^ "." ^ case.name} in
        (Cases.(case.name), H.run_test_case case))
      Cases.list_slow

  let several_circuits_one_input ~zero_knowledge () =
    let open Plonk_test.Cases in
    let qc = Unit_tests_for_each_selector.qc in
    let ql = Unit_tests_for_each_selector.linear_selector_test 0 in
    let circuits =
      Kzg.SMap.of_list [(qc.name, (qc.circuit, 1)); (ql.name, (ql.circuit, 1))]
    in
    let inputs = Kzg.SMap.singleton qc.name [qc.witness] in
    H.test_circuits
      ~name:"several_circuits_one_input"
      ~zero_knowledge
      circuits
      inputs
end

module External_Kzg = External (Plonk.Main_protocol)
module PP_Pack =
  Aggregation.Polynomial_protocol.Make_aggregation
    (Aggregation.Polynomial_commitment)
    (Plonk.Main_protocol.Input_commitment)
module External_Kzg_pack = External (Plonk.Main_protocol.Make (PP_Pack))

let tests =
  let quick =
    let tests_kzg = External_Kzg.tests_quick "KZG" in
    let tests_kzg_pack = External_Kzg_pack.tests_quick "KZG_Pack" in
    List.map
      (fun (n, f) -> Alcotest.test_case n `Quick (f ~zero_knowledge:false))
      (tests_kzg @ tests_kzg_pack
      @ [
          ("Subset of proofs (KZG)", External_Kzg.several_circuits_one_input);
          ( "Subset of proofs (Pack)",
            External_Kzg_pack.several_circuits_one_input );
        ])
  in
  let slow =
    let tests_kzg = External_Kzg.tests_slow "KZG" in
    let tests_kzg_pack = External_Kzg_pack.tests_slow "KZG_Pack" in
    List.map
      (fun (n, f) ->
        Alcotest.
          [
            test_case n `Slow (f ~zero_knowledge:false);
            test_case (n ^ " zk") `Slow (f ~zero_knowledge:true);
          ])
      (tests_kzg @ tests_kzg_pack)
    |> List.flatten
  in
  quick @ slow
