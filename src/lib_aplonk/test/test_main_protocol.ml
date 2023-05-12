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

module External = struct
  module Cases = Plonk_test.Cases
  module SMap = Plonk.SMap
  open Aplonk.Pi_parameters

  let no_pi_cases =
    Cases.Unit_tests_for_each_selector.list
    @ [Cases.General.no_public_inputs]
    @ Cases.Range_Checks.list

  let one_pi_cases =
    Cases.General_circuits.list @ Cases.General.list_one_public_input

  let pi_rollup_cases = Cases.(aggregate_cases Big_circuit.list)

  module No_input_PIs = struct
    let get_pi_module _ = (module No_public_input : CircuitPI)
  end

  module One_input_PIs = struct
    let get_pi_module _ = (module One_public_input : CircuitPI)
  end

  module Rollup_PIs = struct
    let get_pi_module _ = (module Rollup_example : CircuitPI)
  end

  let upper_bound_no_pi ~zero_knowledge () =
    let module Main = Aplonk.Main_protocol.Make (No_input_PIs) in
    let module H = Plonk_test.Helpers.Make (Main) in
    let open Plonk_test.Cases in
    let qc = Unit_tests_for_each_selector.qc in

    let circuits = SMap.of_list [(qc.name, (qc.circuit, 4))] in
    let inputs = SMap.singleton qc.name [qc.witness] in
    H.test_circuits ~name:"upper_bound_no_pi" ~zero_knowledge circuits inputs

  let upper_bound_pi_rollup ~zero_knowledge () =
    let module Main = Aplonk.Main_protocol.Make (Rollup_PIs) in
    let module H = Plonk_test.Helpers.Make (Main) in
    let open Plonk_test.Cases in
    let nb_proofs = 3 in
    let nb_proofs_added = 1 in
    let _name, circuits_map, inputs, _outcome =
      Big_circuit.make ~nb_proofs ~public_input_size:2 ~k:3 |> aggregate_cases
    in
    let circuits =
      SMap.map (fun (c, n) -> (c, n + nb_proofs_added)) circuits_map
    in
    H.test_circuits ~name:"upper_bound_pi" ~zero_knowledge circuits inputs

  let tests_quick pc_name =
    let prefix s (n, f) = (s ^ "." ^ n, f) in
    let no_pi_tests =
      let module Main = Aplonk.Main_protocol.Make (No_input_PIs) in
      let module H = Plonk_test.Helpers.Make (Main) in
      List.map
        (fun case -> (Cases.(case.name), H.run_test_case case ~verbose:false))
        no_pi_cases
    in
    let one_pi_tests =
      let module Main = Aplonk.Main_protocol.Make (One_input_PIs) in
      let module H = Plonk_test.Helpers.Make (Main) in
      List.map
        (fun case -> (Cases.(case.name), H.run_test_case case ~verbose:false))
        one_pi_cases
    in
    let pi_rollup_case =
      let module Main = Aplonk.Main_protocol.Make (Rollup_PIs) in
      let module H = Plonk_test.Helpers.Make (Main) in
      let name, circuits_map, inputs_map, outcome =
        Cases.(aggregate_cases Big_circuit.list)
      in
      ( name,
        fun ~zero_knowledge () ->
          H.test_circuits ~name ~zero_knowledge circuits_map inputs_map ~outcome
      )
    in
    let multi_tests =
      let open Cases.Unit_tests_for_each_selector in
      let open Cases.General in
      let ql = linear_selector_test 0 in
      let qr = linear_selector_test 1 in
      let qd = linear_selector_test 3 in
      let qe = linear_selector_test 4 in
      let qlg = next_linear_selector_test 0 in
      let qrg = next_linear_selector_test 1 in
      let module PIs = struct
        let pi_map =
          SMap.of_list
            [
              (qc.name, (module No_public_input : CircuitPI));
              (ql.name, (module No_public_input : CircuitPI));
              (qr.name, (module No_public_input : CircuitPI));
              (qd.name, (module No_public_input : CircuitPI));
              (qe.name, (module No_public_input : CircuitPI));
              (qlg.name, (module No_public_input : CircuitPI));
              (qrg.name, (module No_public_input : CircuitPI));
              ( Cases.Range_Checks.basic.name,
                (module No_public_input : CircuitPI) );
              ( Cases.Range_Checks.valid.name,
                (module No_public_input : CircuitPI) );
              (zero_values.name, (module One_public_input : CircuitPI));
              (non_zero_values.name, (module One_public_input : CircuitPI));
            ]

        let get_pi_module circuit_name = SMap.find circuit_name pi_map
      end in
      let module Main = Aplonk.Main_protocol.Make (PIs) in
      let module H = Plonk_test.Helpers.Make (Main) in
      List.concat_map
        (fun cases ->
          let name, circuits_map, inputs_map, outcome =
            Cases.(aggregate_cases cases)
          in
          [
            ( name,
              fun ~zero_knowledge () ->
                H.test_circuits
                  ~name
                  ~zero_knowledge
                  ~outcome
                  circuits_map
                  inputs_map );
          ])
        [
          [qc; ql; qr; qd; qe];
          (* FIXME: aPlonk doesn’t work when used with two circuits with different evaluation points for wires *)
          (* [ql; qlg; Cases.Range_Checks.valid; Cases.Range_Checks.basic]; *)
          [qc; Cases.Range_Checks.basic];
          [non_zero_values; non_zero_values; zero_values];
          [qlg; qrg; non_zero_values; zero_values];
        ]
    in
    no_pi_tests @ one_pi_tests @ [pi_rollup_case]
    @ [
        ("nb_proofs no pi", upper_bound_no_pi);
        ("nb_proofs pi_rollup", upper_bound_pi_rollup);
      ]
    @ multi_tests
    |> List.map (prefix pc_name)
end

let tests =
  List.map
    (fun (n, f) -> Alcotest.test_case n `Quick (f ~zero_knowledge:false))
    (External.tests_quick "aplonk")
