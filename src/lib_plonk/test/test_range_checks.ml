open Plonk
open Bls

let one = Scalar.one

let two = Scalar.(one + one)

module Internal = struct
  open Range_check_gate.Range_check_gate_impl (Polynomial_protocol)

  let test_min_z () =
    let upper_bound = 4 in
    let x = Random.int (1 lsl upper_bound) |> Scalar.of_int in
    let evals = RangeChecks.partial_z upper_bound x in
    assert (Scalar.eq (List.nth evals 0) x) ;
    let last = List.nth evals (upper_bound - 1) in
    assert (Scalar.(eq last one || eq last zero))
end

module External (PC : Polynomial_commitment.S) = struct
  module PP = Polynomial_protocol.Make (PC)
  module H = Plonk_test.Helpers.Make (Main_protocol)
  open Plonk_test.Cases

  let test_range_checks_single ~zero_knowledge () =
    let wires = Array.map Array.to_list General.circuit.wires in
    let gates = SMap.map Array.to_list General.circuit.gates in
    let circuit =
      Plonk.Circuit.make
        ~wires
        ~gates
        ~public_input_size:General.circuit.public_input_size
        ~range_checks:([4; 6], 4)
        ()
    in
    let inputs = General.witness in
    H.test_circuit ~name:"Range_Checks_single" ~zero_knowledge circuit inputs ;
    let circuit =
      Plonk.Circuit.make
        ~wires
        ~gates
        ~public_input_size:General.circuit.public_input_size
        ~range_checks:([1; 3; 4; 6], 2)
        ()
    in
    try
      H.test_circuit
        ~name:"Range_Checks_single_wrong"
        ~zero_knowledge
        circuit
        inputs
    with Plonk.Main_protocol.Rest_not_null _ -> ()

  (* This test does not work since several proofs are not supported yet *)
  let test_range_checks_multi ~zero_knowledge () =
    let wires = Array.map Array.to_list General.circuit.wires in
    let gates = SMap.map Array.to_list General.circuit.gates in
    let circuit1 =
      Plonk.Circuit.make
        ~wires
        ~gates
        ~public_input_size:General.circuit.public_input_size
        ~range_checks:([4; 6], 4)
        ()
    in
    let witness = General.witness in
    let circuit2 =
      Plonk.Circuit.make
        ~wires
        ~gates
        ~public_input_size:General.circuit.public_input_size
        ~range_checks:([1; 2], 2)
        ()
    in
    let circuits =
      SMap.of_list [("circuit1", (circuit1, 2)); ("circuit2", (circuit2, 1))]
    in
    let inputs =
      SMap.of_list [("circuit1", [witness; witness]); ("circuit2", [witness])]
    in

    H.test_circuits ~name:"Range_Checks_multi" ~zero_knowledge circuits inputs ;

    let circuit3 =
      Plonk.Circuit.make
        ~wires
        ~gates
        ~public_input_size:General.circuit.public_input_size
        ~range_checks:([1; 3; 4; 6], 2)
        ()
    in
    let circuits = SMap.add_unique "circuit3" (circuit3, 1) circuits in
    let inputs = SMap.add_unique "circuit3" [witness] inputs in
    try
      H.test_circuits
        ~name:"Range_Checks_multi_wrong"
        ~zero_knowledge
        circuits
        inputs
    with Plonk.Main_protocol.Rest_not_null _ -> ()

  let tests_quick pc_name =
    [
      (pc_name ^ ".Range_Checks_single", test_range_checks_single)
      (* (pc_name ^ ".Range_Checks_multi", test_range_checks_multi); *);
    ]
end

module External_Kzg = External (Polynomial_commitment)
module External_Kzg_pack = External (Aggregation.Polynomial_commitment)

(* TODO zk tests *)
let tests =
  let tests_kzg = External_Kzg.tests_quick "KZG" in
  let tests_kzg_pack = External_Kzg_pack.tests_quick "KZG_Pack" in
  List.map
    (fun (n, f) -> Alcotest.test_case n `Quick (f ~zero_knowledge:false))
    (tests_kzg @ tests_kzg_pack)
