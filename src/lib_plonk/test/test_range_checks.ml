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
  module MP = Main_protocol.Make (Polynomial_protocol.Make (PC))
  module H = Plonk_test.Helpers.Make (MP)
  open Plonk_test.Cases

  let tests_quick pc_name =
    List.map
      (fun case ->
        (case.name, H.run_test_case {case with name = pc_name ^ "." ^ case.name}))
      Range_Checks.list
    @ List.map
        (H.test_aggregated_cases ~prefix:pc_name)
        Range_Checks.[[valid; valid; valid_bis]]
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
