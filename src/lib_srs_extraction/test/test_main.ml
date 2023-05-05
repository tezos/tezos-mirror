open Libsrs

let path = project_root // Filename.dirname __FILE__

let test_lagrange () =
  let input_file = path // "phase1radix2m5" in
  let g1_elements = Powers_of_tau.to_g1s input_file in
  let g2_elements = Powers_of_tau.to_g2s input_file in
  Checks.equality g1_elements g2_elements

let tests = [("lagrange zcash_srs", test_lagrange)]

let () =
  let wrap = List.map (fun (name, f) -> Alcotest.test_case name `Quick f) in
  Alcotest.run ~__FILE__ "SRS" [("Srs", wrap tests)]
