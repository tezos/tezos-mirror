open Polynomial

let () = Random.self_init ()

let test_next_power_of_two () =
  let vectors = [(2, 2); (3, 4); (7, 8); (12, 16)] in
  List.iter
    (fun (x, expected_result) ->
      let res = Utils.next_power_of_two x in
      if res != expected_result then
        Alcotest.failf
          "Expected result is %d (for %d), got %d"
          expected_result
          x
          res)
    vectors

let () =
  let open Alcotest in
  run
    "Utils"
    [
      ( "Reorganizing the coefficients",
        [test_case "Next power of two" `Quick test_next_power_of_two] );
    ]
