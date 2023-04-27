module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

let test_poseidon128_batch4_optim () =
  let width = 3 in
  let batch_size = 4 in
  let r_f = 8 in
  let r_p = 56 in
  let arc = Array.map Fp.of_string Ark_poseidon128.v in
  (* We format the MDS in a matrix. *)
  let mds = Array.map (fun a -> Array.map Fp.of_string a) Mds_poseidon128.v in
  let _, constants, _, _unbatched_arc =
    Mec.Permutation.HadesLinearOptimisation.compute_updated_constants
      (module Fp)
      r_p
      r_f
      width
      batch_size
      arc
      mds
  in
  (* 30 comes from the fact that we create 3 tmp variables, needing 3, 4 and 5
     vars (as they are recursively computed) and a state of 3 vars needing the 3
     inputs and 3 tmp vars: 3 + 4 + 5 + 3*6 = 30.
     6 comes from the round constants needed to compute the former 6 tmp and output vars.
  *)
  let nb_constants_expected = (6 + 30) * r_p / batch_size in
  let constants_expected =
    Array.map Fp.of_string Poseidon128_linear_trick_expected_output.v
  in
  assert (Array.length constants = nb_constants_expected) ;
  assert (Array.for_all2 Fp.eq constants constants_expected)

let () =
  Alcotest.run
    ~__FILE__
    "Linear trick"
    [
      ( "Regression test",
        [Alcotest.test_case "Poseidon128" `Quick test_poseidon128_batch4_optim]
      );
    ]
