let () =
  let p = Bls12_381.Fq12.random () in
  let res = Bls12_381.Pairing.final_exponentiation_exn p in
  Printf.printf
    "p = %s\nResult: %s\n"
    (Hex.show (Hex.of_bytes (Bls12_381.Fq12.to_bytes p)))
    (Hex.show (Hex.of_bytes (Bls12_381.GT.to_bytes res)))
