let () =
  let g1 = Bls12_381.G1.random () in
  let g2 = Bls12_381.G2.random () in
  let _res = Bls12_381.Pairing.pairing g1 g2 in
  Printf.printf
    "g1 = %s\ng2 = %s\ne(g1, g2) = \n"
    (Hex.show (Hex.of_bytes (Bls12_381.G1.to_bytes g1)))
    (Hex.show (Hex.of_bytes (Bls12_381.G2.to_bytes g2)))

(* (Hex.show (Hex.of_bytes (Bls12_381.Fq12.to_bytes res))) *)
