let () = Random.self_init ()

let () =
  let g1 = Bls12_381.G2.random () in
  let g2 = Bls12_381.G2.random () in
  let s = Bls12_381.Fr.random () in
  Printf.printf "g1 = %s\n" Hex.(show (of_bytes (Bls12_381.G2.to_bytes g1))) ;
  Printf.printf "g2 = %s\n" Hex.(show (of_bytes (Bls12_381.G2.to_bytes g2))) ;
  Printf.printf "s = %s\n" Hex.(show (of_bytes (Bls12_381.Fr.to_bytes s))) ;
  Printf.printf
    "g1 + g2 = %s\n"
    Hex.(show (of_bytes Bls12_381.G2.(to_bytes (add g1 g2)))) ;
  Printf.printf
    "[-]g1 = %s\n"
    Hex.(show (of_bytes Bls12_381.G2.(to_bytes (negate g1)))) ;
  Printf.printf
    "[s]g1 = %s\n"
    Hex.(show (of_bytes Bls12_381.G2.(to_bytes (mul g1 s))))
