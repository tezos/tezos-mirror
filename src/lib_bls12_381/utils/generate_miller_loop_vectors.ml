let () =
  let n = Random.int 3 + 1 in
  let g1s = List.init n (fun _i -> Bls12_381.G1.random ()) in
  let g2s = List.init n (fun _i -> Bls12_381.G2.random ()) in
  let g1s_string =
    String.concat
      "; "
      (List.map
         (fun g1 -> Hex.(show (of_bytes (Bls12_381.G1.to_bytes g1))))
         g1s)
  in
  let g2s_string =
    String.concat
      "; "
      (List.map
         (fun g2 -> Hex.(show (of_bytes (Bls12_381.G2.to_bytes g2))))
         g2s)
  in
  let points = List.combine g1s g2s in
  let res = Bls12_381.Pairing.miller_loop points in
  Printf.printf
    "g1s = [%s]\ng2 = [%s]\nmiller_loop(g1s, g2s) = %s\n"
    g1s_string
    g2s_string
    (Hex.show (Hex.of_bytes (Bls12_381.Fq12.to_bytes res)))
