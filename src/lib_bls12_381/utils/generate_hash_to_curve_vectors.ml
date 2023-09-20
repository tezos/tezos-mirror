let () = Random.self_init ()

let () =
  let size_dst = 1 + Random.int 48 in
  let msg_dst = 1 + Random.int 512 in
  let dst = Bytes.init size_dst (fun _i -> char_of_int @@ Random.int 256) in
  let msg = Bytes.init msg_dst (fun _i -> char_of_int @@ Random.int 256) in
  let output_g1 = Bls12_381.G1.hash_to_curve msg dst in
  let output_g2 = Bls12_381.G2.hash_to_curve msg dst in
  Printf.printf "Msg = %s\n" Hex.(show (of_bytes msg)) ;
  Printf.printf "Dst = %s\n" Hex.(show (of_bytes dst)) ;
  Printf.printf
    "hash_to_curve_g1(msg, dst) = %s\n"
    Hex.(show (of_bytes (Bls12_381.G1.to_bytes output_g1))) ;
  Printf.printf
    "hash_to_curve_g2(msg, dst) = %s\n"
    Hex.(show (of_bytes (Bls12_381.G2.to_bytes output_g2)))
