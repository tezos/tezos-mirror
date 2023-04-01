let digest_b (type p) (module P : S.PERMUTATION with type parameters = p)
    (parameters : p) input b =
  let state_size = Array.length input in
  if state_size mod b != 0 then failwith "b must divide the state size" ;
  let m = state_size / b in
  let ctxt = P.allocate_ctxt parameters in
  let () = P.set_state ctxt input in
  let () = P.apply_permutation ctxt in
  let final_state = P.get_state ctxt in
  let output = Array.init m (fun _ -> Bls12_381.Fr.(copy zero)) in
  let rec aux i j =
    if i = m then output
    else if j = b then aux (i + 1) 0
    else (
      Bls12_381.Fr.add_inplace
        output.(i)
        output.(i)
        (Bls12_381.Fr.add final_state.(i + (m * j)) input.(i + (m * j))) ;
      aux i (j + 1))
  in
  aux 0 0

let digest (type p) (module P : S.PERMUTATION with type parameters = p)
    (parameters : p) input =
  let res = digest_b (module P) parameters input (Array.length input) in
  res.(0)
