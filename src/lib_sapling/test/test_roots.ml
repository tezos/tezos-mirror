module R = Rustzcash
module Core = Core.Client
module Storage = Storage.Make_Storage (Core)

(* Test the roots buffer.
   Fill up the buffer with [size] + 10 random hashes via [add], then look them up using the [mem] function. All but the first 10 should be in the
   buffer.
*)
let test_add_mem () =
  let open Storage.Roots in
  let random_hashes () =
    Core.Hash.of_bytes_exn (Tezos_crypto.Hacl.Rand.gen 32)
  in
  (* List of 10 random hashes *)
  let first_10_hashes = List.init 10 (fun _ -> random_hashes ()) in
  (* List of [size] random hashes *)
  let size_hashes = List.init (Int32.to_int size) (fun _ -> random_hashes ()) in
  (* Roots buffer full of random hashes *)
  let buffer =
    List.fold_left
      (fun buf h -> add h buf)
      empty
      (List.append first_10_hashes size_hashes)
  in
  (* All but the oldest 10 hashes should be in [buffer] *)
  assert (List.for_all (fun h -> mem h buffer) size_hashes) ;
  assert (List.for_all (fun h -> not (mem h buffer)) first_10_hashes)

let tests = [("add_mem", `Quick, test_add_mem)]

let () = Alcotest.run ~__FILE__ "sapling" [("roots", tests)]
