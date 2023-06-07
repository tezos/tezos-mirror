module R = Rustzcash
module Core = Core.Client
module Storage = Storage.Make_Storage (Core)

let ba_of_hex h = Hex.to_bytes_exn (`Hex h)

(* Some test vectors are in bigendian *)
let ba_of_hex_be h =
  let reverse s =
    let l = Bytes.length s in
    let res = Bytes.create l in
    Bytes.fill res 0 (Bytes.length res) '0' ;
    for i = 0 to l - 1 do
      Bytes.set res (l - 1 - i) (Bytes.get s i)
    done ;
    res
  in
  reverse (ba_of_hex h)

let test_hash () =
  let open R in
  (* test vectors from zcash/src/gtest/test_pedersen_hash.cpp *)
  let a =
    to_hash
      (ba_of_hex_be
         "87a086ae7d2252d58729b30263fb7b66308bf94ef59a76c9c86e7ea016536505")
  in
  let b =
    to_hash
      (ba_of_hex_be
         "a75b84a125b2353da7e8d96ee2a15efe4de23df9601b9d9564ba59de57130406")
  in
  let result =
    to_hash
      (ba_of_hex_be
         "5bf43b5736c19b714d1f462c9d22ba3492c36e3d9bbd7ca24d94b440550aa561")
  in
  assert (result = merkle_hash ~height:25 a b) ;
  (* test empty tree root computation *)
  let expected_root =
    ba_of_hex_be
      "3e49b5f954aa9d3545bc6c37744661eea48d7c34e3000d82b7f0010c30f4c2fb"
  in
  let res = ref tree_uncommitted in
  for i = 0 to 31 do
    res := merkle_hash ~height:i !res !res
  done ;
  assert (!res = to_hash expected_root)

let test_merkle () =
  let open Storage.Tree in
  (* arbitrary hashes to insert in tree *)
  let hash1 =
    Core.Commitment.of_bytes_exn
    @@ ba_of_hex
         "87a086ae7d2252d58729b30263fb7b66308bf94ef59a76c9c86e7ea016536505"
  in
  let hash2 =
    Core.Commitment.of_bytes_exn
    @@ ba_of_hex
         "a75b84a125b2353da7e8d96ee2a15efe4de23df9601b9d9564ba59de57130406"
  in
  let _hash3 =
    Core.Commitment.of_bytes_exn
    @@ ba_of_hex
         "5bf43b5736c19b714d1f462c9d22ba3492c36e3d9bbd7ca24d94b440550aa561"
  in
  let uncommitted_cm = Core.Hash.(to_commitment @@ uncommitted ~height:0) in
  (* inserting uncommitted should not change the ref
     we can also check that get_witness doesn't assert false *)
  let expected_root =
    Core.Hash.of_bytes_exn
    @@ ba_of_hex_be
         "3e49b5f954aa9d3545bc6c37744661eea48d7c34e3000d82b7f0010c30f4c2fb"
  in
  assert (get_root empty = expected_root) ;
  let t = add empty [uncommitted_cm] in
  assert (get_root t = expected_root) ;
  let rec loop i t =
    if i > 1000 then ()
    else
      let _w = get_witness t (Int64.of_int i) in
      let t2 = add t [uncommitted_cm] in
      assert (get_root t2 = expected_root) ;
      loop (i + 1) t2
  in
  loop 0 t ;
  (* test a full tree *)
  let depth = 13 in
  let t = insert Empty depth 0L [hash1] in
  let _w = get_witness_height t depth 0L in
  let rec loop t i =
    if i < 8192 (* = 2^13 *) then (
      let t = insert t depth (Int64.of_int i) [hash2] in
      let _w = get_witness_height t depth (Int64.of_int i) in
      assert (get_cm_height t (Int64.of_int i) depth = hash2) ;
      loop t (i + 1))
    else t
  in
  let _t_final = loop t 1 in
  (* test that the change is propagated up to the root *)
  let t1 = add empty [uncommitted_cm] in
  let t2 = add t1 [hash1] in
  assert (get_root t2 != expected_root) ;
  (* test vectors from zcash-master/src/test/data/merkle_commitments_sapling.json & merkle_roots_sapling.json
     adding to the empty tree the first commitment we get the first root, the second the second etc... *)
  let expected_merkle_root =
    [|
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "8c3daa300c9710bf24d2595536e7c80ff8d147faca726636d28e8683a0c27703";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "8611f17378eb55e8c3c3f0a5f002e2b0a7ca39442fc928322b8072d1079c213d";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "3db73b998d536be0e1c2ec124df8e0f383ae7b602968ff6a5276ca0695023c46";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "7ac2e6442fec5970e116dfa4f2ee606f395366cafb1fa7dfd6c3de3ce18c4363";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "6a8f11ab2a11c262e39ed4ea3825ae6c94739ccf94479cb69402c5722b034532";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "149595eed0b54a7e694cc8a68372525b9ae2c7b102514f527460db91eb690565";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "8c0432f1994a2381a7a4b5fda770336011f9e0b30784f9a5597901619c797045";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "e780c48d70420601f3313ff8488d7766b70c059c53aa3cda2ff1ef57ff62383c";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "f919f03caaed8a2c60f58c0d43838f83e670dc7e8ccd25daa04a13f3e8f45541";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "74f32b36629724038e71cbd6823b5a666440205a7d1a9242e95870b53d81f34a";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "a4af205a4e1ee02102866b23a68930ac33efda9235832f49b17fcc4939be4525";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "a946a42f1636045a16e65b2308e036d9da70089686c87c692e45912bd1cab772";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "a1db2dbac055364c1cb43cbeb49c7e2815bff855122602a2ad0fb981a91e0e39";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "16329b3ba4f0640f4d306532d9ea6ba0fbf0e70e44ed57d27b4277ed9cda6849";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "7b6523b2d9b23f72fec6234aa6a1f8fae3dba1c6a266023ea8b1826feba7a25c";
      Core.Hash.of_bytes_exn
      @@ ba_of_hex
           "5c0bea7e17bde5bee4eb795c2eec3d389a68da587b36dd687b134826ecc09308";
    |]
  in
  (* commitments are in bigendian *)
  let commitments =
    [|
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "556f3af94225d46b1ef652abc9005dee873b2e245eef07fd5be587e0f21023b0";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "5814b127a6c6b8f07ed03f0f6e2843ff04c9851ff824a4e5b4dad5b5f3475722";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "6c030e6d7460f91668cc842ceb78cdb54470469e78cd59cf903d3a6e1aa03e7c";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "30a0d08406b9e3693ee4c062bd1e6816f95bf14f5a13aafa1d57942c6c1d4250";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "12fc3e7298eb327a88abcc406fbe595e45dddd9b4209803b2e0baa3a8663ecaa";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "021a35cfe13d16891c1409d0f6e8865f51dd54792e5108a6f9e55e0dd44867f7";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "2e0bfc1e123edcb6252251611650f3667371f781b60302385c414716c75e8abc";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "11a5e54bf9a9b57e1c163904999ad1527f1e126c685111e18193decca2dd1ada";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "4674f7836089063143fc18b673b2d92f888c63380e3680385d47bcdbd5fe273a";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "0830165f36a69e416d51cc09cc5668692dee35d98539d3317999fdf87d8fcac7";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "02372c746664e0898576972ca6d0500c7c8ec42f144622349d133b06e837faf0";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "08c6d7dd3d2e387f7b84d6769f2b6cbe308918ab81e0f7321bd0945868d7d4e6";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "26e8c4061f2ad984d19f2c0a4436b9800e529069c0b0d3186d4683e83bb7eb8c";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "037cc2391338956026521beca5c81b541b7f2d1ead7758bf4d1588dbbcb8fa22";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "1cc467cfd2b504e156c9a38bc5c0e4f5ea6cc208054d2d0653a7e561ac3a3ef4";
      Core.Commitment.of_bytes_exn
      @@ ba_of_hex_be
           "15ac4057a9a94536eca9802de65e985319e89627c9c64bc94626b712bc61363a";
    |]
  in
  (* We check by inserting one by one *)
  let t = insert Empty 4 0L [commitments.(0)] in
  let rec loop i t =
    assert (get_root_height t 4 = expected_merkle_root.(i)) ;
    if i < 15 then (
      let new_tree = insert t 4 (Int64.of_int (i + 1)) [commitments.(i + 1)] in
      assert (
        get_cm_height new_tree (Int64.of_int (i + 1)) 4 = commitments.(i + 1)) ;
      loop (i + 1) new_tree)
  in
  loop 0 t ;
  (* We check by inserting by batch *)
  for i = 0 to 15 do
    let list_to_insert = List.init (i + 1) (fun i -> commitments.(i)) in
    let t = insert Empty 4 0L list_to_insert in
    assert (get_root_height t 4 = expected_merkle_root.(i)) ;
    assert (get_cm_height t (Int64.of_int i) 4 = commitments.(i))
  done ;
  ()

let test_merkle2 () =
  let open Storage.Tree in
  let uncommitted_cm =
    Core.Hash.of_bytes_exn
    @@ ba_of_hex
         "0100000000000000000000000000000000000000000000000000000000000000"
  in
  (* Compute the hash^i(uncomm) (ie. the default nodes at different heights)
     from the leaf up to one step before the root
     This is the authentication path for a tree with one commitment *)
  let witness_unflat = Array.make 32 uncommitted_cm in
  Array.iteri
    (fun i x ->
      if i < 31 then
        witness_unflat.(i + 1) <- Core.Hash.merkle_hash ~height:i x x)
    witness_unflat ;
  (* test creation of default nodes at all heights *)
  for i = 0 to 32 - 1 do
    assert (Core.Hash.uncommitted ~height:i = witness_unflat.(i))
  done ;
  let h =
    Core.Hash.of_bytes_exn
    @@ ba_of_hex
         "87a086ae7d2252d58729b30263fb7b66308bf94ef59a76c9c86e7ea016536505"
  in
  (* compute the root *)
  let _, root =
    Array.fold_left
      (fun a b ->
        let i, hash = a in
        (i + 1, Core.Hash.merkle_hash ~height:i hash b))
      (0, h)
      witness_unflat
  in
  (* Create a tree with only cm of depth 32 *)
  let cm = Core.Hash.to_commitment h in
  (* TODO remove of_commitment and add hash_to_commitment *)
  let tree = add empty [cm] in
  (* compute the root with tree format *)
  let root_3 = get_root tree in
  assert (root_3 = root) ;
  (* compute witness *)
  let witness = Bytes.create ((32 * 33) + 1 + 8) in
  Bytes.set witness 0 (Char.chr 32) ;
  for i = 0 to 31 do
    Bytes.set witness ((i * 33) + 1) (Char.chr 32) ;
    for j = 0 to 31 do
      Bytes.set
        witness
        (((31 - i) * 33) + j + 2)
        (Bytes.get (Core.Hash.to_bytes @@ witness_unflat.(i)) j)
    done
  done ;
  for i = 0 to 7 do
    Bytes.set witness ((32 * 33) + 1 + i) '\x00'
  done ;
  (* compute witness with high level function *)
  let witness_2 = get_witness tree 0L in
  assert (Some witness = witness_2)

let test_merkle3 () =
  let open Storage.Tree in
  let cm = Core.Hash.(to_commitment @@ uncommitted ~height:0) in
  let runs = 200L in
  assert (get_root empty = Core.Hash.uncommitted ~height:32) ;
  let rec loop t pos =
    if pos > runs then ()
    else
      let t = add t [cm] in
      assert (size t = Int64.succ pos) ;
      assert (get_cm t pos = Some cm) ;
      assert (get_root t = Core.Hash.uncommitted ~height:32) ;
      let l = get_from t 0L |> Stdlib.Option.get in
      assert (Compare.List_length_with.(l = Int64.to_int pos + 1)) ;
      List.iter (fun e -> assert (e = cm)) l ;
      let l = get_from t pos |> Stdlib.Option.get in
      assert (Compare.List_length_with.(l = 1)) ;
      assert (List.hd l = cm) ;
      assert (get_from t (Int64.succ pos) = None) ;
      loop t (Int64.succ pos)
  in
  loop empty 0L

(* Test batch insertions against indviduals ones. *)
let test_batch_insertion () =
  let open Storage.Tree in
  let random_cm () =
    Core.Commitment.of_bytes_exn (Tezos_crypto.Hacl.Rand.gen 32)
  in
  let random_cms = List.init 33 (fun _ -> random_cm ()) in
  (* List of trees with tree i having the first i commitments
     inserted one at the time *)
  let partial_trees =
    List.fold_left
      (fun list_tree cm -> add (List.hd list_tree) [cm] :: list_tree)
      [empty]
      random_cms
  in
  (* List of trees with tree i having the first i commitments
     inserted all at once *)
  let rec build_partial_trees_batch list_todo list_done res =
    match list_todo with
    | [] -> res
    | cm :: list_todo ->
        let list_done = list_done @ [cm] in
        build_partial_trees_batch
          list_todo
          list_done
          (add empty list_done :: res)
  in
  let partial_trees_batch = build_partial_trees_batch random_cms [] [empty] in
  assert (partial_trees = partial_trees_batch) ;
  (* List of trees with tree i having the first i commitments
     inserted in two batches.
     This tests insertion on a tree that is not empty, unlike the above. *)
  let rec build_partial_trees_2_batches list_todo list_done res =
    match list_todo with
    | [] -> res
    | cm :: list_todo ->
        let list_done = list_done @ [cm] in
        let n = List.length list_done in
        let n1 = Random.int n in
        let n2 = n - n1 in
        let list_todo_1 = List.init n1 (fun i -> List.nth list_done i) in
        let list_todo_2 = List.init n2 (fun i -> List.nth list_done (i + n1)) in
        build_partial_trees_2_batches
          list_todo
          list_done
          (add (add empty list_todo_1) list_todo_2 :: res)
  in
  let partial_trees_2_batches =
    build_partial_trees_2_batches random_cms [] [empty]
  in
  assert (partial_trees = partial_trees_2_batches)

(* Bench batch insertions against indviduals ones. *)
(* with a 10k long list: batch 4.504671 once 127.857964 *)
let bench_batch_insertion () =
  let open Storage.Tree in
  let random_cm () =
    Core.Commitment.of_bytes_exn (Tezos_crypto.Hacl.Rand.gen 32)
  in
  let random_cms = List.init 33 (fun _ -> random_cm ()) in
  let start = Unix.gettimeofday () in
  let single_insert =
    List.fold_left (fun tree cm -> add tree [cm]) empty random_cms
  in
  let time_once = Unix.gettimeofday () -. start in
  let start = Unix.gettimeofday () in
  let batch_insert = add empty random_cms in
  let time_batch = Unix.gettimeofday () -. start in
  assert (single_insert = batch_insert) ;
  Printf.printf "batch %f once %f" time_batch time_once ;
  assert (time_batch < time_once)

let tests =
  [
    ("hash", `Quick, test_hash);
    ("merkle", `Quick, test_merkle);
    ("merkle2", `Quick, test_merkle2);
    ("merkle3", `Quick, test_merkle3);
    ("test_batch_insertion", `Quick, test_batch_insertion);
    ("bench_batch_insertion", `Quick, bench_batch_insertion);
  ]

let () = Alcotest.run ~__FILE__ "sapling" [("merkle", tests)]
