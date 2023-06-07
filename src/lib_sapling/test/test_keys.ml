module R = Rustzcash

let test_keys () =
  let open Keys.Vector in
  List.iter
    (fun v ->
      (* for some reason sk that should be the seed doesn't work, even
         though with another vectors from zip32 it does. *)
      let ak = R.ask_to_ak v.ask in
      assert (ak = v.ak) ;
      let nk = R.nsk_to_nk v.nsk in
      assert (nk = v.nk) ;
      let ivk = R.crh_ivk ak nk in
      assert (ivk = v.ivk) ;
      let pkd = R.ivk_to_pkd ivk v.default_d in
      assert (pkd = v.default_pk_d) ;
      assert (R.check_diversifier v.default_d) ;
      let nf =
        R.compute_nf
          v.default_d
          pkd
          ~amount:v.note_v
          v.note_r
          ak
          nk
          ~position:v.note_pos
      in
      assert (nf = v.note_nf) ;
      let cm = R.compute_cm v.default_d pkd ~amount:v.note_v v.note_r in
      assert (cm = v.note_cm) ;
      let esk = R.to_esk @@ R.generate_r () in
      let epk = R.ka_derivepublic v.default_d esk in
      let ka1 = R.ka_agree_sender pkd esk in
      let ka2 = R.ka_agree_receiver epk ivk in
      assert (ka1 = ka2))
    vectors

let test_vectors_zip32 () =
  let open Keys in
  List.iter
    (fun v ->
      let open Vk in
      (match v.xsk with
      | Some xsk ->
          let xfvk = of_sk xsk in
          assert (xfvk = v.xfvk)
      | None -> ()) ;
      let j0 = default_index in
      let j1 = index_succ j0 in
      let j2 = index_succ j1 in
      let jmax = R.to_diversifier_index (Bytes.make 11 '\xff') in
      let res_j0, address0 = new_address v.xfvk j0 in
      (match v.d0 with
      | Some d ->
          assert (res_j0 = j0) ;
          assert (address0.diversifier = d)
      | None -> ()) ;
      let res_j1, address1 = new_address v.xfvk j1 in
      (match v.d1 with
      | Some d ->
          assert (res_j1 = j1) ;
          assert (address1.diversifier = d)
      | None -> assert (res_j1 <> j1)) ;
      let res_j2, address2 = new_address v.xfvk j2 in
      (match v.d2 with
      | Some d ->
          assert (res_j2 = j2) ;
          assert (address2.diversifier = d)
      | None -> assert (res_j2 <> j2)) ;
      match v.dmax with
      | Some d ->
          let res_jmax, address_max = new_address v.xfvk jmax in
          assert (res_jmax = jmax) ;
          assert (address_max.diversifier = d)
      | None -> ())
    vectors_zip32

let test_zip32 () =
  let open Keys in
  let v = List.nth vectors_zip32 0 in
  let open Sk in
  let open Vk in
  (* tests from zcash/src/gtest/test_zip32.cpp *)
  let seed = Bytes.init 32 char_of_int in
  let xsk = of_seed seed in
  assert (xsk.depth = ba_of_hex "00") ;
  assert (xsk.parent_fvk_tag = ba_of_hex "00000000") ;
  assert (xsk.child_index = ba_of_hex "00000000") ;
  assert (xsk.chain_code = v.c) ;
  assert (xsk.expsk.ask = Stdlib.Option.get v.ask) ;
  assert (xsk.expsk.nsk = Stdlib.Option.get v.nsk) ;
  assert (xsk.expsk.ovk = v.ovk) ;
  assert (xsk.dk = v.dk) ;
  let xfvk = of_sk xsk in
  assert (xfvk = v.xfvk) ;
  let _j, address = new_address xfvk default_index in
  assert (address.diversifier = Stdlib.Option.get v.d0) ;
  (* TODO continue test with derivation once implemented *)
  ()

let tests =
  [
    ("keys", `Quick, test_keys);
    ("vectors_zip32", `Quick, test_vectors_zip32);
    ("zip32", `Quick, test_zip32);
  ]

let () = Alcotest.run ~__FILE__ "sapling" [("keys", tests)]
