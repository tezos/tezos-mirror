open Alcotest
open Uecc

let nb_iterations = 10

let checki = check int

let test_export_p256 () =
  match keypair () with
  | None -> assert false
  | Some (sk, pk) ->
      let sk_bytes = to_bytes ~compress:false sk in
      let pk_bytes = to_bytes ~compress:false pk in
      checki __LOC__ sk_size (Bytes.length sk_bytes) ;
      checki __LOC__ (pk_size + 1) (Bytes.length pk_bytes) ;
      match sk_of_bytes sk_bytes, pk_of_bytes pk_bytes with
      | Some (sk', pk'), Some pk'' ->
          assert (equal sk sk') ;
          assert (equal pk pk') ;
          assert (equal pk pk'') ;
          assert (equal pk' pk') ;
      | _ -> assert false

let test_export_p256 () =
  for _i = 0 to nb_iterations - 1 do
    test_export_p256 ()
  done

let test_export_p256_compressed () =
  match keypair () with
  | None -> assert false
  | Some (sk, pk) ->
      let sk_bytes = to_bytes sk in
      let pk_bytes = to_bytes pk in
      checki __LOC__ sk_size (Bytes.length sk_bytes) ;
      checki __LOC__ compressed_size (Bytes.length pk_bytes) ;
      match sk_of_bytes sk_bytes,
            pk_of_bytes pk_bytes with
      | Some (sk', pk'), Some pk'' ->
          assert (equal sk sk') ;
          assert (equal pk pk') ;
          assert (equal pk pk'') ;
          assert (equal pk' pk') ;
      | _ -> assert false

let test_export_p256_compressed () =
  for _i = 0 to nb_iterations - 1 do
    test_export_p256_compressed ()
  done

let test_keypair_p256 curve =
  match keypair curve with
  | None -> assert false
  | Some (sk, pk) ->
      assert (equal sk sk) ;
      assert (equal pk pk) ;
      let pk' = neuterize sk in
      assert (equal pk pk')

let test_keypair_p256 curve =
  for _i = 0 to nb_iterations - 1 do
    test_keypair_p256 curve
  done

let msg =
  Bytes.of_string "Voulez-vous coucher avec moi, ce soir ?"

let test_sign_p256 () =
  match keypair () with
  | None -> assert false
  | Some (sk, pk) ->
      let signature = Bytes.create pk_size in
      begin match write_sign sk signature ~msg with
        | nb_written when nb_written = pk_size ->
            assert (verify pk ~msg ~signature)
        | _ -> assert false
      end ;
      match sign sk msg with
      | None -> assert false
      | Some signature ->
          assert (verify pk ~msg ~signature)

let test_sign_p256 () =
  for _i = 0 to nb_iterations - 1 do
    test_sign_p256 ()
  done

let basic = [
  "export", `Quick, test_export_p256 ;
  "export_compressed", `Quick, test_export_p256_compressed ;
  "keypair", `Quick, test_keypair_p256 ;
]

let () =
  Alcotest.run "uecc" [
    "basic", basic ;
  ]
