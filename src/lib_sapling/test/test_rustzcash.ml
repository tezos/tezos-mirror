let test_init () = Rustzcash.init_params ()

(* test custom block for ctx *)
let test_ctx_prove () =
  let rec loop n =
    if n <= 0 then ()
    else
      let ctx = Rustzcash.proving_ctx_init () in
      Gc.full_major () ;
      Rustzcash.proving_ctx_free ctx ;
      Gc.full_major () ;
      loop (n - 1)
  in
  loop 100

let test_ctx_verif () =
  let rec loop n =
    if n <= 0 then ()
    else
      let ctx = Rustzcash.verification_ctx_init () in
      Gc.full_major () ;
      Rustzcash.verification_ctx_free ctx ;
      Gc.full_major () ;
      loop (n - 1)
  in
  loop 100

(* Check that an error is raised when calling make_binding_sig with an
   unauthorised amount *)
let test_fail_make_binding_sig () =
  let balance = Int64.succ Rustzcash.max_amount in
  let proving_ctx = Rustzcash.proving_ctx_init () in
  let sighash = Rustzcash.to_sighash @@ Bytes.create 32 in
  try
    let _ = Rustzcash.make_binding_sig proving_ctx ~balance sighash in
    Rustzcash.proving_ctx_free proving_ctx ;
    assert false
  with Invalid_argument _ ->
    Rustzcash.proving_ctx_free proving_ctx ;
    ()

(* Check that an error is raised when calling final_check with an unauthorised
   amount*)
let test_fail_final_check () =
  let verification_ctx = Rustzcash.verification_ctx_init () in
  let balance = Int64.succ Rustzcash.max_amount in
  let sighash = Rustzcash.to_sighash @@ Bytes.create 32 in
  let binding_sig = Rustzcash.to_binding_sig @@ Bytes.create 64 in
  try
    let _ =
      Rustzcash.final_check verification_ctx balance binding_sig sighash
    in
    Rustzcash.verification_ctx_free verification_ctx ;
    assert false
  with Invalid_argument _ ->
    Rustzcash.verification_ctx_free verification_ctx ;
    ()

let test_ivk_to_pkd () =
  let test_inputs =
    [ ( "308bc4ae65c4cac5b53d1b381b44516e345a1cea6188449d203545c542df0a05",
        "ad56e205a5b571a37d55db",
        "b506a8f09503d75d5b2c8695406da89b115e75a29235848544f59d993b7f460b" );
      ( "173f3720483b1c083e473d3e224b332827440a33064024383b0a021925320903",
        "1e4d03294f183f3b234711",
        "d0ca223251c4958f6e08024f7a2ada9fc2dcce7b67cacc5db3976012c6d0c83f" );
      ( "4b2a4a10471f4918021e2e122c1f201e454a052c054e4738061522441d322f00",
        "47440f480f1f35273a4c28",
        "7d2e1a773ba52d3de6ae458fa3076c585e5f01a1d0fa127134efeccb36f25e12" ) ]
  in
  List.iter
    (fun (ivk_hex, diversifier_hex, expected_pkd_hex) ->
      let ivk = `Hex ivk_hex in
      let diversifier = `Hex diversifier_hex in
      let result =
        Tezos_sapling.Rustzcash.(
          of_pkd
            (ivk_to_pkd
               (to_ivk (Hex.to_bytes ivk))
               (Option.get (to_diversifier (Hex.to_bytes diversifier)))))
      in
      let res = Hex.show (Hex.of_bytes result) in
      if not (res = expected_pkd_hex) then
        Alcotest.failf "Expected pkd value %s, got %s" expected_pkd_hex res)
    test_inputs

let test_failing_ivk_to_pkd () =
  (* ivk does not have its last 5 bits set to 0, hence it is not in the ivk domain *)
  let test_inputs =
    [ ( "0254fb145abaae0c9e48ab452da04e961201c0332b3eac4e6bd0bf184c8ebca3",
        "b87451711fd507357488d2" );
      ( "3fdc0e75f7481fd37a299491cb38075a7154a5f2bf0453fab010b2331510ceea",
        "2fcb83c49f854535ac2924" ) ]
  in
  List.iter
    (fun (ivk_hex, diversifier_hex) ->
      let ivk = `Hex ivk_hex in
      let diversifier = `Hex diversifier_hex in
      try
        ( ignore
        @@ Tezos_sapling.Rustzcash.(
             ivk_to_pkd
               (to_ivk (Hex.to_bytes ivk))
               (Option.get (to_diversifier (Hex.to_bytes diversifier)))) ) ;
        assert (
          Alcotest.failf
            "ivk_to_pkd should have failed on ivk input %s. The first 5 bits \
             (if in big endian) should be 0"
            ivk_hex )
      with Assert_failure _ -> ())
    test_inputs

let tests =
  [ ("init", `Quick, test_init);
    ("proving context", `Quick, test_ctx_prove);
    ("verification context", `Quick, test_ctx_verif);
    ("fail binding_sig", `Quick, test_fail_make_binding_sig);
    ("fail final_check", `Quick, test_fail_final_check);
    ("test ivk_to_pkd", `Quick, test_ivk_to_pkd);
    ("test failing ivk_to_pkd", `Quick, test_failing_ivk_to_pkd) ]

let () = Alcotest.run "sapling" [("rustzcash", tests)]
