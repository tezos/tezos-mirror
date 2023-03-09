(* Test the [Rustzcash.find_params] function. *)
let test_find_params () =
  (* Helper function to quickly define tests. *)
  let test ~env ~files ?(cwd = ".") expected_result =
    let fail x =
      Printf.ksprintf
        (fun s ->
          Alcotest.failf
            "%s with environment [%s] and files [%s]"
            s
            (List.map (fun (n, v) -> n ^ ", " ^ v) env |> String.concat "; ")
            (String.concat "; " files))
        x
    in
    try
      let {Rustzcash.spend_path; output_path} =
        Rustzcash.find_params
          ~getenv_opt:(fun name -> List.assoc_opt name env)
          ~getcwd:(fun () -> cwd)
          ~file_exists:(fun path -> List.mem path files)
          ()
      in
      match expected_result with
      | None -> fail "Did not expect to find Zcash parameter files"
      | Some (expected_spend_path, expected_output_path) ->
          if spend_path <> expected_spend_path then
            fail
              "Expected spend_path = %S, got %S"
              expected_spend_path
              spend_path ;
          if output_path <> expected_output_path then
            fail
              "Expected output_path = %S, got %S"
              expected_output_path
              output_path
    with Rustzcash.Params_not_found _ -> (
      match expected_result with
      | None -> ()
      | Some _ -> fail "Expected to find Zcash parameter files")
  in
  (* Test cases where parameter files are installed in one location
     and we expect to find them. *)
  test
    ~env:[("XDG_DATA_HOME", "~/xdg")]
    ~files:
      [
        "~/xdg/.local/share/zcash-params/sapling-spend.params";
        "~/xdg/.local/share/zcash-params/sapling-output.params";
      ]
    (Some
       ( "~/xdg/.local/share/zcash-params/sapling-spend.params",
         "~/xdg/.local/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:[("XDG_DATA_DIRS", "/xdg/data")]
    ~files:
      [
        "/xdg/data/zcash-params/sapling-spend.params";
        "/xdg/data/zcash-params/sapling-output.params";
      ]
    (Some
       ( "/xdg/data/zcash-params/sapling-spend.params",
         "/xdg/data/zcash-params/sapling-output.params" )) ;
  test
    ~env:[("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2")]
    ~files:
      [
        "/xdg/data1/zcash-params/sapling-spend.params";
        "/xdg/data1/zcash-params/sapling-output.params";
      ]
    (Some
       ( "/xdg/data1/zcash-params/sapling-spend.params",
         "/xdg/data1/zcash-params/sapling-output.params" )) ;
  test
    ~env:[("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2")]
    ~files:
      [
        "/xdg/data2/zcash-params/sapling-spend.params";
        "/xdg/data2/zcash-params/sapling-output.params";
      ]
    (Some
       ( "/xdg/data2/zcash-params/sapling-spend.params",
         "/xdg/data2/zcash-params/sapling-output.params" )) ;
  test
    ~env:[("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2:::")]
    ~files:
      [
        "/xdg/data2/zcash-params/sapling-spend.params";
        "/xdg/data2/zcash-params/sapling-output.params";
      ]
    (Some
       ( "/xdg/data2/zcash-params/sapling-spend.params",
         "/xdg/data2/zcash-params/sapling-output.params" )) ;
  test
    ~env:[("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0")]
    ~files:
      [
        "~/.opam/4.10.0/share/zcash-params/sapling-spend.params";
        "~/.opam/4.10.0/share/zcash-params/sapling-output.params";
      ]
    (Some
       ( "~/.opam/4.10.0/share/zcash-params/sapling-spend.params",
         "~/.opam/4.10.0/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:[("PWD", "~/fake-pwd")]
    ~files:
      [
        "~/fake-pwd/_opam/share/zcash-params/sapling-spend.params";
        "~/fake-pwd/_opam/share/zcash-params/sapling-output.params";
      ]
    (Some
       ( "~/fake-pwd/_opam/share/zcash-params/sapling-spend.params",
         "~/fake-pwd/_opam/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:[]
    ~files:
      [
        "./_opam/share/zcash-params/sapling-spend.params";
        "./_opam/share/zcash-params/sapling-output.params";
      ]
    (Some
       ( "./_opam/share/zcash-params/sapling-spend.params",
         "./_opam/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:[("HOME", "~")]
    ~files:
      [
        "~/.zcash-params/sapling-spend.params";
        "~/.zcash-params/sapling-output.params";
      ]
    (Some
       ( "~/.zcash-params/sapling-spend.params",
         "~/.zcash-params/sapling-output.params" )) ;
  test
    ~env:[("HOME", "~")]
    ~files:
      [
        "~/.local/share/zcash-params/sapling-spend.params";
        "~/.local/share/zcash-params/sapling-output.params";
      ]
    (Some
       ( "~/.local/share/zcash-params/sapling-spend.params",
         "~/.local/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:[]
    ~files:
      [
        "/usr/local/share/zcash-params/sapling-spend.params";
        "/usr/local/share/zcash-params/sapling-output.params";
      ]
    (Some
       ( "/usr/local/share/zcash-params/sapling-spend.params",
         "/usr/local/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:[]
    ~files:
      [
        "/usr/share/zcash-params/sapling-spend.params";
        "/usr/share/zcash-params/sapling-output.params";
      ]
    (Some
       ( "/usr/share/zcash-params/sapling-spend.params",
         "/usr/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:[("HOMEBREW_PREFIX", "/opt/homebrew")]
    ~files:
      [
        "/opt/homebrew/share/zcash-params/sapling-spend.params";
        "/opt/homebrew/share/zcash-params/sapling-output.params";
      ]
    (Some
       ( "/opt/homebrew/share/zcash-params/sapling-spend.params",
         "/opt/homebrew/share/zcash-params/sapling-output.params" )) ;
  (* Test cases where parameter files are installed in several locations
     and we expect to find the one with higher priority. *)
  let files =
    [
      "~/xdg/.local/share/zcash-params/sapling-spend.params";
      "~/xdg/.local/share/zcash-params/sapling-output.params";
      "/xdg/data1/zcash-params/sapling-spend.params";
      "/xdg/data1/zcash-params/sapling-output.params";
      "/xdg/data2/zcash-params/sapling-spend.params";
      "/xdg/data2/zcash-params/sapling-output.params";
      "~/.opam/4.10.0/share/zcash-params/sapling-spend.params";
      "~/.opam/4.10.0/share/zcash-params/sapling-output.params";
      "~/fake-pwd/_opam/share/zcash-params/sapling-spend.params";
      "~/fake-pwd/_opam/share/zcash-params/sapling-output.params";
      "./_opam/share/zcash-params/sapling-spend.params";
      "./_opam/share/zcash-params/sapling-output.params";
      "~/.zcash-params/sapling-spend.params";
      "~/.zcash-params/sapling-output.params";
      "~/.local/share/zcash-params/sapling-spend.params";
      "~/.local/share/zcash-params/sapling-output.params";
      "/usr/local/share/zcash-params/sapling-spend.params";
      "/usr/local/share/zcash-params/sapling-output.params";
      "/usr/share/zcash-params/sapling-spend.params";
      "/usr/share/zcash-params/sapling-output.params";
    ]
  in
  test
    ~env:
      [
        ("XDG_DATA_HOME", "~/xdg");
        ("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0");
        ("HOME", "~");
      ]
    ~files
    (Some
       ( "~/xdg/.local/share/zcash-params/sapling-spend.params",
         "~/xdg/.local/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:
      [
        ("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0");
        ("HOME", "~");
      ]
    ~files
    (Some
       ( "/xdg/data1/zcash-params/sapling-spend.params",
         "/xdg/data1/zcash-params/sapling-output.params" )) ;
  test
    ~env:
      [
        ("XDG_DATA_DIRS", "/xdg/data2:/xdg/data1");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0");
        ("HOME", "~");
      ]
    ~files
    (Some
       ( "/xdg/data2/zcash-params/sapling-spend.params",
         "/xdg/data2/zcash-params/sapling-output.params" )) ;
  test
    ~env:
      [
        ("XDG_DATA_DIRS", "/xdg/inexisting");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0");
        ("HOME", "~");
      ]
    ~files
    (Some
       ( "~/.opam/4.10.0/share/zcash-params/sapling-spend.params",
         "~/.opam/4.10.0/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:
      [
        ("XDG_DATA_HOME", "~/xdg/inexisting");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.11.0");
        ("HOME", "/root");
      ]
    ~files
    (Some
       ( "./_opam/share/zcash-params/sapling-spend.params",
         "./_opam/share/zcash-params/sapling-output.params" )) ;
  test
    ~env:
      [
        ("XDG_DATA_HOME", "~/xdg/inexisting");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.11.0");
        ("HOME", "/root");
      ]
    ~cwd:"/pwd"
    ~files
    (Some
       ( "/usr/local/share/zcash-params/sapling-spend.params",
         "/usr/local/share/zcash-params/sapling-output.params" )) ;
  (* Test cases where parameter files are installed in several locations
     but one of them only contains one of the two files and we expect to
     use the location with lower priority. *)
  test
    ~env:
      [
        ("XDG_DATA_HOME", "~/xdg");
        ("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0");
        ("HOME", "~");
      ]
    ~files:
      [
        "~/xdg/.local/share/zcash-params/sapling-output.params";
        "/xdg/data1/zcash-params/sapling-spend.params";
        "/xdg/data1/zcash-params/sapling-output.params";
        "/xdg/data2/zcash-params/sapling-spend.params";
        "/xdg/data2/zcash-params/sapling-output.params";
        "~/.opam/4.10.0/share/zcash-params/sapling-spend.params";
      ]
    (Some
       ( "/xdg/data1/zcash-params/sapling-spend.params",
         "/xdg/data1/zcash-params/sapling-output.params" )) ;
  test
    ~env:
      [
        ("XDG_DATA_HOME", "~/xdg");
        ("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0");
        ("HOME", "~");
      ]
    ~files:
      [
        "~/xdg/.local/share/zcash-params/sapling-output.params";
        "/xdg/data1/zcash-params/sapling-spend.params";
        "/xdg/data2/zcash-params/sapling-output.params";
        "~/.opam/4.10.0/share/zcash-params/sapling-spend.params";
        "/usr/share/zcash-params/sapling-spend.params";
        "/usr/share/zcash-params/sapling-output.params";
      ]
    (Some
       ( "/usr/share/zcash-params/sapling-spend.params",
         "/usr/share/zcash-params/sapling-output.params" )) ;
  (* Test cases where we do not expect to find parameter files. *)
  test
    ~env:
      [
        ("XDG_DATA_HOME", "~/xdg");
        ("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0");
        ("HOME", "~");
      ]
    ~files:[]
    None ;
  test
    ~env:
      [
        ("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0");
        ("HOME", "~");
      ]
    ~files:
      [
        "~/xdg/.local/share/zcash-params/sapling-output.params";
        "~/xdg/.local/share/zcash-params/sapling-output.params";
      ]
    None ;
  test
    ~env:
      [
        ("XDG_DATA_HOME", "~/xdg");
        ("XDG_DATA_DIRS", "/xdg/data1:/xdg/data2");
        ("OPAM_SWITCH_PREFIX", "~/.opam/4.10.0");
        ("HOME", "~");
      ]
    ~files:
      [
        "~/xdg/.local/share/zcash-params/sapling-output.params";
        "/xdg/data1/zcash-params/sapling-spend.params";
        "/xdg/data2/zcash-params/sapling-output.params";
        "~/.opam/4.10.0/share/zcash-params/sapling-spend.params";
        "/usr/share/zcash-params/sapling-output.params";
      ]
    None ;
  ()

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
    [
      ( "308bc4ae65c4cac5b53d1b381b44516e345a1cea6188449d203545c542df0a05",
        "ad56e205a5b571a37d55db",
        "b506a8f09503d75d5b2c8695406da89b115e75a29235848544f59d993b7f460b" );
      ( "173f3720483b1c083e473d3e224b332827440a33064024383b0a021925320903",
        "1e4d03294f183f3b234711",
        "d0ca223251c4958f6e08024f7a2ada9fc2dcce7b67cacc5db3976012c6d0c83f" );
      ( "4b2a4a10471f4918021e2e122c1f201e454a052c054e4738061522441d322f00",
        "47440f480f1f35273a4c28",
        "7d2e1a773ba52d3de6ae458fa3076c585e5f01a1d0fa127134efeccb36f25e12" );
    ]
  in
  List.iter
    (fun (ivk_hex, diversifier_hex, expected_pkd_hex) ->
      let ivk = `Hex ivk_hex in
      let diversifier = `Hex diversifier_hex in
      let result =
        Tezos_sapling.Rustzcash.(
          of_pkd
            (ivk_to_pkd
               (to_ivk (Hex.to_bytes_exn ivk))
               (Option.get (to_diversifier (Hex.to_bytes_exn diversifier)))))
      in
      let res = Hex.show (Hex.of_bytes result) in
      if not (res = expected_pkd_hex) then
        Alcotest.failf "Expected pkd value %s, got %s" expected_pkd_hex res)
    test_inputs

let test_failing_ivk_to_pkd () =
  (* ivk does not have its last 5 bits set to 0, hence it is not in the ivk domain *)
  let test_inputs =
    [
      ( "0254fb145abaae0c9e48ab452da04e961201c0332b3eac4e6bd0bf184c8ebca3",
        "b87451711fd507357488d2" );
      ( "3fdc0e75f7481fd37a299491cb38075a7154a5f2bf0453fab010b2331510ceea",
        "2fcb83c49f854535ac2924" );
    ]
  in
  List.iter
    (fun (ivk_hex, diversifier_hex) ->
      let ivk = `Hex ivk_hex in
      let diversifier = `Hex diversifier_hex in
      try
        (ignore
        @@ Tezos_sapling.Rustzcash.(
             ivk_to_pkd
               (to_ivk (Hex.to_bytes_exn ivk))
               (Option.get (to_diversifier (Hex.to_bytes_exn diversifier))))) ;
        assert (
          Alcotest.failf
            "ivk_to_pkd should have failed on ivk input %s. The first 5 bits \
             (if in big endian) should be 0"
            ivk_hex)
      with Assert_failure _ -> ())
    test_inputs

let tests =
  [
    ("find params", `Quick, test_find_params);
    ("init", `Quick, test_init);
    ("proving context", `Quick, test_ctx_prove);
    ("verification context", `Quick, test_ctx_verif);
    ("fail binding_sig", `Quick, test_fail_make_binding_sig);
    ("fail final_check", `Quick, test_fail_final_check);
    ("test ivk_to_pkd", `Quick, test_ivk_to_pkd);
    ("test failing ivk_to_pkd", `Quick, test_failing_ivk_to_pkd);
  ]

let () = Alcotest.run ~__FILE__ "sapling" [("rustzcash", tests)]
