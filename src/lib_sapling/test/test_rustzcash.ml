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

let tests =
  [ ("init", `Quick, test_init);
    ("proving context", `Quick, test_ctx_prove);
    ("verification context", `Quick, test_ctx_verif);
    ("fail binding_sig", `Quick, test_fail_make_binding_sig);
    ("fail final_check", `Quick, test_fail_final_check) ]

let () = Alcotest.run "sapling" [("rustzcash", tests)]
