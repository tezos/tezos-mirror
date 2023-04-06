(** Testing
    -------
    Component:    Client Base
    Invocation:   dune exec src/lib_client_base/test/main.exe
    Subject:      On Password-Based Key Derivation Function 2 (PBKDF2)
                  when using hash functions SHA256 and SHA512.
*)

(* PBKDF2 *)
let test_pbkdf2 (module A : Tezos_crypto.Hacl.Hash.S) ~password ~salt ~count
    ~dk_len ~dk =
  let module P = Pbkdf.Make (A) in
  let salt = Tezos_stdlib.Hex.to_bytes_exn (`Hex salt) in
  let dk = Tezos_stdlib.Hex.to_bytes_exn (`Hex dk) in
  let password = Bytes.of_string password in
  fun () ->
    let edk = P.pbkdf2 ~password ~salt ~count ~dk_len in
    let sedk = Bytes.to_string edk in
    let sdk = Bytes.to_string dk in
    Alcotest.check Alcotest.string "PBKDF2 test" sedk sdk

(* let test_pbkdf2_invalid_arg ~prf ~password ~salt ~count ~dk_len ~msg () =
 *   let salt = Nocrypto.Uncommon.Cs.of_hex salt
 *   and password = Cstruct.of_string password
 *   in
 *   Alcotest.check_raises
 *     msg
 *     (Invalid_argument msg)
 *     (fun () -> ignore (Pbkdf.pbkdf2 ~prf ~password ~salt ~count ~dk_len)) *)

(* Taken from https://github.com/randombit/botan/blob/master/src/tests/data/pbkdf/pbkdf2.vec *)

(** Using SHA256 hash. From password "xyz" and salt
   "0001020304050607", iterates 10,000 times the derivation algorithm
   to derive a 48-byte key. The expected derived key shall equal to
   [~dk].
*)
let pbkdf2_test11 =
  test_pbkdf2
    (module Tezos_crypto.Hacl.Hash.SHA256)
    ~password:"xyz"
    ~salt:"0001020304050607"
    ~count:10000
    ~dk_len:48l
    ~dk:
      "defd2987fa26a4672f4d16d98398432ad95e896bf619f6a6b8d4ed1faf98e8b531b39ffb66966d0e115a6cd8e70b72d0"

(** Using SHA512 hash. From password "xyz" and salt
   "0001020304050607", iterates 10,000 times the derivation algorithm
   to derive a 48-byte key. The expected derived key shall equal to
   [~dk].
*)
let pbkdf2_test13 =
  test_pbkdf2
    (module Tezos_crypto.Hacl.Hash.SHA512)
    ~password:"xyz"
    ~salt:"0001020304050607"
    ~count:10000
    ~dk_len:48l
    ~dk:
      "daf8a734327745eb63d19054dbd4018a682cef11086a1bfb63fdbc16158c2f8b0742802f36aef1b1df92accbea5d31a5"

let pbkdf2_tests =
  [
    ("Test Case 11", `Quick, pbkdf2_test11);
    ("Test Case 13", `Quick, pbkdf2_test13);
  ]

let () = Alcotest.run ~__FILE__ "PBKDF Tests" [("PBKDF2 tests", pbkdf2_tests)]
