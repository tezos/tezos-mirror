module Alcotest = Alcotest_glue

module type T = sig
  val tests : unit Alcotest.test list

  val tests_lwt : unit Alcotest_lwt.test list
end

type t = (module T)

let runtest l =
  let _ =
    Alcotest.run
      "tesoz-crypto"
      (List.fold_left
         (fun acc (e : t) ->
           let module M : T = (val e) in
           List.append M.tests acc)
         []
         l)
  in
  let _ =
    Lwt_main.run
    @@ Alcotest_lwt.run
         "tesoz-crypto-lwt"
         (List.fold_left
            (fun acc (e : t) ->
              let module M : T = (val e) in
              List.append M.tests_lwt acc)
            []
            l)
  in
  ()

;;
runtest
  [
    (module Test_base58);
    (module Test_blake2b);
    (module Test_crypto_box);
    (module Test_deterministic_nonce);
    (module Test_ed25519);
    (module Test_hacl);
    (module Test_merkle);
    (module Test_p256);
    (module Test_pvss);
    (module Test_signature);
    (module Test_timelock);
  ]
