module type T = sig
  val tests : unit Alcotest.test list
end

type t = (module T)

let runtest l =
  let _ =
    Alcotest.run
      "tezos-crypto"
      (List.fold_left
         (fun acc (e : t) ->
           let module M : T = (val e) in
           List.append M.tests acc)
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
    (module Test_merkle);
    (module Test_p256);
    (module Test_pvss);
    (module Test_signature);
    (module Test_timelock);
    (module Test_context_hash);
    (module Test_bls12_381);
  ]
