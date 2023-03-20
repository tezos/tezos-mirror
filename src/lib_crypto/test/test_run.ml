module type T = sig
  val tests : unit Alcotest.test list
end

type t = (module T)

let runtest l =
  let _ =
    Alcotest.run
      ~__FILE__
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
    (module Test_merkle);
    (module Test_signature);
    (module Test_signature_encodings);
    (module Test_timelock_legacy);
    (module Test_context_hash);
  ]
