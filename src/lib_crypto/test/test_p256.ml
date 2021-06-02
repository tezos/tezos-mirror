module type B58CHECK = sig
  type t

  val pp : Format.formatter -> t -> unit

  include S.B58_DATA with type t := t
end

let test_b58check_roundtrip :
    type t. (module B58CHECK with type t = t) -> t -> unit =
 fun m input ->
  let module M = (val m) in
  let testable = Alcotest.testable M.pp ( = ) in
  Roundtrips.test_rt_opt
    "b58check"
    testable
    M.to_b58check
    M.of_b58check_opt
    input

let test_b58check_roundtrips () =
  let (pubkey_hash, pubkey, seckey) = P256.generate_key () in
  test_b58check_roundtrip (module P256.Public_key_hash) pubkey_hash ;
  test_b58check_roundtrip (module P256.Public_key) pubkey ;
  test_b58check_roundtrip (module P256.Secret_key) seckey

let test_b58check_invalid input =
  Roundtrips.test_decode_opt_fail
    "b58check"
    (Alcotest.testable P256.Public_key_hash.pp P256.Public_key_hash.( = ))
    P256.Public_key_hash.of_b58check_opt
    input

let test_b58check_invalids () =
  List.iter
    test_b58check_invalid
    [
      "ThisIsGarbageNotACheck";
      "\x00";
      String.make 1000 '\x00';
      String.make 2048 'a';
      String.init 2048 (fun _ -> Char.chr (Random.int 256));
      "";
    ]

let of_hex hex = Cstruct.(to_bytes (of_hex hex))

let test_pkh_encodings () =
  let test_encoded_pkh (input, test) =
    let encoding = P256.Public_key_hash.b58check_encoding in
    let input = of_hex input in
    let pkh = P256.Public_key_hash.of_bytes_exn input in
    let encoded = Base58.simple_encode encoding pkh in
    assert (String.equal encoded test)
  in
  List.iter test_encoded_pkh Key_encoding_vectors.p256_pkhs

let test_key_encodings () =
  let test_encoded_key (seed, pkh_b58, pk_b58, sk_b58) =
    let seed = of_hex seed in
    let (pkh_test, pk_test, sk_test) = P256.generate_key ~seed () in
    let pkh_test =
      Base58.simple_encode P256.Public_key_hash.b58check_encoding pkh_test
    in
    let pk_test =
      Base58.simple_encode P256.Public_key.b58check_encoding pk_test
    in
    let sk_test =
      Base58.simple_encode P256.Secret_key.b58check_encoding sk_test
    in
    assert (String.equal pkh_test pkh_b58) ;
    assert (String.equal pk_test pk_b58) ;
    assert (String.equal sk_test sk_b58)
  in
  List.iter test_encoded_key Key_encoding_vectors.p256_key_encodings

let tests =
  [
    ("b58check.roundtrip", `Quick, test_b58check_roundtrips);
    ("b58check.invalid", `Slow, test_b58check_invalids);
    ("b58 pkh encodings", `Slow, test_pkh_encodings);
    ("b58 key encodings", `Slow, test_key_encodings);
  ]

let () = Alcotest.run "tezos-crypto" [("p256", tests)]
