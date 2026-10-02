(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Signer messages
    Invocation:   dune exec src/lib_signer_services/test/main.exe \
                  -- --file test_signer_messages.ml
    Subject:      Binary encoding of the requests sent to a socket signer.
*)

module S = Tezos_crypto.Signature
open Signer_messages

let versions = [S.Version_0; S.Version_1; S.Version_2; S.Version_3]

let pkh_of_algo algo =
  let pkh, _pk, _sk = S.generate_key ~algo () in
  pkh

let pp_pkh ppf = function
  | Pkh pkh -> Format.fprintf ppf "Pkh %a" S.Public_key_hash.pp pkh
  | Pkh_with_version (pkh, _) ->
      Format.fprintf ppf "Pkh_with_version %a" S.Public_key_hash.pp pkh

let equal_pkh a b =
  match (a, b) with
  | Pkh x, Pkh y -> S.Public_key_hash.equal x y
  | Pkh_with_version (x, vx), Pkh_with_version (y, vy) ->
      S.Public_key_hash.equal x y && vx = vy
  | Pkh _, Pkh_with_version _ | Pkh_with_version _, Pkh _ -> false

let pkh_testable = Alcotest.testable pp_pkh equal_pkh

let request pkh =
  Sign.Request.{pkh; data = Bytes.of_string "payload"; signature = None}

let encode pkh =
  Data_encoding.Binary.to_bytes Sign.Request.encoding (request pkh)

let encode_exn pkh =
  match encode pkh with
  | Ok bytes -> bytes
  | Error err ->
      Alcotest.failf
        "%a is not encodable: %a"
        pp_pkh
        pkh
        Data_encoding.Binary.pp_write_error
        err

let decode bytes = Data_encoding.Binary.of_bytes Sign.Request.encoding bytes

(* The first byte of a request is the tag of its public key hash: the [pkh]
   field comes first, and union tags are one byte wide. *)
let tag bytes = Char.code (Bytes.get bytes 0)

(** The tags must stay those of [Signature_V3.raw_encoding], so that a request
    remains readable by a signer linked against a different signature
    version. *)
let test_tags () =
  List.iter
    (fun (algo, expected) ->
      let pkh = Pkh (pkh_of_algo algo) in
      Alcotest.(check int)
        (Format.asprintf "tag of %a" pp_pkh pkh)
        expected
        (tag (encode_exn pkh)))
    [(S.Ed25519, 0); (S.Secp256k1, 1); (S.P256, 2); (S.Bls, 3); (S.Mldsa44, 4)]

(** Every unversioned request round-trips. BLS is the exception: it has no
    unversioned case, so it is normalised to the latest version. *)
let test_roundtrip () =
  List.iter
    (fun algo ->
      let pkh = pkh_of_algo algo in
      let expected =
        match algo with
        | S.Bls -> Pkh_with_version (pkh, S.V_latest.version)
        | _ -> Pkh pkh
      in
      match decode (encode_exn (Pkh pkh)) with
      | Ok decoded ->
          Alcotest.check
            pkh_testable
            (Format.asprintf "round-trip of %a" pp_pkh (Pkh pkh))
            expected
            decoded.pkh
      | Error err ->
          Alcotest.failf
            "cannot decode %a: %a"
            pp_pkh
            (Pkh pkh)
            Data_encoding.Binary.pp_read_error
            err)
    S.algos

(** A versioned BLS request round-trips, version included. *)
let test_bls_version_roundtrip () =
  let pkh = pkh_of_algo S.Bls in
  List.iter
    (fun version ->
      let versioned = Pkh_with_version (pkh, version) in
      match decode (encode_exn versioned) with
      | Ok decoded ->
          Alcotest.check pkh_testable "round-trip" versioned decoded.pkh ;
          let same_version =
            match decoded.pkh with
            | Pkh_with_version (_, v) -> v = version
            | Pkh _ -> false
          in
          Alcotest.(check bool) "version is preserved" true same_version
      | Error err ->
          Alcotest.failf
            "cannot decode a versioned BLS request: %a"
            Data_encoding.Binary.pp_read_error
            err)
    versions

(** [Pkh_with_version] has no case for the other schemes, so building one is a
    programming error that only shows up when the request is written. This is
    the invariant {!Signer_messages.request_pkh} exists to uphold. *)
let test_versioned_non_bls_is_not_encodable () =
  List.iter
    (fun algo ->
      if algo <> S.Bls then
        let pkh = Pkh_with_version (pkh_of_algo algo, S.V_latest.version) in
        match encode pkh with
        | Error _ -> ()
        | Ok _ -> Alcotest.failf "%a should not be encodable" pp_pkh pkh)
    S.algos

(** Whatever {!Signer_messages.request_pkh} builds is encodable and names the
    same key back. This covers every request the socket backend can send. *)
let test_request_pkh_is_always_encodable () =
  List.iter
    (fun algo ->
      let pkh = pkh_of_algo algo in
      List.iter
        (fun version ->
          let built = request_pkh ?version pkh in
          match decode (encode_exn built) with
          | Ok decoded ->
              let decoded_pkh =
                match decoded.pkh with
                | Pkh pkh | Pkh_with_version (pkh, _) -> pkh
              in
              Alcotest.(check string)
                "same key"
                (S.Public_key_hash.to_b58check pkh)
                (S.Public_key_hash.to_b58check decoded_pkh)
          | Error err ->
              Alcotest.failf
                "cannot decode %a: %a"
                pp_pkh
                built
                Data_encoding.Binary.pp_read_error
                err)
        (None :: List.map Option.some versions))
    S.algos

(** The versioned case is reserved for BLS: a peer must not be able to reach it
    with a key of another scheme. All public key hashes are 20 bytes, so a
    versioned BLS request can be turned into such a message by overwriting the
    tag of its inner public key hash. *)
let test_versioned_case_rejects_non_bls () =
  let valid = encode_exn (Pkh_with_version (pkh_of_algo S.Bls, S.Version_3)) in
  Alcotest.(check int) "outer tag is the versioned one" 3 (tag valid) ;
  (* Byte 1 is the tag of the inner [Public_key_hash.encoding], which the
     versioned case embeds whole. *)
  Alcotest.(check int) "inner tag is BLS" 3 (Char.code (Bytes.get valid 1)) ;
  List.iter
    (fun inner_tag ->
      let hostile = Bytes.copy valid in
      Bytes.set hostile 1 (Char.chr inner_tag) ;
      match decode hostile with
      | Error _ -> ()
      | Ok _ ->
          Alcotest.failf
            "the versioned case accepted a public key hash of tag %d"
            inner_tag)
    [0; 1; 2; 4]

let tests =
  [
    Alcotest.test_case "tags" `Quick test_tags;
    Alcotest.test_case "roundtrip" `Quick test_roundtrip;
    Alcotest.test_case "bls_version_roundtrip" `Quick test_bls_version_roundtrip;
    Alcotest.test_case
      "versioned_non_bls_is_not_encodable"
      `Quick
      test_versioned_non_bls_is_not_encodable;
    Alcotest.test_case
      "request_pkh_is_always_encodable"
      `Quick
      test_request_pkh_is_always_encodable;
    Alcotest.test_case
      "versioned_case_rejects_non_bls"
      `Quick
      test_versioned_case_rejects_non_bls;
  ]

let () =
  Alcotest.run
    ~__FILE__
    "tezos-signer-services"
    [("sign_request_encoding", tests)]
