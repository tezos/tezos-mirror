(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Crypto
    Invocation:   dune exec src/lib_crypto/test/main.exe
    Dependencies: src/lib_crypto/test/roundtrips.ml
    Subject:      Encoding in Base58
*)

let test_roundtrip_safe input =
  Roundtrips.test_rt_opt
    "safe base58"
    Alcotest.string
    Base58.safe_encode
    Base58.safe_decode
    input

let test_roundtrip_raw input =
  Roundtrips.test_rt_opt
    "raw base58"
    Alcotest.string
    Base58.raw_encode
    Base58.raw_decode
    input

let inputs =
  [
    "abc";
    string_of_int max_int;
    "0";
    "00";
    "000";
    "0000";
    "0000000000000000";
    String.make 64 '0';
    "1";
    "11";
    "111";
    "1111";
    String.make 2048 '0';
    "2";
    "22";
    "5";
    "Z";
    String.make 2048 'Z';
    "z";
    "zz";
    "zzzzzzzz";
    String.make 2048 'z';
    (*loads of ascii characters: codes between 32 and 126 *)
    String.init 1000 (fun i -> Char.chr (32 + (i mod (126 - 32))));
    "";
  ]

(** Safe Base58Check-encoding then decoding (provided with error
    detections)
*)
let test_roundtrip_safes () = List.iter test_roundtrip_safe inputs

(** Base58-encoding then decoding (no error detection) *)
let test_roundtrip_raws () = List.iter test_roundtrip_raw inputs

let test_safety input =
  Roundtrips.test_decode_opt_safe
    "safe base58"
    Alcotest.string
    Base58.safe_decode
    input

(** Safe Base58Check-decoding (provided with error detections) *)
let test_safetys () = List.iter test_safety inputs

(** Decoding is bounded to [Base58.encoded_max_length] unconditionally: an
    input longer than that is rejected before the quadratic [raw_decode]
    runs, even when its checksum is valid. Inputs within the bound are
    unaffected. *)
let test_default_bound () =
  let cap = Base58.encoded_max_length in
  (* A genuinely valid Base58Check string longer than the default bound. *)
  let long = String.make 7000 '\xff' in
  let long_encoded = Base58.safe_encode long in
  Alcotest.(check bool)
    "test payload exceeds the default bound"
    true
    (String.length long_encoded > cap) ;
  Alcotest.(check (option string))
    "long valid input is rejected"
    None
    (Base58.safe_decode long_encoded) ;
  (* A valid encoding within the bound decodes fine. *)
  let short = String.make 100 '\xff' in
  let short_encoded = Base58.safe_encode short in
  Alcotest.(check bool)
    "control payload is within the default bound"
    true
    (String.length short_encoded <= cap) ;
  Alcotest.(check (option string))
    "short input decodes"
    (Some short)
    (Base58.safe_decode short_encoded)

(** [register_encoding] refuses any encoding longer than
    [Base58.encoded_max_length], so the default decode bound can never reject a
    valid encoding. *)
let test_register_encoding_guard () =
  (* [length] bytes encode to [~length * 1.37] Base58 characters, so a 7000-byte
     payload exceeds the [8192] cap. *)
  let module M = Base58.Make (struct
    type context = unit
  end) in
  match
    M.register_encoding
      ~prefix:"\255\255\255"
      ~length:7000
      ~to_raw:(fun s -> s)
      ~of_raw:(fun s -> Some s)
      ~wrap:(fun _ -> assert false)
  with
  | _ -> Alcotest.fail "registering an over-long encoding should be rejected"
  | exception Invalid_argument msg ->
      (* [register_encoding] raises [Invalid_argument] for other reasons too
         (variable-length or ambiguous prefix): require the length guard. *)
      if not (msg =~ rex "exceeds the maximum") then
        Alcotest.failf
          "register_encoding rejected the encoding for an unexpected reason: %s"
          msg

(** Documents the worst-case wall-clock of a single decode at the default bound:
    [raw_decode] is O(n^2), so this is the ceiling a caller pays on
    attacker-controlled input. The assertion is a loose regression guard. *)
let test_decode_cost_at_bound () =
  let s = String.make Base58.encoded_max_length 'z' in
  (* warm up *)
  ignore (Base58.safe_decode s) ;
  let iters = 20 in
  let t0 = Sys.time () in
  for _ = 1 to iters do
    ignore (Base58.safe_decode s)
  done ;
  let ms = (Sys.time () -. t0) /. float_of_int iters *. 1000. in
  Format.printf
    "@[<v>Base58 decode of a %d-char input (default bound): %.3f ms@]@."
    Base58.encoded_max_length
    ms ;
  Alcotest.(check bool) "decode stays well under one second" true (ms < 1000.)

(** For every currently-registered Base58Check encoding, [prefix ^ payload]
    round-trips exactly through {!Base58.safe_encode}/{!Base58.safe_decode},
    and the resulting encoded string is exactly [encoded_length] characters
    -- the invariant the default bound relies on (see
    [Base58.simple_decode]'s doc comment): a valid encoding of any
    registered type is always exactly [encoded_length] characters, never
    more. This only exercises the base58/checksum/prefix layer, not each
    type's own [to_raw]/[of_raw] semantics (which vary per type, validate
    type-specific structure such as curve points, and so aren't generically
    testable by iterating the registry alone). *)
let test_registered_encodings_roundtrip () =
  let payload_byte = '\x2a' in
  Base58.Internal_for_tests.iter_encodings
    (fun ~prefix ~length ~encoded_length ->
      let raw = prefix ^ String.make length payload_byte in
      let encoded = Base58.safe_encode raw in
      Alcotest.(check int)
        (Printf.sprintf "encoded length for prefix %S" prefix)
        encoded_length
        (String.length encoded) ;
      Alcotest.(check (option string))
        (Printf.sprintf "roundtrip for prefix %S" prefix)
        (Some raw)
        (Base58.safe_decode encoded))

(** A checksum-valid string carrying a registered binary prefix but a payload
    of the wrong length must be rejected ([None]), not crash with
    [Assert_failure]. The default length bound does not catch such inputs when
    the payload is *shorter* than expected (the text is then shorter than
    [encoded_length]), nor, for the generic {!Base58.decode}, when it is only
    slightly longer (the text stays below [max_encoded_size ()]). Covers every
    registered encoding through {!Base58.decode}, for an empty payload, a
    payload one byte short and one byte too long. *)
let test_registered_encodings_wrong_length () =
  let payload_byte = '\x2a' in
  Base58.Internal_for_tests.iter_encodings
    (fun ~prefix ~length ~encoded_length:_ ->
      List.iter
        (fun k ->
          let forged =
            Base58.safe_encode (prefix ^ String.make k payload_byte)
          in
          let decoded = try Ok (Base58.decode forged) with exn -> Error exn in
          match decoded with
          | Ok None -> ()
          | Ok (Some _) ->
              Alcotest.failf
                "prefix %S, payload of %d bytes instead of %d: decoded"
                prefix
                k
                length
          | Error exn ->
              Alcotest.failf
                "prefix %S, payload of %d bytes instead of %d: raised %s"
                prefix
                k
                length
                (Printexc.to_string exn))
        [0; length - 1; length + 1])

(** Same as {!test_registered_encodings_wrong_length}, through
    {!Base58.simple_decode} and the [of_b58check_opt] functions built on it
    (or, for the [Signature] curve unions, on {!Base58.decode}): an undersized
    payload gives a text shorter than [encoded_length], so the upfront length
    bound lets it through to [of_raw]. *)
let test_simple_decode_undersized () =
  let check name of_b58check_opt prefix length =
    List.iter
      (fun k ->
        let forged = Base58.safe_encode (prefix ^ String.make k '\x7a') in
        let decoded = try Ok (of_b58check_opt forged) with exn -> Error exn in
        match decoded with
        | Ok None -> ()
        | Ok (Some _) ->
            Alcotest.failf "%s: %d-byte payload %S decoded" name k forged
        | Error exn ->
            Alcotest.failf
              "%s: %d-byte payload %S raised %s"
              name
              k
              forged
              (Printexc.to_string exn))
      [0; length - 1]
  in
  let module Chain_id = Hashed.Chain_id in
  check "Chain_id" Chain_id.of_b58check_opt Base58.Prefix.chain_id Chain_id.size ;
  check
    "Chain_id (simple_decode)"
    (Base58.simple_decode Chain_id.b58check_encoding)
    Base58.Prefix.chain_id
    Chain_id.size ;
  check
    "Ed25519.Public_key_hash"
    Signature.Ed25519.Public_key_hash.of_b58check_opt
    Base58.Prefix.ed25519_public_key_hash
    Signature.Ed25519.Public_key_hash.size ;
  check
    "Ed25519.Public_key"
    Signature.Ed25519.Public_key.of_b58check_opt
    Base58.Prefix.ed25519_public_key
    (* Ed25519 public keys are 32 bytes. *) 32 ;
  check
    "Signature.Public_key_hash"
    Signature.Public_key_hash.of_b58check_opt
    Base58.Prefix.ed25519_public_key_hash
    Signature.Ed25519.Public_key_hash.size ;
  check
    "Signature.Public_key"
    Signature.Public_key.of_b58check_opt
    Base58.Prefix.ed25519_public_key
    (* Ed25519 public keys are 32 bytes. *) 32

(* Checksum-valid strings carrying the raw prefix ["\255\255"] of the
   placeholder encodings registered by the [Signature] and
   [Aggregate_signature] modules, with a payload of the registered length
   (2 bytes). *)
let placeholder_inputs =
  List.map
    (fun payload -> Base58.safe_encode ("\255\255" ^ payload))
    ["\000\000"; "\001\002"; "\255\255"]

(** The placeholder encodings are never meant to decode anything: a
    checksum-valid string with their prefix and length must be rejected
    ([None]) by {!Base58.decode} and by the [of_b58check_opt] functions built
    on it, not crash with [Assert_failure]. *)
let test_placeholder_prefix () =
  List.iter
    (fun s ->
      let check name f =
        Alcotest.(check bool) (name ^ " " ^ s) true (f s = None)
      in
      check "Base58.decode" (fun s -> Option.map ignore (Base58.decode s)) ;
      let check_sig name (of_pkh, of_pk, of_sk, of_sig) =
        check (name ^ ".Public_key_hash") (fun s ->
            Option.map ignore (of_pkh s)) ;
        check (name ^ ".Public_key") (fun s -> Option.map ignore (of_pk s)) ;
        check (name ^ ".Secret_key") (fun s -> Option.map ignore (of_sk s)) ;
        check name (fun s -> Option.map ignore (of_sig s))
      in
      check_sig
        "Signature.V0"
        Signature.V0.
          ( Public_key_hash.of_b58check_opt,
            Public_key.of_b58check_opt,
            Secret_key.of_b58check_opt,
            of_b58check_opt ) ;
      check_sig
        "Signature.V1"
        Signature.V1.
          ( Public_key_hash.of_b58check_opt,
            Public_key.of_b58check_opt,
            Secret_key.of_b58check_opt,
            of_b58check_opt ) ;
      check_sig
        "Signature.V2"
        Signature.V2.
          ( Public_key_hash.of_b58check_opt,
            Public_key.of_b58check_opt,
            Secret_key.of_b58check_opt,
            of_b58check_opt ) ;
      check_sig
        "Signature.V3"
        Signature.V3.
          ( Public_key_hash.of_b58check_opt,
            Public_key.of_b58check_opt,
            Secret_key.of_b58check_opt,
            of_b58check_opt ) ;
      check_sig
        "Aggregate_signature"
        Aggregate_signature.
          ( Public_key_hash.of_b58check_opt,
            Public_key.of_b58check_opt,
            Secret_key.of_b58check_opt,
            of_b58check_opt ))
    ("jpV15smzBkM" :: placeholder_inputs)

(** For every encoding in the global registry of this test binary,
    {!Base58.decode} never raises on a checksum-valid string carrying its
    prefix, whatever the payload length, including the registered one.
    This only covers the encodings registered by the modules linked into
    this binary, not all of lib_crypto's. It covers none of the protocols'
    encodings either: each protocol environment registers them in its own
    registry, not in the global one.
    Unlike {!test_registered_encodings_wrong_length}, the decoded value is
    not checked: a payload of the right length may legitimately decode. *)
let test_registered_encodings_never_raise () =
  Base58.Internal_for_tests.iter_encodings
    (fun ~prefix ~length ~encoded_length:_ ->
      List.iter
        (fun k ->
          List.iter
            (fun b ->
              let forged = Base58.safe_encode (prefix ^ String.make k b) in
              match Base58.decode forged with
              | _ -> ()
              | exception exn ->
                  Alcotest.failf
                    "prefix %S, payload of %d x %C: raised %s"
                    prefix
                    k
                    b
                    (Printexc.to_string exn))
            ['\000'; '\x2a'; '\xff'])
        [0; length - 1; length; length + 1])

let tests : unit Alcotest.test list =
  [
    ( "base58",
      [
        ("safe decoding", `Quick, test_safetys);
        ("safe encoding/decoding", `Quick, test_roundtrip_safes);
        ("raw encoding/decoding", `Quick, test_roundtrip_raws);
        ("default bound", `Quick, test_default_bound);
        ("register_encoding length guard", `Quick, test_register_encoding_guard);
        ("decode cost at default bound", `Quick, test_decode_cost_at_bound);
        ( "registered encodings roundtrip",
          `Quick,
          test_registered_encodings_roundtrip );
        ( "registered encodings reject wrong payload length",
          `Quick,
          test_registered_encodings_wrong_length );
        ( "simple_decode rejects undersized payload",
          `Quick,
          test_simple_decode_undersized );
        ("placeholder prefix", `Quick, test_placeholder_prefix);
        ( "registered encodings never raise",
          `Quick,
          test_registered_encodings_never_raise );
      ] );
  ]
