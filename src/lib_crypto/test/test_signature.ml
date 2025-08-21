(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

let test_size () =
  let open Signature in
  let length =
    let _pkh, pk, _sk = generate_key ~algo:Ed25519 () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    (WithExceptions.Option.get ~loc:__LOC__
    @@ Data_encoding.Binary.fixed_length Signature.Ed25519.Public_key.encoding)
    + 1
  in
  assert (Compare.Int.(expected = length)) ;
  let length =
    let _pkh, pk, _sk = generate_key ~algo:P256 () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    (WithExceptions.Option.get ~loc:__LOC__
    @@ Data_encoding.Binary.fixed_length Signature.P256.Public_key.encoding)
    + 1
  in
  assert (Compare.Int.(expected = length)) ;
  let length =
    let _pkh, pk, _sk = generate_key ~algo:Secp256k1 () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    (WithExceptions.Option.get ~loc:__LOC__
    @@ Data_encoding.Binary.fixed_length Signature.Secp256k1.Public_key.encoding
    )
    + 1
  in
  assert (Compare.Int.(expected = length)) ;
  let length =
    let _pkh, pk, _sk = generate_key ~algo:Bls () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    (WithExceptions.Option.get ~loc:__LOC__
    @@ Data_encoding.Binary.fixed_length Bls.Public_key.encoding)
    + 1
  in
  assert (Compare.Int.(expected = length))

let test_of_bytes_without_validation () =
  List.iter
    (fun algo ->
      let _pkh, pk, _sk = Signature.generate_key ~algo () in
      let bytes =
        Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding pk
      in
      let pk2 = Signature.Public_key.of_bytes_without_validation bytes in
      assert (Some pk = pk2))
    [Ed25519; Secp256k1; P256; Bls]

let secp256k1_sig_to_hex signature =
  Hex.show @@ Hex.of_string
  @@ Data_encoding.Binary.to_string_exn Signature.Secp256k1.encoding signature

let get_secp256k1_secret_key sk =
  Option.value_f ~default:(fun () -> failwith "Invalid secret key.")
  @@ Option.bind
       (Hex.to_string (`Hex sk))
       (Data_encoding.Binary.of_string_opt
          Signature.Secp256k1.Secret_key.encoding)

let test_secp256k1_keccak256 () =
  let get_signed_msgs secret_key =
    List.map
      (fun msg ->
        Signature.Secp256k1.sign_keccak256 secret_key (String.to_bytes msg))
      Vectors_secp256k1_keccak256.msgs
  in
  let verify_signatures ~pk ~sigs =
    Result.value_f ~default:(fun () ->
        failwith "Signature verification failed.")
    @@ List.iter2
         ~when_different_lengths:(fun () ->
           failwith "Different lengths in the given signatures.")
         (fun msg expected_sig ->
           let check_outcome =
             Signature.Secp256k1.check_keccak256
               pk
               expected_sig
               (String.to_bytes msg)
           in
           Alcotest.(
             check bool "verify secp256k1-keccak256 sigs" check_outcome true))
         Vectors_secp256k1_keccak256.msgs
         sigs
  in
  List.iteri
    (fun i key ->
      let sk = get_secp256k1_secret_key key in
      let pk = Signature.Secp256k1.Secret_key.to_public_key sk in
      let sigs = get_signed_msgs sk in
      let hex_sigs = List.map secp256k1_sig_to_hex sigs in
      let expected_sigs = Vectors_secp256k1_keccak256.sigs.(i) in
      verify_signatures ~pk ~sigs ;
      Alcotest.(
        check
          (list string)
          "equal secp256k1-keccak256 sigs"
          hex_sigs
          expected_sigs))
    Vectors_secp256k1_keccak256.keys

let tests =
  [
    ( "signature",
      [
        ("size", `Quick, test_size);
        ( "test_of_bytes_without_validation",
          `Quick,
          test_of_bytes_without_validation );
        ( "secp256k1-keccak256 scheme implementation integrity",
          `Quick,
          test_secp256k1_keccak256 );
      ] );
  ]
