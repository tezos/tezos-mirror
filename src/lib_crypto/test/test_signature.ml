(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
    let (_pkh, pk, _sk) = generate_key ~algo:Ed25519 () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    (WithExceptions.Option.get ~loc:__LOC__
    @@ Data_encoding.Binary.fixed_length Ed25519.Public_key.encoding)
    + 1
  in
  assert (Compare.Int.(expected = length)) ;
  let length =
    let (_pkh, pk, _sk) = generate_key ~algo:P256 () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    (WithExceptions.Option.get ~loc:__LOC__
    @@ Data_encoding.Binary.fixed_length P256.Public_key.encoding)
    + 1
  in
  assert (Compare.Int.(expected = length)) ;
  let length =
    let (_pkh, pk, _sk) = generate_key ~algo:Secp256k1 () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    (WithExceptions.Option.get ~loc:__LOC__
    @@ Data_encoding.Binary.fixed_length Secp256k1.Public_key.encoding)
    + 1
  in
  assert (Compare.Int.(expected = length))

let test_of_bytes_without_validation () =
  List.iter
    (fun algo ->
      let (_pkh, pk, _sk) = Signature.generate_key ~algo () in
      let bytes =
        Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding pk
      in
      let pk2 = Signature.Public_key.of_bytes_without_validation bytes in
      assert (Some pk = pk2))
    [Ed25519; Secp256k1; P256]

let tests =
  [
    ( "signature",
      [
        ("size", `Quick, test_size);
        ( "test_of_bytes_without_validation",
          `Quick,
          test_of_bytes_without_validation );
      ] );
  ]
