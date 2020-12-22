(* The MIT License (MIT)
 *
 *   Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 *   Permission is hereby granted, free of charge, to any person obtaining a copy
 *   of this software and associated documentation files (the "Software"), to deal
 *   in the Software without restriction, including without limitation the rights
 *   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *   copies of the Software, and to permit persons to whom the Software is
 *   furnished to do so, subject to the following conditions:
 *
 *   The above copyright notice and this permission notice shall be included in all
 *   copies or substantial portions of the Software.
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *   SOFTWARE. *)

let test_size () =
  let open Signature in
  let length =
    let (_pkh, pk, _sk) = generate_key ~algo:Ed25519 () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    Data_encoding.Binary.fixed_length_exn Ed25519.Public_key.encoding + 1
  in
  assert (Compare.Int.(expected = length)) ;
  let length =
    let (_pkh, pk, _sk) = generate_key ~algo:P256 () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    Data_encoding.Binary.fixed_length_exn P256.Public_key.encoding + 1
  in
  assert (Compare.Int.(expected = length)) ;
  let length =
    let (_pkh, pk, _sk) = generate_key ~algo:Secp256k1 () in
    Public_key.size pk
  in
  let expected =
    (* add 1 for the tag of union encoding *)
    Data_encoding.Binary.fixed_length_exn Secp256k1.Public_key.encoding + 1
  in
  assert (Compare.Int.(expected = length))

let size = [("size", `Quick, test_size)]

let () = Alcotest.run "hacl" [("size", size)]
