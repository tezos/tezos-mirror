(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Data_encoding.Encoding

let test_too_big_fails i =
  let exception Failed in
  let encoder = int32 in
  let encodable = i in
  let encoded = Data_encoding.Binary.to_string_exn encoder encodable in
  let decoder = int31 in
  try
    let _decoded = Data_encoding.Binary.of_string_exn decoder encoded in
    raise Failed
  with
  | Failed as exc -> raise exc
  | _ -> ()

let test_32_is_not_31 () =
  test_too_big_fails Int32.max_int ;
  test_too_big_fails Int32.(sub max_int 1l) ;
  test_too_big_fails Int32.(sub max_int 3124l) ;
  test_too_big_fails Int32.min_int ;
  test_too_big_fails Int32.(add min_int 1l) ;
  test_too_big_fails Int32.(add min_int 3124l) ;
  ()

let test_small_enough_succeeds i =
  let encoder = int31 in
  let encodable = i in
  let encoded = Data_encoding.Binary.to_string_exn encoder encodable in
  let decoder = int31 in
  let decoded = Data_encoding.Binary.of_string_exn decoder encoded in
  assert (encodable = decoded)

let test_31_is_31 () =
  test_small_enough_succeeds 0 ;
  test_small_enough_succeeds 0x23f4 ;
  test_small_enough_succeeds Int32.(to_int (div max_int 2l)) ;
  test_small_enough_succeeds Int32.(to_int (div min_int 2l)) ;
  ()

let test_small_enough_is_compatible i =
  let encoder = int32 in
  let encodable = i in
  let encoded = Data_encoding.Binary.to_string_exn encoder encodable in
  let decoder = int31 in
  let decoded = Data_encoding.Binary.of_string_exn decoder encoded in
  assert (encodable = Int32.of_int decoded)

let test_small_32_is_31 () =
  test_small_enough_is_compatible 0l ;
  test_small_enough_is_compatible 0x23f4l ;
  test_small_enough_is_compatible Int32.(div max_int 2l) ;
  test_small_enough_is_compatible Int32.(div min_int 2l) ;
  ()

let tests =
  [
    ("int32-max-int-not-an-int31", `Quick, test_32_is_not_31);
    ("small-int32-is-int31", `Quick, test_small_32_is_31);
    ("int31-is-int31", `Quick, test_31_is_31);
  ]
