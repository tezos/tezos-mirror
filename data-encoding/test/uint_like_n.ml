(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

let expect_fail_serialise ?max_value i =
  let encoding = Data_encoding.uint_like_n ?max_value () in
  match Data_encoding.Binary.to_string_opt encoding i with
  | None -> ()
  | Some _ -> raise (Failure "Unexpected success")

let expect_fail_serialise () =
  if Sys.int_size >= 32 then expect_fail_serialise Int32.(to_int max_int) ;
  expect_fail_serialise (-1) ;
  expect_fail_serialise ~max_value:1 2 ;
  expect_fail_serialise ~max_value:2 (-1)

let expect_fail_deserialise ?max_value s =
  let encoding = Data_encoding.uint_like_n ?max_value () in
  match Data_encoding.Binary.of_string_opt encoding s with
  | None -> ()
  | Some _ -> raise (Failure "Unexpected success")

let expect_fail_deserialise () =
  let s z = Data_encoding.Binary.to_string_exn Data_encoding.n z in
  expect_fail_deserialise (s (Z.of_int32 Int32.max_int)) ;
  expect_fail_deserialise (s (Z.of_int64 Int64.max_int)) ;
  expect_fail_deserialise ~max_value:1 (s @@ Z.of_int 2) ;
  expect_fail_deserialise ~max_value:1 (s @@ Z.of_int 999) ;
  expect_fail_deserialise ~max_value:240 (s @@ Z.of_int 241) ;
  expect_fail_deserialise ~max_value:240 (s @@ Z.of_int 242) ;
  expect_fail_deserialise ~max_value:240 (s @@ Z.of_int 701) ;
  expect_fail_deserialise ~max_value:240 (s @@ Z.of_int 999)

let expect_success_roundtrip ?max_value i =
  let encoding = Data_encoding.uint_like_n ?max_value () in
  match Data_encoding.Binary.to_string_opt encoding i with
  | None -> raise (Failure "Cannot serialise")
  | Some s -> (
      match Data_encoding.Binary.of_string_opt encoding s with
      | None -> raise (Failure "Cannot deserialise")
      | Some v -> assert (i = v))

let expect_success_roundtrip () =
  List.iter (expect_success_roundtrip ~max_value:4) [0; 1; 2; 3; 4] ;
  List.iter
    (expect_success_roundtrip ?max_value:None)
    [0; 1; 2; 3; 4; 200000; 374289; 1 lsl 20; (1 lsl 30) - 1]

let expect_success_deserilaisation ?max_value s =
  let encoding = Data_encoding.uint_like_n ?max_value () in
  match Data_encoding.Binary.of_string_opt encoding s with
  | None -> raise (Failure "Cannot deserialise")
  | Some _ -> ()

let expect_success_deserilaisation () =
  let s z = Data_encoding.Binary.to_string_exn Data_encoding.n z in
  expect_success_deserilaisation (s Z.zero) ;
  expect_success_deserilaisation (s Z.one) ;
  expect_success_deserilaisation (s @@ Z.of_int 100) ;
  expect_success_deserilaisation (s @@ Z.of_int 243234) ;
  expect_success_deserilaisation ~max_value:200 (s Z.zero) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 123) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 126) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 189) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 199) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 200)

let tests =
  [
    ("serialise-out-of-range-fails", `Quick, expect_fail_serialise);
    ("deserialise-out-of-range-fails", `Quick, expect_fail_deserialise);
    ("roundtrip-succeeds", `Quick, expect_success_roundtrip);
    ("deserialise-an-n-succeeds", `Quick, expect_success_deserilaisation);
  ]
