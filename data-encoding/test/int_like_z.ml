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

let expect_fail_serialise ?min_value ?max_value i =
  let encoding = Data_encoding.int_like_z ?min_value ?max_value () in
  match Data_encoding.Binary.to_string_opt encoding i with
  | None -> ()
  | Some s ->
      let msg =
        Format.asprintf
          "Unexpected success (v:%d, range:[%s;%s], result:%s)"
          i
          (match min_value with None -> "-" | Some v -> string_of_int v)
          (match max_value with None -> "-" | Some v -> string_of_int v)
          (let (`Hex h) = Hex.of_string s in
           h)
      in
      raise (Failure msg)

let expect_fail_serialise () =
  if Sys.int_size >= 32 then expect_fail_serialise Int32.(to_int max_int) ;
  if Sys.int_size >= 32 then expect_fail_serialise Int32.(to_int min_int) ;
  expect_fail_serialise ~max_value:1 2 ;
  expect_fail_serialise ~max_value:(-100) 0 ;
  expect_fail_serialise ~max_value:(-100) (-3) ;
  expect_fail_serialise ~max_value:(-100) 100 ;
  expect_fail_serialise ~min_value:0 (-1) ;
  expect_fail_serialise ~min_value:(-1) (-2) ;
  expect_fail_serialise ~min_value:100 0 ;
  expect_fail_serialise ~min_value:100 (-3) ;
  expect_fail_serialise ~min_value:100 99 ;
  expect_fail_serialise ~min_value:100 ~max_value:120 99 ;
  expect_fail_serialise ~min_value:100 ~max_value:120 121 ;
  expect_fail_serialise ~min_value:100 ~max_value:120 2000 ;
  expect_fail_serialise ~min_value:100 ~max_value:120 (-10) ;
  expect_fail_serialise ~min_value:(-100) ~max_value:(-50) 0 ;
  expect_fail_serialise ~min_value:(-100) ~max_value:(-50) (-101) ;
  expect_fail_serialise ~min_value:(-100) ~max_value:(-50) (-4321) ;
  expect_fail_serialise ~min_value:(-100) ~max_value:(-50) 4321 ;
  expect_fail_serialise ~min_value:(-100) ~max_value:(-50) (-49)

let expect_fail_deserialise ?min_value ?max_value s =
  let encoding = Data_encoding.int_like_z ?min_value ?max_value () in
  match Data_encoding.Binary.of_string_opt encoding s with
  | None -> ()
  | Some _ -> raise (Failure "Unexpected success")

let expect_fail_deserialise () =
  let s z = Data_encoding.Binary.to_string_exn Data_encoding.z z in
  expect_fail_deserialise (s (Z.of_int32 Int32.max_int)) ;
  expect_fail_deserialise (s (Z.of_int32 Int32.min_int)) ;
  expect_fail_deserialise (s (Z.of_int64 Int64.max_int)) ;
  expect_fail_deserialise (s (Z.of_int64 Int64.min_int)) ;
  expect_fail_deserialise ~max_value:1 (s @@ Z.of_int 2) ;
  expect_fail_deserialise ~max_value:1 (s @@ Z.of_int 999) ;
  expect_fail_deserialise ~max_value:240 (s @@ Z.of_int 241) ;
  expect_fail_deserialise ~max_value:240 (s @@ Z.of_int 242) ;
  expect_fail_deserialise ~max_value:240 (s @@ Z.of_int 701) ;
  expect_fail_deserialise ~max_value:240 (s @@ Z.of_int 999) ;
  expect_fail_deserialise ~min_value:(-1) (s @@ Z.neg @@ Z.of_int 2) ;
  expect_fail_deserialise ~min_value:(-1) (s @@ Z.neg @@ Z.of_int 999) ;
  expect_fail_deserialise ~min_value:(-240) (s @@ Z.neg @@ Z.of_int 241) ;
  expect_fail_deserialise ~min_value:(-240) (s @@ Z.neg @@ Z.of_int 242) ;
  expect_fail_deserialise ~min_value:(-240) (s @@ Z.neg @@ Z.of_int 701) ;
  expect_fail_deserialise ~min_value:(-240) (s @@ Z.neg @@ Z.of_int 999) ;
  expect_fail_deserialise ~min_value:(-1) ~max_value:1 (s @@ Z.of_int 2) ;
  expect_fail_deserialise ~min_value:(-1) ~max_value:1 (s @@ Z.neg @@ Z.of_int 2)

let expect_success_roundtrip ?min_value ?max_value i =
  let encoding = Data_encoding.int_like_z ?min_value ?max_value () in
  match Data_encoding.Binary.to_string encoding i with
  | Error err ->
      let msg =
        Format.asprintf
          "Cannot serialise (v:%d, range:[%s;%s], error:%a)"
          i
          (match min_value with None -> "-" | Some v -> string_of_int v)
          (match max_value with None -> "-" | Some v -> string_of_int v)
          Data_encoding.Binary.pp_write_error
          err
      in
      raise (Failure msg)
  | Ok s -> (
      match Data_encoding.Binary.of_string_opt encoding s with
      | None -> raise (Failure "Cannot deserialise")
      | Some v -> assert (i = v))

let expect_success_roundtrip () =
  List.iter
    (expect_success_roundtrip ~min_value:(-1) ~max_value:4)
    [-1; 0; 1; 2; 3; 4] ;
  List.iter
    (expect_success_roundtrip ?min_value:None ?max_value:None)
    [
      -1;
      -344;
      -(1 lsl 30);
      0;
      1;
      2;
      3;
      4;
      200000;
      374289;
      1 lsl 20;
      (1 lsl 30) - 1;
    ]

let expect_success_deserilaisation ?min_value ?max_value s =
  let encoding = Data_encoding.int_like_z ?min_value ?max_value () in
  match Data_encoding.Binary.of_string_opt encoding s with
  | None -> raise (Failure "Cannot deserialise")
  | Some _ -> ()

let expect_success_deserilaisation () =
  let s z = Data_encoding.Binary.to_string_exn Data_encoding.z z in
  expect_success_deserilaisation (s Z.zero) ;
  expect_success_deserilaisation (s Z.one) ;
  expect_success_deserilaisation (s @@ Z.of_int 100) ;
  expect_success_deserilaisation (s @@ Z.of_int 243234) ;
  expect_success_deserilaisation ~max_value:200 (s Z.zero) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 123) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 126) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 189) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 199) ;
  expect_success_deserilaisation ~max_value:200 (s @@ Z.of_int 200) ;
  expect_success_deserilaisation (s @@ Z.neg @@ Z.one) ;
  expect_success_deserilaisation (s @@ Z.of_int (-100)) ;
  expect_success_deserilaisation (s @@ Z.of_int (-243234)) ;
  expect_success_deserilaisation ~min_value:(-200) (s Z.zero) ;
  expect_success_deserilaisation ~min_value:(-200) (s @@ Z.neg @@ Z.of_int 123) ;
  expect_success_deserilaisation ~min_value:(-200) (s @@ Z.neg @@ Z.of_int 126) ;
  expect_success_deserilaisation ~min_value:(-200) (s @@ Z.neg @@ Z.of_int 189) ;
  expect_success_deserilaisation ~min_value:(-200) (s @@ Z.neg @@ Z.of_int 199) ;
  expect_success_deserilaisation ~min_value:(-200) (s @@ Z.neg @@ Z.of_int 200) ;
  expect_success_deserilaisation
    ~min_value:(-200)
    ~max_value:200
    (s @@ Z.neg @@ Z.of_int 200) ;
  expect_success_deserilaisation
    ~min_value:(-200)
    ~max_value:200
    (s @@ Z.neg @@ Z.of_int 199) ;
  expect_success_deserilaisation
    ~min_value:(-200)
    ~max_value:200
    (s @@ Z.of_int 200) ;
  expect_success_deserilaisation
    ~min_value:(-200)
    ~max_value:200
    (s @@ Z.of_int 199) ;
  expect_success_deserilaisation ~min_value:(-200) ~max_value:200 (s @@ Z.zero)

let tests =
  [
    ("serialise-out-of-range-fails", `Quick, expect_fail_serialise);
    ("deserialise-out-of-range-fails", `Quick, expect_fail_deserialise);
    ("roundtrip-succeeds", `Quick, expect_success_roundtrip);
    ("deserialise-a-z-succeeds", `Quick, expect_success_deserilaisation);
  ]
