(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let ground_values =
  [
    `O [];
    `Bool true;
    `Bool false;
    `Float 0.0;
    `Float 0.1;
    `Float Float.epsilon;
    `Float 1.0;
    `A [];
    `Null;
    `String "";
    `String "0";
    `String "\"";
    `String "'";
    `String ";";
    `String "0";
    `String "00";
  ]

let values_1 =
  (* [concat_map] is only available since OCaml.4.10, for tests we dont care
     about perf so we use [concat]+[map] *)
  List.concat
  @@ List.map
       (fun (a, b) ->
         [
           `O [("x", a)];
           `O [("yy", a); ("x", `Null); ("z", `A []); ("t", b)];
           `A [a];
           `A [a; b];
           `A [a; `Float 0.33; `Float 0.33; b; `Float 0.33];
         ])
       (List.map2 (fun x y -> (x, y)) ground_values (List.rev ground_values))

let all_values = ground_values @ values_1

(* Setting up the actual tests *)
let test_json (j : Data_encoding.Json.t) =
  let json =
    try Data_encoding.Json.construct Data_encoding.Json.encoding j
    with exc -> failwith ("Cannot construct: " ^ Printexc.to_string exc)
  in
  let jj =
    try Data_encoding.Json.destruct Data_encoding.Json.encoding json
    with Data_encoding.Json.Cannot_destruct (_, _) ->
      failwith "Cannot destruct"
  in
  assert (j = jj)

let test_jsons () = List.iter test_json all_values

let test_binary (j : Data_encoding.Json.t) =
  let s =
    try Data_encoding.Binary.to_string_exn Data_encoding.Json.encoding j
    with exc -> failwith ("Cannot construct: " ^ Printexc.to_string exc)
  in
  let jj =
    try Data_encoding.Binary.of_string_exn Data_encoding.Json.encoding s
    with _ -> failwith "Cannot destruct"
  in
  assert (j = jj)

let test_binaries () = List.iter test_binary all_values

let tests =
  [
    ("json_roundtrip_in_json", `Quick, test_jsons);
    ("json_roundtrip_in_binary", `Quick, test_binaries);
  ]
