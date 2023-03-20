(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Environment structs
    Invocation:   dune exec src/lib_protocol_environment/test/main.exe
    Subject:      Environment structs modifications to Data_encoding
                  e.g. in src/lib_protocol_environment/structs/v5_data_encoding.ml
*)

open Tezos_protocol_environment_structs.V8

type t = {x : int; y : string Data_encoding.lazy_t}

let test_unparsable_lazyexpr () =
  let parsed_encoding = Data_encoding.(lazy_encoding (Fixed.string Plain 3)) in
  let enc =
    let open Data_encoding in
    conv
      (fun {x; y} -> (x, y))
      (fun (x, y) -> {x; y})
      (obj2 (req "x" int8) (req "y" parsed_encoding))
  in
  let bytes_v = Hex.(to_bytes_exn (`Hex "030000000401020304")) in
  let v = Data_encoding.Binary.of_bytes_exn enc bytes_v in
  let json =
    try Data_encoding.Json.construct enc v
    with exn ->
      Alcotest.failf
        "Unexpected exception in %s:@\n%s"
        __LOC__
        (Printexc.to_string exn)
  in
  let expected_json =
    `O
      [
        ("x", `Float 3.000000);
        ("y", `O [("unparsed-binary", `String "01020304")]);
      ]
  in
  Assert.equal
    ~pp:Data_encoding.Json.pp
    ~msg:"Constructed json is incorrect"
    ~loc:__LOC__
    expected_json
    json ;
  Lwt.return_unit

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "proto-env-v8-data-encoding"
       [
         ( "lazy",
           [
             Alcotest_lwt.test_case "unparsable_lazyexpr" `Quick (fun _ ->
                 test_unparsable_lazyexpr);
           ] );
       ]
