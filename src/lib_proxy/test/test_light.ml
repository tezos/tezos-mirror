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

(** Testing
    -------
    Component:    Client
    Invocation:   dune exec src/lib_proxy/test/main.exe
    Dependencies: src/lib_proxy/test/light_lib.ml
*)

let two_uris = ["http://foobar"; "http://schplaf"]

let to_json min_agreement uris : Data_encoding.json =
  let json_strings = `A (List.map (fun s -> `String s) uris) in
  `O [("min_agreement", `Float min_agreement); ("uris", json_strings)]

let to_json_default_agreement uris : Data_encoding.json =
  let json_strings = `A (List.map (fun s -> `String s) uris) in
  `O [("uris", json_strings)]

(** Test that valid values of the min_agreement field
    of the [sources_config] record are indeed accepted. *)
let test_good_min_agreement_parsing _ =
  let floats = [0.1; 0.9; 1.0] in
  let results =
    List.map
      (fun f ->
        to_json f two_uris |> Tezos_proxy.Light.destruct_sources_config
        |> Result.is_ok)
      floats
  in
  Alcotest.(check (list bool))
    "All floats are accepted"
    List.(repeat (length floats) true)
    results

(** Test that the default value of [min_agreement] is 1 when not specified. *)
let test_good_min_agreement_parsing_default_agreement _ =
  let result =
    to_json_default_agreement two_uris
    |> Tezos_proxy.Light.destruct_sources_config
    |> Result.fold ~ok:Fun.id ~error:(fun e -> Alcotest.fail e)
  in
  Alcotest.(check (float 0.))
    "Default min agreement is 1"
    1.
    result.min_agreement

(** Test that invalid values of the min_agreement field
    of the [sources_config] record are indeed rejected. *)
let test_wrong_min_agreement_parsing _ =
  let floats = [-1.0; -0.9; 0.0; 1.1; 9.; 99.; 500.0] in
  let results =
    List.map
      (fun f ->
        to_json f two_uris |> Tezos_proxy.Light.destruct_sources_config
        |> Result.is_ok)
      floats
  in
  Alcotest.(check (list bool))
    "All floats are rejected"
    List.(repeat (length floats) false)
    results

(** Test that invalid values of the uris field
    of the [sources_config] record are indeed rejected. *)
let test_wrong_uris_parsing _ =
  let uris_lists = [[]; ["http://foobar"]] in
  let results =
    List.map
      (fun uris ->
        to_json 1.0 uris |> Tezos_proxy.Light.destruct_sources_config
        |> Result.is_ok)
      uris_lists
  in
  Alcotest.(check (list bool))
    "All URIs lists are rejected"
    List.(repeat (length uris_lists) false)
    results

(** Test that the example content of [--sources] is valid *)
let test_parse_example_sources _ =
  let check_parsed = function
    | Error errmsg ->
        Alcotest.failf "Parsing should have succeeded, but obtained: %s" errmsg
    | Ok x -> x
  in
  let json =
    Data_encoding.Json.from_string Tezos_proxy.Light.example_sources
    |> check_parsed
  in
  Tezos_proxy.Light.destruct_sources_config json |> check_parsed |> ignore ;
  ()

let () =
  Alcotest.run
    "tezos-light"
    [
      ( "light",
        [
          Alcotest.test_case
            "test valid min_agreement parsing"
            `Quick
            test_good_min_agreement_parsing;
          Alcotest.test_case
            "test invalid min_agreement parsing"
            `Quick
            test_wrong_min_agreement_parsing;
          Alcotest.test_case
            "test invalid uris parsing"
            `Quick
            test_wrong_uris_parsing;
          Alcotest.test_case
            "test default min agreement"
            `Quick
            test_good_min_agreement_parsing_default_agreement;
          Alcotest.test_case
            "test parsing sources example"
            `Quick
            test_parse_example_sources;
        ] );
    ]
