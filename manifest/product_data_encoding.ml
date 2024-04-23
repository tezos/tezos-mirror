(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2021-2023 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2022-2023 Trili Tech <contact@trili.tech>         *)
(* SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>              *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

include Product (struct
  let name = "data-encoding"

  let source = ["data-encoding"]
end)

let opam_version = "1.0.1"

let json_data_encoding_stdlib =
  public_lib
    "json-data-encoding.stdlib"
    ~path:"data-encoding/json-data-encoding/src"
    ~synopsis:"Type-safe encoding to and decoding from JSON"
    ~opam_version
    ~js_compatible:true
    ~wrapped:false
    ~bisect_ppx:No
    ~opam:"json-data-encoding"
    ~modules:["json_data_encoding_stdlib"; "list_override"]
    ~deps:[uri]

let json_data_encoding =
  public_lib
    "json-data-encoding"
    ~internal_name:"json_data_encoding"
    ~path:"data-encoding/json-data-encoding/src"
    ~synopsis:"Type-safe encoding to and decoding from JSON"
    ~opam_version
    ~js_compatible:true
    ~wrapped:false
    ~bisect_ppx:No
    ~modules:["json_encoding"; "json_query"; "json_repr"; "json_schema"]
    ~deps:[uri; hex; json_data_encoding_stdlib |> open_]

let _json_data_encoding_tests =
  tests
    [
      "test_big_streaming";
      "test_destruct";
      "test_generated";
      "test_list_map";
      "test_mu";
      "test_seq_is_lazy";
    ]
    ~opam:"json-data-encoding"
    ~path:"data-encoding/json-data-encoding/test"
    ~js_compatible:true
    ~modes:[Native; JS]
    ~deps:[json_data_encoding; crowbar; alcotest; js_of_ocaml_compiler]

let json_data_encoding_bson =
  public_lib
    "json-data-encoding-bson"
    ~internal_name:"json_data_encoding_bson"
    ~path:"data-encoding/json-data-encoding/src"
    ~synopsis:"Type-safe encoding to and decoding from JSON (bson support)"
    ~opam_version
    ~js_compatible:true
    ~wrapped:false
    ~bisect_ppx:No
    ~modules:["json_repr_bson"]
    ~deps:
      [json_data_encoding; ocplib_endian; json_data_encoding_stdlib |> open_]

let _json_data_encoding_bson_tests =
  test
    "test_bson_relaxation"
    ~opam:"json-data-encoding-bson"
    ~path:"data-encoding/json-data-encoding/test-bson"
    ~deps:[crowbar; alcotest; json_data_encoding; json_data_encoding_bson]

let _json_data_encoding_browser =
  public_lib
    "json-data-encoding-browser"
    ~internal_name:"json_data_encoding_browser"
    ~path:"data-encoding/json-data-encoding/src"
    ~synopsis:"Native representation of JSON documents"
    ~opam_version
    ~js_compatible:true
    ~wrapped:false
    ~bisect_ppx:No
    ~modules:["json_repr_browser"]
    ~deps:
      [
        json_data_encoding;
        js_of_ocaml |> open_;
        json_data_encoding_stdlib |> open_;
      ]

let data_encoding =
  public_lib
    "data-encoding"
    ~internal_name:"data_encoding"
    ~path:"data-encoding/src"
    ~synopsis:"Library of JSON and binary encoding combinators"
    ~opam_version
    ~js_compatible:true
    ~preprocess:[pps ppx_hash]
    ~bisect_ppx:No
    ~deps:
      [
        ezjsonm;
        zarith;
        zarith_stubs_js;
        hex;
        json_data_encoding;
        json_data_encoding_bson;
        bigstringaf;
        ppx_hash;
      ]
    ~dune:Dune.[[S "include"; S "dune.inc"]]

let _data_encoding_tests =
  test
    "test"
    ~opam:"data-encoding"
    ~path:"data-encoding/test"
    ~js_compatible:true
    ~modes:[Native; JS]
    ~deps:
      [data_encoding; zarith; zarith_stubs_js; alcotest; js_of_ocaml_compiler]

let _data_encoding_expect_tests =
  private_lib
    "data_encoding_expect_tests"
    ~path:"data-encoding/test/expect"
    ~inline_tests:ppx_expect
    ~bisect_ppx:No
    ~deps:[data_encoding; zarith; zarith_stubs_js; ezjsonm; bigstringaf]
    ~opam:"data-encoding"

(* Some tests require [--stack-size] to be runnable with node.js.
   The version of node in our runners does not support [--stack-size]. *)
let _data_encoding_pbt_tests =
  tests
    [
      "test_generated";
      "test_legacy_compatibility";
      "test_json_stream";
      "test_json_stream_sizes";
      "test_classifiers";
      "json_roundtrip_in_binary";
    ]
    ~opam:"data-encoding"
    ~path:"data-encoding/test/pbt"
    ~js_compatible:false
    ~modes:[Native]
    ~bisect_ppx:No
    ~deps:[data_encoding; zarith; zarith_stubs_js; crowbar; bigstringaf]

(* the other projects can depend on data-encoding, but as an "external"
   dependency *)
let json_data_encoding =
  external_lib ~js_compatible:true "json-data-encoding" (V.exactly opam_version)

let data_encoding =
  external_lib
    ~js_compatible:true
    ~main_module:"Data_encoding"
    "data-encoding"
    (V.exactly opam_version)
