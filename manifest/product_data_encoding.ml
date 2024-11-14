(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2021-2023 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2022-2023 Trili Tech <contact@trili.tech>         *)
(* SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>              *)
(*                                                                           *)
(*****************************************************************************)

(* See comment in mli file about special, transient status of data-encoding.

   On the implementation side, the consequence is that:
   - We use the ["octez"] product to define targets.
   - We don't define a custom version number.
   - We mark a conflict with the already released data-encoding.
*)

open Manifest
open Externals

let product_source = ["data-encoding"]

include Product (struct
  let name = "octez"

  let source = product_source
end)

let conflicts =
  [
    external_lib "json_data_encoding" V.True;
    external_lib "json_data_encoding_bson" V.True;
    external_lib "json_data_encoding_browser" V.True;
    external_lib "data-encoding" V.True;
  ]

let json_data_encoding_stdlib =
  public_lib
    "octez-libs.json-data-encoding.stdlib"
    ~internal_name:"json_data_encoding_stdlib"
    ~path:"data-encoding/json-data-encoding/src"
    ~conflicts
    ~wrapped:false
    ~bisect_ppx:No
    ~opam:"octez-libs"
    ~modules:["json_data_encoding_stdlib"; "list_override"]
    ~deps:[uri]

let json_data_encoding =
  public_lib
    "octez-libs.json-data-encoding"
    ~internal_name:"json_data_encoding"
    ~path:"data-encoding/json-data-encoding/src"
    ~conflicts
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
    ~opam:"octez-libs"
    ~path:"data-encoding/json-data-encoding/test"
    ~deps:[json_data_encoding; crowbar; alcotest]

let json_data_encoding_bson =
  public_lib
    "octez-libs.json-data-encoding-bson"
    ~internal_name:"json_data_encoding_bson"
    ~path:"data-encoding/json-data-encoding/src"
    ~conflicts
    ~wrapped:false
    ~bisect_ppx:No
    ~modules:["json_repr_bson"]
    ~deps:
      [json_data_encoding; ocplib_endian; json_data_encoding_stdlib |> open_]

let _json_data_encoding_bson_tests =
  test
    "test_bson_relaxation"
    ~opam:"octez-libs"
    ~path:"data-encoding/json-data-encoding/test-bson"
    ~deps:[crowbar; alcotest; json_data_encoding; json_data_encoding_bson]

let data_encoding =
  public_lib
    "octez-libs.data-encoding"
    ~internal_name:"data_encoding"
    ~path:"data-encoding/src"
    ~conflicts
    ~preprocess:(pps ppx_hash)
    ~bisect_ppx:No
    ~deps:
      [
        ezjsonm;
        zarith;
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
    ~opam:"octez-libs"
    ~path:"data-encoding/test"
    ~deps:[data_encoding; zarith; alcotest]

let _data_encoding_expect_tests =
  private_lib
    "data_encoding_expect_tests"
    ~path:"data-encoding/test/expect"
    ~inline_tests:ppx_expect
    ~bisect_ppx:No
    ~deps:[data_encoding; zarith; ezjsonm; bigstringaf]
    ~opam:"octez-libs"

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
    ~opam:"octez-libs"
    ~path:"data-encoding/test/pbt"
    ~bisect_ppx:No
    ~deps:[data_encoding; zarith; crowbar; bigstringaf]
