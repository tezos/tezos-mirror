(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Trili Tech <contact@trili.tech>              *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals
open Product_data_encoding

let product_source = ["resto/"]

include Product (struct
  let name = "octez"

  let source = product_source
end)

let conflicts =
  [
    external_lib "resto" V.True;
    external_lib "resto-json" V.True;
    external_lib "resto-directory" V.True;
    external_lib "resto-cohttp" V.True;
    external_lib "resto-cohttp-client" V.True;
    external_lib "resto-acl" V.True;
    external_lib "resto-cohttp-server" V.True;
    external_lib "resto-cohttp-self-serving-client" V.True;
  ]

let resto =
  public_lib
    "octez-libs.resto"
    ~internal_name:"resto"
    ~path:"resto/src"
    ~wrapped:false
    ~bisect_ppx:No
    ~modules:["resto"]
    ~deps:[uri; logs_fmt; fmt_tty]
    ~conflicts

let resto_directory =
  public_lib
    "octez-libs.resto-directory"
    ~internal_name:"resto_directory"
    ~path:"resto/src"
    ~wrapped:false
    ~bisect_ppx:No
    ~modules:["resto_directory"]
    ~deps:[lwt; resto]
    ~conflicts

let resto_cohttp =
  public_lib
    "octez-libs.resto-cohttp"
    ~internal_name:"resto_cohttp"
    ~path:"resto/src"
    ~bisect_ppx:No
    ~modules:["media_type"; "cors"]
    ~deps:[resto; resto_directory; cohttp_lwt]
    ~conflicts

let resto_acl =
  public_lib
    "octez-libs.resto-acl"
    ~internal_name:"resto_acl"
    ~path:"resto/src"
    ~bisect_ppx:No
    ~modules:["acl"]
    ~deps:[resto; uri]
    ~conflicts

let resto_cohttp_server =
  public_lib
    "octez-libs.resto-cohttp-server"
    ~internal_name:"resto_cohttp_server"
    ~path:"resto/src"
    ~bisect_ppx:No
    ~modules:["server"]
    ~deps:
      [
        resto;
        resto_directory;
        resto_acl |> open_;
        resto_cohttp |> open_;
        cohttp_lwt_unix;
        conduit_lwt_unix;
      ]
    ~conflicts

let resto_cohttp_client =
  public_lib
    "octez-libs.resto-cohttp-client"
    ~internal_name:"resto_cohttp_client"
    ~path:"resto/src"
    ~bisect_ppx:No
    ~modules:["client"]
    ~deps:[resto; resto_directory; resto_cohttp |> open_; uri; lwt]
    ~conflicts

let resto_cohttp_self_serving_client =
  public_lib
    "octez-libs.resto-cohttp-self-serving-client"
    ~internal_name:"resto_cohttp_self_serving_client"
    ~path:"resto/src"
    ~bisect_ppx:No
    ~modules:["self_serving_client"]
    ~deps:
      [
        resto;
        resto_directory;
        resto_acl |> open_;
        resto_cohttp |> open_;
        resto_cohttp_client |> open_;
        resto_cohttp_server;
        uri;
        lwt;
      ]
    ~conflicts

let resto_json =
  public_lib
    "octez-libs.resto-json"
    ~internal_name:"resto_json"
    ~path:"resto/src"
    ~wrapped:false
    ~bisect_ppx:No
    ~modules:["resto_json"]
    ~deps:[json_data_encoding; json_data_encoding_bson; resto]
    ~conflicts

let test_fixtures =
  private_lib
    "fixtures"
    ~opam:"octez-libs"
    ~path:"resto/test"
    ~bisect_ppx:No
    ~modules:["directory"; "services"]
    ~deps:[resto_directory; resto_json; lwt]

let test_utils =
  private_lib
    "util"
    ~opam:"octez-libs"
    ~path:"resto/test"
    ~bisect_ppx:No
    ~modules:["util"]
    ~deps:[alcotest; alcotest_lwt; lwt]

let _resto_acl_unit_test =
  test
    "acl_unit_test"
    ~opam:"octez-libs"
    ~path:"resto/test"
    ~deps:[resto_acl |> open_]
    ~modules:["acl_unit_test"]

let _resto_acl_integration_test =
  test
    "acl_integration_test"
    ~opam:"octez-libs"
    ~path:"resto/test"
    ~deps:
      [
        lwt;
        lwt_unix;
        json_data_encoding;
        ezjsonm;
        (* cohttp; *)
        cohttp_lwt;
        cohttp_lwt_unix;
        resto_json;
        resto;
        resto_acl;
        resto_cohttp_server;
        resto_cohttp_client;
      ]
    ~modules:["acl_integration_test"]

let _chunked_output_integration_test =
  test
    "chunked_output_integration_test"
    ~opam:"octez-libs"
    ~path:"resto/test"
    ~deps:
      [
        lwt;
        lwt_unix;
        json_data_encoding;
        ezjsonm;
        (* cohttp; *)
        cohttp_lwt;
        cohttp_lwt_unix;
        resto_json;
        resto_acl;
        resto_cohttp_server;
        resto_cohttp_client;
      ]
    ~modules:["chunked_output_integration_test"]

let _resto_tests =
  test
    "resto_test"
    ~opam:"octez-libs"
    ~path:"resto/test"
    ~deps:
      [
        test_fixtures;
        test_utils;
        resto_directory;
        resto_json;
        json_data_encoding;
        json_data_encoding_bson;
        lwt;
        lwt_unix;
      ]
    ~modules:["resto_test"]

let _resto_self_serving_client_test =
  test
    "self_serving_client_test"
    ~opam:"octez-libs"
    ~path:"resto/test"
    ~deps:
      [
        resto_directory;
        resto_json;
        resto_cohttp_self_serving_client;
        resto_cohttp;
        resto_cohttp_client;
        resto_cohttp_server;
        json_data_encoding;
        ezjsonm;
        lwt;
        lwt_unix;
      ]
    ~modules:["self_serving_client_test"]

let _resto_directory_tests =
  test
    "directory_test"
    ~opam:"octez-libs"
    ~path:"resto/test"
    ~deps:
      [
        test_fixtures;
        test_utils;
        resto_directory;
        resto_json;
        resto_cohttp;
        lwt;
        alcotest;
        alcotest_lwt;
        fmt;
      ]
    ~modules:["directory_test"]
