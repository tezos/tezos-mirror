(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Trili Tech <contact@trili.tech>              *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

let product_source = ["prometheus/"]

include Product (struct
  let name = "octez"

  let source = product_source
end)

let conflicts =
  [external_lib "prometheus" V.True; external_lib "prometheus-app" V.True]

let prometheus =
  public_lib
    "octez-libs.prometheus"
    ~internal_name:"prometheus"
    ~path:"prometheus/src"
    ~deps:[lwt; astring; asetmap; re]
    ~conflicts

let prometheus_app =
  public_lib
    "octez-libs.prometheus-app"
    ~internal_name:"prometheus_app"
    ~path:"prometheus/app"
    ~wrapped:false
    ~modules:["Prometheus_app"]
    ~deps:[prometheus; lwt; cohttp_lwt; astring; asetmap; fmt; re]
    ~conflicts

let conflicts =
  conflicts
  @ [
      external_sublib
        (external_lib "prometheus-app" V.True)
        "prometheus-app.unix";
    ]

let prometheus_app_unix =
  public_lib
    "octez-libs.prometheus-app.unix"
    ~internal_name:"prometheus_app_unix"
    ~path:"prometheus/app"
    ~wrapped:false
    ~modules:["Prometheus_unix"]
    ~deps:
      [
        prometheus;
        prometheus_app;
        cmdliner;
        cohttp_lwt;
        cohttp_lwt_unix;
        logs_fmt;
        fmt_tty;
      ]
    ~conflicts

let _prometheus_example =
  private_exe
    "example"
    ~path:"prometheus/examples"
    ~opam:"octez-libs"
    ~deps:
      [prometheus; prometheus_app_unix; cmdliner; cohttp_lwt; cohttp_lwt_unix]

let _prometheus_test =
  test
    "test"
    ~opam:"octez-libs"
    ~path:"prometheus/tests"
    ~deps:[prometheus; prometheus_app; alcotest; alcotest_lwt]
