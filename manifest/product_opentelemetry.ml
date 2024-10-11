(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

let product_source = ["opentelemetry/"]

include Product (struct
  let name = "octez"

  let source = product_source
end)

let conflicts = [external_lib "opentelemetry-client-cohttp-lwt" V.True]

let opentelemetry_client_cohttp_lwt =
  public_lib
    "octez-libs.opentelemetry-client-cohttp-lwt"
    ~internal_name:"opentelemetry_client_cohttp_lwt"
    ~path:"opentelemetry/src/client-cohttp-lwt"
    ~preprocess:(pps lwt_ppx)
    ~deps:
      [
        cohttp;
        Product_cohttp.cohttp_lwt;
        Product_cohttp.cohttp_lwt_unix;
        lwt_ppx;
        mtime;
        mtime_clock_os;
        opentelemetry;
      ]
    ~conflicts
