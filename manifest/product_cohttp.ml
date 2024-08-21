(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Trili Tech <contact@trili.tech>              *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

let product_source = ["cohttp/"]

include Product (struct
  let name = "octez"

  let source = product_source
end)

let conflicts =
  [external_lib "cohttp-lwt" V.True; external_lib "cohttp-lwt-unix" V.True]

let cohttp_lwt =
  public_lib
    "octez-libs.cohttp-lwt"
    ~internal_name:"cohttp_lwt"
    ~path:"cohttp/cohttp-lwt/src"
    ~preprocess:(pps ppx_sexp_conv)
    ~deps:[lwt; uri; cohttp; logs; logs_lwt]
    ~conflicts

let cohttp_lwt_unix =
  public_lib
    "octez-libs.cohttp-lwt-unix"
    ~internal_name:"cohttp_lwt_unix"
    ~path:"cohttp/cohttp-lwt-unix/src"
    ~preprocess:(pps ppx_sexp_conv)
    ~deps:
      [
        fmt;
        logs;
        logs_lwt;
        conduit_lwt;
        magic_mime;
        lwt_unix;
        conduit_lwt_unix;
        cohttp;
        cohttp_lwt;
        logs_fmt;
      ]
    ~conflicts
