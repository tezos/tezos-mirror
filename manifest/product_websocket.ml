(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

let product_source = ["websocket/"]

include Product (struct
  let name = "etherlink"

  let source = product_source
end)

let websocket =
  public_lib
    "octez-evm-node-libs.websocket"
    ~internal_name:"websocket"
    ~path:"websocket/core"
    ~deps:[astring; base64; ocplib_endian; conduit; cohttp; mirage_crypto_rng]
    ~conflicts:[external_lib "websocket" V.True]

let websocket_cohttp_lwt =
  public_lib
    "octez-evm-node-libs.websocket_cohttp_lwt"
    ~internal_name:"websocket_cohttp_lwt"
    ~path:"websocket/lwt"
    ~modules:["websocket_cohttp_lwt"]
    ~deps:[cohttp_lwt_unix; websocket]

let websocket_lwt_unix =
  public_lib
    "octez-evm-node-libs.websocket_lwt_unix"
    ~internal_name:"websocket_lwt_unix"
    ~path:"websocket/lwt"
    ~modules:["websocket_lwt_unix"]
    ~deps:[lwt_log; cohttp_lwt_unix; websocket]
