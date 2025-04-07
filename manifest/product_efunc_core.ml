(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori     <contact@functori.com>                    *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

include Product (struct
  let name = "efunc_core"

  let source = ["efunc-core/"]
end)

let efunc_core =
  public_lib
    "efunc_core"
    ~path:"efunc-core/src"
    ~synopsis:"Efunc is a lib for crafting EVM compatible transactions and more"
    ~opam:"efunc_core"
    ~bisect_ppx:No
    ~deps:
      [
        zarith;
        Product_data_encoding.data_encoding |> open_;
        lwt;
        rope;
        secp256k1_internal;
        digestif;
        fmt;
      ]
    ~release_status:Released
