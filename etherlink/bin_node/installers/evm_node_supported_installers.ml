(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let mainnet = Option.get (Installers.read "mainnet-installer.wasm")

let braeburn = Option.get (Installers.read "braeburn-installer.wasm")
