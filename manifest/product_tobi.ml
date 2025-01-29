(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

include Product (struct
  let name = "Tobi"

  let source = ["tobi"]
end)

let _ci_bin_main =
  public_exe
    "tobi"
    ~internal_name:"main"
    ~synopsis:"CLI for Tobi, which allows to install components"
    ~path:"tobi/src"
    ~deps:[unix; clap; opam_file_format]
    ~release_status:Unreleased
    ~bisect_ppx:No
