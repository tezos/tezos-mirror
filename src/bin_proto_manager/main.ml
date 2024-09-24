(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Cmdliner

let wip () = Log.warning "This application is under development"

let wip_t = Term.(const wip $ const ())

let cmd =
  let doc =
    "help to automatize most of the process involved in the new \
     stabilisation/release of protocol amendments"
  in
  let info =
    Cmd.info
      "octez-proto-manager"
      ~version:Tezos_version_value.Bin_version.octez_version_string
      ~doc
  in
  Cmd.v info wip_t

let () = exit (Cmd.eval cmd)
