(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Cmdliner

let from =
  let doc = "The source protocol to stabilise, snapshot or update the hash." in
  Arg.(
    required
    & opt (some string) None
    & info ["f"; "from"] ~docv:"<protocol_source>" ~doc)

let target =
  let doc = "The target protocol to stabilise or snapshot." in
  Arg.(
    required
    & opt (some string) None
    & info ["t"; "to"] ~docv:"<protocol_target>" ~doc)

let stabilise =
  let doc = "Stabilise a protocol." in
  let info = Cmd.info "stabilise" ~doc in
  let cmd = Term.(const Proto_manager.stabilise $ from $ target) in
  Cmd.v info cmd

let snapshot =
  let doc = "Snapshot a protocol." in
  let info = Cmd.info "snapshot" ~doc in
  let cmd = Term.(const Proto_manager.snapshot $ from $ target) in
  Cmd.v info cmd

let hash =
  let doc = "Update the hash of a protocol." in
  let info = Cmd.info "hash" ~doc in
  let cmd = Term.(const Proto_manager.hash $ from) in
  Cmd.v info cmd

let cmd =
  let doc =
    "help to automatize most of the process involved in the new \
     stabilisation/release of protocol amendments"
  in
  let info = Cmd.info "proto-manager" ~version:"0.1" ~doc in
  Cmd.group info [stabilise; snapshot; hash]

let () = exit (Cmd.eval cmd)
