(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Cmdliner

let from action =
  let doc =
    Format.asprintf
      "The source protocol to %s. It must be of the form '%a'."
      action
      Proto_manager.pp_pattern
      Proto_manager.protocol_pattern
  in
  Arg.(
    required
    & opt (some string) None
    & info ["f"; "from"] ~docv:"<protocol_source>" ~doc)

let target action pattern =
  let doc =
    Format.asprintf
      "The target protocol to %s. It must be different than <protocol_source> \
       and of the form '%a'."
      action
      Proto_manager.pp_pattern
      pattern
  in
  Arg.(
    required
    & opt (some string) None
    & info ["t"; "to"] ~docv:"<protocol_target>" ~doc)

let git_dir =
  let doc =
    "The root directory of a clone of tezos/tezos git repository. By default, \
     it is the current working directory."
  in
  Arg.(
    value
    & opt dir (Sys.getcwd ())
    & info ["g"; "git-directory"] ~docv:"<git_directory>" ~doc)

let stabilise =
  let action = "stabilise" in
  let doc = "Stabilise a protocol." in
  let info = Cmd.info "stabilise" ~doc in
  let cmd =
    Term.(
      const Proto_manager.Stabilise.run
      $ git_dir $ from action
      $ target action Proto_manager.protocol_pattern)
  in
  Cmd.v info cmd

let snapshot =
  let action = "snapshot" in
  let doc = "Snapshot a protocol." in
  let info = Cmd.info "snapshot" ~doc in
  let cmd =
    Term.(
      const Proto_manager.Snapshot.run
      $ git_dir $ from action
      $ target action Proto_manager.Snapshot.target_pattern)
  in
  Cmd.v info cmd

let hash =
  let action = "update the hash" in
  let doc = "Update the hash of a protocol." in
  let info = Cmd.info "hash" ~doc in
  let cmd = Term.(const Proto_manager.Hash.run $ git_dir $ from action) in
  Cmd.v info cmd

let cmd =
  let doc =
    "help to automatize most of the process involved in the new \
     stabilisation/release of protocol amendments"
  in
  let info = Cmd.info "proto-manager" ~version:"0.1" ~doc in
  Cmd.group info [stabilise; snapshot; hash]

let () = exit (Cmd.eval cmd)
