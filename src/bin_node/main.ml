(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* This can be removed once the protocol is fixed
   (currently there is [to_int Int32.max_int] which is obviously invalid). *)
let () =
  if Sys.word_size <> 64 then (
    prerr_endline "Non-64 bit architectures are not supported." ;
    exit 1 )

let () =
  let log s = Node_logging.fatal_error "%s" s in
  Lwt_exit.exit_on ~log Sys.sigint ;
  Lwt_exit.exit_on ~log Sys.sigterm

let () =
  if Filename.basename Sys.argv.(0) = Updater.compiler_name then (
    try
      Tezos_protocol_compiler.Compiler.main
        Tezos_protocol_compiler_native.Native.driver ;
      Stdlib.exit 0
    with exn ->
      Format.eprintf "%a\n%!" Opterrors.report_error exn ;
      Stdlib.exit 1 )

let () =
  if Filename.basename Sys.argv.(0) = "tezos-validator" then (
    try Stdlib.exit (Lwt_main.run @@ Validator.main ())
    with exn ->
      Format.eprintf "%a\n%!" Opterrors.report_error exn ;
      Stdlib.exit 1 )

let term =
  let open Cmdliner.Term in
  ret (const (`Help (`Pager, None)))

let description =
  [ `S "DESCRIPTION";
    `P "Entry point for initializing, configuring and running a Tezos node.";
    `P Node_identity_command.Manpage.command_description;
    `P Node_run_command.Manpage.command_description;
    `P Node_config_command.Manpage.command_description;
    `P Node_upgrade_command.Manpage.command_description;
    `P Node_snapshot_command.Manpage.command_description;
    `P Node_reconstruct_command.Manpage.command_description ]

let man = description @ Node_run_command.Manpage.examples

let info =
  let version =
    Tezos_version.Current_git_info.abbreviated_commit_hash ^ " ("
    ^ Tezos_version.Current_git_info.committer_date ^ ") ("
    ^ Tezos_version.Version.current_string ^ ")"
  in
  Cmdliner.Term.info ~doc:"The Tezos node" ~man ~version "tezos-node"

let commands =
  [ Node_run_command.cmd;
    Node_config_command.cmd;
    Node_identity_command.cmd;
    Node_upgrade_command.cmd;
    Node_snapshot_command.cmd;
    Node_reconstruct_command.cmd ]

let () =
  Random.self_init () ;
  match Cmdliner.Term.eval_choice (term, info) commands with
  | `Error _ ->
      exit 1
  | `Help ->
      exit 0
  | `Version ->
      exit 1
  | `Ok () ->
      exit 0
