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
    try
      let is_valid_directory =
        Array.length Sys.argv = 3
        && Sys.argv.(1) = "--socket-dir"
        && Sys.file_exists Sys.argv.(2)
        && Sys.is_directory Sys.argv.(2)
      in
      if not is_valid_directory then
        invalid_arg
          "Invalid arguments provided for the validator: expected \
           'tezos-validator --socket-dir <dir>'." ;
      Stdlib.exit
        (Lwt_main.run
           ( Lwt_exit.wrap_and_exit
             @@ Validator.main ~socket_dir:Sys.argv.(2) ()
           >>= function
           | Ok () -> Lwt_exit.exit_and_wait 0 | Error _ -> Lwt.return 1 ))
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
  let version = Tezos_version.Bin_version.version_string in
  Cmdliner.Term.info ~doc:"The Tezos node" ~man ~version "tezos-node"

let commands =
  [ Node_run_command.cmd;
    Node_config_command.cmd;
    Node_identity_command.cmd;
    Node_upgrade_command.cmd;
    Node_snapshot_command.cmd;
    Node_reconstruct_command.cmd ]

(* This call is not strictly necessary as the parameters are initialized
   lazily the first time a Sapling operation (validation or forging) is
   done. This is what the client does.
   For a long running binary however it is important to make sure that the
   parameters files are there at the start and avoid failing much later while
   validating an operation. Plus paying this cost upfront means that the first
   validation will not be more expensive. *)
let () =
  try Tezos_sapling.Core.Validator.init_params ()
  with exn ->
    Printf.eprintf
      "Failed to initialize Zcash parameters: %s"
      (Printexc.to_string exn) ;
    exit 1

let () =
  Random.self_init () ;
  match Cmdliner.Term.eval_choice (term, info) commands with
  | `Error _ ->
      exit 1
  | `Help ->
      exit 0
  | `Version ->
      exit 0
  | `Ok () ->
      exit 0
