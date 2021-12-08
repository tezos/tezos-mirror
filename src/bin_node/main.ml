(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let () =
  Printexc.register_printer @@ function
  | Unix.Unix_error (code, "", _) -> Some (Unix.error_message code)
  | Unix.Unix_error (code, function_name, "") ->
      Some (Printf.sprintf "in %s: %s" function_name (Unix.error_message code))
  | Unix.Unix_error (code, function_name, arguments) ->
      Some
        (Printf.sprintf
           "in %s %S: %s"
           function_name
           arguments
           (Unix.error_message code))
  | _ -> None

let () =
  (* The default allocation policy of Octez is "best-fit" which gives
     the best compromise in terms of performances and memory
     consumption. This default policy can be changed if the user set
     an environment variable. *)
  (* Any change to this constant should be replicated into the
     external validator in [src/bin_validation/main_validator.ml]. *)
  let default_allocation_policy = 2 in
  let current = Gc.get () in
  (match Sys.getenv_opt "OCAMLRUNPARAM" with
  | None -> Gc.set {current with allocation_policy = default_allocation_policy}
  | Some _ -> ()) ;
  if (Gc.get ()).allocation_policy <> default_allocation_policy then
    Format.eprintf
      "WARNING: Default allocation policy changed: %d (default %d)@."
      current.allocation_policy
      default_allocation_policy

(* This can be removed once the protocol is fixed
   (currently there is [to_int Int32.max_int] which is obviously invalid). *)
let () =
  if Sys.word_size <> 64 then (
    prerr_endline "Non-64 bit architectures are not supported." ;
    exit 1)

let () =
  if Filename.basename Sys.argv.(0) = Updater.compiler_name then (
    try
      Tezos_protocol_compiler.Compiler.main
        Tezos_protocol_compiler_native.Native.driver ;
      Stdlib.exit 0
    with exn ->
      Format.eprintf "%a\n%!" Opterrors.report_error exn ;
      Stdlib.exit 1)

let () =
  if Filename.basename Sys.argv.(0) = "tezos-validator" then
    Tezos_validator.Command_line.run ()

let term =
  let open Cmdliner.Term in
  ret (const (`Help (`Pager, None)))

let description =
  [
    `S "DESCRIPTION";
    `P "Entry point for initializing, configuring and running a Tezos node.";
    `P Node_identity_command.Manpage.command_description;
    `P Node_run_command.Manpage.command_description;
    `P Node_replay_command.Manpage.command_description;
    `P Node_config_command.Manpage.command_description;
    `P Node_upgrade_command.Manpage.command_description;
    `P Node_snapshot_command.Manpage.command_description;
    `P Node_reconstruct_command.Manpage.command_description;
    `P Node_storage_command.Manpage.command_description;
  ]

let man = description @ Node_run_command.Manpage.examples

let info =
  let version = Tezos_version.Bin_version.version_string in
  Cmdliner.Term.info ~doc:"The Tezos node" ~man ~version "tezos-node"

let commands =
  [
    Node_run_command.cmd;
    Node_replay_command.cmd;
    Node_config_command.cmd;
    Node_identity_command.cmd;
    Node_upgrade_command.cmd;
    Node_snapshot_command.cmd;
    Node_reconstruct_command.cmd;
    Node_storage_command.cmd;
  ]

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
  | `Error _ -> exit 1
  | `Help -> exit 0
  | `Version -> exit 0
  | `Ok () -> exit 0
