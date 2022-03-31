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

type error += Locked_directory

let () =
  register_error_kind
    `Permanent
    ~id:"main.reconstruct.locked_directory"
    ~title:"Locked directory"
    ~description:"The data directory to reconstruct is used by another process."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The data directory to reconstruct is locked by another process. You \
         should first turn off the node before reconstructing its storage.")
    Data_encoding.empty
    (function Locked_directory -> Some () | _ -> None)
    (fun () -> Locked_directory)

(** Main *)

module Term = struct
  let process args sandbox_file =
    let run =
      Internal_event_unix.init () >>= fun () ->
      Node_shared_arg.read_and_patch_config_file args >>=? fun node_config ->
      let data_dir = node_config.data_dir in
      let ({genesis; _} : Node_config_file.blockchain_network) =
        node_config.blockchain_network
      in
      (match
         (node_config.blockchain_network.genesis_parameters, sandbox_file)
       with
      | (None, None) -> return_none
      | (Some parameters, None) ->
          return_some (parameters.context_key, parameters.values)
      | (_, Some filename) -> (
          Lwt_utils_unix.Json.read_file filename >>= function
          | Error _err -> fail (Node_run_command.Invalid_sandbox_file filename)
          | Ok json -> return_some ("sandbox_parameter", json)))
      >>=? fun sandbox_parameters ->
      Lwt_lock_file.try_with_lock
        ~when_locked:(fun () -> fail Locked_directory)
        ~filename:(Node_data_version.lock_file data_dir)
      @@ fun () ->
      let context_dir = Node_data_version.context_dir data_dir in
      let store_dir = Node_data_version.store_dir data_dir in
      let patch_context =
        Patch_context.patch_context genesis sandbox_parameters
      in
      Tezos_store.Reconstruction.reconstruct
        ~patch_context
        ~store_dir
        ~context_dir
        genesis
        ~user_activated_upgrades:
          node_config.blockchain_network.user_activated_upgrades
        ~user_activated_protocol_overrides:
          node_config.blockchain_network.user_activated_protocol_overrides
        ~operation_metadata_size_limit:
          node_config.shell.block_validator_limits.operation_metadata_size_limit
      >>=? fun () -> return_unit
    in
    match Lwt_main.run @@ Lwt_exit.wrap_and_exit run with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_trace err)

  let sandbox =
    let open Cmdliner in
    let doc =
      "Run the storage reconstruction in sandbox mode. P2P to non-localhost \
       addresses are disabled, and constants of the economic protocol can be \
       altered with an optional JSON file. $(b,IMPORTANT): Using sandbox mode \
       affects the node state and subsequent runs of Tezos node must also use \
       sandbox mode. In order to run the node in normal mode afterwards, a \
       full reset must be performed (by removing the node's data directory)."
    in
    Arg.(
      value
      & opt (some non_dir_file) None
      & info
          ~docs:Node_shared_arg.Manpage.misc_section
          ~doc
          ~docv:"FILE.json"
          ["sandbox"])

  let term =
    let open Cmdliner.Term in
    ret (const process $ Node_shared_arg.Term.args $ sandbox)
end

module Manpage = struct
  let command_description =
    "The $(b,reconstruct) command is meant to reconstruct the partial storage \
     of a node running in $(b,full) mode in order to recover a complete \
     $(b,archive) mode storage."

  let description = [`S "DESCRIPTION"; `P command_description]

  let options = []

  let examples =
    [
      `S "EXAMPLES";
      `I
        ( "$(b,Reconstruct the storage of a full mode node )",
          "./tezos-node reconstruct" );
    ]

  let man = description @ options @ examples @ Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info ~doc:"Manage storage reconstruction" ~man "reconstruct"
end

let cmd = (Term.term, Manpage.info)
