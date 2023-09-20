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
  let process data_dir config_file sandbox_file progress_display_mode =
    let run =
      let open Lwt_result_syntax in
      let*! () = Tezos_base_unix.Internal_event_unix.init () in
      let* data_dir, node_config =
        Shared_arg.resolve_data_dir_and_config_file ?data_dir ?config_file ()
      in
      let ({genesis; _} : Config_file.blockchain_network) =
        node_config.blockchain_network
      in
      let* sandbox_parameters =
        match
          (node_config.blockchain_network.genesis_parameters, sandbox_file)
        with
        | None, None -> return_none
        | Some parameters, None ->
            return_some (parameters.context_key, parameters.values)
        | _, Some filename -> (
            let*! r = Lwt_utils_unix.Json.read_file filename in
            match r with
            | Error _err ->
                tzfail (Node_run_command.Invalid_sandbox_file filename)
            | Ok json -> return_some ("sandbox_parameter", json))
      in
      Lwt_lock_file.try_with_lock
        ~when_locked:(fun () -> tzfail Locked_directory)
        ~filename:(Data_version.lock_file data_dir)
      @@ fun () ->
      let context_dir = Data_version.context_dir data_dir in
      let store_dir = Data_version.store_dir data_dir in
      let patch_context =
        Patch_context.patch_context genesis sandbox_parameters
      in
      Reconstruction.reconstruct
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
        ~progress_display_mode
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
          ~docs:Shared_arg.Manpage.misc_section
          ~doc
          ~docv:"FILE.json"
          ["sandbox"])

  let progress_display_mode =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "Determine whether the progress animation will be displayed to the \
         logs. 'auto' will display progress animation only to a TTY. 'always' \
         will display progress animation to any file descriptor. 'never' will \
         not display progress animation."
    in
    Arg.(
      value
      & opt (enum Animation.progress_display_mode_enum) Animation.Auto
      & info
          ~docs:Shared_arg.Manpage.misc_section
          ~doc
          ~docv:"<auto|always|never>"
          ["progress-display-mode"])

  let term =
    let open Cmdliner.Term in
    ret
      (const process $ Shared_arg.Term.data_dir $ Shared_arg.Term.config_file
     $ sandbox $ progress_display_mode)
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
          "./octez-node reconstruct" );
    ]

  let man = description @ options @ examples @ Shared_arg.Manpage.bugs

  let info =
    Cmdliner.Cmd.info ~doc:"Manage storage reconstruction" ~man "reconstruct"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
