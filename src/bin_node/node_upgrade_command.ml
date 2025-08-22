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

type error += Invalid_directory of string

let () =
  register_error_kind
    `Permanent
    ~id:"main.reconstruct.invalid_directory"
    ~title:"Invalid directory"
    ~description:"The data directory to upgrade cannot be found."
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "The data directory to upgrade cannot be found at %s"
        path)
    Data_encoding.(obj1 (req "path" string))
    (function Invalid_directory path -> Some path | _ -> None)
    (fun path -> Invalid_directory path)

(** Main *)

module Term = struct
  type upgrade = Storage

  let subcommand_arg =
    let parser = function
      | "storage" -> `Ok Storage
      | s -> `Error ("invalid argument: " ^ s)
    and printer ppf = function Storage -> Format.fprintf ppf "storage" in
    let open Cmdliner.Arg in
    let doc = "Upgrade to perform. Possible values: $(b, storage)." in
    required
    & pos 0 (some (parser, printer)) None
    & info [] ~docv:"UPGRADE" ~doc

  let upgrade_status ~data_dir =
    let open Lwt_result_syntax in
    let* available_upgrade = Data_version.upgrade_status ~data_dir in
    (* To ease the scripting of this command, we cut the workflow
       earlier and return an adequate exit code. *)
    let exit_code =
      if available_upgrade then
        (* Exiting with 1 stands for an error that aims to reflect
           that an upgrade is needed. *)
        1
      else 0
    in
    Lwt_exit.exit_and_raise exit_code

  let run_upgrade ~data_dir ~sandbox_file (config : Config_file.t) =
    let open Lwt_result_syntax in
    let* sandbox_parameters =
      match (config.blockchain_network.genesis_parameters, sandbox_file) with
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
    let genesis = config.blockchain_network.genesis in
    Data_version.upgrade_data_dir
      ~data_dir
      genesis
      ~chain_name:config.blockchain_network.chain_name
      ~sandbox_parameters

  let process subcommand data_dir config_file status sandbox_file =
    let run =
      let open Lwt_result_syntax in
      let*! () = Tezos_base_unix.Internal_event_unix.init () in
      match subcommand with
      | Storage -> (
          let* data_dir, config =
            Shared_arg.resolve_data_dir_and_config_file
              ?data_dir
              ?config_file
              ()
          in
          let*! r =
            Lwt_lock_file.with_lock
              ~when_locked:
                (`Fail
                   (Exn
                      (Failure
                         (Format.sprintf
                            "Failed to lock the data directory '%s'. Is a \
                             `octez-node` running?"
                            data_dir))))
              ~filename:(Data_version.lock_file data_dir)
              (fun () ->
                if status then upgrade_status ~data_dir
                else run_upgrade ~data_dir ~sandbox_file config)
          in
          match r with
          | Error (Exn (Unix.Unix_error (Unix.ENOENT, _, _)) :: _) ->
              (* The provided data directory to upgrade cannot be
                 found. *)
              tzfail (Invalid_directory data_dir)
          | Ok v -> Lwt.return (Ok v)
          | errs -> Lwt.return errs)
    in
    Lwt.Exception_filter.(set handle_all_except_runtime) ;
    match
      Tezos_base_unix.Event_loop.main_run ~process_name:"node upgrade"
      @@ fun () -> Lwt_exit.wrap_and_exit run
    with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_trace err)

  let status =
    let open Cmdliner.Arg in
    let doc =
      "Displays available upgrades. The command returns the exit code 1 when \
       an upgrade is available. Otherwise, 0 is returned."
    in
    value & flag & info ~doc ["status"]

  let sandbox =
    let open Cmdliner in
    let doc =
      "Run the upgrade storage in sandbox mode. P2P to non-localhost addresses \
       are disabled, and constants of the economic protocol can be altered \
       with an optional JSON file. $(b,IMPORTANT): Using sandbox mode affects \
       the node state and subsequent runs of Tezos node must also use sandbox \
       mode. In order to run the node in normal mode afterwards, a full reset \
       must be performed (by removing the node's data directory)."
    in
    Arg.(
      value
      & opt (some non_dir_file) None
      & info
          ~docs:Shared_arg.Manpage.misc_section
          ~doc
          ~docv:"FILE.json"
          ["sandbox"])

  let term =
    Cmdliner.Term.(
      ret
        (const process $ subcommand_arg $ Shared_arg.Term.data_dir
       $ Shared_arg.Term.config_file $ status $ sandbox))
end

module Manpage = struct
  let command_description =
    "The $(b,upgrade) command is meant to manage upgrades of the node."

  let description =
    [
      `S "DESCRIPTION";
      `P command_description;
      `P "Available upgrades are:";
      `P "$(b,storage) will upgrade the node disk storage (if needed).";
    ]

  let man =
    description
    @
    (* [ `S misc_docs ] @ *)
    Shared_arg.Manpage.bugs

  let info = Cmdliner.Cmd.info ~doc:"Manage node upgrades" ~man "upgrade"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
