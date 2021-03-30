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

  let process subcommand args status =
    let run =
      Internal_event_unix.init () >>= fun () ->
      match subcommand with
      | Storage -> (
          Node_config_file.read args.Node_shared_arg.config_file
          >>=? fun config ->
          Lwt_lock_file.is_locked (Node_data_version.lock_file config.data_dir)
          >>=? function
          | true ->
              failwith
                "Failed to lock the data directory '%s'. Is a `tezos-node` \
                 running?"
                config.data_dir
          | false ->
              let data_dir = config.data_dir in
              Lwt_lock_file.create
                ~unlink_on_exit:true
                (Node_data_version.lock_file config.data_dir)
              >>=? fun () ->
              let genesis = config.blockchain_network.genesis in
              if status then Node_data_version.upgrade_status data_dir
              else
                Node_data_version.upgrade_data_dir
                  ~data_dir
                  genesis
                  ~chain_name:config.blockchain_network.chain_name )
    in
    match Lwt_main.run @@ Lwt_exit.wrap_and_exit run with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_error err)

  let status =
    let open Cmdliner.Arg in
    let doc = "Displays available upgrades." in
    value & flag & info ~doc ["status"]

  let term =
    Cmdliner.Term.(
      ret (const process $ subcommand_arg $ Node_shared_arg.Term.args $ status))
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
    @ (* [ `S misc_docs ] @ *)
    Node_shared_arg.Manpage.bugs

  let info = Cmdliner.Term.info ~doc:"Manage node upgrades" ~man "upgrade"
end

let cmd = (Term.term, Manpage.info)
