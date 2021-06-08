(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
    ~description:
      "The data directory to reconstruct is used by another process."
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
  let process args =
    let run =
      Internal_event_unix.init ()
      >>= fun () ->
      Node_shared_arg.read_and_patch_config_file
        ~ignore_bootstrap_peers:true
        args
      >>=? fun config ->
      let data_dir = config.data_dir in
      Node_data_version.ensure_data_dir data_dir
      >>=? fun () ->
      Lwt_lock_file.is_locked (Node_data_version.lock_file data_dir)
      >>=? fun is_locked ->
      fail_when is_locked Locked_directory
      >>=? fun () ->
      let context_root = Node_data_version.context_dir data_dir in
      let store_root = Node_data_version.store_dir data_dir in
      Store.init ~mapsize:409_600_000_000L store_root
      >>=? fun store ->
      let genesis = config.blockchain_network.genesis in
      State.init ~context_root ~store_root genesis
      >>=? fun (state, chain_state, context_index, history_mode) ->
      let chain_id = Chain_id.of_block_hash genesis.Genesis.block in
      fail_unless
        (history_mode = History_mode.Full)
        (Snapshots.Cannot_reconstruct history_mode)
      >>=? fun () ->
      Snapshots.reconstruct
        chain_id
        ~user_activated_upgrades:
          config.blockchain_network.user_activated_upgrades
        ~user_activated_protocol_overrides:
          config.blockchain_network.user_activated_protocol_overrides
        store
        chain_state
        context_index
      >>=? fun () ->
      Store.close store ;
      State.close state >>= fun () -> return_unit
    in
    match Lwt_main.run @@ Lwt_exit.wrap_and_exit run with
    | Ok () ->
        `Ok ()
    | Error err ->
        `Error (false, Format.asprintf "%a" pp_print_error err)

  let term =
    let open Cmdliner.Term in
    ret (const process $ Node_shared_arg.Term.args)
end

module Manpage = struct
  let command_description =
    "The $(b,reconstruct) command is meant to reconstruct the partial storage \
     of a node running in $(b,full) mode in order to recover a complete \
     $(b,archive) mode storage."

  let description = [`S "DESCRIPTION"; `P command_description]

  let options = []

  let examples =
    [ `S "EXAMPLES";
      `I
        ( "$(b,Reconstruct the storage of a full mode node )",
          "./tezos-node reconstruct" ) ]

  let man = description @ options @ examples @ Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info ~doc:"Manage storage reconstruction" ~man "reconstruct"
end

let cmd = (Term.term, Manpage.info)
