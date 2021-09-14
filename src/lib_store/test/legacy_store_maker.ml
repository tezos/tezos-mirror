(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module is meant to be used by python's tests to create legacy
    stores that will be used to test legacy features. *)

open Legacy_utils
open Filename.Infix

let available_snapshots = function
  | History_mode.Legacy.Archive -> [History_mode.Legacy.Full; Rolling]
  | Full -> [Full; Rolling]
  | Rolling -> [Rolling]

let run ~base_dir ~legacy_store_builder_exe ~with_snapshots nb_blocks =
  (* Build a new store un archive mode *)
  build_new_store
    nb_blocks
    ~base_dir
    ~patch_context
    ~history_mode:History_mode.Archive
  >>=? fun (store, genesis, genesis_block, blocks) ->
  (* From the baked blocks, build all available history modes *)
  let history_modes = History_mode.Legacy.[Archive; Full; Rolling] in
  List.iter_es
    (fun legacy_history_mode ->
      let legacy_data_dir =
        base_dir
        // Format.asprintf
             "%a_store_to_upgrade"
             History_mode.Legacy.pp
             legacy_history_mode
      in
      Format.printf "[Legacy store maker] Building %s@.@." legacy_data_dir ;
      let legacy_store_dir = legacy_data_dir // "store" in
      let legacy_context_dir = legacy_data_dir // "context" in
      build_old_store
        ~genesis
        ~genesis_block
        ~legacy_history_mode
        ~store
        ~legacy_data_dir
        ~legacy_store_builder_exe
        blocks
      >>=? fun _ ->
      (if with_snapshots then
       List.iter_es
         (fun snapshot_mode ->
           let snapshot_file =
             base_dir
             // Format.asprintf
                  "snapshot_from_%a_storage.%a"
                  History_mode.Legacy.pp
                  legacy_history_mode
                  History_mode.Legacy.pp
                  snapshot_mode
           in
           let head_hash =
             Store.Block.hash
               List.(last_opt blocks |> WithExceptions.Option.get ~loc:__LOC__)
           in
           Format.printf
             "[Legacy store maker] Exporting snapshot file: %s@.@."
             snapshot_file ;
           Legacy_snapshots.export
             ~export_rolling:(snapshot_mode = History_mode.Legacy.Rolling)
             ~store_root:legacy_store_dir
             ~context_root:legacy_context_dir
             ~genesis
             snapshot_file
             head_hash)
         (available_snapshots legacy_history_mode)
      else return_unit)
      >>=? fun () -> return_unit)
    history_modes
  >>=? fun () -> return_unit

let () =
  let base_dir = Sys.argv.(1) in
  let legacy_store_builder_exe = Sys.argv.(2) in
  let nb_blocks = `Blocks (int_of_string Sys.argv.(3)) in
  let with_snapshots = match Sys.argv.(4) with "true" -> true | _ -> false in
  match
    Lwt_main.run
      (run ~base_dir ~legacy_store_builder_exe ~with_snapshots nb_blocks)
  with
  | Ok () -> ()
  | Error err ->
      Format.eprintf "%a@." pp_print_trace err ;
      exit 1
