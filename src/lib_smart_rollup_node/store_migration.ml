(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

open Store_version

type version_result = Version_known | Unintialized_version

let messages_store_location ~storage_dir =
  let open Filename.Infix in
  storage_dir // "messages"

let version_of_unversioned_store ~storage_dir ~index_buffer_size =
  let open Lwt_syntax in
  let path = messages_store_location ~storage_dir in
  let* messages_store_v0 =
    Store_v0.Messages.load ~path ~cache_size:1 Read_only ~index_buffer_size
  and* messages_store_v1 =
    Store_v1.Messages.load ~path ~cache_size:1 Read_only ~index_buffer_size
  in
  let cleanup () =
    let open Lwt_syntax in
    let* (_ : unit tzresult) =
      match messages_store_v0 with
      | Error _ -> Lwt.return_ok ()
      | Ok s -> Store_v0.Messages.close s
    and* (_ : unit tzresult) =
      match messages_store_v1 with
      | Error _ -> Lwt.return_ok ()
      | Ok s -> Store_v1.Messages.close s
    in
    return_unit
  in
  let guess_version () =
    let open Lwt_result_syntax in
    match (messages_store_v0, messages_store_v1) with
    | Ok _, Error _ -> return_some V0
    | Error _, Ok _ -> return_some V1
    | Ok _, Ok _ ->
        (* Empty store, both loads succeed *)
        return_none
    | Error _, Error _ ->
        failwith
          "Cannot determine unversioned store version (no messages decodable)"
  in
  Lwt.finalize guess_version cleanup

let version_of_store ~storage_dir ~index_buffer_size =
  let open Lwt_result_syntax in
  let* version = Store_version.read_version_file ~dir:storage_dir in
  match version with
  | Some v -> return_some (v, Version_known)
  | None ->
      let+ v = version_of_unversioned_store ~storage_dir ~index_buffer_size in
      Option.map (fun v -> (v, Unintialized_version)) v

module type MIGRATION_ACTIONS = sig
  type from_store

  type dest_store

  val migrate_block_action :
    from_store -> dest_store -> Sc_rollup_block.t -> unit tzresult Lwt.t

  val final_actions :
    storage_dir:string ->
    tmp_dir:string ->
    from_store ->
    dest_store ->
    unit tzresult Lwt.t
end

module type S = sig
  val migrate :
    Metadata.t ->
    storage_dir:string ->
    index_buffer_size:int ->
    unit tzresult Lwt.t
end

let migrations = Stdlib.Hashtbl.create 7

module Make
    (S_from : Store_sig.S)
    (S_dest : Store_sig.S)
    (Actions : MIGRATION_ACTIONS
                 with type from_store := Store_sigs.ro S_from.t
                  and type dest_store := Store_sigs.rw S_dest.t) : S = struct
  let tmp_dir ~storage_dir =
    Filename.concat (Configuration.default_storage_dir storage_dir)
    @@ Format.asprintf
         "migration_%a_%a"
         Store_version.pp
         S_from.version
         Store_version.pp
         S_dest.version

  let migrate metadata ~storage_dir ~index_buffer_size =
    let open Lwt_result_syntax in
    let* source_store =
      S_from.load
        Read_only
        ~l2_blocks_cache_size:1
        storage_dir
        ~index_buffer_size
    in

    let tmp_dir = tmp_dir ~storage_dir in
    let*! tmp_dir_exists = Lwt_utils_unix.dir_exists tmp_dir in
    let*? () =
      if tmp_dir_exists then
        error_with
          "Store migration (from %a to %a) is already ongoing. Wait for it to \
           finish or remove %S and restart."
          Store_version.pp
          S_from.version
          Store_version.pp
          S_dest.version
          tmp_dir
      else Ok ()
    in
    let*! () = Lwt_utils_unix.create_dir tmp_dir in
    let* dest_store =
      S_dest.load Read_write ~l2_blocks_cache_size:1 tmp_dir ~index_buffer_size
    in
    let cleanup () =
      let open Lwt_syntax in
      let* (_ : unit tzresult) = S_from.close source_store
      and* (_ : unit tzresult) = S_dest.close dest_store in
      (* Don't remove migration dir to allow for later resume. *)
      return_unit
    in
    let run_migration () =
      let* () =
        S_from.iter_l2_blocks
          ~progress:
            (Format.asprintf
               "Migrating store from %a to %a"
               Store_version.pp
               S_from.version
               Store_version.pp
               S_dest.version)
          metadata
          source_store
          (Actions.migrate_block_action source_store dest_store)
      in
      let* () =
        Actions.final_actions ~storage_dir ~tmp_dir source_store dest_store
      in
      let*! () = Lwt_utils_unix.remove_dir tmp_dir in
      Store_version.write_version_file ~dir:storage_dir S_dest.version
    in
    Lwt.finalize run_migration cleanup

  let () = Stdlib.Hashtbl.add migrations S_from.version (S_dest.version, migrate)
end

let migration_path ~from ~dest =
  let rec path acc from dest =
    if from = dest then Some (List.rev acc)
    else
      let first_steps =
        Stdlib.Hashtbl.find_all migrations from
        |> List.stable_sort (fun (va, _) (vb, _) ->
               (* Try biggest jumps first that don't go beyond the
                  destination *)
               if va > dest && vb > dest then Stdlib.compare va vb
               else if va > dest then 1
               else if vb > dest then -1
               else Stdlib.compare vb va)
      in
      (* Recursively look for migration sub-paths *)
      let paths =
        List.filter_map
          (fun (step, migration) -> path (migration :: acc) step dest)
          first_steps
      in
      (* Choose shortest migration path *)
      List.stable_sort List.compare_lengths paths |> List.hd
  in
  path [] from dest

let maybe_run_migration metadata ~storage_dir ~index_buffer_size =
  let open Lwt_result_syntax in
  let* current_version = version_of_store ~storage_dir ~index_buffer_size in
  let last_version = Store.version in
  match (current_version, last_version) with
  | None, _ ->
      (* Store not initialized, write last version *)
      Store_version.write_version_file ~dir:storage_dir last_version
  | Some (current, versioned), last when last = current -> (
      match versioned with
      | Unintialized_version ->
          Store_version.write_version_file ~dir:storage_dir last_version
      | Version_known ->
          (* Up to date, nothing to do *)
          return_unit)
  | Some (current, _), last -> (
      let migrations = migration_path ~from:current ~dest:last in
      match migrations with
      | None ->
          failwith
            "Store version %a is not supported by this rollup node because \
             there is no migration path from it to %a."
            Store_version.pp
            current
            Store_version.pp
            last
      | Some migrations ->
          Format.printf "Starting store migration@." ;
          let+ () =
            List.iter_es
              (fun migrate -> migrate metadata ~storage_dir ~index_buffer_size)
              migrations
          in
          Format.printf "Store migration completed@.")

module V1_migrations = struct
  let convert_store_messages
      (messages, (block_hash, timestamp, number_of_messages)) =
    ( messages,
      (false (* is migration block *), block_hash, timestamp, number_of_messages)
    )

  let migrate_messages (v0_store : _ Store_v0.t) (v1_store : _ Store_v1.t)
      (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* v0_messages =
      Store_v0.Messages.read v0_store.messages l2_block.header.inbox_witness
    in
    match v0_messages with
    | None -> return_unit
    | Some v0_messages ->
        let value, header = convert_store_messages v0_messages in
        Store_v1.Messages.append
          v1_store.messages
          ~key:l2_block.header.inbox_witness
          ~header
          ~value

  (* In place migration of processed slots under new key name by hand *)
  let migrate_dal_processed_slots_irmin (v1_store : _ Store_v1.t) =
    let open Lwt_syntax in
    let open Store_v1 in
    let info () =
      let date =
        Tezos_base.Time.(
          System.now () |> System.to_protocol |> Protocol.to_seconds)
      in
      let author =
        Format.asprintf
          "Rollup node %a"
          Tezos_version_parser.pp
          Tezos_version_value.Current_git_info.octez_version
      in
      let message = "Migration store from v0 to v1" in
      Irmin_store.Raw_irmin.Info.v ~author ~message date
    in
    let store = Irmin_store.Raw_irmin.unsafe v1_store.irmin_store in
    let old_root = Store_v0.Dal_processed_slots.path in
    let new_root = Dal_slots_statuses.path in
    let* old_tree = Irmin_store.Raw_irmin.find_tree store old_root in
    match old_tree with
    | None -> return_unit
    | Some _ ->
        (* Move the tree in the new key *)
        Irmin_store.Raw_irmin.with_tree_exn
          ~info
          store
          new_root
          (fun _new_tree -> return old_tree)

  let final_actions ~storage_dir ~tmp_dir (_v0_store : _ Store_v0.t)
      (v1_store : _ Store_v1.t) =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_utils_unix.remove_dir (messages_store_location ~storage_dir)
    in
    let*! () =
      Lwt_unix.rename
        (messages_store_location ~storage_dir:tmp_dir)
        (messages_store_location ~storage_dir)
    in
    let*! () = migrate_dal_processed_slots_irmin v1_store in
    return_unit

  module From_v0 =
    Make (Store_v0) (Store_v1)
      (struct
        let migrate_block_action = migrate_messages

        let final_actions = final_actions
      end)
end

module V2_migrations = struct
  let messages_store_location ~storage_dir =
    let open Filename.Infix in
    storage_dir // "messages"

  let commitments_store_location ~storage_dir =
    let open Filename.Infix in
    storage_dir // "commitments"

  let inboxes_store_location ~storage_dir =
    let open Filename.Infix in
    storage_dir // "inboxes"

  let migrate_messages read_messages (v2_store : _ Store_v2.t)
      (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* v1_messages = read_messages l2_block.header.inbox_witness in
    match v1_messages with
    | None -> return_unit
    | Some (messages, _v1_header) ->
        let header = l2_block.header.block_hash in
        Store_v2.Messages.append
          v2_store.messages
          ~key:l2_block.header.inbox_witness
          ~header
          ~value:messages

  let migrate_commitment read_commitment (v2_store : _ Store_v2.t)
      (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    match l2_block.header.commitment_hash with
    | None -> return_unit
    | Some commitment_hash -> (
        let* v1_commitment = read_commitment commitment_hash in
        match v1_commitment with
        | None -> return_unit
        | Some commitment ->
            Store_v2.Commitments.append
              v2_store.commitments
              ~key:commitment_hash
              ~value:commitment)

  let migrate_inbox read_inbox (v2_store : _ Store_v2.t)
      (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* v1_inbox = read_inbox l2_block.header.inbox_hash in
    match v1_inbox with
    | None -> return_unit
    | Some (inbox, ()) ->
        Store_v2.Inboxes.append
          v2_store.inboxes
          ~key:l2_block.header.inbox_hash
          ~value:inbox

  let final_actions ~storage_dir ~tmp_dir _ _ =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_utils_unix.remove_dir (messages_store_location ~storage_dir)
    in
    let*! () =
      Lwt_utils_unix.remove_dir (commitments_store_location ~storage_dir)
    in
    let*! () =
      Lwt_utils_unix.remove_dir (inboxes_store_location ~storage_dir)
    in
    let*! () =
      Lwt_unix.rename
        (messages_store_location ~storage_dir:tmp_dir)
        (messages_store_location ~storage_dir)
    in
    let*! () =
      Lwt_unix.rename
        (commitments_store_location ~storage_dir:tmp_dir)
        (commitments_store_location ~storage_dir)
    in
    let*! () =
      Lwt_unix.rename
        (inboxes_store_location ~storage_dir:tmp_dir)
        (inboxes_store_location ~storage_dir)
    in
    return_unit

  module From_v1 =
    Make (Store_v1) (Store_v2)
      (struct
        let migrate_block_action v1_store v2_store l2_block =
          let open Lwt_result_syntax in
          let* () =
            migrate_messages
              (Store_v1.Messages.read v1_store.Store_v1.messages)
              v2_store
              l2_block
          and* () =
            migrate_commitment
              (Store_v1.Commitments.find v1_store.commitments)
              v2_store
              l2_block
          and* () =
            migrate_inbox
              (Store_v1.Inboxes.read v1_store.inboxes)
              v2_store
              l2_block
          in
          return_unit

        let final_actions = final_actions
      end)

  module From_v0 =
    Make (Store_v0) (Store_v2)
      (struct
        let migrate_block_action v0_store v2_store l2_block =
          let open Lwt_result_syntax in
          let* () =
            migrate_messages
              (Store_v0.Messages.read v0_store.Store_v0.messages)
              v2_store
              l2_block
          and* () =
            migrate_commitment
              (Store_v0.Commitments.find v0_store.commitments)
              v2_store
              l2_block
          and* () =
            migrate_inbox
              (Store_v0.Inboxes.read v0_store.inboxes)
              v2_store
              l2_block
          in
          return_unit

        let final_actions = final_actions
      end)
end

module V3_migrations = struct
  let levels_store_location ~storage_dir =
    let open Filename.Infix in
    storage_dir // "levels_to_hashes"

  let recompute_level (v3_store : _ Store_v3.t) (l2_block : Sc_rollup_block.t) =
    Store_v3.Levels_to_hashes.add
      ~flush:true
      v3_store.levels_to_hashes
      l2_block.header.level
      l2_block.header.block_hash

  let final_actions ~storage_dir ~tmp_dir _ _ =
    let open Lwt_result_syntax in
    let*! () = Lwt_utils_unix.remove_dir (levels_store_location ~storage_dir) in
    let*! () =
      Lwt_unix.rename
        (levels_store_location ~storage_dir:tmp_dir)
        (levels_store_location ~storage_dir)
    in
    return_unit

  module From_v2 =
    Make (Store_v2) (Store_v3)
      (struct
        let migrate_block_action _v2_store v3_store l2_block =
          recompute_level v3_store l2_block

        let final_actions = final_actions
      end)
end

module V4_migrations = struct
  let messages_store_location ~storage_dir =
    let open Filename.Infix in
    storage_dir // "messages"

  let migrate_messages (v3_store : _ Store_v3.t) (v4_store : _ Store_v4.t)
      (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* v3_messages =
      Store_v3.Messages.read v3_store.messages l2_block.header.inbox_witness
    in
    match v3_messages with
    | None -> return_unit
    | Some (messages, _v3_header) ->
        let header = l2_block.header.predecessor in
        Store_v4.Messages.append
          v4_store.messages
          ~key:l2_block.header.inbox_witness
          ~header
          ~value:messages

  let final_actions ~storage_dir ~tmp_dir _ _ =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_utils_unix.remove_dir (messages_store_location ~storage_dir)
    in
    let*! () =
      Lwt_unix.rename
        (messages_store_location ~storage_dir:tmp_dir)
        (messages_store_location ~storage_dir)
    in
    return_unit

  module From_v3 =
    Make (Store_v3) (Store_v4)
      (struct
        let migrate_block_action v3_store v4_store l2_block =
          migrate_messages v3_store v4_store l2_block

        let final_actions = final_actions
      end)
end
