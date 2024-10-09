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

let storage_dir data_dir = Filename.concat data_dir "storage"

let messages_store_location ~storage_dir =
  let open Filename.Infix in
  storage_dir // "messages"

let version_of_unversioned_store ~storage_dir =
  let open Lwt_syntax in
  let path = messages_store_location ~storage_dir in
  let* messages_store_v0 =
    Store_v0.Messages.load ~path ~cache_size:1 Read_only ~index_buffer_size:100
  and* messages_store_v1 =
    Store_v1.Messages.load ~path ~cache_size:1 Read_only ~index_buffer_size:100
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

let version_of_store ~storage_dir =
  let open Lwt_result_syntax in
  let* version = Store_version.read_version_file ~dir:storage_dir in
  match version with
  | Some v -> return_some (v, Version_known)
  | None ->
      let+ v = version_of_unversioned_store ~storage_dir in
      Option.map (fun v -> (v, Unintialized_version)) v

(** Type of parameter for migration functor {!Make}. *)
module type MIGRATION_ACTIONS = sig
  (** Type of store from which data is migrated. *)
  type from_store

  (** Type of store to which the data is migrated. *)
  type dest_store

  (** Action or actions to migrate data associated to a block. NOTE:
      [dest_store] is an empty R/W store initialized in a temporary location. *)
  val migrate_block_action :
    from_store -> dest_store -> Sc_rollup_block.t -> unit tzresult Lwt.t

  (** The final actions to be performed in the migration. In particular, this is
      where data from the temporary store in [dest_store] in [tmp_dir] should be
      reported in the actual [storage_dir]. *)
  val final_actions :
    data_dir:string ->
    tmp_dir:string ->
    from_store ->
    dest_store ->
    unit tzresult Lwt.t
end

let migrations = Stdlib.Hashtbl.create 7

module type STORE = sig
  type 'a t constraint 'a = [< `Read | `Write > `Read]

  val version : Store_version.t

  val close : _ t -> unit tzresult Lwt.t

  val load : 'a Store_sigs.mode -> data_dir:string -> 'a t tzresult Lwt.t

  val iter_l2_blocks :
    ?progress:string ->
    Metadata.t ->
    _ t ->
    (Sc_rollup_block.t -> unit tzresult Lwt.t) ->
    unit tzresult Lwt.t
end

module Make
    (S_from : STORE)
    (S_dest : STORE)
    (Actions : MIGRATION_ACTIONS
                 with type from_store := Store_sigs.ro S_from.t
                  and type dest_store := Store_sigs.rw S_dest.t) =
struct
  let tmp_dir ~data_dir =
    Filename.concat data_dir
    @@ Format.asprintf
         "store_migration_%a_%a"
         Store_version.pp
         S_from.version
         Store_version.pp
         S_dest.version

  let migrate metadata ~data_dir =
    let open Lwt_result_syntax in
    let* source_store = S_from.load Read_only ~data_dir in

    let tmp_dir = tmp_dir ~data_dir in
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
    let* dest_store = S_dest.load Read_write ~data_dir:tmp_dir in
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
        Actions.final_actions ~data_dir ~tmp_dir source_store dest_store
      in
      let*! () = Lwt_utils_unix.remove_dir tmp_dir in
      Store_version.write_version_file
        ~dir:(storage_dir data_dir)
        S_dest.version
    in
    Lwt.finalize run_migration cleanup

  let () = Stdlib.Hashtbl.add migrations S_from.version (S_dest.version, migrate)
end

module Wrap_old (S : Store_sig.S) : STORE with type 'a t = 'a S.t = struct
  include S

  let load mode ~data_dir =
    load
      mode
      (storage_dir data_dir)
      ~index_buffer_size:100
      ~l2_blocks_cache_size:1
end

module Make_old_old (S_from : Store_sig.S) (S_dest : Store_sig.S) =
  Make (Wrap_old (S_from)) (Wrap_old (S_dest))

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

let maybe_run_migration metadata last_version ~data_dir =
  let open Lwt_result_syntax in
  let storage_dir = storage_dir data_dir in
  let* current_version = version_of_store ~storage_dir in
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
            List.iter_es (fun migrate -> migrate metadata ~data_dir) migrations
          in
          Format.printf "Store migration completed@.")

module V1_migrations = struct
  let messages_store_location ~data_dir =
    let open Filename.Infix in
    storage_dir data_dir // "messages"

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
      let author = "Rollup node" in
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

  let final_actions ~data_dir ~tmp_dir (_v0_store : _ Store_v0.t)
      (v1_store : _ Store_v1.t) =
    let open Lwt_result_syntax in
    let*! () = Lwt_utils_unix.remove_dir (messages_store_location ~data_dir) in
    let*! () =
      Lwt_unix.rename
        (messages_store_location ~data_dir:tmp_dir)
        (messages_store_location ~data_dir)
    in
    let*! () = migrate_dal_processed_slots_irmin v1_store in
    return_unit

  module From_v0 =
    Make_old_old (Store_v0) (Store_v1)
      (struct
        let migrate_block_action = migrate_messages

        let final_actions = final_actions
      end)
end

module V2_migrations = struct
  let messages_store_location ~data_dir =
    let open Filename.Infix in
    storage_dir data_dir // "messages"

  let commitments_store_location ~data_dir =
    let open Filename.Infix in
    storage_dir data_dir // "commitments"

  let inboxes_store_location ~data_dir =
    let open Filename.Infix in
    storage_dir data_dir // "inboxes"

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

  let final_actions ~data_dir ~tmp_dir _ _ =
    let open Lwt_result_syntax in
    let*! () = Lwt_utils_unix.remove_dir (messages_store_location ~data_dir) in
    let*! () =
      Lwt_utils_unix.remove_dir (commitments_store_location ~data_dir)
    in
    let*! () = Lwt_utils_unix.remove_dir (inboxes_store_location ~data_dir) in
    let*! () =
      Lwt_unix.rename
        (messages_store_location ~data_dir:tmp_dir)
        (messages_store_location ~data_dir)
    in
    let*! () =
      Lwt_unix.rename
        (commitments_store_location ~data_dir:tmp_dir)
        (commitments_store_location ~data_dir)
    in
    let*! () =
      Lwt_unix.rename
        (inboxes_store_location ~data_dir:tmp_dir)
        (inboxes_store_location ~data_dir)
    in
    return_unit

  module From_v1 =
    Make_old_old (Store_v1) (Store_v2)
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
    Make_old_old (Store_v0) (Store_v2)
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
  let levels_store_location ~data_dir =
    let open Filename.Infix in
    storage_dir data_dir // "levels_to_hashes"

  let recompute_level (v3_store : _ Store_v3.t) (l2_block : Sc_rollup_block.t) =
    Store_v3.Levels_to_hashes.add
      ~flush:true
      v3_store.levels_to_hashes
      l2_block.header.level
      l2_block.header.block_hash

  let final_actions ~data_dir ~tmp_dir _ _ =
    let open Lwt_result_syntax in
    let*! () = Lwt_utils_unix.remove_dir (levels_store_location ~data_dir) in
    let*! () =
      Lwt_unix.rename
        (levels_store_location ~data_dir:tmp_dir)
        (levels_store_location ~data_dir)
    in
    return_unit

  module From_v2 =
    Make_old_old (Store_v2) (Store_v3)
      (struct
        let migrate_block_action _v2_store v3_store l2_block =
          recompute_level v3_store l2_block

        let final_actions = final_actions
      end)
end

module V4_migrations = struct
  let messages_store_location ~data_dir =
    let open Filename.Infix in
    storage_dir data_dir // "messages"

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

  let final_actions ~data_dir ~tmp_dir _ _ =
    let open Lwt_result_syntax in
    let*! () = Lwt_utils_unix.remove_dir (messages_store_location ~data_dir) in
    let*! () =
      Lwt_unix.rename
        (messages_store_location ~data_dir:tmp_dir)
        (messages_store_location ~data_dir)
    in
    return_unit

  module From_v3 =
    Make_old_old (Store_v3) (Store_v4)
      (struct
        let migrate_block_action v3_store v4_store l2_block =
          migrate_messages v3_store v4_store l2_block

        let final_actions = final_actions
      end)
end

module V5_sqlite_migrations = struct
  module Wraped_v5 : STORE with type 'a t = 'a Store_v5.t = struct
    include Store_v5

    type 'a t = 'a Store_v5.t constraint 'a = [< `Read | `Write > `Read]

    let load mode ~data_dir = init mode ~data_dir

    let close store = close store |> Lwt_result.ok

    let iter_l2_blocks ?progress:_ _ _ _ =
      (* unsused *)
      assert false
  end

  let migrate_messages (v4_store : _ Store_v4.t) (v5_store : _ Store_v5.t)
      (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* messages =
      Store_v4.Messages.read v4_store.messages l2_block.header.inbox_witness
    in
    match messages with
    | None -> failwith "Missing messages for %ld" l2_block.header.level
    | Some (messages, _pred) ->
        Store_v5.Messages.store
          v5_store
          ~level:l2_block.header.level
          l2_block.header.inbox_witness
          messages

  let migrate_inbox (v4_store : _ Store_v4.t) (v5_store : _ Store_v5.t)
      (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* inbox =
      Store_v4.Inboxes.read v4_store.inboxes l2_block.header.inbox_hash
    in
    match inbox with
    | None -> failwith "Missing inbox for %ld" l2_block.header.level
    | Some (inbox, ()) ->
        let* _inbox_hash = Store_v5.Inboxes.store v5_store inbox in
        return_unit

  let migrate_commitment (v4_store : _ Store_v4.t) (v5_store : _ Store_v5.t)
      (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    match l2_block.header.commitment_hash with
    | None -> return_unit
    | Some commitment_hash -> (
        let* commitment =
          Store_v4.Commitments.read v4_store.commitments commitment_hash
        in
        match commitment with
        | None -> failwith "Missing commitment for %ld" l2_block.header.level
        | Some (commitment, ()) -> (
            let* _commitment_hash =
              Store_v5.Commitments.store v5_store commitment
            in
            let* commitment_published_at_level =
              Store_v4.Commitments_published_at_level.find
                v4_store.commitments_published_at_level
                commitment_hash
            in
            match commitment_published_at_level with
            | None -> return_unit
            | Some {first_published_at_level; published_at_level} ->
                Store_v5.Commitments_published_at_levels.register
                  v5_store
                  commitment_hash
                  {first_published_at_level; published_at_level}))

  let migrate_dal_slot_headers (v4_store : _ Store_v4.t)
      (v5_store : _ Store_v5.t) (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* headers =
      Store_v4.Dal_slots_headers.list_values
        v4_store.irmin_store
        ~primary_key:l2_block.header.block_hash
    in
    List.iter_es
      (Store_v5.Dal_slots_headers.store v5_store l2_block.header.block_hash)
      headers

  let migrate_dal_slot_statuses (v4_store : _ Store_v4.t)
      (v5_store : _ Store_v5.t) (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* statuses =
      Store_v4.Dal_slots_statuses.list_secondary_keys_with_values
        v4_store.irmin_store
        ~primary_key:l2_block.header.block_hash
    in
    List.iter_es
      (fun (slot_index, status) ->
        Store_v5.Dal_slots_statuses.store
          v5_store
          l2_block.header.block_hash
          slot_index
          status)
      statuses

  let migrate_l2_block (v4_store : _ Store_v4.t) (v5_store : _ Store_v5.t)
      (l2_block : Sc_rollup_block.t) =
    let open Lwt_result_syntax in
    let* () =
      Store_v5.L2_levels.store
        v5_store
        l2_block.header.level
        l2_block.header.block_hash
    in
    let* () = migrate_messages v4_store v5_store l2_block in
    let* () = migrate_inbox v4_store v5_store l2_block in
    let* () = migrate_commitment v4_store v5_store l2_block in
    let* () = migrate_dal_slot_headers v4_store v5_store l2_block in
    let* () = migrate_dal_slot_statuses v4_store v5_store l2_block in
    Store_v5.L2_blocks.store v5_store l2_block

  let migrate_outbox_messages (v4_store : _ Store_v4.t)
      (v5_store : _ Store_v5.t) =
    let open Lwt_result_syntax in
    Store_v4.Outbox_messages.iter v4_store.outbox_messages
    @@ fun ~outbox_level ~messages ~executed_messages ->
    let*? indexes = Bitset.from_list messages in
    let* () =
      Store_v5.Outbox_messages.register_outbox_messages
        v5_store
        ~outbox_level
        ~indexes
    in
    List.iter_es
      (fun index ->
        Store_v5.Outbox_messages.set_outbox_message_executed
          v5_store
          ~outbox_level
          ~index)
      executed_messages

  let migrate_protocols (v4_store : _ Store_v4.t) (v5_store : _ Store_v5.t) =
    let open Lwt_result_syntax in
    let* protocols = Store_v4.Protocols.read v4_store.protocols in
    match protocols with
    | None -> return_unit
    | Some protocols ->
        List.rev protocols
        |> List.iter_es
             (fun Store_v4.Protocols.{level; proto_level; protocol} ->
               let level =
                 match level with
                 | First_known l -> Store_v5.Protocols.First_known l
                 | Activation_level l -> Activation_level l
               in
               Store_v5.Protocols.store v5_store {level; proto_level; protocol})

  let migrate_rollup_node_state (v4_store : _ Store_v4.t)
      (v5_store : _ Store_v5.t) =
    let open Lwt_result_syntax in
    (* migrate lcc *)
    let* lcc = Store_v4.Lcc.read v4_store.lcc in
    let* () =
      match lcc with
      | None -> return_unit
      | Some {commitment; level} ->
          Store_v5.State.LCC.set v5_store (commitment, level)
    in
    (* migrate lpc *)
    let* lpc = Store_v4.Lpc.read v4_store.lpc in
    let* () =
      match lpc with
      | None -> return_unit
      | Some commitment ->
          let hash = Commitment.hash commitment in
          Store_v5.State.LPC.set v5_store (hash, commitment.inbox_level)
    in
    (* migrate head *)
    let* head = Store_v4.L2_head.read v4_store.l2_head in
    let* () =
      match head with
      | None -> return_unit
      | Some head ->
          Store_v5.State.L2_head.set
            v5_store
            (head.header.block_hash, head.header.level)
    in
    (* migrate finalized *)
    let* finalized =
      Store_v4.Last_finalized_level.read v4_store.last_finalized_level
    in
    let* () =
      match finalized with
      | None -> return_unit
      | Some finalized_level ->
          let* finalized_hash =
            Store_v5.L2_levels.find v5_store finalized_level
          in
          let finalized_hash =
            WithExceptions.Option.get ~loc:__LOC__ finalized_hash
          in
          Store_v5.State.Finalized_level.set
            v5_store
            (finalized_hash, finalized_level)
    in
    (* migrate context split *)
    let* split =
      Store_v4.Last_context_split.read v4_store.last_context_split_level
    in
    let* () =
      match split with
      | None -> return_unit
      | Some l -> Store_v5.State.Last_context_split.set v5_store l
    in
    (* migrate last GC *)
    let* gc = Store_v4.Gc_levels.read v4_store.gc_levels in
    let* () =
      match gc with
      | None -> return_unit
      | Some {last_gc_level; first_available_level} ->
          let* () =
            Store_v5.State.Last_gc_target.set v5_store first_available_level
          in
          Store_v5.State.Last_gc_triggered_at.set v5_store last_gc_level
    in
    (* migrate last successful GC *)
    let* gc = Store_v4.Gc_levels.read v4_store.successful_gc_levels in
    let* () =
      match gc with
      | None -> return_unit
      | Some {last_gc_level; first_available_level} ->
          let* () =
            Store_v5.State.Last_successful_gc_target.set
              v5_store
              first_available_level
          in
          Store_v5.State.Last_successful_gc_triggered_at.set
            v5_store
            last_gc_level
    in
    (* migrate history mode *)
    let* history_mode = Store_v4.History_mode.read v4_store.history_mode in
    let* () =
      match history_mode with
      | None -> return_unit
      | Some Archive -> Store_v5.State.History_mode.set v5_store Archive
      | Some Full -> Store_v5.State.History_mode.set v5_store Full
    in
    return_unit

  let final_actions ~data_dir ~tmp_dir v4_store v5_store =
    let open Lwt_result_syntax in
    let* () = migrate_outbox_messages v4_store v5_store in
    let* () = migrate_protocols v4_store v5_store in
    let* () = migrate_rollup_node_state v4_store v5_store in
    let*! () = Lwt_utils_unix.remove_dir (storage_dir data_dir) in
    let mv file =
      let src = Filename.concat tmp_dir file in
      let*! exists = Lwt_unix.file_exists src in
      if not exists then Lwt.return_unit
      else Lwt_unix.rename src (Filename.concat data_dir file)
    in
    let*! () =
      List.iter_s mv Sql_store.(sqlite_file_name :: extra_sqlite_files)
    in
    return_unit

  module From_v4 =
    Make
      (Wrap_old (Store_v4)) (Wraped_v5)
      (struct
        let migrate_block_action = migrate_l2_block

        let final_actions = final_actions
      end)
end
