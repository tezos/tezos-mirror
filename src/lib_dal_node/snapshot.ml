(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(*
  This module uses the [Key_value_store.Read] module to open the store in
  readonly mode, ensuring that the export leaves source data untouched, while
  still allowing another DAL node to run.
*)

open Filename.Infix

(** Read a value from a Single_value_store module in readonly mode.
    Fails if the value is not found. *)
let read_from_store (type value) ~root_dir
    (module Store : Single_value_store.S with type value = value) =
  let open Lwt_result_syntax in
  let* store = Store.init_readonly ~root_dir in
  let* value_opt = Store.load store in
  match value_opt with
  | Some value -> return value
  | None -> failwith "Value not found in store at %s" root_dir

(** Iterate through all levels in the given range, calling [f] for each level. *)
let iterate_levels ~min_published_level ~max_published_level f =
  let open Lwt_result_syntax in
  let rec iterate_levels level =
    if Compare.Int32.(level > max_published_level) then return_unit
    else
      let* () = f level in
      iterate_levels (Int32.succ level)
  in
  iterate_levels min_published_level

(** Iterate through all levels and slot indices in the given range,
    calling [f] for each [{slot_level; slot_index}] slot id. *)
let iterate_all_slots ~min_published_level ~max_published_level f =
  let open Lwt_result_syntax in
  iterate_levels ~min_published_level ~max_published_level @@ fun level ->
  let rec iterate_slot_index slot_index =
    if slot_index >= Constants.number_of_slots then return_unit
    else
      let slot_id = Types.Slot_id.{slot_level = level; slot_index} in
      let* () = f slot_id in
      iterate_slot_index (slot_index + 1)
  in
  iterate_slot_index 0

(** Copy a value from source KVS to destination KVS.
    Returns [Ok ()] if the value was copied or if src value is not found. *)
let kvs_copy_value src_store dst_store file_layout file key =
  let open Lwt_result_syntax in
  let* exists =
    Key_value_store.Read.value_exists src_store file_layout file key
  in
  if not exists then return_unit
  else
    let* value =
      Key_value_store.Read.read_value src_store file_layout file key
    in
    Key_value_store.write_value
      ~override:false
      dst_store
      file_layout
      file
      key
      value

module Export = struct
  (** Export a filtered subset of the skip_list SQLite database by iterating
      through levels in the range [min_published_level, max_published_level]
      and copying the data using Dal_store_sqlite3 functions. *)
  let export_skip_list ~src ~dst ~min_published_level ~max_published_level =
    let open Lwt_result_syntax in
    let*! () = Lwt_utils_unix.create_dir dst in
    (* Initialize destination database with empty schema *)
    let* dst_db =
      Dal_store_sqlite3.Skip_list_cells.init
        ~data_dir:dst
        ~perm:Sqlite.Read_write
        ()
    in
    (* Open source database in read-only mode *)
    let* src_db =
      Dal_store_sqlite3.Skip_list_cells.init
        ~data_dir:src
        ~perm:Sqlite.(Read_only {pool_size = 1})
        ()
    in
    Lwt.finalize
      (fun () ->
        iterate_levels ~min_published_level ~max_published_level @@ fun level ->
        (* Get all skip list cells for this level from source *)
        let* cells =
          Dal_store_sqlite3.Skip_list_cells.find_by_level
            src_db
            ~published_level:level
        in
        (* For each slot in this level, get complete info and insert into destination *)
        let* items_with_lag =
          List.map_es
            (fun (_cell, hash, slot_index) ->
              let slot_id = Types.Slot_id.{slot_level = level; slot_index} in
              let* result =
                Dal_store_sqlite3.Skip_list_cells.find_by_slot_id_opt
                  src_db
                  slot_id
              in
              match result with
              | Some (cell, attestation_lag) ->
                  return (hash, cell, slot_index, attestation_lag)
              | None -> failwith "Cell found by level but not by slot_id")
            cells
        in
        match items_with_lag with
        | [] -> return_unit
        | (_, _, _, attestation_lag) :: _ ->
            let attested_level = Int32.(add level (of_int attestation_lag)) in
            Dal_store_sqlite3.Skip_list_cells.insert
              dst_db
              ~attested_level
              items_with_lag
              Fun.id)
      (fun () ->
        let open Lwt_syntax in
        let* _ = Dal_store_sqlite3.Skip_list_cells.close src_db in
        let* _ = Dal_store_sqlite3.Skip_list_cells.close dst_db in
        return_unit)

  (** Export slots for all published slots in the given level range.
      Copies slot files from source to destination directory. *)
  let export_slots ~cryptobox ~src ~dst ~min_published_level
      ~max_published_level =
    let open Lwt_result_syntax in
    let*! () = Lwt_utils_unix.create_dir dst in
    let* src_store =
      Key_value_store.Read.init ~lru_size:Constants.slots_store_lru_size src
    in
    let* dst_store =
      Key_value_store.init
        ~lru_size:Constants.slots_store_lru_size
        ~root_dir:dst
    in
    Lwt.finalize
      (fun () ->
        iterate_all_slots ~min_published_level ~max_published_level
        @@ fun slot_id ->
        let Cryptobox.{slot_size; _} = Cryptobox.parameters cryptobox in
        let file_layout = Store.Slots.file_layout in
        kvs_copy_value src_store dst_store file_layout (slot_id, slot_size) ())
      (fun () ->
        let open Lwt_syntax in
        let* _ = Key_value_store.Read.close src_store in
        let* _ = Key_value_store.close dst_store in
        return_unit)

  (** Export shards for all slots in the given level range.
      Copies shard files from source to destination directory. *)
  let export_shards ~src ~dst ~min_published_level ~max_published_level =
    let open Lwt_result_syntax in
    let*! () = Lwt_utils_unix.create_dir dst in
    let* src_store =
      Key_value_store.Read.init ~lru_size:Constants.shards_store_lru_size src
    in
    let* dst_store =
      Key_value_store.init
        ~lru_size:Constants.shards_store_lru_size
        ~root_dir:dst
    in
    Lwt.finalize
      (fun () ->
        (* For each level, we need to check all possible slot indices.
           In practice, we should get this from the skip_list data,
           but for now we'll scan for existing files. *)
        iterate_all_slots ~min_published_level ~max_published_level
        @@ fun slot_id ->
        let file_layout = Store.Shards_disk.file_layout in
        let*! count_result =
          Key_value_store.Read.count_values src_store file_layout slot_id
        in
        match count_result with
        | Error _ ->
            (* Slot file doesn't exist, skip *)
            return_unit
        | Ok 0 ->
            (* No shards for this slot *)
            return_unit
        | Ok _count ->
            (* Copy all shards for this slot *)
            let rec copy_shard shard_index =
              if shard_index >= Constants.number_of_shards then return_unit
              else
                let* () =
                  kvs_copy_value
                    src_store
                    dst_store
                    file_layout
                    slot_id
                    shard_index
                in
                copy_shard (shard_index + 1)
            in
            copy_shard 0)
      (fun () ->
        let open Lwt_syntax in
        let* _ = Key_value_store.Read.close src_store in
        let* _ = Key_value_store.close dst_store in
        return_unit)

  let export ~data_dir:src_data_dir ~config_file ~endpoint ~min_published_level
      ~max_published_level dst_data_dir =
    let open Lwt_result_syntax in
    let* config = Configuration_file.load ~config_file in
    let endpoint = Option.value ~default:config.endpoint endpoint in
    let config = Configuration_file.{config with endpoint} in
    let cctxt = Rpc_context.make config.Configuration_file.endpoint in
    let* header, proto_plugins = L1_helpers.wait_for_block_with_plugin cctxt in
    let*? (module Plugin : Dal_plugin.T), proto_parameters =
      Proto_plugins.get_plugin_and_parameters_for_level
        proto_plugins
        ~level:header.Block_header.shell.level
    in
    let profile_ctxt = Profile_manager.empty in
    (* Initialize crypto as needed by file layouts. *)
    let* cryptobox, _ =
      Node_context.init_cryptobox config proto_parameters profile_ctxt
    in
    (* Set crypto box share size hook. *)
    Value_size_hooks.set_share_size
      (Cryptobox.Internal_for_tests.encoded_share_size cryptobox) ;
    let store_path data_dir =
      (* We only need data_dir field, so we can just use default as base. *)
      Configuration_file.store_path {Configuration_file.default with data_dir}
    in
    let src_root_dir = store_path src_data_dir in
    let dst_root_dir = store_path dst_data_dir in
    let*! dst_exists = Lwt_unix.file_exists dst_root_dir in
    let* () =
      if dst_exists then
        failwith
          "Destination directory %s already exists. Please remove it or choose \
           a different destination."
          dst_root_dir
      else return_unit
    in
    let* chain_id =
      read_from_store ~root_dir:src_root_dir (module Store.Chain_id)
    in
    let* first_seen_level =
      read_from_store ~root_dir:src_root_dir (module Store.First_seen_level)
    in
    let* last_processed_level =
      read_from_store ~root_dir:src_root_dir (module Store.Last_processed_level)
    in
    (* min_published_level: max of first_seen_level and requested min *)
    let min_published_level =
      match min_published_level with
      | None -> first_seen_level
      | Some requested -> max first_seen_level requested
    in
    (* The DAL node stops validating shards published at a level older than
       last_processed_level - (slack + attestation_lag), which means that data
       before this point won't be updated by the DAL node. We cap the
       max_published_level to this value to avoid exporting stale data. *)
    let max_published_level =
      let latest_frozen_level =
        Int32.(
          sub
            last_processed_level
            (of_int
               (Constants.validation_slack + Constants.attestation_lag + 1)))
      in
      match max_published_level with
      | None -> latest_frozen_level
      | Some requested -> min latest_frozen_level requested
    in
    (* Export slots *)
    let* () =
      let src_slot_dir = src_root_dir // Store.Stores_dirs.slot in
      let dst_slot_dir = dst_root_dir // Store.Stores_dirs.slot in
      export_slots
        ~cryptobox
        ~src:src_slot_dir
        ~dst:dst_slot_dir
        ~min_published_level
        ~max_published_level
    in
    (* Export shards *)
    let* () =
      let src_shard_dir = src_root_dir // Store.Stores_dirs.shard in
      let dst_shard_dir = dst_root_dir // Store.Stores_dirs.shard in
      export_shards
        ~src:src_shard_dir
        ~dst:dst_shard_dir
        ~min_published_level
        ~max_published_level
    in
    (* Export skip_list *)
    let* () =
      let dst_skip_list_dir =
        dst_root_dir // Store.Stores_dirs.skip_list_cells
      in
      let src_skip_list_dir =
        src_root_dir // Store.Stores_dirs.skip_list_cells
      in
      export_skip_list
        ~src:src_skip_list_dir
        ~dst:dst_skip_list_dir
        ~min_published_level
        ~max_published_level
    in
    let* chain_id_store = Store.Chain_id.init ~root_dir:dst_root_dir in
    let* () = Store.Chain_id.save chain_id_store chain_id in
    let* first_seen_store =
      Store.First_seen_level.init ~root_dir:dst_root_dir
    in
    let* () =
      Store.First_seen_level.save first_seen_store min_published_level
    in
    let* last_processed_store =
      Store.Last_processed_level.init ~root_dir:dst_root_dir
    in
    let* () =
      Store.Last_processed_level.save last_processed_store max_published_level
    in
    return_unit
end

let export = Export.export
