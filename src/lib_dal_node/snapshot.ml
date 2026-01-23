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
let iterate_slots ~min_published_level ~max_published_level ~slots f =
  iterate_levels ~min_published_level ~max_published_level @@ fun level ->
  List.iter_es
    (fun slot_index -> f Types.Slot_id.{slot_level = level; slot_index})
    slots

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

module Merge = struct
  (** Export a filtered subset of the skip_list SQLite database by iterating
      through levels in the range [min_published_level, max_published_level]
      and copying the data using Dal_store_sqlite3 functions. *)
  let merge_skip_list ~src ~dst ~min_published_level ~max_published_level ~slots
      =
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
    let module SlotsSet = Set.Make (Int) in
    let slots = SlotsSet.of_list slots in
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
          List.filter_map_es
            (fun (_cell, hash, slot_index) ->
              if SlotsSet.mem slot_index slots then
                let slot_id = Types.Slot_id.{slot_level = level; slot_index} in
                let* result =
                  Dal_store_sqlite3.Skip_list_cells.find_by_slot_id_opt
                    src_db
                    slot_id
                in
                match result with
                | Some (cell, attestation_lag) ->
                    return_some (hash, cell, slot_index, attestation_lag)
                | None ->
                    failwith
                      "Cell for slot_id (%ld, %d) not found."
                      level
                      slot_index
              else return_none)
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
  let merge_slots ~cryptobox ~src ~dst ~min_published_level ~max_published_level
      ~slots =
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
        iterate_slots ~min_published_level ~max_published_level ~slots
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
  let merge_shards ~src ~dst ~min_published_level ~max_published_level ~slots
      ~number_of_shards =
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
        iterate_slots ~min_published_level ~max_published_level ~slots
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
              if shard_index >= number_of_shards then return_unit
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

  let merge ~frozen_only ~src_root_dir ~config_file ~endpoint
      ~min_published_level ~max_published_level ~slots ~dst_root_dir =
    let open Lwt_result_syntax in
    let* config =
      Configuration_file.exit_on_configuration_error
        ~emit:Event.emit_configuration_loading_failed
      @@ Configuration_file.load ~config_file ()
    in
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
    let slots =
      Option.value
        ~default:(Stdlib.List.init proto_parameters.number_of_slots Fun.id)
        slots
    in
    (* Set crypto box share size hook. *)
    Value_size_hooks.set_share_size (Cryptobox.encoded_share_size cryptobox) ;
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
        if frozen_only then
          Int32.(
            sub
              last_processed_level
              (of_int
                 (Constants.validation_slack + proto_parameters.attestation_lag
                + 1)))
        else last_processed_level
      in
      match max_published_level with
      | None -> latest_frozen_level
      | Some requested -> min latest_frozen_level requested
    in
    (* Export slots *)
    let* () =
      let src_slot_dir = src_root_dir // Store.Stores_dirs.slot in
      let dst_slot_dir = dst_root_dir // Store.Stores_dirs.slot in
      merge_slots
        ~cryptobox
        ~src:src_slot_dir
        ~dst:dst_slot_dir
        ~min_published_level
        ~max_published_level
        ~slots
    in
    (* Export shards *)
    let number_of_shards =
      proto_parameters.cryptobox_parameters.number_of_shards
    in
    let* () =
      let src_shard_dir = src_root_dir // Store.Stores_dirs.shard in
      let dst_shard_dir = dst_root_dir // Store.Stores_dirs.shard in
      merge_shards
        ~src:src_shard_dir
        ~dst:dst_shard_dir
        ~min_published_level
        ~max_published_level
        ~slots
        ~number_of_shards
    in
    (* Export skip_list *)
    let* () =
      let dst_skip_list_dir =
        dst_root_dir // Store.Stores_dirs.skip_list_cells
      in
      let src_skip_list_dir =
        src_root_dir // Store.Stores_dirs.skip_list_cells
      in
      merge_skip_list
        ~src:src_skip_list_dir
        ~dst:dst_skip_list_dir
        ~min_published_level
        ~max_published_level
        ~slots
    in
    let* chain_id_store = Store.Chain_id.init ~root_dir:dst_root_dir in
    let* () = Store.Chain_id.save chain_id_store chain_id in
    let* first_seen_store =
      Store.First_seen_level.init ~root_dir:dst_root_dir
    in
    let* existing_first_seen = Store.First_seen_level.load first_seen_store in
    let new_first_seen_level =
      match existing_first_seen with
      | None -> min_published_level
      | Some existing -> min existing min_published_level
    in
    let* () =
      Store.First_seen_level.save first_seen_store new_first_seen_level
    in
    let* last_processed_store =
      Store.Last_processed_level.init ~root_dir:dst_root_dir
    in
    let* existing_last_processed =
      Store.Last_processed_level.load last_processed_store
    in
    let new_last_processed_level =
      match existing_last_processed with
      | None -> max_published_level
      | Some existing -> max existing max_published_level
    in
    let* () =
      Store.Last_processed_level.save
        last_processed_store
        new_last_processed_level
    in
    return_unit
end

let export ~data_dir ~config_file ~endpoint ~min_published_level
    ~max_published_level ~slots dst =
  let open Lwt_result_syntax in
  let store_path data_dir =
    Configuration_file.store_path {Configuration_file.default with data_dir}
  in
  let src_root_dir = store_path data_dir in
  let dst_root_dir = store_path dst in
  let*! dst_exists = Lwt_unix.file_exists dst_root_dir in
  let* () =
    if dst_exists then
      failwith
        "Destination directory %s already exists. Please remove it or choose a \
         different destination."
        dst_root_dir
    else return_unit
  in
  Merge.merge
    ~frozen_only:true
    ~src_root_dir
    ~config_file
    ~endpoint
    ~min_published_level
    ~max_published_level
    ~slots
    ~dst_root_dir

let import ?(check = true) ~data_dir:dst ~config_file ~endpoint
    ~min_published_level ~max_published_level ~slots src =
  if check then
    failwith
      "Import with checks is not yet implemented. Use --no-check if you want \
       to bypass imported data validation.\n"
  else
    let store_path data_dir =
      Configuration_file.store_path {Configuration_file.default with data_dir}
    in
    let src_root_dir = store_path src in
    let dst_root_dir = store_path dst in
    Merge.merge
      ~frozen_only:false
      ~src_root_dir
      ~config_file
      ~endpoint
      ~min_published_level
      ~max_published_level
      ~slots
      ~dst_root_dir
