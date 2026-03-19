(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(*
  This module exports/imports DAL node store data in two formats:

  1. Plain data directory (default): copies store files directly using
     the {!Key_value_store.Read} interface.

  2. Compressed tar archive (with [--compress]): exports data as a tar
     archive with individually gzip-compressed entries. Import detects
     the format by file extension ([.tar]).

  Export reads from the source KVS via the {!Key_value_store.Read}
  interface, ensuring that the export leaves source data untouched,
  while still allowing another DAL node to run.
*)

open Filename.Infix
module KVS = Key_value_store

let ensure_tar_extension path =
  let tar_extension = ".tar" in
  if Filename.check_suffix path tar_extension then path
  else path ^ tar_extension

(** Tar entry filename for the skip list binary dump. *)
let skip_list_binary_filename = Store.Stores_dirs.skip_list_cells // "cells.bin"

(** Tar entry names for metadata values. *)
let chain_id_filename = "chain_id"

let first_seen_level_filename = "first_seen_level"

let last_processed_level_filename = "last_processed_level"

let version_filename = "version"

(** Current snapshot format version.

    Bump this constant whenever the tar archive format changes in a
    backward-incompatible way (new mandatory entries, changed encoding,
    removed entries, etc.). The import code rejects archives whose version
    does not match this value. *)
let current_snapshot_version = 1

(** {2 Gzip helpers}

    Per-entry gzip compression/decompression using {!Zlib}. *)

(** Apply a zlib transformation ([compress] or [uncompress]) to [input] bytes,
    using [buf_size] as the initial output buffer capacity. The buffer grows
    dynamically if needed. *)
let zlib_transform ~transform ~buf_size input =
  let buf = Buffer.create buf_size in
  let input_ofs = ref 0 in
  let refill output =
    let available = Bytes.length input - !input_ofs in
    let len = min available (Bytes.length output) in
    Bytes.blit input !input_ofs output 0 len ;
    input_ofs := !input_ofs + len ;
    len
  in
  let flush output len = Buffer.add_subbytes buf output 0 len in
  transform refill flush ;
  Buffer.to_bytes buf

(** Compress [input] bytes using zlib deflate.

    The output buffer is initialized with capacity [size / 2] as a cheap
    initial guess. The exact compression ratio depends on the input, but we do
    not need a precise estimate here because [zlib_transform] uses a growable
    buffer and will expand it automatically if needed. *)
let gzip_compress_bytes ?(level = 6) input =
  zlib_transform
    ~transform:(Zlib.compress ~level ~header:true)
    ~buf_size:(Bytes.length input / 2)
    input

(** {2 Skip list binary format}

    The skip list data is serialised as a flat binary stream to avoid the
    overhead of a full SQLite snapshot (B-tree structure, WAL, indices).
    Records are written in [published_level] order with no file header or
    trailer; the end of the stream is detected by EOF.

    Each record is a 4-byte big-endian length prefix followed by the
    [Data_encoding.Binary] serialisation of the 5-tuple
    [(published_level, slot_index, attestation_lag, hash, cell)].
    Using [Data_encoding] preserves the [Skip_list_hash.t] /
    [Skip_list_cell.t] abstract types end-to-end without unsafe casts. *)

(** Data encoding for one skip list record. *)
let skip_list_record_encoding =
  let open Data_encoding in
  tup5
    int32
    uint16
    int31
    Dal_proto_types.Skip_list_hash.encoding
    Dal_proto_types.Skip_list_cell.encoding

(** Encode and write one skip list record to [fd], prefixed by its 4-byte
    big-endian length. *)
let write_skip_list_record fd record =
  let open Lwt_syntax in
  let encoded =
    Data_encoding.Binary.to_bytes_exn skip_list_record_encoding record
  in
  let len_bytes = Bytes.create 4 in
  Bytes.set_int32_be len_bytes 0 (Int32.of_int (Bytes.length encoded)) ;
  let* () = Lwt_utils_unix.write_bytes fd len_bytes in
  Lwt_utils_unix.write_bytes fd encoded

(** Write [bytes] directly as a tar entry named [filename]. *)
let add_bytes_to_tar tar ~bytes ~filename =
  Octez_tar_helpers.add_raw_and_finalize tar ~filename ~f:(fun fd ->
      Lwt_utils_unix.write_bytes fd bytes)

(** Read a value from a Single_value_store module in readonly mode.
    Fails if the value is not found. *)
let read_from_store (type value) ~root_dir
    (module Store : Single_value_store.S with type value = value) =
  let open Lwt_result_syntax in
  let* store = Store.init_readonly ~root_dir in
  let* value_opt = Store.load store in
  let* () = Store.close store in
  match value_opt with
  | Some value -> return value
  | None -> failwith "Value not found in store at %s" root_dir

(** Save a value using a Single_value_store module in read-write mode. *)
let save_to_store (type value) ~root_dir
    (module Store : Single_value_store.S with type value = value) value =
  let open Lwt_result_syntax in
  let* store = Store.init ~root_dir in
  let* () = Store.save store value in
  Store.close store

(** Save a level metadata value, merging with any existing value using [merge].
    If no existing value is found, [value] is saved as-is. *)
let save_merged_level (type value) ~root_dir
    (module Store : Single_value_store.S with type value = value) ~merge value =
  let open Lwt_result_syntax in
  let* store = Store.init ~root_dir in
  let* existing = Store.load store in
  let effective =
    match existing with Some v -> merge v value | None -> value
  in
  let* () = Store.save store effective in
  Store.close store

(** Number of levels in the inclusive range
    [[min_published_level, max_published_level]]. *)
let nb_levels ~min_published_level ~max_published_level =
  Int32.(to_int (sub max_published_level min_published_level)) + 1

(** Iterate through all levels in the given range, calling [f] for each level. *)
let iterate_levels ?notify ~min_published_level ~max_published_level f =
  let open Lwt_result_syntax in
  let rec iterate_levels level =
    if Compare.Int32.(level > max_published_level) then return_unit
    else
      let* () = f level in
      let*! () = match notify with None -> Lwt.return_unit | Some n -> n () in
      iterate_levels (Int32.succ level)
  in
  iterate_levels min_published_level

(** Format a slot_id as a tar-entry filename component: [<level>_<index>]. *)
let slot_id_to_filename (slot_id : Types.Slot_id.t) =
  Format.asprintf "%ld_%d" slot_id.slot_level slot_id.slot_index

(** Parse a [<level>_<index>] filename into a slot_id. *)
let slot_id_of_filename filename =
  let basename = Filename.basename filename in
  match String.split_on_char '_' basename with
  | level_str :: index_str :: _ -> (
      match (Int32.of_string_opt level_str, int_of_string_opt index_str) with
      | Some level, Some index ->
          Some Types.Slot_id.{slot_level = level; slot_index = index}
      | _ -> None)
  | _ -> None

(** Copy a value from source KVS to destination KVS.
    Returns [Ok ()] if the value was copied or if src value is not found. *)
let kvs_copy_value src_store dst_store file_layout ~slot_id:file ~key
    (level, index, kind) =
  let open Lwt_result_syntax in
  let* exists =
    Key_value_store.Read.value_exists src_store file_layout file key
  in
  if not exists then
    let*! () = Event.emit_cannot_export_snapshot_data ~level ~index ~kind in
    return_unit
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

(** {1 Shared initialization} *)

(** Context shared by export operations. *)
type export_context = {
  min_published_level : int32;
  max_published_level : int32;
  chain_id : Chain_id.t;
  proto_parameters : Types.proto_parameters;
  proto_plugins : Proto_plugins.t;
}

(** Load configuration, resolve endpoint, connect to L1, and retrieve
    current protocol parameters for the head level. *)
let init_rpc_context ~config_file ~endpoint =
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
  let head_level = header.Block_header.shell.level in
  let*? _plugin, proto_parameters =
    Proto_plugins.get_plugin_and_parameters_for_level
      proto_plugins
      ~level:head_level
  in
  return (config, cctxt, header, proto_plugins, proto_parameters)

(** Initialize cryptographic parameters needed for file layouts. *)
let init_cryptoboxes ~cctxt ~header ~config ~first_seen_level proto_plugins =
  let open Lwt_result_syntax in
  let profile_ctxt = Profile_manager.empty in
  let* _proto_cryptoboxes =
    Proto_cryptoboxes.init
      ~cctxt
      ~header
      ~config
      ~first_seen_level
      profile_ctxt
      proto_plugins
  in
  return_unit

(** Verify that two chain ids match. [expected_name] and [found_name]
    describe the origin of each id for the error message.

    Two chain ids must be consistent during snapshot operations:
    - The {b L1 node} chain id (fetched via RPC),
    - The {b source store} chain id (read from the DAL node data directory).

    The L1 node is used as the reference. The source is checked against it in
    {!init_export_context}. The destination store is checked against the source
    in {!Merge.merge}. *)
let check_chain_id ~expected ~found ~expected_name ~found_name =
  let open Lwt_result_syntax in
  if Chain_id.(expected <> found) then
    failwith
      "Chain id mismatch: %s chain id is %a but %s chain id is %a."
      found_name
      Chain_id.pp
      found
      expected_name
      Chain_id.pp
      expected
  else return_unit

(** Shared initialization for export and import operations.
    Loads config, connects to L1, reads source store metadata,
    initializes cryptographic parameters, and computes level bounds. *)
let init_export_context ~frozen_only ~src_root_dir ~config_file ~endpoint
    ~min_published_level ~max_published_level =
  let open Lwt_result_syntax in
  let* config, cctxt, header, proto_plugins, proto_parameters =
    init_rpc_context ~config_file ~endpoint
  in
  let* first_seen_level =
    read_from_store ~root_dir:src_root_dir (module Store.First_seen_level)
  in
  let* () =
    init_cryptoboxes ~cctxt ~header ~config ~first_seen_level proto_plugins
  in
  let* chain_id =
    read_from_store ~root_dir:src_root_dir (module Store.Chain_id)
  in
  let* l1_chain_id = L1_helpers.fetch_l1_chain_id cctxt in
  let* () =
    check_chain_id
      ~expected:l1_chain_id
      ~found:chain_id
      ~expected_name:"L1 node"
      ~found_name:"source store"
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
  return
    {
      min_published_level;
      max_published_level;
      chain_id;
      proto_parameters;
      proto_plugins;
    }

(** Copy skip list cells from a source SQLite database to a destination one,
    filtering by level range and slots. [get_slots level] returns the list
    of slot indices to export for the given [level]. *)
let copy_skip_list_cells ?notify ~src_dir ~dst_dir ~min_published_level
    ~max_published_level ~get_slots () =
  let open Lwt_result_syntax in
  let*! () = Lwt_utils_unix.create_dir dst_dir in
  let* dst_db =
    Dal_store_sqlite3.Skip_list_cells.init
      ~data_dir:dst_dir
      ~perm:Sqlite.Read_write
      ()
  in
  let* src_db =
    Dal_store_sqlite3.Skip_list_cells.init
      ~data_dir:src_dir
      ~perm:Sqlite.(Read_only {pool_size = 1})
      ()
  in
  let module SlotsSet = Set.Make (Int) in
  (* Number of levels to group into a single SQLite transaction.  Batching
     amortises fsync overhead while keeping WAL file growth bounded. *)
  let batch_size = 1000 in
  Lwt.finalize
    (fun () ->
      Dal_store_sqlite3.Skip_list_cells.use dst_db @@ fun conn ->
      let insert_level conn level =
        let open Lwt_result_syntax in
        let*? slots = get_slots level in
        let slots_set = SlotsSet.of_list slots in
        let* cells_with_lag =
          Dal_store_sqlite3.Skip_list_cells.find_by_level
            src_db
            ~published_level:level
        in
        let items_with_lag =
          List.filter_map
            (fun (cell, hash, slot_index, attestation_lag) ->
              if SlotsSet.mem slot_index slots_set then
                Some (hash, cell, slot_index, attestation_lag)
              else None)
            cells_with_lag
        in
        (* Insert each item individually with its correct attested_level.
           Items at the same published_level may have different attestation
           lags so a single attested_level cannot be shared across all items. *)
        List.iter_es
          (fun (hash, cell, slot_index, attestation_lag) ->
            let attested_level = Int32.(add level (of_int attestation_lag)) in
            Dal_store_sqlite3.Skip_list_cells.insert
              ~conn
              dst_db
              ~attested_level
              [(hash, cell, slot_index, attestation_lag)]
              Fun.id)
          items_with_lag
      in
      let rec run_batch start_level =
        if Compare.Int32.(start_level > max_published_level) then return_unit
        else
          let batch_end =
            Int32.(
              min
                max_published_level
                (add start_level (of_int (batch_size - 1))))
          in
          let* () =
            Sqlite.with_transaction conn @@ fun conn ->
            let rec run_level l =
              if Compare.Int32.(l > batch_end) then return_unit
              else
                let* () = insert_level conn l in
                let*! () =
                  match notify with None -> Lwt.return_unit | Some n -> n ()
                in
                run_level (Int32.succ l)
            in
            run_level start_level
          in
          run_batch (Int32.succ batch_end)
      in
      run_batch min_published_level)
    (fun () ->
      let open Lwt_syntax in
      let* _ = Dal_store_sqlite3.Skip_list_cells.close src_db in
      let* _ = Dal_store_sqlite3.Skip_list_cells.close dst_db in
      return_unit)

(** Build a [get_slots] function that resolves slot indices for a given
    level using protocol parameters. When [slots] is [None], all slot
    indices are included. *)
let get_slots_for_level ~proto_plugins ~slots level =
  let open Result_syntax in
  let+ _, proto_parameters =
    Proto_plugins.get_plugin_and_parameters_for_level proto_plugins ~level
  in
  Option.value
    ~default:(Stdlib.List.init proto_parameters.number_of_slots Fun.id)
    slots

(** {1 Plain data directory export/import (default)} *)

module Merge = struct
  (** Export a filtered subset of the skip_list SQLite database by iterating
      through levels in the range [min_published_level, max_published_level]
      and copying the data using Dal_store_sqlite3 functions. *)
  let merge_skip_list ?notify ~src ~dst ~min_published_level
      ~max_published_level ~slots ~proto_plugins () =
    let open Lwt_result_syntax in
    let*! () =
      Event.emit_snapshot_copying ~resource:"skip_list" ~step:"start"
    in
    let get_slots = get_slots_for_level ~proto_plugins ~slots in
    let* () =
      copy_skip_list_cells
        ?notify
        ~src_dir:src
        ~dst_dir:dst
        ~min_published_level
        ~max_published_level
        ~get_slots
        ()
    in
    let*! () =
      Event.emit_snapshot_copying ~resource:"skip_list" ~step:"success"
    in
    return_unit

  (** Export slots for all published slots in the given level range.
      Copies slot files from source to destination directory. *)
  let merge_slots ?notify ~src ~dst ~min_published_level ~max_published_level
      ~slots proto_plugins =
    let open Lwt_result_syntax in
    let*! () = Event.emit_snapshot_copying ~resource:"slots" ~step:"start" in
    let*! () = Lwt_utils_unix.create_dir dst in
    let* src_store =
      Key_value_store.Read.init ~lru_size:Constants.slots_store_lru_size src
    in
    let* dst_store =
      Key_value_store.init
        ~lru_size:Constants.slots_store_lru_size
        ~root_dir:dst
    in
    let copy_slot ~slot_level slot_index =
      let slot_id = Types.Slot_id.{slot_level; slot_index} in
      let* file_layout = Store.Slots.get_file_layout ~slot_id in
      kvs_copy_value
        src_store
        dst_store
        file_layout
        ~slot_id
        (slot_id.slot_level, slot_id.slot_index, "slot ")
        ~key:()
    in
    let close () =
      let open Lwt_result_syntax in
      let* () = Key_value_store.Read.close src_store in
      let* () = Key_value_store.close dst_store in
      return_unit
    in
    let*! res =
      iterate_levels ?notify ~min_published_level ~max_published_level
      @@ fun slot_level ->
      let*? slots = get_slots_for_level ~proto_plugins ~slots slot_level in
      List.iter_es (copy_slot ~slot_level) slots
    in
    let* () = close () in
    let*! () =
      if Result.is_ok res then
        Event.emit_snapshot_copying ~resource:"slots" ~step:"success"
      else Lwt.return_unit
    in
    Lwt.return res

  (** Export shards for all slots in the given level range.
      Copies shard files from source to destination directory. *)
  let merge_shards ?notify ~src ~dst ~min_published_level ~max_published_level
      ~slots ~proto_plugins ~shards_store_lru_size () =
    let open Lwt_result_syntax in
    let*! () = Event.emit_snapshot_copying ~resource:"shards" ~step:"start" in
    let*! () = Lwt_utils_unix.create_dir dst in
    let* src_store =
      Key_value_store.Read.init ~lru_size:shards_store_lru_size src
    in
    let* dst_store =
      Key_value_store.init ~lru_size:shards_store_lru_size ~root_dir:dst
    in
    let copy_shard ~slot_level ~number_of_shards slot_index =
      let slot_id = Types.Slot_id.{slot_level; slot_index} in
      let* file_layout = Store.Shards_disk.get_file_layout ~slot_id in
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
                  ~slot_id
                  ~key:shard_index
                  ( slot_id.slot_level,
                    shard_index,
                    Format.sprintf "shard %d-" slot_id.slot_index )
              in
              copy_shard (shard_index + 1)
          in
          copy_shard 0
    in
    let close () =
      let open Lwt_result_syntax in
      let* () = Key_value_store.Read.close src_store in
      Key_value_store.close dst_store
    in
    let*! res =
      iterate_levels ?notify ~min_published_level ~max_published_level
      @@ fun slot_level ->
      let*? _, proto_parameters =
        Proto_plugins.get_plugin_and_parameters_for_level
          proto_plugins
          ~level:slot_level
      in
      let*? slots = get_slots_for_level ~proto_plugins ~slots slot_level in
      List.iter_es
        (copy_shard
           ~slot_level
           ~number_of_shards:
             proto_parameters.cryptobox_parameters.number_of_shards)
        slots
    in
    let* () = close () in
    let*! () =
      if Result.is_ok res then
        Event.emit_snapshot_copying ~resource:"shards" ~step:"success"
      else Lwt.return_unit
    in
    Lwt.return res

  let merge ~progress_display_mode ~frozen_only ~src_root_dir ~config_file
      ~endpoint ~min_published_level ~max_published_level ~slots ~dst_root_dir
      ~event_path ~event_kind =
    let open Lwt_result_syntax in
    let* {
           min_published_level;
           max_published_level;
           chain_id;
           proto_parameters = head_proto_parameters;
           proto_plugins;
         } =
      init_export_context
        ~frozen_only
        ~src_root_dir
        ~config_file
        ~endpoint
        ~min_published_level
        ~max_published_level
    in
    let*! () =
      Event.emit_snapshot_status
        ~path:event_path
        ~kind:event_kind
        ~status:"start"
        ~min_level:(Some min_published_level)
        ~max_level:(Some max_published_level)
    in
    let total = nb_levels ~min_published_level ~max_published_level in
    (* Verify destination store chain_id matches, if it already exists *)
    let* () =
      let*! dir_exists = Lwt_unix.file_exists dst_root_dir in
      if not dir_exists then return_unit
      else
        let*! result =
          read_from_store ~root_dir:dst_root_dir (module Store.Chain_id)
        in
        match result with
        | Ok dst_chain_id ->
            check_chain_id
              ~expected:chain_id
              ~found:dst_chain_id
              ~expected_name:"source"
              ~found_name:"destination store"
        | _ -> return_unit
    in
    (* Export slots *)
    let* () =
      let src_slot_dir = src_root_dir // Store.Stores_dirs.slot in
      let dst_slot_dir = dst_root_dir // Store.Stores_dirs.slot in
      Animation.display_progress
        ~progress_display_mode
        ~pp_print_step:(fun fmt i ->
          Format.fprintf fmt "Copying slots: %d/%d levels" i total)
        (fun notify ->
          merge_slots
            ~notify
            ~src:src_slot_dir
            ~dst:dst_slot_dir
            ~min_published_level
            ~max_published_level
            ~slots
            proto_plugins)
    in
    (* Export shards *)
    let shards_store_lru_size =
      Constants.shards_store_lru_size
        ~number_of_slots:head_proto_parameters.number_of_slots
    in
    let* () =
      let src_shard_dir = src_root_dir // Store.Stores_dirs.shard in
      let dst_shard_dir = dst_root_dir // Store.Stores_dirs.shard in
      Animation.display_progress
        ~progress_display_mode
        ~pp_print_step:(fun fmt i ->
          Format.fprintf fmt "Copying shards: %d/%d levels" i total)
        (fun notify ->
          merge_shards
            ~notify
            ~src:src_shard_dir
            ~dst:dst_shard_dir
            ~min_published_level
            ~max_published_level
            ~slots
            ~proto_plugins
            ~shards_store_lru_size
            ())
    in
    (* Export skip_list *)
    let* () =
      let dst_skip_list_dir =
        dst_root_dir // Store.Stores_dirs.skip_list_cells
      in
      let src_skip_list_dir =
        src_root_dir // Store.Stores_dirs.skip_list_cells
      in
      Animation.display_progress
        ~progress_display_mode
        ~pp_print_step:(fun fmt i ->
          Format.fprintf fmt "Copying skip list: %d/%d levels" i total)
        (fun notify ->
          merge_skip_list
            ~notify
            ~src:src_skip_list_dir
            ~dst:dst_skip_list_dir
            ~min_published_level
            ~max_published_level
            ~slots
            ~proto_plugins
            ())
    in
    let* () =
      save_to_store ~root_dir:dst_root_dir (module Store.Chain_id) chain_id
    in
    let* () =
      save_merged_level
        ~root_dir:dst_root_dir
        (module Store.First_seen_level)
        ~merge:min
        min_published_level
    in
    let* () =
      save_merged_level
        ~root_dir:dst_root_dir
        (module Store.Last_processed_level)
        ~merge:max
        max_published_level
    in
    let*! () =
      Event.emit_snapshot_status
        ~path:event_path
        ~kind:event_kind
        ~status:"success"
        ~min_level:(Some min_published_level)
        ~max_level:(Some max_published_level)
    in
    return_unit
end

(** {1 Compressed tar archive export} *)

module Export_tar = struct
  (** Export slots: for each slot_id in the range, gzip-compress and store the
      raw KVS file verbatim.  No decode/re-encode cycle needed. *)
  let export_slots ?notify tar ~src_slot_dir ~min_published_level
      ~max_published_level ~slots ~proto_plugins =
    iterate_levels ?notify ~min_published_level ~max_published_level
    @@ fun level ->
    let open Lwt_result_syntax in
    let*? slots = get_slots_for_level ~proto_plugins ~slots level in
    List.iter_es
      (fun slot_index ->
        let slot_id = Types.Slot_id.{slot_level = level; slot_index} in
        let filepath = src_slot_dir // slot_id_to_filename slot_id in
        let*! exists = Lwt_unix.file_exists filepath in
        if not exists then return_unit
        else
          let*! content = Lwt_io.(with_file ~mode:Input filepath read) in
          let compressed =
            gzip_compress_bytes (Bytes.unsafe_of_string content)
          in
          let tar_filename =
            Store.Stores_dirs.slot // slot_id_to_filename slot_id
          in
          let*! () =
            add_bytes_to_tar tar ~bytes:compressed ~filename:tar_filename
          in
          return_unit)
      slots

  (** Compact shard binary format:
        - share_size:       int32 BE  (4 bytes)  — bytes per share
        - number_of_shards: int32 BE  (4 bytes)  — total slots in the KVS file
        - count:            int32 BE  (4 bytes)  — number of occupied slots
        - for each shard:   index (int32 BE, 4 bytes) + raw share bytes
      The share bytes are copied verbatim from the KVS file — no
      [Cryptobox.share_encoding] decode/re-encode cycle.  The format is
      self-describing so the importer can reconstruct the KVS file without
      consulting proto_parameters. *)

  (** Read the compact shard payload for one KVS file.  Only the bitset
      ([number_of_shards] bytes) and the occupied shard data are read;
      the pre-allocated zero-padding is never touched.

      [share_size] and [number_of_shards] are derived from the file size,
      not from proto_parameters, so the format is independent of the
      cryptobox configuration at call time. *)
  let read_compact_shards filepath ~number_of_shards =
    let open Lwt_syntax in
    let* stat = Lwt_unix.stat filepath in
    let file_size = stat.Unix.st_size in
    if file_size <= KVS.file_prefix_bitset_size then Lwt.return Bytes.empty
    else
      let share_size =
        (file_size - KVS.file_prefix_bitset_size) / number_of_shards
      in
      Lwt_io.(
        with_file ~mode:Input filepath (fun ic ->
            (* Read only the first [number_of_shards] bytes of the bitset. *)
            let bitset = Bytes.create number_of_shards in
            let* () = read_into_exactly ic bitset 0 number_of_shards in
            let occupied = ref [] in
            for i = number_of_shards - 1 downto 0 do
              if Bytes.get bitset i = '\001' then occupied := i :: !occupied
            done ;
            let occupied = !occupied in
            let count = List.length occupied in
            if count = 0 then Lwt.return Bytes.empty
            else
              let payload = Bytes.create (12 + (count * (4 + share_size))) in
              Bytes.set_int32_be payload 0 (Int32.of_int share_size) ;
              Bytes.set_int32_be payload 4 (Int32.of_int number_of_shards) ;
              Bytes.set_int32_be payload 8 (Int32.of_int count) ;
              let pos = ref 12 in
              let share_buf = Bytes.create share_size in
              let* () =
                Lwt_list.iter_s
                  (fun i ->
                    let offset =
                      KVS.file_prefix_bitset_size + (i * share_size)
                    in
                    let* () = set_position ic (Int64.of_int offset) in
                    let* () = read_into_exactly ic share_buf 0 share_size in
                    Bytes.set_int32_be payload !pos (Int32.of_int i) ;
                    pos := !pos + 4 ;
                    Bytes.blit share_buf 0 payload !pos share_size ;
                    pos := !pos + share_size ;
                    Lwt.return_unit)
                  occupied
              in
              Lwt.return payload))

  (** Export shards: for each slot_id in the range, extract only the occupied
      shard entries from the KVS file and store a compact self-describing
      gzip-compressed binary in the tar archive.

      KVS shard files are pre-allocated for [number_of_shards] entries (~1.1MB)
      regardless of occupancy.  For a sparse node only a small fraction of
      entries are set; the rest is zero-padding.  Reading the whole file and
      compressing it (even at level 1) scans megabytes of zeros per file.
      Instead we:
        1. Read only the bitset ([number_of_shards] bytes) from the KVS file.
        2. For each occupied index, seek directly to its offset and read
           [share_size] raw bytes — no [Cryptobox.share_encoding] decode cycle.
        3. Gzip-compress the compact payload.
      For a sparse node holding 10 shards per slot (out of 512) this reads
      ~4KB (bitset) + 10 × share_size ≈ 26KB per file instead of ~1.1MB,
      reducing total I/O from ~5.4GB to ~130MB for 4883 files. *)
  let export_shards ?notify tar ~src_shard_dir ~min_published_level
      ~max_published_level ~slots ~proto_plugins =
    iterate_levels ?notify ~min_published_level ~max_published_level
    @@ fun level ->
    let open Lwt_result_syntax in
    let*? slots = get_slots_for_level ~proto_plugins ~slots level in
    let*? _, proto_parameters =
      Proto_plugins.get_plugin_and_parameters_for_level proto_plugins ~level
    in
    let number_of_shards =
      proto_parameters.cryptobox_parameters.number_of_shards
    in
    List.iter_es
      (fun slot_index ->
        let slot_id = Types.Slot_id.{slot_level = level; slot_index} in
        let filepath = src_shard_dir // slot_id_to_filename slot_id in
        let*! exists = Lwt_unix.file_exists filepath in
        if not exists then return_unit
        else
          let*! payload = read_compact_shards filepath ~number_of_shards in
          if Bytes.length payload = 0 then return_unit
          else
            let compressed = gzip_compress_bytes payload in
            let tar_filename =
              Store.Stores_dirs.shard // slot_id_to_filename slot_id
            in
            let*! () =
              add_bytes_to_tar tar ~bytes:compressed ~filename:tar_filename
            in
            return_unit)
      slots

  (** Export skip list: reads from SQLite, serialises each matching row into a
      compact flat binary file, and streams it into the tar archive.  This
      avoids carrying the full SQLite overhead (B-tree structure, WAL,
      indices) in the snapshot. *)
  let export_skip_list ?notify tar ~src_root_dir ~min_published_level
      ~max_published_level ~slots ~proto_plugins =
    let open Lwt_result_syntax in
    let src_skip_list_dir = src_root_dir // Store.Stores_dirs.skip_list_cells in
    let get_slots = get_slots_for_level ~proto_plugins ~slots in
    let module SlotsSet = Set.Make (Int) in
    let* src_db =
      Dal_store_sqlite3.Skip_list_cells.init
        ~data_dir:src_skip_list_dir
        ~perm:Sqlite.(Read_only {pool_size = 1})
        ()
    in
    Lwt.finalize
      (fun () ->
        let* () =
          Octez_tar_helpers.add_raw_and_finalize
            tar
            ~filename:skip_list_binary_filename
            ~f:(fun fd ->
              iterate_levels ?notify ~min_published_level ~max_published_level
              @@ fun level ->
              let*? slots = get_slots level in
              let slots_set = SlotsSet.of_list slots in
              let* cells =
                Dal_store_sqlite3.Skip_list_cells.find_by_level
                  src_db
                  ~published_level:level
              in
              List.iter_es
                (fun (cell, hash, slot_index, attestation_lag) ->
                  if SlotsSet.mem slot_index slots_set then
                    let*! () =
                      write_skip_list_record
                        fd
                        (level, slot_index, attestation_lag, hash, cell)
                    in
                    return_unit
                  else return_unit)
                cells)
        in
        return_unit)
      (fun () ->
        let open Lwt_syntax in
        let* _ = Dal_store_sqlite3.Skip_list_cells.close src_db in
        Lwt.return_unit)

  (** Export a single metadata value as a JSON-encoded entry. *)
  let export_metadata_value tar ~tar_filename encoding value =
    let bytes =
      Data_encoding.Json.construct encoding value
      |> Data_encoding.Json.to_string |> Bytes.of_string
    in
    add_bytes_to_tar tar ~bytes ~filename:tar_filename

  (** Main export function: reads from source store via KVS.Read,
      writes a tar archive. *)
  let run ~progress_display_mode ~src_root_dir ~config_file ~endpoint
      ~min_published_level ~max_published_level ~slots ~dst_tar_file =
    let open Lwt_result_syntax in
    let* {
           min_published_level;
           max_published_level;
           chain_id;
           proto_parameters = _;
           proto_plugins;
         } =
      init_export_context
        ~frozen_only:true
        ~src_root_dir
        ~config_file
        ~endpoint
        ~min_published_level
        ~max_published_level
    in
    let*! () =
      Event.emit_snapshot_status
        ~path:dst_tar_file
        ~kind:"archive"
        ~status:"start"
        ~min_level:(Some min_published_level)
        ~max_level:(Some max_published_level)
    in
    let src_slot_dir = src_root_dir // Store.Stores_dirs.slot in
    let src_shard_dir = src_root_dir // Store.Stores_dirs.shard in
    (* Open tar archive for output *)
    let*! tar = Octez_tar_helpers.open_out ~file:dst_tar_file in
    Lwt.finalize
      (fun () ->
        let total = nb_levels ~min_published_level ~max_published_level in
        (* Export skip_list first so import can validate/apply it upfront. *)
        let*! () =
          Event.emit_snapshot_copying ~resource:"skip_list" ~step:"start"
        in
        let* () =
          Animation.display_progress
            ~progress_display_mode
            ~pp_print_step:(fun fmt i ->
              Format.fprintf fmt "Exporting skip list: %d/%d levels" i total)
            (fun notify ->
              export_skip_list
                ~notify
                tar
                ~src_root_dir
                ~min_published_level
                ~max_published_level
                ~slots
                ~proto_plugins)
        in
        let*! () =
          Event.emit_snapshot_copying ~resource:"skip_list" ~step:"success"
        in
        (* Export slots *)
        let*! () =
          Event.emit_snapshot_copying ~resource:"slots" ~step:"start"
        in
        let* () =
          Animation.display_progress
            ~progress_display_mode
            ~pp_print_step:(fun fmt i ->
              Format.fprintf fmt "Exporting slots: %d/%d levels" i total)
            (fun notify ->
              export_slots
                ~notify
                tar
                ~src_slot_dir
                ~min_published_level
                ~max_published_level
                ~slots
                ~proto_plugins)
        in
        let*! () =
          Event.emit_snapshot_copying ~resource:"slots" ~step:"success"
        in
        (* Export shards *)
        let*! () =
          Event.emit_snapshot_copying ~resource:"shards" ~step:"start"
        in
        let* () =
          Animation.display_progress
            ~progress_display_mode
            ~pp_print_step:(fun fmt i ->
              Format.fprintf fmt "Exporting shards: %d/%d levels" i total)
            (fun notify ->
              export_shards
                ~notify
                tar
                ~src_shard_dir
                ~min_published_level
                ~max_published_level
                ~slots
                ~proto_plugins)
        in
        let*! () =
          Event.emit_snapshot_copying ~resource:"shards" ~step:"success"
        in
        (* Export metadata *)
        let*! () =
          export_metadata_value
            tar
            ~tar_filename:version_filename
            Data_encoding.int31
            current_snapshot_version
        in
        let*! () =
          export_metadata_value
            tar
            ~tar_filename:chain_id_filename
            Chain_id.encoding
            chain_id
        in
        let*! () =
          export_metadata_value
            tar
            ~tar_filename:first_seen_level_filename
            Data_encoding.int32
            min_published_level
        in
        let*! () =
          export_metadata_value
            tar
            ~tar_filename:last_processed_level_filename
            Data_encoding.int32
            max_published_level
        in
        let*! () =
          Event.emit_snapshot_status
            ~path:dst_tar_file
            ~kind:"archive"
            ~status:"success"
            ~min_level:(Some min_published_level)
            ~max_level:(Some max_published_level)
        in
        return_unit)
      (fun () -> Octez_tar_helpers.close_out tar)
end

(** {2 Gzip decompress helpers} *)

(** Decompress gzip-compressed [input] bytes.

    The output buffer is initialized with capacity [size * 2] as a cheap
    initial guess mirroring {!gzip_compress_bytes}. We do not need a precise
    estimate here because [zlib_transform] uses a growable buffer and will
    expand it automatically if needed. *)
let gzip_decompress_bytes input =
  zlib_transform
    ~transform:(Zlib.uncompress ~header:true)
    ~buf_size:(Bytes.length input * 2)
    input

(** Try to read one skip list record.  Returns [None] at EOF; raises
    [End_of_file] if the stream is truncated mid-record. *)
let read_skip_list_record ic =
  let header = Bytes.create 4 in
  let n = input ic header 0 4 in
  if n = 0 then None
  else (
    if n < 4 then really_input ic header n (4 - n) ;
    let len = Int32.to_int (Bytes.get_int32_be header 0) in
    let encoded = Bytes.create len in
    really_input ic encoded 0 len ;
    Some (Data_encoding.Binary.of_bytes_exn skip_list_record_encoding encoded))

(** Load a tar entry, decompress it, and return raw bytes. *)
let load_and_decompress tar file =
  let open Lwt_syntax in
  let* raw = Octez_tar_helpers.load_file tar file in
  Lwt.return (gzip_decompress_bytes (Bytes.of_string raw))

(** Import skip list records from a binary file into the destination SQLite
    store.  Records are filtered by [min_published_level],
    [max_published_level], and [slots].  Inserts are batched in transactions
    of [batch_size] records grouped by [attested_level].  [notify] is called
    after each matching record so that [display_progress ~every:batch_size]
    shows the actual record count. *)
let import_skip_list_binary ?notify ~dst_db ~bin_file ~min_published_level
    ~max_published_level ~slots () =
  let open Lwt_result_syntax in
  let ic = open_in_bin bin_file in
  Lwt.finalize
    (fun () ->
      Dal_store_sqlite3.Skip_list_cells.use dst_db @@ fun conn ->
      let batch_size = 1000 in
      (* Each pending element: (hash, cell, slot_index, attestation_lag)
         grouped under a common attested_level, ready for [insert ~conn]. *)
      let pending_attested = ref Int32.minus_one in
      let pending = ref [] in
      let flush_batch () =
        let records = List.rev !pending in
        pending := [] ;
        match records with
        | [] -> return_unit
        | _ ->
            let attested_level = !pending_attested in
            Sqlite.with_transaction conn @@ fun conn ->
            Dal_store_sqlite3.Skip_list_cells.insert
              ~conn
              dst_db
              ~attested_level
              records
              Fun.id
      in
      let rec loop count =
        match read_skip_list_record ic with
        | None -> flush_batch ()
        | Some (published_level, slot_index, attestation_lag, hash, cell) ->
            let level_ok =
              Compare.Int32.(published_level >= min_published_level)
              && Compare.Int32.(published_level <= max_published_level)
            in
            let slot_ok =
              match slots with
              | None -> true
              | Some s -> List.mem ~equal:Int.equal slot_index s
            in
            if level_ok && slot_ok then (
              let attested_level =
                Int32.(add published_level (of_int attestation_lag))
              in
              (* Flush when the attested_level changes or the batch is full. *)
              let* () =
                if
                  !pending_attested <> attested_level
                  || count mod batch_size = 0
                then (
                  let* () = flush_batch () in
                  pending_attested := attested_level ;
                  return_unit)
                else return_unit
              in
              pending := (hash, cell, slot_index, attestation_lag) :: !pending ;
              (* Call notify once per record so that [display_progress
                 ~every:batch_size] displays the true record count. *)
              let*! () =
                match notify with None -> Lwt.return_unit | Some n -> n ()
              in
              loop (count + 1))
            else loop count
      in
      loop 0)
    (fun () ->
      Stdlib.close_in ic ;
      Lwt.return_unit)

(** {1 Compressed tar archive import} *)

(** Buffer size in bytes for streaming large files (e.g., SQLite databases) to
    tar archives during snapshot import. 64KB provides a good balance between
    memory usage and I/O efficiency. *)
let snapshot_tar_streaming_buffer_size = 64 * 1024

module Import_tar = struct
  (** Check whether a tar entry should be imported given optional
      slot and level filters. Metadata and skip_list entries are always
      imported. Slot and shard entries are filtered by slot index and
      published level. *)
  let should_import_file ~slots ~min_published_level ~max_published_level
      filename =
    let dirname = Filename.dirname filename in
    if dirname = Store.Stores_dirs.slot || dirname = Store.Stores_dirs.shard
    then
      match slot_id_of_filename filename with
      | None -> false
      | Some slot_id ->
          let level_ok =
            (match min_published_level with
            | None -> true
            | Some min_level -> Compare.Int32.(slot_id.slot_level >= min_level))
            &&
            match max_published_level with
            | None -> true
            | Some max_level -> Compare.Int32.(slot_id.slot_level <= max_level)
          in
          let slot_ok =
            match slots with
            | None -> true
            | Some allowed_slots ->
                List.mem ~equal:Int.equal slot_id.slot_index allowed_slots
          in
          level_ok && slot_ok
    else true

  (** Decode a JSON-encoded value from a raw tar entry string. *)
  let decode_json_entry (type a) ~name (encoding : a Data_encoding.t) raw =
    let open Lwt_result_syntax in
    match Data_encoding.Json.from_string raw with
    | Error _ -> failwith "Cannot decode %s from snapshot" name
    | Ok json -> (
        try return (Data_encoding.Json.destruct encoding json)
        with _ -> failwith "Cannot decode %s from snapshot" name)

  (** Pre-scan the tar to find and decode a JSON-encoded metadata entry. *)
  let read_metadata_entry (type a) tar ~filename ~name
      (encoding : a Data_encoding.t) =
    let open Lwt_result_syntax in
    let*! file_opt = Octez_tar_helpers.find_file tar ~filename in
    match file_opt with
    | None -> failwith "Snapshot is missing %s entry" name
    | Some file ->
        let*! raw = Octez_tar_helpers.load_file tar file in
        decode_json_entry ~name encoding raw

  let read_chain_id tar =
    read_metadata_entry
      tar
      ~filename:chain_id_filename
      ~name:"chain_id"
      Chain_id.encoding

  let read_first_seen_level tar =
    read_metadata_entry
      tar
      ~filename:first_seen_level_filename
      ~name:"first_seen_level"
      Data_encoding.int32

  let read_last_processed_level tar =
    read_metadata_entry
      tar
      ~filename:last_processed_level_filename
      ~name:"last_processed_level"
      Data_encoding.int32

  let read_version tar =
    read_metadata_entry
      tar
      ~filename:version_filename
      ~name:"version"
      Data_encoding.int31

  (** Import a metadata entry by decoding it and saving via
      Single_value_store. *)
  let import_metadata_entry (type value) tar file ~root_dir ~name
      (module S : Single_value_store.S with type value = value) encoding =
    let open Lwt_result_syntax in
    let*! raw = Octez_tar_helpers.load_file tar file in
    let* value = decode_json_entry ~name encoding raw in
    save_to_store ~root_dir (module S) value

  let load_int32_metadata tar file ~name =
    let open Lwt_result_syntax in
    let*! raw = Octez_tar_helpers.load_file tar file in
    let* value = decode_json_entry ~name Data_encoding.int32 raw in
    if Compare.Int32.(value >= 0l) then return value
    else failwith "Invalid %s: must be non-negative (got %ld)" name value

  (** Import a slot entry: decompress the raw KVS bytes from the tar entry,
      decode the slot value using [read_values_from_bytes], and write it via
      [KVS.write_value]. *)
  let import_slot_entry tar file ~dst_slot_store ~filename =
    let open Lwt_result_syntax in
    match slot_id_of_filename filename with
    | None -> failwith "Cannot parse slot_id from filename: %s" filename
    | Some slot_id -> (
        let* file_layout = Store.Slots.get_file_layout ~slot_id in
        let*! kvs_bytes = load_and_decompress tar file in
        let result_seq =
          KVS.Read.read_values_from_bytes
            file_layout
            kvs_bytes
            (Seq.return (slot_id, ()))
        in
        let*! slot_data_opt =
          Seq_s.fold_left
            (fun _ (_, (), result) ->
              match result with Ok v -> Some v | Error _ -> None)
            None
            result_seq
        in
        match slot_data_opt with
        | None -> return_unit
        | Some slot_data ->
            KVS.write_value
              ~override:false
              dst_slot_store
              file_layout
              slot_id
              ()
              slot_data)

  (** Import a shard entry: decompress the compact binary, reconstruct the KVS
      file in memory, and write it directly to [dst_shard_dir].  No
      [Cryptobox.share_encoding] decode/re-encode cycle; share bytes are
      placed at the correct offsets and the bitset is set accordingly. *)
  let import_shard_entry tar file ~dst_shard_dir ~filename =
    let open Lwt_result_syntax in
    match slot_id_of_filename filename with
    | None -> failwith "Cannot parse slot_id from filename: %s" filename
    | Some slot_id ->
        let*! compact_bytes = load_and_decompress tar file in
        let len = Bytes.length compact_bytes in
        if len < 12 then return_unit
        else
          let share_size = Int32.to_int (Bytes.get_int32_be compact_bytes 0) in
          let number_of_shards =
            Int32.to_int (Bytes.get_int32_be compact_bytes 4)
          in
          let count = Int32.to_int (Bytes.get_int32_be compact_bytes 8) in
          let file_size =
            KVS.file_prefix_bitset_size + (number_of_shards * share_size)
          in
          let file_buf = Bytes.make file_size '\000' in
          let pos = ref 12 in
          for _ = 0 to count - 1 do
            let idx = Int32.to_int (Bytes.get_int32_be compact_bytes !pos) in
            pos := !pos + 4 ;
            Bytes.set file_buf idx '\001' ;
            Bytes.blit
              compact_bytes
              !pos
              file_buf
              (KVS.file_prefix_bitset_size + (idx * share_size))
              share_size ;
            pos := !pos + share_size
          done ;
          let dst_filepath = dst_shard_dir // slot_id_to_filename slot_id in
          let*! () =
            Lwt_io.(
              with_file ~mode:Output dst_filepath (fun oc ->
                  write oc (Bytes.to_string file_buf)))
          in
          return_unit

  (** Main import function: initializes stores, reads tar entries,
      and writes data using KVS/Store interfaces. *)
  let run ~progress_display_mode ~dst_root_dir ~slots ~min_published_level
      ~max_published_level ~config_file ~endpoint ~tar_file =
    let open Lwt_result_syntax in
    let* config, cctxt, header, proto_plugins, _proto_parameters =
      init_rpc_context ~config_file ~endpoint
    in
    (* Open the tar once. Reading metadata triggers one full header scan and
       caches the file list; the subsequent list_files call reuses that cache
       without rescanning. *)
    let*! tar = Octez_tar_helpers.open_in ~file:tar_file in
    Lwt.finalize
      (fun () ->
        let* snapshot_version = read_version tar in
        let* () =
          if snapshot_version <> current_snapshot_version then
            failwith
              "Snapshot version mismatch: expected %d but got %d. This \
               snapshot was created with an incompatible version of the DAL \
               node."
              current_snapshot_version
              snapshot_version
          else return_unit
        in
        let* snapshot_chain_id = read_chain_id tar in
        let* first_seen_level = read_first_seen_level tar in
        let* last_processed_level = read_last_processed_level tar in
        (* Validate chain_id against L1 *)
        let* l1_chain_id = L1_helpers.fetch_l1_chain_id cctxt in
        let* () =
          check_chain_id
            ~expected:l1_chain_id
            ~found:snapshot_chain_id
            ~expected_name:"L1 node"
            ~found_name:"snapshot"
        in
        let* () =
          init_cryptoboxes
            ~cctxt
            ~header
            ~config
            ~first_seen_level
            proto_plugins
        in
        let dst_slot_dir = dst_root_dir // Store.Stores_dirs.slot in
        let dst_shard_dir = dst_root_dir // Store.Stores_dirs.shard in
        let* dst_slot_store =
          KVS.init
            ~lru_size:Constants.slots_store_lru_size
            ~root_dir:dst_slot_dir
        in
        let*! () = Lwt_utils_unix.create_dir dst_shard_dir in
        Lwt.finalize
          (fun () ->
            let min_skip_list_level =
              match min_published_level with
              | Some l -> max first_seen_level l
              | None -> first_seen_level
            in
            let max_skip_list_level =
              match max_published_level with
              | Some l -> min last_processed_level l
              | None -> last_processed_level
            in
            let*! () =
              Event.emit_snapshot_status
                ~path:tar_file
                ~kind:"archive"
                ~status:"start"
                ~min_level:(Some min_skip_list_level)
                ~max_level:(Some max_skip_list_level)
            in
            (* Import skip_list first, before all other entries.  Stream the
               binary dump from the tar entry, then stream-insert rows into
               the destination SQLite store. *)
            let*! () =
              Event.emit_snapshot_copying ~resource:"skip_list" ~step:"start"
            in
            let* () =
              Animation.display_progress
                ~every:1000
                ~progress_display_mode
                ~pp_print_step:(fun fmt i ->
                  Format.fprintf fmt "Importing skip list: %d records" i)
                (fun notify ->
                  let*! skip_list_file_opt =
                    Octez_tar_helpers.find_file
                      tar
                      ~filename:skip_list_binary_filename
                  in
                  match skip_list_file_opt with
                  | None ->
                      failwith
                        "Snapshot is missing skip list entry: %s"
                        skip_list_binary_filename
                  | Some file ->
                      let tmp_dir =
                        Filename.temp_dir "dal_import_skip_list" ""
                      in
                      Lwt.finalize
                        (fun () ->
                          let bin_path = tmp_dir // "cells.bin" in
                          let*! () =
                            Octez_tar_helpers.copy_to_file
                              tar
                              file
                              ~dst:bin_path
                              ~buffer_size:snapshot_tar_streaming_buffer_size
                          in
                          let dst_dir =
                            dst_root_dir // Store.Stores_dirs.skip_list_cells
                          in
                          let*! () = Lwt_utils_unix.create_dir dst_dir in
                          let* dst_db =
                            Dal_store_sqlite3.Skip_list_cells.init
                              ~data_dir:dst_dir
                              ~perm:Sqlite.Read_write
                              ()
                          in
                          Lwt.finalize
                            (fun () ->
                              import_skip_list_binary
                                ~notify
                                ~dst_db
                                ~bin_file:bin_path
                                ~min_published_level:min_skip_list_level
                                ~max_published_level:max_skip_list_level
                                ~slots
                                ())
                            (fun () ->
                              let open Lwt_syntax in
                              let* _ =
                                Dal_store_sqlite3.Skip_list_cells.close dst_db
                              in
                              Lwt.return_unit))
                        (fun () ->
                          let open Lwt_syntax in
                          let* exists = Lwt_unix.file_exists tmp_dir in
                          if exists then Lwt_utils_unix.remove_dir tmp_dir
                          else return_unit))
            in
            let*! () =
              Event.emit_snapshot_copying ~resource:"skip_list" ~step:"success"
            in
            let*! files = Octez_tar_helpers.list_files tar in
            let total = List.length files in
            let*! () =
              Event.emit_snapshot_copying
                ~resource:"shard_and_slot_entries"
                ~step:"start"
            in
            let* () =
              Animation.display_progress
                ~progress_display_mode
                ~pp_print_step:(fun fmt i ->
                  Format.fprintf
                    fmt
                    "Importing shard and slot entries: %d/%d"
                    i
                    total)
                (fun notify ->
                  List.iter_es
                    (fun file ->
                      let filename = Octez_tar_helpers.get_filename file in
                      let* () =
                        if
                          String.equal filename skip_list_binary_filename
                          || not
                               (should_import_file
                                  ~slots
                                  ~min_published_level
                                  ~max_published_level
                                  filename)
                        then return_unit
                        else
                          let dirname = Filename.dirname filename in
                          if dirname = Store.Stores_dirs.slot then
                            import_slot_entry tar file ~dst_slot_store ~filename
                          else if dirname = Store.Stores_dirs.shard then
                            import_shard_entry tar file ~dst_shard_dir ~filename
                          else if String.equal filename chain_id_filename then
                            import_metadata_entry
                              tar
                              file
                              ~root_dir:dst_root_dir
                              ~name:"chain_id"
                              (module Store.Chain_id)
                              Chain_id.encoding
                          else if
                            String.equal filename first_seen_level_filename
                          then
                            let* tar_value =
                              load_int32_metadata
                                tar
                                file
                                ~name:first_seen_level_filename
                            in
                            let bounded =
                              match min_published_level with
                              | None -> tar_value
                              | Some user_min -> max tar_value user_min
                            in
                            save_merged_level
                              ~root_dir:dst_root_dir
                              (module Store.First_seen_level)
                              ~merge:min
                              bounded
                          else if
                            String.equal filename last_processed_level_filename
                          then
                            let* tar_value =
                              load_int32_metadata
                                tar
                                file
                                ~name:last_processed_level_filename
                            in
                            let bounded =
                              match max_published_level with
                              | None -> tar_value
                              | Some user_max -> min tar_value user_max
                            in
                            save_merged_level
                              ~root_dir:dst_root_dir
                              (module Store.Last_processed_level)
                              ~merge:max
                              bounded
                          else if String.equal filename version_filename then
                            (* Version was already validated before the loop. *)
                            return_unit
                          else
                            failwith
                              "Snapshot contains unknown file: %s"
                              filename
                      in
                      let*! () = notify () in
                      return_unit)
                    files)
            in
            let*! () =
              Event.emit_snapshot_copying
                ~resource:"shard_and_slot_entries"
                ~step:"success"
            in
            let*! () =
              Event.emit_snapshot_status
                ~path:tar_file
                ~kind:"archive"
                ~status:"success"
                ~min_level:(Some min_skip_list_level)
                ~max_level:(Some max_skip_list_level)
            in
            return_unit)
          (fun () ->
            let open Lwt_syntax in
            let* _ = KVS.close dst_slot_store in
            return_unit))
      (fun () -> Octez_tar_helpers.close_in tar)
end

let store_path data_dir =
  Configuration_file.store_path {Configuration_file.default with data_dir}

let init_logging () =
  let log_cfg =
    Tezos_base_unix.Logs_simple_config.create_cfg ~advertise_levels:true ()
  in
  let internal_events =
    Tezos_base_unix.Internal_event_unix.make_with_defaults ~log_cfg ()
  in
  Tezos_base_unix.Internal_event_unix.init ~config:internal_events ()

let export ?(compress = false) ?(progress_display_mode = Animation.Auto)
    ~data_dir ~config_file ~endpoint ~min_published_level ~max_published_level
    ~slots dst =
  let open Lwt_result_syntax in
  let*! () = init_logging () in
  let src_root_dir = store_path data_dir in
  if compress then
    let dst_tar_file = ensure_tar_extension dst in
    let*! dst_exists = Lwt_unix.file_exists dst_tar_file in
    let* () =
      if dst_exists then
        failwith
          "Destination file %s already exists. Please remove it or choose a \
           different destination."
          dst_tar_file
      else return_unit
    in
    let* () =
      Export_tar.run
        ~progress_display_mode
        ~src_root_dir
        ~config_file
        ~endpoint
        ~min_published_level
        ~max_published_level
        ~slots
        ~dst_tar_file
    in
    return_unit
  else
    let dst_root_dir = store_path dst in
    let*! dst_exists = Lwt_unix.file_exists dst_root_dir in
    let* () =
      if dst_exists then
        failwith
          "Destination directory %s already exists. Please remove it or choose \
           a different destination."
          dst_root_dir
      else return_unit
    in
    let* () =
      Merge.merge
        ~progress_display_mode
        ~frozen_only:true
        ~src_root_dir
        ~config_file
        ~endpoint
        ~min_published_level
        ~max_published_level
        ~slots
        ~dst_root_dir
        ~event_path:dst_root_dir
        ~event_kind:"data dir"
    in
    return_unit

let is_tar_file path = Filename.check_suffix path ".tar"

let import ?(check = true) ?(progress_display_mode = Animation.Auto)
    ~data_dir:dst ~config_file ~endpoint ~min_published_level
    ~max_published_level ~slots src =
  let open Lwt_result_syntax in
  let*! () = init_logging () in
  if check then
    failwith
      "Import with checks is not yet implemented. Use --no-check if you want \
       to bypass imported data validation.\n"
  else
    (* Detect tar format: explicit .tar suffix, or <src>.tar file exists
       (export always appends .tar via [ensure_tar_extension]). *)
    let tar_candidate = ensure_tar_extension src in
    let*! is_tar =
      if is_tar_file src then Lwt.return_true
      else Lwt_unix.file_exists tar_candidate
    in
    if is_tar then
      let tar_file = if is_tar_file src then src else tar_candidate in
      let dst_root_dir = store_path dst in
      let* () =
        Import_tar.run
          ~progress_display_mode
          ~dst_root_dir
          ~slots
          ~min_published_level
          ~max_published_level
          ~config_file
          ~endpoint
          ~tar_file
      in
      return_unit
    else
      let src_root_dir = store_path src in
      let dst_root_dir = store_path dst in
      let* () =
        Merge.merge
          ~progress_display_mode
          ~frozen_only:false
          ~src_root_dir
          ~config_file
          ~endpoint
          ~min_published_level
          ~max_published_level
          ~slots
          ~dst_root_dir
          ~event_path:src
          ~event_kind:"data dir"
      in
      return_unit
