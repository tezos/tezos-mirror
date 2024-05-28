(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Snapshots_events
open Store_types

type error +=
  | Incompatible_history_mode of {
      requested : History_mode.t;
      stored : History_mode.t;
    }
  | Invalid_export_block of {
      block : Block_hash.t option;
      reason :
        [ `Pruned
        | `Pruned_pred
        | `Unknown
        | `Unknown_ancestor
        | `Caboose
        | `Genesis
        | `Not_enough_pred ];
    }
  | Invalid_export_path of string
  | Snapshot_file_not_found of string
  | Inconsistent_protocol_hash of {
      expected : Protocol_hash.t;
      got : Protocol_hash.t;
    }
  | Inconsistent_context_hash of {
      expected : Context_hash.t;
      got : Context_hash.t;
    }
  | Inconsistent_context of Context_hash.t
  | Cannot_decode_protocol of Protocol_hash.t
  | Cannot_write_metadata of string
  | Cannot_read of {
      kind :
        [ `Version
        | `Metadata
        | `Block_data
        | `Context
        | `Protocol_table
        | `Protocol
        | `Cemented_cycle ];
      path : string;
    }
  | Inconsistent_floating_store of block_descriptor * block_descriptor
  | Missing_target_block of block_descriptor
  | Cannot_read_floating_store of string
  | Cannot_retrieve_block_interval
  | Invalid_cemented_file of string
  | Missing_cemented_file of string
  | Corrupted_floating_store
  | Invalid_protocol_file of string
  | Target_block_validation_failed of Block_hash.t * string
  | Directory_already_exists of string
  | Empty_floating_store
  | Cannot_remove_tmp_export_directory of string
  | Inconsistent_version_import of {expected : int list; got : int}
  | Inconsistent_chain_import of {
      expected : Distributed_db_version.Name.t;
      got : Distributed_db_version.Name.t;
    }
  | Inconsistent_history_mode_import of {
      requested : History_mode.t;
      stored : History_mode.t;
    }
  | Inconsistent_imported_block of Block_hash.t * Block_hash.t
  | Invalid_chain_store_export of Chain_id.t * string
  | Cannot_export_snapshot_format

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"snapshots.incompatible_export"
    ~title:"Incompatible snapshot export"
    ~description:
      "The requested history mode for the snapshot is not compatible with the \
       given storage."
    ~pp:(fun ppf (requested, stored) ->
      Format.fprintf
        ppf
        "The requested history mode (%a) for the snapshot export is not \
         compatible with the given storage (running with history mode %a)."
        History_mode.pp_short
        requested
        History_mode.pp_short
        stored)
    (obj2
       (req "stored" History_mode.encoding)
       (req "requested" History_mode.encoding))
    (function
      | Incompatible_history_mode {requested; stored} -> Some (requested, stored)
      | _ -> None)
    (fun (requested, stored) -> Incompatible_history_mode {requested; stored}) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.invalid_export_block"
    ~title:"Invalid export block"
    ~description:"Invalid block provided for snapshot export."
    ~pp:(fun ppf (hash, reason) ->
      Format.fprintf
        ppf
        "The selected block %a is invalid: %s."
        (Format.pp_print_option
           ~none:(fun fmt () -> Format.fprintf fmt "(n/a)")
           Block_hash.pp)
        hash
        (match reason with
        | `Pruned -> "the block is too old and has been pruned"
        | `Pruned_pred -> "its predecessor has been pruned"
        | `Unknown -> "the block is unknown"
        | `Unknown_ancestor -> "the block's ancestor is unknown"
        | `Genesis -> "the genesis block is not a valid export point"
        | `Caboose -> "the caboose block is not a valid export point"
        | `Not_enough_pred -> "not enough of the block's predecessors are known"))
    (obj2
       (opt "block" Block_hash.encoding)
       (req
          "reason"
          (string_enum
             [
               ("pruned", `Pruned);
               ("pruned_pred", `Pruned_pred);
               ("unknown", `Unknown);
               ("unknown_ancestor", `Unknown_ancestor);
               ("genesis", `Genesis);
               ("caboose", `Genesis);
               ("not_enough_pred", `Not_enough_pred);
             ])))
    (function
      | Invalid_export_block {block; reason} -> Some (block, reason) | _ -> None)
    (fun (block, reason) -> Invalid_export_block {block; reason}) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.invalid_export_path"
    ~title:"Invalid export path"
    ~description:"Invalid path to export snapshot"
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "Failed to export snapshot: the file or directory %s already exists."
        path)
    (obj1 (req "path" string))
    (function Invalid_export_path path -> Some path | _ -> None)
    (fun path -> Invalid_export_path path) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.snapshot_file_not_found"
    ~title:"Snapshot file not found"
    ~description:"The snapshot file cannot be found."
    ~pp:(fun ppf given_file ->
      Format.fprintf ppf "The snapshot file %s does not exists." given_file)
    (obj1 (req "given_snapshot_file" string))
    (function Snapshot_file_not_found file -> Some file | _ -> None)
    (fun file -> Snapshot_file_not_found file) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.inconsistent_protocol_hash"
    ~title:"Inconsistent protocol hash"
    ~description:"The announced protocol hash doesn't match the computed hash."
    ~pp:(fun ppf (oph, oph') ->
      Format.fprintf
        ppf
        "Inconsistent protocol_hash. Expected: %a, got %a."
        Protocol_hash.pp
        oph
        Protocol_hash.pp
        oph')
    (obj2
       (req "expected" Protocol_hash.encoding)
       (req "got" Protocol_hash.encoding))
    (function
      | Inconsistent_protocol_hash {expected; got} -> Some (expected, got)
      | _ -> None)
    (fun (expected, got) -> Inconsistent_protocol_hash {expected; got}) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.inconsistent_context_hash"
    ~title:"Inconsistent context hash"
    ~description:"The announced context hash doesn't match the computed hash."
    ~pp:(fun ppf (oph, oph') ->
      Format.fprintf
        ppf
        "Inconsistent context_hash. Expected: %a, got %a."
        Context_hash.pp
        oph
        Context_hash.pp
        oph')
    (obj2
       (req "expected" Context_hash.encoding)
       (req "got" Context_hash.encoding))
    (function
      | Inconsistent_context_hash {expected; got} -> Some (expected, got)
      | _ -> None)
    (fun (expected, got) -> Inconsistent_context_hash {expected; got}) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.inconsistent_context"
    ~title:"Inconsistent context"
    ~description:"Inconsistent context after restore."
    ~pp:(fun ppf h ->
      Format.fprintf
        ppf
        "Failed to checkout context %a after restoring it."
        Context_hash.pp
        h)
    (obj1 (req "context_hash" Context_hash.encoding))
    (function Inconsistent_context h -> Some h | _ -> None)
    (fun h -> Inconsistent_context h) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.cannot_decode_protocol"
    ~title:"Protocol import cannot decode"
    ~description:"Failed to decode file when importing protocol"
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Cannot decode the protocol in file: %a"
        Protocol_hash.pp
        hash)
    (obj1 (req "filename" Protocol_hash.encoding))
    (function Cannot_decode_protocol hash -> Some hash | _ -> None)
    (fun hash -> Cannot_decode_protocol hash) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.cannot_write_metadata"
    ~title:"Cannot write metadata"
    ~description:"Cannot write metadata while exporting snapshot."
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Cannot write metadata while exporting snapshot: %s."
        msg)
    (obj1 (req "msg" string))
    (function Cannot_write_metadata msg -> Some msg | _ -> None)
    (fun msg -> Cannot_write_metadata msg) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.cannot_read"
    ~title:"Cannot read"
    ~description:"Cannot read some snapshot data."
    ~pp:(fun ppf (kind, path) ->
      let kind =
        match kind with
        | `Version -> "version"
        | `Metadata -> "metadata"
        | `Block_data -> "block data"
        | `Context -> "context"
        | `Protocol_table -> "protocol table"
        | `Protocol -> "protocol"
        | `Cemented_cycle -> "cemented cycle"
      in
      Format.fprintf ppf "Cannot read snapshot's %s from %s." kind path)
    (obj2
       (req
          "kind"
          (string_enum
             [
               ("version", `Version);
               ("metadata", `Metadata);
               ("block_data", `Block_data);
               ("context", `Context);
               ("protocol_table", `Protocol_table);
               ("protocol", `Protocol);
               ("cemented_cycle", `Cemented_cycle);
             ]))
       (req "path" string))
    (function Cannot_read {kind; path} -> Some (kind, path) | _ -> None)
    (fun (kind, path) -> Cannot_read {kind; path}) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.inconsistent_floating_store"
    ~title:"Inconsistent floating store"
    ~description:"The floating block store is inconsistent."
    ~pp:(fun ppf (target_blk, first_blk) ->
      Format.fprintf
        ppf
        "Failed to export floating store, the first block %a is above the \
         target block %a (broken invariant)."
        pp_block_descriptor
        first_blk
        pp_block_descriptor
        target_blk)
    (obj2
       (req "target" block_descriptor_encoding)
       (req "first" block_descriptor_encoding))
    (function
      | Inconsistent_floating_store (target, first) -> Some (target, first)
      | _ -> None)
    (fun (target, first) -> Inconsistent_floating_store (target, first)) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.missing_target_block"
    ~title:"Missing target block in floating stores"
    ~description:"Floating stores does not contain the target block."
    ~pp:(fun ppf target_blk ->
      Format.fprintf
        ppf
        "Failed to export floating blocks as the target block %a cannot be \
         found."
        pp_block_descriptor
        target_blk)
    (obj1 (req "target" block_descriptor_encoding))
    (function Missing_target_block descr -> Some descr | _ -> None)
    (fun descr -> Missing_target_block descr) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.cannot_read_floating_stores"
    ~title:"Cannot read floating stores"
    ~description:"Unable to read floating stores."
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "Cannot read the floating blocks stores: %s" msg)
    (obj1 (req "msg" string))
    (function Cannot_read_floating_store msg -> Some msg | _ -> None)
    (fun msg -> Cannot_read_floating_store msg) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.cannot_retrieve_block_interval"
    ~title:"Cannot retrieve block interval"
    ~description:"Cannot retrieve block interval from store"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Cannot retrieve block interval: failed to retrieve blocks.")
    unit
    (function Cannot_retrieve_block_interval -> Some () | _ -> None)
    (fun () -> Cannot_retrieve_block_interval) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.invalid_cemented_file"
    ~title:"Invalid cemented file"
    ~description:
      "Encountered an invalid cemented file while restoring the cemented store"
    ~pp:(fun ppf file ->
      Format.fprintf
        ppf
        "Failed to restore cemented blocks. Encountered an invalid file '%s'."
        file)
    (obj1 (req "file" string))
    (function Invalid_cemented_file s -> Some s | _ -> None)
    (fun s -> Invalid_cemented_file s) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.missing_cemented_file"
    ~title:"Missing cemented file"
    ~description:"Cannot find cemented file while restoring cemented store"
    ~pp:(fun ppf file ->
      Format.fprintf
        ppf
        "Failed to restore cemented blocks. The cycle '%s' is missing."
        file)
    (obj1 (req "cycle" string))
    (function Missing_cemented_file s -> Some s | _ -> None)
    (fun s -> Missing_cemented_file s) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.corrupted_floating_store"
    ~title:"Corrupted floating store"
    ~description:"Failed to read floating store"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to restore floating blocks. The floating store is corrupted.")
    unit
    (function Corrupted_floating_store -> Some () | _ -> None)
    (fun () -> Corrupted_floating_store) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.protocol_import_invalid_file"
    ~title:"Protocol import invalid file"
    ~description:"Failed to import protocol as the filename is invalid"
    ~pp:(fun ppf filename ->
      Format.fprintf
        ppf
        "Failed to import protocol. The protocol file '%s' is invalid"
        filename)
    (obj1 (req "filename" string))
    (function Invalid_protocol_file filename -> Some filename | _ -> None)
    (fun filename -> Invalid_protocol_file filename) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.target_block_validation_failed"
    ~title:"target block validation failed"
    ~description:"Failed to validate the target block."
    ~pp:(fun ppf (h, errs) ->
      Format.fprintf ppf "Failed to validate block %a: %s" Block_hash.pp h errs)
    (obj2 (req "block" Block_hash.encoding) (req "errors" string))
    (function
      | Target_block_validation_failed (h, errs) -> Some (h, errs) | _ -> None)
    (fun (h, errs) -> Target_block_validation_failed (h, errs)) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.directory_already_exists"
    ~title:"Directory already exists"
    ~description:"The given data directory already exists."
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Failed to import snapshot as the given directory %s already exists."
        s)
    (obj1 (req "path" string))
    (function Directory_already_exists s -> Some s | _ -> None)
    (fun s -> Directory_already_exists s) ;
  register_error_kind
    `Permanent
    ~id:"snapshot.empty_floating_store"
    ~title:"Empty floating store"
    ~description:"Floating store is empty."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to export floating blocks: the floating store does not contain \
         any blocks (broken invariant).")
    unit
    (function Empty_floating_store -> Some () | _ -> None)
    (fun () -> Empty_floating_store) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.cannot_remove_tmp_export_directory"
    ~title:"Cannot remove temporary export directory"
    ~description:"Cannot create temporary directory for exporting snapshot."
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Cannot export snapshot: the temporary snapshot directory already \
         exists and cannot be removed. Please remove %s and restart the \
         snapshot export."
        msg)
    (obj1 (req "message" string))
    (function Cannot_remove_tmp_export_directory str -> Some str | _ -> None)
    (fun str -> Cannot_remove_tmp_export_directory str) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.inconsistent_version_import"
    ~title:"Inconsistent version import"
    ~description:"The imported snapshot's version is not supported."
    ~pp:(fun ppf (expected, got) ->
      Format.fprintf
        ppf
        "The version of the snapshot file %d is not compatible with the node. \
         Only the following versions can be imported: %a."
        got
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
            pp_print_int)
        expected)
    (obj2 (req "expected" (list int31)) (req "got" int31))
    (function
      | Inconsistent_version_import {expected; got} -> Some (expected, got)
      | _ -> None)
    (fun (expected, got) -> Inconsistent_version_import {expected; got}) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.inconsistent_chain_import"
    ~title:"Inconsistent chain import"
    ~description:
      "The imported chain is inconsistent with the target data directory."
    ~pp:(fun ppf (expected, got) ->
      Format.fprintf
        ppf
        "The chain name contained in the snapshot file (%a) is not consistent \
         with the network configured in the targeted data directory (%a). \
         Please check your configuration file."
        Distributed_db_version.Name.pp
        expected
        Distributed_db_version.Name.pp
        got)
    (obj2
       (req "expected" Distributed_db_version.Name.encoding)
       (req "got" Distributed_db_version.Name.encoding))
    (function
      | Inconsistent_chain_import {expected; got} -> Some (expected, got)
      | _ -> None)
    (fun (expected, got) -> Inconsistent_chain_import {expected; got}) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.inconsistent_history_mode_import"
    ~title:"Inconsistent history_mode import"
    ~description:
      "The imported history mode is inconsistent with the target data \
       directory."
    ~pp:(fun ppf (requested, stored) ->
      Format.fprintf
        ppf
        "The history mode contained in the snapshot file (%a) is not \
         consistent with the one configured in the targeted data directory \
         (%a). Please check your configuration file."
        History_mode.pp
        requested
        History_mode.pp
        stored)
    (obj2
       (req "requested" History_mode.encoding)
       (req "stored" History_mode.encoding))
    (function
      | Inconsistent_history_mode_import {requested; stored} ->
          Some (requested, stored)
      | _ -> None)
    (fun (requested, stored) ->
      Inconsistent_history_mode_import {requested; stored}) ;
  register_error_kind
    `Permanent
    ~id:"context_dump.inconsistent_imported_block"
    ~title:"Inconsistent imported block"
    ~description:"The imported block is not the expected one."
    ~pp:(fun ppf (got, exp) ->
      Format.fprintf
        ppf
        "The block contained in the file is %a instead of %a."
        Block_hash.pp
        got
        Block_hash.pp
        exp)
    (obj2
       (req "block_hash" Block_hash.encoding)
       (req "block_hash_expected" Block_hash.encoding))
    (function
      | Inconsistent_imported_block (got, exp) -> Some (got, exp) | _ -> None)
    (fun (got, exp) -> Inconsistent_imported_block (got, exp)) ;
  register_error_kind
    `Permanent
    ~id:"Snapshot.invalid_chain_store_export"
    ~title:"Invalid chain store export"
    ~description:"Error while exporting snapshot"
    ~pp:(fun ppf (chain_id, store_dir) ->
      Format.fprintf
        ppf
        "Failed to export snapshot. Cannot find chain %a from store located at \
         directory %s."
        Chain_id.pp_short
        chain_id
        store_dir)
    Data_encoding.(
      obj2 (req "chain_id" Chain_id.encoding) (req "store_dir" string))
    (function
      | Invalid_chain_store_export (chain_id, store_dir) ->
          Some (chain_id, store_dir)
      | _ -> None)
    (fun (chain_id, store_dir) ->
      Invalid_chain_store_export (chain_id, store_dir)) ;
  register_error_kind
    `Permanent
    ~id:"Snapshot.cannot_export_snapshot_format"
    ~title:"Cannot export snapshot format"
    ~description:"Cannot export snapshot format"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Cannot export snapshot with a storage that was created with Octez v13 \
         (or earlier). Please refer to the documentation and consider \
         switching to the default minimal indexing strategy to enable snapshot \
         exports. ")
    unit
    (function Cannot_export_snapshot_format -> Some () | _ -> None)
    (fun () -> Cannot_export_snapshot_format)

(* This module handles snapshot's versioning system. *)
module Version = struct
  type t = int

  let (encoding : t Data_encoding.t) =
    let open Data_encoding in
    obj1 (req "version" int31)

  (* Current version of the snapshots, since 0.0.7.
   * Previous versions are:
   * - 1: snapshot exported with storage 0.0.1 to 0.0.4
   * - 2: snapshot exported with storage 0.0.4 to 0.0.6
   * - 3: snapshot exported with storage 0.0.7
   * - 4: snapshot exported with storage 0.0.8
   * - 5: snapshot exported with new protocol tables and "à la GC"
       context (storage 2.0)
   * - 6: new context representation introduced by irmin 3.7.2
   * - 7: fix tar snapshots corrupted generation
   *)

  (* Used for old snapshot format versions *)
  let legacy_version = 4

  let current_version = 7

  (* List of versions that are supported *)
  let supported_versions =
    [
      (legacy_version, `Legacy_format);
      (5, `Legacy);
      (6, `Legacy);
      (current_version, `Current);
    ]

  let is_supported version =
    match List.assq_opt version supported_versions with
    | Some _ -> true
    | None -> false

  (* Returns true if the given version uses a legacy data format. *)
  let is_legacy_format version =
    let open Lwt_result_syntax in
    match List.assq_opt version supported_versions with
    | None ->
        tzfail
          (Inconsistent_version_import
             {expected = List.map fst supported_versions; got = version})
    | Some `Legacy_format -> return_true
    | Some _ -> return_false
end

(* The [default_index_log_size] defines the default maximal value for
   the log_size, used by the index for storing in memory values, while
   importing a snapshot. The current limit of 30M aims to target
   around 1GB of memory usage. Increasing this variable may slightly
   reduce the import time. *)
let default_index_log_size = 30_000_000

let snapshot_rw_file_perm = 0o644

let snapshot_ro_file_perm = 0o444

let snapshot_dir_perm = 0o755

module Snapshot_metadata = struct
  type metadata = {
    chain_name : Distributed_db_version.Name.t;
    history_mode : History_mode.t;
    block_hash : Block_hash.t;
    level : Int32.t;
    timestamp : Time.Protocol.t;
  }

  let metadata_encoding =
    let open Data_encoding in
    conv
      (fun {chain_name; history_mode; block_hash; level; timestamp} ->
        (chain_name, history_mode, block_hash, level, timestamp))
      (fun (chain_name, history_mode, block_hash, level, timestamp) ->
        {chain_name; history_mode; block_hash; level; timestamp})
      (obj5
         (req "chain_name" Distributed_db_version.Name.encoding)
         (req "mode" History_mode.encoding)
         (req "block_hash" Block_hash.encoding)
         (req "level" int32)
         (req "timestamp" Time.Protocol.encoding))

  type legacy_metadata = {
    chain_name : Distributed_db_version.Name.t;
    history_mode : History_mode.t;
    block_hash : Block_hash.t;
    level : Int32.t;
    timestamp : Time.Protocol.t;
    context_elements : int;
  }

  let legacy_metadata_encoding =
    let open Data_encoding in
    conv
      (fun {
             chain_name;
             history_mode;
             block_hash;
             level;
             timestamp;
             context_elements;
           } ->
        ( chain_name,
          history_mode,
          block_hash,
          level,
          timestamp,
          context_elements ))
      (fun ( chain_name,
             history_mode,
             block_hash,
             level,
             timestamp,
             context_elements ) ->
        {
          chain_name;
          history_mode;
          block_hash;
          level;
          timestamp;
          context_elements;
        })
      (obj6
         (req "chain_name" Distributed_db_version.Name.encoding)
         (req "mode" History_mode.encoding)
         (req "block_hash" Block_hash.encoding)
         (req "level" int32)
         (req "timestamp" Time.Protocol.encoding)
         (req "context_elements" int31))

  type t = Current of metadata | Legacy of legacy_metadata

  let pp ppf metadata =
    let chain_name, block_hash, level, history_mode, timestamp =
      match metadata with
      | Current {chain_name; block_hash; level; history_mode; timestamp} ->
          (chain_name, block_hash, level, history_mode, timestamp)
      | Legacy {chain_name; block_hash; level; history_mode; timestamp; _} ->
          (chain_name, block_hash, level, history_mode, timestamp)
    in
    Format.fprintf
      ppf
      "chain %a, block hash %a at level %ld, timestamp %a in %a"
      Distributed_db_version.Name.pp
      chain_name
      Block_hash.pp
      block_hash
      level
      Time.Protocol.pp_hum
      timestamp
      History_mode.pp_short
      history_mode

  let get_block_hash = function
    | Current {block_hash; _} -> block_hash
    | Legacy {block_hash; _} -> block_hash

  let get_chain_name = function
    | Current {chain_name; _} -> chain_name
    | Legacy {chain_name; _} -> chain_name

  let get_history_mode = function
    | Current {history_mode; _} -> history_mode
    | Legacy {history_mode; _} -> history_mode

  let read_metadata ~metadata_file =
    let open Lwt_result_syntax in
    let read_json json = Data_encoding.Json.destruct metadata_encoding json in
    let* json = Lwt_utils_unix.Json.read_file metadata_file in
    return (read_json json)

  let read_legacy_metadata ~metadata_file =
    let open Lwt_result_syntax in
    let read_json json =
      Data_encoding.Json.destruct legacy_metadata_encoding json
    in
    let* json = Lwt_utils_unix.Json.read_file metadata_file in
    return (read_json json)
end

(* A snapshot header is made of a version and some metadata. The
   encoding of the version aims to be fixed between snapshots
   version. On the contrary, metadata may evolve with snapshot
   versions. *)
module Snapshot_header = struct
  type snapshot_header = Version.t * Snapshot_metadata.metadata

  let snapshot_header_encoding =
    let open Data_encoding in
    obj1
      (req
         "snapshot_header"
         (merge_objs Version.encoding Snapshot_metadata.metadata_encoding))

  type snapshot_header_legacy = Version.t * Snapshot_metadata.legacy_metadata

  let snapshot_header_legacy_encoding =
    let open Data_encoding in
    obj1
      (req
         "snapshot_header"
         (merge_objs
            Version.encoding
            Snapshot_metadata.legacy_metadata_encoding))

  type t = Current of snapshot_header | Legacy of snapshot_header_legacy

  let pp ppf = function
    | Current (version, metadata) ->
        Format.fprintf
          ppf
          "%a (snapshot version %d)"
          Snapshot_metadata.pp
          (Snapshot_metadata.Current metadata)
          version
    | Legacy (version, metadata) ->
        Format.fprintf
          ppf
          "%a (snapshot version %d)"
          Snapshot_metadata.pp
          (Snapshot_metadata.Legacy metadata)
          version

  let to_json = function
    | Current snapshot_header ->
        Data_encoding.Json.construct snapshot_header_encoding snapshot_header
    | Legacy snapshot_header ->
        Data_encoding.Json.construct
          snapshot_header_legacy_encoding
          snapshot_header

  let get_version = function
    | Current (version, _) -> version
    | Legacy (version, _) -> version

  let get_metadata = function
    | Current (_, metadata) -> Snapshot_metadata.Current metadata
    | Legacy (_, metadata) -> Snapshot_metadata.Legacy metadata
end

type snapshot_format = Tar | Raw

let snapshot_format_encoding =
  Data_encoding.string_enum [("Tar", Tar); ("Raw", Raw)]

let pp_snapshot_format ppf = function
  | Tar -> Format.fprintf ppf "tar (single file)"
  | Raw -> Format.fprintf ppf "directory"

(* To speed up the import of the cemented blocks we increase,
   temporarily the index cache size. *)
let cemented_import_log_size = 100_000

type block_data = {
  block_header : Block_header.t;
  operations : Operation.t list list;
  predecessor_header : Block_header.t;
  predecessor_block_metadata_hash : Block_metadata_hash.t option;
  predecessor_ops_metadata_hash : Operation_metadata_list_list_hash.t option;
  resulting_context_hash : Context_hash.t;
}

let block_data_encoding =
  let open Data_encoding in
  conv
    (fun {
           block_header;
           operations;
           predecessor_header;
           predecessor_block_metadata_hash;
           predecessor_ops_metadata_hash;
           resulting_context_hash;
         } ->
      ( operations,
        block_header,
        predecessor_header,
        predecessor_block_metadata_hash,
        predecessor_ops_metadata_hash,
        resulting_context_hash ))
    (fun ( operations,
           block_header,
           predecessor_header,
           predecessor_block_metadata_hash,
           predecessor_ops_metadata_hash,
           resulting_context_hash ) ->
      {
        block_header;
        operations;
        predecessor_header;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
        resulting_context_hash;
      })
    (obj6
       (req "operations" (list (list (dynamic_size Operation.encoding))))
       (req "block_header" (dynamic_size Block_header.encoding))
       (req "predecessor_header" (dynamic_size Block_header.encoding))
       (opt "predecessor_block_metadata_hash" Block_metadata_hash.encoding)
       (opt
          "predecessor_ops_metadata_hash"
          Operation_metadata_list_list_hash.encoding)
       (req " resulting_context_hash" Context_hash.encoding))

type legacy_block_data = {
  block_header : Block_header.t;
  operations : Operation.t list list;
  predecessor_header : Block_header.t;
  predecessor_block_metadata_hash : Block_metadata_hash.t option;
  predecessor_ops_metadata_hash : Operation_metadata_list_list_hash.t option;
}

let legacy_block_data_encoding =
  let open Data_encoding in
  conv
    (fun {
           block_header;
           operations;
           predecessor_header;
           predecessor_block_metadata_hash;
           predecessor_ops_metadata_hash;
         } ->
      ( operations,
        block_header,
        predecessor_header,
        predecessor_block_metadata_hash,
        predecessor_ops_metadata_hash ))
    (fun ( operations,
           block_header,
           predecessor_header,
           predecessor_block_metadata_hash,
           predecessor_ops_metadata_hash ) ->
      {
        block_header;
        operations;
        predecessor_header;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
      })
    (obj5
       (req "operations" (list (list (dynamic_size Operation.encoding))))
       (req "block_header" (dynamic_size Block_header.encoding))
       (req "predecessor_header" (dynamic_size Block_header.encoding))
       (opt "predecessor_block_metadata_hash" Block_metadata_hash.encoding)
       (opt
          "predecessor_ops_metadata_hash"
          Operation_metadata_list_list_hash.encoding))

let default_snapshot_filename (metadata : Snapshot_metadata.t) =
  let chain_name, block_hash, level, history_mode =
    match metadata with
    | Current {chain_name; block_hash; level; history_mode; _} ->
        (chain_name, block_hash, level, history_mode)
    | Legacy {chain_name; block_hash; level; history_mode; _} ->
        (chain_name, block_hash, level, history_mode)
  in
  (* The generated filename follows this pattern:
     <NETWORK>-<BLOCK_HASH>-<BLOCK_LEVEL>.<SNAPSHOT_HISTORY_MODE> *)
  let default_name =
    Format.asprintf
      "%a-%a-%ld.%a"
      Distributed_db_version.Name.pp
      chain_name
      Block_hash.pp
      block_hash
      level
      History_mode.pp_short
      history_mode
  in
  let unique_name name =
    let rec aux i =
      let new_name = Format.sprintf "%s-%d" name i in
      if Sys.file_exists new_name then aux (i + 1) else new_name
    in
    aux 1
  in
  if Sys.file_exists default_name then unique_name default_name
  else default_name

let ensure_valid_tmp_snapshot_path snapshot_tmp_dir =
  let open Lwt_result_syntax in
  let path = Naming.dir_path snapshot_tmp_dir in
  let exists = Sys.file_exists path in
  if exists then
    Lwt.catch
      (fun () ->
        let*! () = Event.(emit cleaning_tmp_export_directory) path in
        let*! () = Lwt_utils_unix.remove_dir path in
        return_unit)
      (function
        | _ ->
            fail_when
              exists
              (Cannot_remove_tmp_export_directory
                 (Naming.dir_path snapshot_tmp_dir)))
  else return_unit

let ensure_valid_export_path =
  let open Lwt_result_syntax in
  function
  | Some path -> fail_when (Sys.file_exists path) (Invalid_export_path path)
  | None -> return_unit

let clean_all paths =
  List.iter_s
    (fun path ->
      Unit.catch_s (fun () ->
          if Sys.is_directory path then Lwt_utils_unix.remove_dir path
          else Lwt_unix.unlink path))
    paths

(* This module allows to create a tar archive by adding files to it,
   one by one. It can be seen as a list of contiguous files (made of a
   header followed by a raw data) closed by a specific end of file
   flag. *)
module Onthefly : sig
  (* The type of a file contained in the tar archive. It is basically
     a header and a raw data. *)
  type file

  (* The type of an output tar archive. *)
  type o

  (* The type of an input tar archive. *)
  type i

  (* output utilities *)

  (* [open_out ~file] opens a tar archive as an output archive located at
     [file]. *)
  val open_out : file:string -> o Lwt.t

  (* [close_out tar] closes an output tar archive. *)
  val close_out : o -> unit Lwt.t

  (* [add_raw_and_finalize tar ~f ~filename] exposes a file
     descriptor of the tar archive through [f] to be able to write
     arbitrary data in the [tar]. When [f] terminates, a valid tar
     header referenced by [filename] is written *)
  val add_raw_and_finalize :
    o -> f:(Lwt_unix.file_descr -> 'a Lwt.t) -> filename:string -> 'a Lwt.t

  (* [add_file_and_finalize tar ~file ~filename] copies the [file], and
     reference it through the given [filename], into a [tar]. It handles all
     specific operations an returns a handler ready to be enriched. *)
  val add_file_and_finalize : o -> file:string -> filename:string -> unit Lwt.t

  (* [add_directory_and_finalize ?archive_prefix tar ~dir_path] copies
     the [dir_path] and all its sub directories or files into a
     [tar]. By default, the tar archive file path are similar to the
     [dir_path]. They can be overridden using the [archive_prefix].
     It handles all specific operations an returns a handler ready to
     be enriched.
     For example,
     if the directory `/path/to/data` contains 2 files `a` and `b`:
     With the default behaviour, the tar will contain two files:
         - `/path/to/data/a`
         - `/path/to/data/b`
     If the archive_prefix is given with value `local_path`, the tar
     archive will contain:
        - `local_path/a`
        - `local_path/b`
  *)
  val add_directory_and_finalize :
    ?archive_prefix:string -> o -> dir_path:string -> unit Lwt.t

  (* input utilities *)

  (* [open_out ~file] opens a tar archive as an input archive located at
     [file]. *)
  val open_in : file:string -> i Lwt.t

  (* [close_in tar] closes an input tar archive. *)
  val close_in : i -> unit Lwt.t

  (* [list_files tar] returns the list of files contained in the
     [tar]. *)
  val list_files : i -> file list Lwt.t

  (* [get_file tar ~filename] returns the first occurrence of the
     file name [filename] from [tar]. *)
  val get_file : i -> filename:string -> file option Lwt.t

  (* [get_filename file] returns the file name of a [file] contained
     in a tar. *)
  val get_filename : file -> string

  (* [get_file_size file] returns the file size of a [file] contained
     in a tar. *)
  val get_file_size : file -> int64

  (* [get_raw_input_fd tar] returns the file descriptor to read
     directly in the tar file. It is no recommended to use it. *)
  val get_raw_input_fd : i -> Lwt_unix.file_descr

  (* [get_raw_file_ofs file] returns the position offset, from the
     beginning of the tar archive, of the given [file]. *)
  val get_raw_file_ofs : file -> int64

  (* [find_file tar ~filename] returns the file corresponding to the
     given [filename] within the given [tar]. *)
  val find_file : i -> filename:string -> file option Lwt.t

  (* [find_files_with_common_path tar ~pattern] returns, from the [tar] all
      the files matching the given [pattern]. *)
  val find_files_with_common_path : i -> pattern:string -> file list Lwt.t

  (* [read_raw tar file] returns a file descriptor on the [tar] file
     which is pointing toward the data of the given [file] *)
  val read_raw : i -> file -> Lwt_unix.file_descr Lwt.t

  (* [load_file tar file] loads the [file] from the [tar] and returns
     it as bytes.
     Warning, this function loads the whole data in
     memory. *)
  val load_file : i -> file -> string Lwt.t

  (* [load_from_filename tar ~filename] loads the file with the name
     [filename] from the given [tar] and returns it as
     bytes.
     Warning, this function loads the whole data in memory *)
  val load_from_filename : i -> filename:string -> string option Lwt.t

  (* [copy_to_file tar file ~dst] copies the [file] from the [tar]
     into new file designated by [dst]. *)
  val copy_to_file : i -> file -> dst:string -> unit Lwt.t
end = struct
  include Tar

  module Reader = struct
    type in_channel = Lwt_unix.file_descr

    type 'a t = 'a Lwt.t

    let really_read fd = Lwt_cstruct.(complete (read fd))

    let skip (ifd : Lwt_unix.file_descr) (n : int) =
      let open Lwt_syntax in
      let buffer_size = 32768 in
      let buffer = Cstruct.create buffer_size in
      let rec loop (n : int) =
        if n <= 0 then Lwt.return_unit
        else
          let amount = min n buffer_size in
          let block = Cstruct.sub buffer 0 amount in
          let* () = really_read ifd block in
          loop (n - amount)
      in
      loop n
  end

  module Writer = struct
    type out_channel = Lwt_unix.file_descr

    type 'a t = 'a Lwt.t

    let really_write fd = Lwt_cstruct.(complete (write fd))
  end

  module HR = struct
    include Tar.HeaderReader (Lwt) (Reader)

    let read ic = read ~level:Posix ic
  end

  module HW = struct
    include Tar.HeaderWriter (Lwt) (Writer)

    let write oc = write ~level:Posix oc
  end

  type file = {header : Tar.Header.t; data_ofs : Int64.t}

  type o = {
    mutable current_pos : Int64.t;
    mutable data_pos : Int64.t;
    fd : Lwt_unix.file_descr;
  }

  let open_out ~file =
    let open Lwt_syntax in
    let* fd =
      Lwt_unix.openfile file Unix.[O_WRONLY; O_CREAT] snapshot_rw_file_perm
    in
    let data_pos = Int64.of_int (Header.length * 3) in
    let* _ = Lwt_unix.LargeFile.lseek fd data_pos SEEK_SET in
    Lwt.return {current_pos = 0L; data_pos; fd}

  (* Writes the double zero blocks to close the archive, as it is
     defined in the RFC.*)
  let close_out t =
    let open Lwt_syntax in
    let* _eof = Lwt_unix.LargeFile.lseek t.fd t.current_pos SEEK_SET in
    let* () = Writer.really_write t.fd Tar.Header.zero_block in
    let* () = Writer.really_write t.fd Tar.Header.zero_block in
    Lwt_unix.close t.fd

  (* Builds a tar header for the given sequence of bytes *)
  let header_of_bytes ~filename ~data_size (file : Lwt_unix.file_descr) :
      Header.t Lwt.t =
    let open Lwt_syntax in
    let* stat = Lwt_unix.LargeFile.fstat file in
    let file_mode = stat.Lwt_unix.LargeFile.st_perm in
    let user_id = stat.Lwt_unix.LargeFile.st_uid in
    let group_id = stat.Lwt_unix.LargeFile.st_gid in
    let mod_time = Int64.of_float stat.Lwt_unix.LargeFile.st_mtime in
    let link_indicator = Tar.Header.Link.Normal in
    let link_name = "" in
    let devmajor = 0 in
    let devminor = 0 in
    (* Enforce the extended header version (Posix aka pax). All tar
       headers are then expected to be of size [Tar.Header.length * 3
       = 512B x 3]. It is only necessary to set a single field to
       trigger this behavior in the [Tar] library. *)
    let extended =
      Some
        {
          Tar.Header.Extended.access_time = None;
          charset = None;
          comment = None;
          group_id = None;
          gname = None;
          header_charset = None;
          link_path = None;
          mod_time = None;
          path = None;
          file_size = Some data_size;
          user_id = None;
          uname = None;
        }
    in
    let header =
      Tar.Header.make
        ~file_mode
        ~user_id
        ~group_id
        ~mod_time
        ~link_indicator
        ~link_name
        ~devmajor
        ~devminor
        filename
        data_size
    in
    let header = {header with extended} in
    Lwt.return header

  (* [finalize tar ~bytes_written ~filename] writes the header
     corresponding to the quantity of data given through
     [bytes_written] in the [tar]. Then, it finalizes the file and returns a new
     handle. The file descriptor of that handle is positioned to allow
     writing data. *)
  let finalize t ~bytes_written ~filename =
    let open Lwt_syntax in
    (* Build the header based of the bytes_written *)
    let* header = header_of_bytes ~filename ~data_size:bytes_written t.fd in
    (* We are building extended headers which are 512B x 3. *)
    let header_length = Int64.of_int (Header.length * 3) in
    (* Compute and right the adequate padding for finalizing a block data *)
    let c = Tar.Header.zero_padding header in
    let zero_padding = Cstruct.to_bytes c in
    let zero_padding_length = Bytes.length zero_padding in
    (* Make sure that the fd position is after the written data *)
    let* _ =
      Lwt_unix.LargeFile.lseek
        t.fd
        (Int64.add t.data_pos bytes_written)
        SEEK_SET
    in
    let* _ = Lwt_unix.write t.fd zero_padding 0 zero_padding_length in
    (* Go back to the header position to write it *)
    let* _ = Lwt_unix.LargeFile.lseek t.fd t.current_pos SEEK_SET in
    let* () = HW.write header t.fd in
    let next_block_start =
      Int64.(
        add
          (add t.current_pos header_length)
          (add bytes_written (of_int zero_padding_length)))
    in
    let next_data_pos = Int64.(add next_block_start header_length) in
    (* Set fd position to be ready for next data write *)
    let* _ = Lwt_unix.LargeFile.lseek t.fd next_data_pos SEEK_SET in
    t.current_pos <- next_block_start ;
    t.data_pos <- next_data_pos ;
    Lwt.return_unit

  let add_raw_and_finalize t ~f ~filename =
    let open Lwt_syntax in
    let* res =
      Lwt.catch
        (fun () -> f t.fd)
        (function
          | exn ->
              (* Rewind file descriptor to the start of the current data
                 slot. Then, the next write will overwrite the corrupted
                 data. *)
              let* _ = Lwt_unix.LargeFile.lseek t.fd t.data_pos SEEK_SET in
              Lwt.fail exn)
    in
    let* eor = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_CUR in
    let bytes_written = Int64.sub eor t.data_pos in
    let* () = finalize t ~bytes_written ~filename in
    Lwt.return res

  let copy_n ifd ofd n =
    let open Lwt_syntax in
    let block_size = 32768 in
    let buffer = Cstruct.create block_size in
    let rec loop remaining =
      if remaining = 0L then Lwt.return_unit
      else
        let this = Int64.(to_int (min (of_int block_size) remaining)) in
        let block = Cstruct.sub buffer 0 this in
        let* () = Reader.really_read ifd block in
        let* () = Writer.really_write ofd block in
        loop Int64.(sub remaining (of_int this))
    in
    loop n

  let add_file_and_finalize tar ~file ~filename =
    let open Lwt_syntax in
    let* fd = Lwt_unix.openfile file [Unix.O_RDONLY] snapshot_ro_file_perm in
    let* stat = Lwt_unix.LargeFile.fstat fd in
    let file_size = stat.st_size in
    let* () = copy_n fd tar.fd file_size in
    let* () = finalize tar ~bytes_written:file_size ~filename in
    let* () = Lwt_unix.close fd in
    Lwt.return_unit

  let rec readdir dir_handler =
    let open Lwt_syntax in
    Option.catch_os
      ~catch_only:(function End_of_file -> true | _ -> false)
      (fun () ->
        let* d = Lwt_unix.readdir dir_handler in
        match d with
        | filename
          when filename = Filename.current_dir_name
               || filename = Filename.parent_dir_name ->
            readdir dir_handler
        | any -> Lwt.return_some any)

  let enumerate path =
    let open Lwt_syntax in
    let rec aux prefix dir_handler acc =
      let* o = readdir dir_handler in
      match o with
      | Some any ->
          let full_path = Filename.concat prefix any in
          if Sys.is_directory full_path then
            let* new_dir_handler = Lwt_unix.opendir full_path in
            let* sub_folder = aux full_path new_dir_handler [] in
            let* () = Lwt_unix.closedir new_dir_handler in
            aux prefix dir_handler (sub_folder @ acc)
          else aux prefix dir_handler (full_path :: acc)
      | None -> Lwt.return acc
    in
    let* dir_handler = Lwt_unix.opendir path in
    let* res = aux path dir_handler [] in
    let* () = Lwt_unix.closedir dir_handler in
    Lwt.return res

  let add_directory_and_finalize ?archive_prefix tar ~dir_path =
    let open Lwt_syntax in
    let dir_prefix = Filename.dirname dir_path in
    let* file_paths = enumerate dir_path in
    let archive_prefix = Option.value archive_prefix ~default:dir_prefix in
    let files =
      let dir_length = String.length dir_prefix in
      List.map
        (fun file_path ->
          let filename =
            String.sub
              file_path
              (dir_length + 1)
              String.(length file_path - dir_length - 1)
          in
          (file_path, filename))
        file_paths
    in
    List.iter_s
      (fun (file, filename) ->
        add_file_and_finalize
          tar
          ~file
          ~filename:Filename.(concat archive_prefix filename))
      files

  type i = {
    mutable current_pos : Int64.t;
    mutable data_pos : Int64.t;
    fd : Lwt_unix.file_descr;
    mutable files : file list option;
  }

  let open_in ~file =
    let open Lwt_syntax in
    let* fd = Lwt_unix.openfile file Unix.[O_RDONLY] snapshot_ro_file_perm in
    (* We need to retrieve the first header's length. [Tar] will shift
       the offset to the data location in the file: we can infer the
       length from it. *)
    let* _header = HR.read fd in
    let* data_pos = Lwt_unix.LargeFile.lseek fd 0L SEEK_CUR in
    let* _ = Lwt_unix.LargeFile.lseek fd 0L SEEK_SET in
    let files = None in
    Lwt.return {current_pos = 0L; data_pos; fd; files}

  let close_in t =
    Lwt.catch
      (fun () -> Lwt_unix.close t.fd)
      (function
        | Unix.(Unix_error (EBADF, _, _)) -> Lwt.return_unit
        | exn -> Lwt.fail exn)

  (*[list_files tar] returns the list of files contained in the
     [tar]. *)
  let list_files t =
    let open Lwt_syntax in
    let* _ = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_SET in
    (* This implementation is way faster than the one implemented in
       Tar_lwt_unix.Archive.list function which reads the whole file
    *)
    let rec loop pos acc =
      let* _ = Lwt_unix.LargeFile.lseek t.fd pos SEEK_SET in
      let* _ = Lwt_unix.lseek t.fd 0 SEEK_CUR in
      let* r = HR.read t.fd in
      match r with
      | Error `Eof -> Lwt.return (List.rev acc)
      | Ok hdr ->
          (* Header length can be 1024 if extended *)
          let* data_pos = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_CUR in
          let header_length = Int64.sub data_pos pos in
          let file_size = hdr.Tar.Header.file_size in
          let padding =
            Int64.of_int (Tar.Header.compute_zero_padding_length hdr)
          in
          let next_header = Int64.(add (add file_size padding) header_length) in
          let* _ = Lwt_unix.LargeFile.lseek t.fd next_header SEEK_SET in
          let h = {header = hdr; data_ofs = data_pos} in
          loop (Int64.add pos next_header) (h :: acc)
    in
    loop 0L []

  let update_files t files = t.files <- Some files

  let may_update_files t files =
    match t.files with Some _ -> () | None -> update_files t files

  let get_files t =
    let open Lwt_syntax in
    match t.files with
    | Some files -> Lwt.return files
    | None ->
        let* files = list_files t in
        update_files t files ;
        Lwt.return files

  let get_file tar ~filename =
    let open Lwt_syntax in
    let* files = get_files tar in
    Lwt.return
      (List.find_opt (fun {header; _} -> header.file_name = filename) files)

  let get_filename {header; _} = header.Tar.Header.file_name

  let get_file_size {header; _} = header.Tar.Header.file_size

  (*[get_raw tar file] loads the [file] from [tar] in memory *)
  let get_raw t {header; data_ofs} =
    let open Lwt_syntax in
    let* _ = Lwt_unix.LargeFile.lseek t.fd data_ofs SEEK_SET in
    let data_size = Int64.to_int header.file_size in
    let buf = Bytes.create data_size in
    let* _ = Lwt_unix.read t.fd buf 0 data_size in
    Lwt.return (Bytes.unsafe_to_string buf)

  let get_raw_input_fd {fd; _} = fd

  let get_raw_file_ofs {data_ofs; _} = data_ofs

  let find_file t ~filename =
    let open Lwt_syntax in
    (* If the files were already listed, there is no need to read the whole tar archive.*)
    match t.files with
    | Some _ -> get_file t ~filename
    | None ->
        let* _ = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_SET in
        let rec loop pos acc =
          let* _ = Lwt_unix.LargeFile.lseek t.fd pos SEEK_SET in
          let* _ = Lwt_unix.lseek t.fd 0 SEEK_CUR in
          let* r = HR.read t.fd in
          match r with
          | Error `Eof ->
              (* If the end of file is reached, all the files were
                 enumerated without finding the expected one. In this case,
                 the files are updated. *)
              may_update_files t acc ;
              Lwt.return_none
          | Ok hdr ->
              (* Header length are 512B x 3 when extended (which is
                 now the default). *)
              let* data_pos = Lwt_unix.LargeFile.lseek t.fd 0L SEEK_CUR in
              if hdr.file_name = filename then
                Lwt.return_some {header = hdr; data_ofs = data_pos}
              else
                let header_length = Int64.(sub data_pos pos) in
                let file_size = hdr.Tar.Header.file_size in
                let padding =
                  Int64.of_int (Tar.Header.compute_zero_padding_length hdr)
                in
                let next_header_pos =
                  Int64.(add pos (add (add file_size padding) header_length))
                in
                let h = {header = hdr; data_ofs = data_pos} in
                loop next_header_pos (h :: acc)
        in
        loop 0L []

  let find_files_with_common_path t ~pattern =
    let open Lwt_syntax in
    let* files = get_files t in
    let pattern = Re.compile (Re.Perl.re pattern) in
    Lwt.return
      (List.filter
         (fun {header; _} -> Re.execp pattern header.Tar.Header.file_name)
         files)

  let read_raw t {data_ofs; _} =
    let open Lwt_syntax in
    let* _ = Lwt_unix.LargeFile.lseek t.fd data_ofs SEEK_SET in
    Lwt.return t.fd

  let load_file t file = get_raw t file

  let load_from_filename t ~filename =
    let open Lwt_syntax in
    let* o = get_file t ~filename in
    match o with
    | Some hd ->
        let* str = get_raw t hd in
        Lwt.return_some str
    | None -> Lwt.return_none

  let copy_to_file tar {header; data_ofs} ~dst =
    let open Lwt_syntax in
    let* _ = Lwt_unix.LargeFile.lseek tar.fd data_ofs SEEK_SET in
    let* fd =
      Lwt_unix.openfile
        dst
        Unix.[O_WRONLY; O_CREAT; O_TRUNC]
        snapshot_rw_file_perm
    in
    Lwt.finalize
      (fun () -> copy_n tar.fd fd header.Tar.Header.file_size)
      (fun () -> Lwt_unix.close fd)
end

module type EXPORTER = sig
  type t

  val init : string option -> t tzresult Lwt.t

  val write_block_data :
    t ->
    predecessor_header:Block_header.t ->
    predecessor_block_metadata_hash:Block_metadata_hash.t option ->
    predecessor_ops_metadata_hash:Operation_metadata_list_list_hash.t option ->
    export_block:Store.Block.t ->
    resulting_context_hash:Context_hash.t ->
    unit Lwt.t

  val export_context :
    t -> Context.index -> Context_hash.t -> unit tzresult Lwt.t

  val copy_cemented_block :
    t -> file:string -> start_level:int32 -> end_level:int32 -> unit Lwt.t

  val create_cemented_block_indexes :
    t ->
    Cemented_block_store.Cemented_block_level_index.t
    * Cemented_block_store.Cemented_block_hash_index.t

  (* Removes potential lockfiles that could be created by the index
     library to prevent writes during an internal index maintenance
     procedure. *)
  val clear_cemented_block_indexes_lockfiles : t -> unit Lwt.t

  val filter_cemented_block_indexes : t -> limit:int32 -> unit

  val write_floating_blocks :
    t -> f:(Lwt_unix.file_descr -> 'a Lwt.t) -> 'a Lwt.t

  val write_protocols_table :
    t -> f:(Lwt_unix.file_descr -> 'a Lwt.t) -> 'a Lwt.t

  val copy_protocol : t -> src:string -> dst_ph:Protocol_hash.t -> unit Lwt.t

  val cleaner : ?to_clean:string list -> t -> unit Lwt.t

  val finalize : t -> Snapshot_metadata.t -> string tzresult Lwt.t
end

module Raw_exporter : EXPORTER = struct
  type t = {
    snapshot_dir : string option;
    snapshot_tmp_dir : [`Snapshot_tmp_dir] Naming.directory;
    snapshot_cemented_dir : [`Cemented_blocks_dir] Naming.directory;
    snapshot_protocol_dir : [`Protocol_dir] Naming.directory;
  }

  let init snapshot_dir =
    let open Lwt_result_syntax in
    (* Creates the requested export folder and its hierarchy *)
    let snapshot_tmp_dir =
      let tmp_dir = Naming.snapshot_dir ?snapshot_path:snapshot_dir () in
      Naming.snapshot_tmp_dir tmp_dir
    in
    let* () = ensure_valid_export_path snapshot_dir in
    let* () = ensure_valid_tmp_snapshot_path snapshot_tmp_dir in
    let*! () =
      Lwt_unix.mkdir (Naming.dir_path snapshot_tmp_dir) snapshot_dir_perm
    in
    let snapshot_cemented_dir = Naming.cemented_blocks_dir snapshot_tmp_dir in
    let*! () =
      Lwt_unix.mkdir (Naming.dir_path snapshot_cemented_dir) snapshot_dir_perm
    in
    let snapshot_protocol_dir = Naming.protocol_store_dir snapshot_tmp_dir in
    let*! () =
      Lwt_unix.mkdir (Naming.dir_path snapshot_protocol_dir) snapshot_dir_perm
    in
    let version_file =
      Naming.snapshot_version_file snapshot_tmp_dir |> Naming.file_path
    in
    let version_json =
      Data_encoding.Json.construct Version.encoding Version.current_version
    in
    let* () = Lwt_utils_unix.Json.write_file version_file version_json in
    return
      {
        snapshot_dir;
        snapshot_tmp_dir;
        snapshot_cemented_dir;
        snapshot_protocol_dir;
      }

  let write_block_data t ~predecessor_header ~predecessor_block_metadata_hash
      ~predecessor_ops_metadata_hash ~export_block ~resulting_context_hash =
    let open Lwt_syntax in
    let block_data =
      {
        block_header = Store.Block.header export_block;
        operations = Store.Block.operations export_block;
        predecessor_header;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
        resulting_context_hash;
      }
    in
    let bytes =
      Data_encoding.Binary.to_bytes_exn block_data_encoding block_data
    in
    let file =
      Naming.(snapshot_block_data_file t.snapshot_tmp_dir |> file_path)
    in
    let* fd =
      Lwt_unix.openfile
        file
        Unix.[O_CREAT; O_TRUNC; O_WRONLY]
        snapshot_rw_file_perm
    in
    Lwt.finalize
      (fun () -> Lwt_utils_unix.write_bytes fd bytes)
      (fun () -> Lwt_unix.close fd)

  let export_context t context_index context_hash =
    let open Lwt_result_syntax in
    let tmp_context_path =
      Naming.(snapshot_context_file t.snapshot_tmp_dir |> file_path)
    in
    let*! () =
      Context.export_snapshot context_index context_hash ~path:tmp_context_path
    in
    return_unit

  let copy_cemented_block t ~file ~start_level ~end_level =
    let filename =
      Naming.(
        cemented_blocks_file t.snapshot_cemented_dir ~start_level ~end_level
        |> file_path)
    in
    Lwt_utils_unix.copy_file ~src:file ~dst:filename

  let create_cemented_block_indexes t =
    let open Cemented_block_store in
    let fresh_level_index =
      Cemented_block_level_index.v
        ~fresh:true
        ~readonly:false
        ~log_size:cemented_import_log_size
        Naming.(
          cemented_blocks_level_index_dir t.snapshot_cemented_dir |> dir_path)
    in
    let fresh_hash_index =
      Cemented_block_hash_index.v
        ~fresh:true
        ~readonly:false
        ~log_size:cemented_import_log_size
        Naming.(
          cemented_blocks_hash_index_dir t.snapshot_cemented_dir |> dir_path)
    in
    (fresh_level_index, fresh_hash_index)

  let clear_cemented_block_indexes_lockfiles t =
    let open Lwt_syntax in
    let* () =
      Lwt.catch
        (fun () ->
          Lwt_unix.unlink
            Naming.(
              file_path
                (cemented_blocks_hash_lock_file
                   (cemented_blocks_hash_index_dir
                      (cemented_blocks_dir t.snapshot_tmp_dir)))))
        (function
          | Unix.Unix_error (ENOENT, _, _) -> Lwt.return_unit
          | exn -> Lwt.reraise exn)
    in
    Lwt.catch
      (fun () ->
        Lwt_unix.unlink
          Naming.(
            file_path
              (cemented_blocks_level_lock_file
                 (cemented_blocks_level_index_dir
                    (cemented_blocks_dir t.snapshot_tmp_dir)))))
      (function
        | Unix.Unix_error (ENOENT, _, _) -> Lwt.return_unit
        | exn -> Lwt.reraise exn)

  let filter_cemented_block_indexes t ~limit =
    let open Cemented_block_store in
    let fresh_level_index =
      Cemented_block_level_index.v
        ~fresh:false
        ~readonly:false
        ~log_size:10_000
        Naming.(
          cemented_blocks_level_index_dir t.snapshot_cemented_dir |> dir_path)
    in
    let fresh_hash_index =
      Cemented_block_hash_index.v
        ~fresh:false
        ~readonly:false
        ~log_size:10_000
        Naming.(
          cemented_blocks_hash_index_dir t.snapshot_cemented_dir |> dir_path)
    in
    Cemented_block_level_index.filter fresh_level_index (fun (_, level) ->
        level <= limit) ;
    Cemented_block_hash_index.filter fresh_hash_index (fun (level, _) ->
        level <= limit) ;
    Cemented_block_level_index.close fresh_level_index ;
    Cemented_block_hash_index.close fresh_hash_index

  let write_floating_blocks t ~f =
    let open Lwt_syntax in
    let floating_file =
      Naming.(snapshot_floating_blocks_file t.snapshot_tmp_dir |> file_path)
    in
    let* fd =
      Lwt_unix.openfile
        floating_file
        Unix.[O_CREAT; O_TRUNC; O_WRONLY]
        snapshot_rw_file_perm
    in
    Lwt.finalize (fun () -> f fd) (fun () -> Lwt_unix.close fd)

  let write_protocols_table t ~f =
    let open Lwt_syntax in
    let* fd =
      Lwt_unix.openfile
        Naming.(
          snapshot_protocol_levels_file t.snapshot_tmp_dir |> encoded_file_path)
        Unix.[O_CREAT; O_TRUNC; O_WRONLY]
        snapshot_rw_file_perm
    in
    Lwt.finalize (fun () -> f fd) (fun () -> Lwt_unix.close fd)

  let copy_protocol t ~src ~dst_ph =
    let dst =
      Naming.(
        protocol_file (protocol_store_dir t.snapshot_tmp_dir) dst_ph
        |> file_path)
    in
    Lwt_utils_unix.copy_file ~src ~dst

  let write_metadata t (metadata : Snapshot_metadata.t) =
    let metadata_file =
      Naming.(snapshot_metadata_file t.snapshot_tmp_dir |> file_path)
    in
    let metadata_json =
      match metadata with
      | Current metadata ->
          Data_encoding.Json.(
            construct Snapshot_metadata.metadata_encoding metadata)
      | Legacy _ ->
          (* The export of legacy snapshots is not allowed. *)
          assert false
    in
    Lwt_utils_unix.Json.write_file metadata_file metadata_json

  let cleaner ?to_clean t =
    let open Lwt_syntax in
    let* () = Event.(emit cleaning_after_failure ()) in
    let paths =
      match to_clean with
      | Some paths -> paths
      | None -> [Naming.dir_path t.snapshot_tmp_dir]
    in
    clean_all paths

  let finalize t metadata =
    let open Lwt_result_syntax in
    let snapshot_filename =
      match t.snapshot_dir with
      | Some path -> path
      | None -> default_snapshot_filename metadata
    in
    let* () = write_metadata t metadata in
    protect
      ~on_error:(fun errors ->
        let*! () = cleaner ~to_clean:[Naming.dir_path t.snapshot_tmp_dir] t in
        Lwt.return (Error errors))
      (fun () ->
        let*! () =
          Lwt_unix.rename (Naming.dir_path t.snapshot_tmp_dir) snapshot_filename
        in
        return snapshot_filename)
end

module Tar_exporter : EXPORTER = struct
  type t = {
    snapshot_file : string option;
    snapshot_tar : [`Tar_archive] Naming.directory;
    snapshot_tar_file : [`Snapshot_tar_file] Naming.file;
    snapshot_tmp_dir : [`Snapshot_tmp_dir] Naming.directory;
    snapshot_tmp_cemented_dir : [`Cemented_blocks_dir] Naming.directory;
    snapshot_cemented_dir : [`Cemented_blocks_dir] Naming.directory;
    snapshot_protocol_dir : [`Protocol_dir] Naming.directory;
    tar : Onthefly.o;
  }

  let init snapshot_file =
    let open Lwt_result_syntax in
    (* Creates the requested export folder and its hierarchy *)
    let snapshot_tmp_dir =
      let tmp_dir = Naming.snapshot_dir ?snapshot_path:snapshot_file () in
      Naming.snapshot_tmp_dir tmp_dir
    in
    let* () = ensure_valid_export_path snapshot_file in
    let* () = ensure_valid_tmp_snapshot_path snapshot_tmp_dir in
    let*! () =
      Lwt_unix.mkdir (Naming.dir_path snapshot_tmp_dir) snapshot_dir_perm
    in
    let snapshot_tmp_cemented_dir =
      Naming.cemented_blocks_dir snapshot_tmp_dir
    in
    let snapshot_tar = Naming.snapshot_tar_root in
    let snapshot_cemented_dir = Naming.cemented_blocks_dir snapshot_tar in
    let snapshot_protocol_dir = Naming.protocol_store_dir snapshot_tar in
    let snapshot_tar_file = Naming.snapshot_tmp_tar_file snapshot_tmp_dir in
    let*! tar =
      Onthefly.open_out ~file:(snapshot_tar_file |> Naming.file_path)
    in
    let version_file =
      Naming.snapshot_version_file snapshot_tmp_dir |> Naming.file_path
    in
    let version_json =
      Data_encoding.Json.construct Version.encoding Version.current_version
    in
    let* () = Lwt_utils_unix.Json.write_file version_file version_json in
    let*! () =
      Onthefly.add_file_and_finalize
        tar
        ~file:version_file
        ~filename:(Filename.basename version_file)
    in
    return
      {
        snapshot_file;
        snapshot_tar;
        snapshot_tar_file;
        snapshot_tmp_dir;
        snapshot_tmp_cemented_dir;
        snapshot_cemented_dir;
        snapshot_protocol_dir;
        tar;
      }

  let write_block_data t ~predecessor_header ~predecessor_block_metadata_hash
      ~predecessor_ops_metadata_hash ~export_block ~resulting_context_hash =
    let block_data =
      {
        block_header = Store.Block.header export_block;
        operations = Store.Block.operations export_block;
        predecessor_header;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
        resulting_context_hash;
      }
    in
    let bytes =
      Data_encoding.Binary.to_bytes_exn block_data_encoding block_data
    in
    Onthefly.add_raw_and_finalize
      t.tar
      ~f:(fun fd -> Lwt_utils_unix.write_bytes fd bytes)
      ~filename:Naming.(snapshot_block_data_file t.snapshot_tar |> file_path)

  let export_context t context_index context_hash =
    let open Lwt_result_syntax in
    let tmp_context_path =
      Naming.(snapshot_context_file t.snapshot_tmp_dir |> file_path)
    in
    let*! () =
      Context.export_snapshot context_index context_hash ~path:tmp_context_path
    in
    let*! () =
      Onthefly.add_directory_and_finalize
        ~archive_prefix:"" (* /context/ was already added *)
        t.tar
        ~dir_path:tmp_context_path
    in
    let*! () = Lwt_utils_unix.remove_dir tmp_context_path in
    return_unit

  let copy_cemented_block t ~file ~start_level ~end_level =
    let cemented_filename =
      Naming.(
        cemented_blocks_file t.snapshot_cemented_dir ~start_level ~end_level
        |> file_path)
    in
    Onthefly.add_file_and_finalize t.tar ~file ~filename:cemented_filename

  let create_cemented_block_indexes t =
    let open Cemented_block_store in
    let fresh_level_index =
      Cemented_block_level_index.v
        ~fresh:true
        ~readonly:false
        ~log_size:cemented_import_log_size
        Naming.(
          cemented_blocks_level_index_dir t.snapshot_tmp_cemented_dir
          |> dir_path)
    in
    let fresh_hash_index =
      Cemented_block_hash_index.v
        ~fresh:true
        ~readonly:false
        ~log_size:cemented_import_log_size
        Naming.(
          cemented_blocks_hash_index_dir t.snapshot_tmp_cemented_dir |> dir_path)
    in
    (fresh_level_index, fresh_hash_index)

  let clear_cemented_block_indexes_lockfiles t =
    let open Lwt_syntax in
    let* () =
      Lwt.catch
        (fun () ->
          Lwt_unix.unlink
            Naming.(
              file_path
                (cemented_blocks_hash_lock_file
                   (cemented_blocks_hash_index_dir t.snapshot_tmp_cemented_dir))))
        (function
          | Unix.Unix_error (ENOENT, _, _) -> Lwt.return_unit
          | exn -> Lwt.reraise exn)
    in
    let* () =
      Lwt.catch
        (fun () ->
          Lwt_unix.unlink
            Naming.(
              file_path
                (cemented_blocks_level_lock_file
                   (cemented_blocks_level_index_dir t.snapshot_tmp_cemented_dir))))
        (function
          | Unix.Unix_error (ENOENT, _, _) -> Lwt.return_unit
          | exn -> Lwt.reraise exn)
    in
    let* () =
      Onthefly.add_directory_and_finalize
        ~archive_prefix:(Naming.dir_path t.snapshot_cemented_dir)
        t.tar
        ~dir_path:
          Naming.(
            cemented_blocks_hash_index_dir t.snapshot_tmp_cemented_dir
            |> dir_path)
    in
    Onthefly.add_directory_and_finalize
      ~archive_prefix:(Naming.dir_path t.snapshot_cemented_dir)
      t.tar
      ~dir_path:
        Naming.(
          cemented_blocks_level_index_dir t.snapshot_tmp_cemented_dir
          |> dir_path)

  let filter_cemented_block_indexes t ~limit =
    let open Cemented_block_store in
    let fresh_level_index =
      Cemented_block_level_index.v
        ~fresh:false
        ~readonly:false
        ~log_size:10_000
        Naming.(
          cemented_blocks_level_index_dir t.snapshot_tmp_cemented_dir
          |> dir_path)
    in
    let fresh_hash_index =
      Cemented_block_hash_index.v
        ~fresh:false
        ~readonly:false
        ~log_size:10_000
        Naming.(
          cemented_blocks_hash_index_dir t.snapshot_tmp_cemented_dir |> dir_path)
    in
    Cemented_block_level_index.filter fresh_level_index (fun (_, level) ->
        level <= limit) ;
    Cemented_block_hash_index.filter fresh_hash_index (fun (level, _) ->
        level <= limit) ;
    Cemented_block_level_index.close fresh_level_index ;
    Cemented_block_hash_index.close fresh_hash_index

  let write_floating_blocks t ~f =
    Onthefly.add_raw_and_finalize
      t.tar
      ~f
      ~filename:
        Naming.(snapshot_floating_blocks_file t.snapshot_tar |> file_path)

  let write_protocols_table t ~f =
    Onthefly.add_raw_and_finalize
      t.tar
      ~f
      ~filename:
        Naming.(
          snapshot_protocol_levels_file t.snapshot_tar |> encoded_file_path)

  let copy_protocol t ~src ~dst_ph =
    let dst =
      Filename.(
        concat
          (Naming.dir_path t.snapshot_protocol_dir)
          (Protocol_hash.to_b58check dst_ph))
    in
    Onthefly.add_file_and_finalize t.tar ~file:src ~filename:dst

  let write_metadata t metadata =
    let open Lwt_result_syntax in
    let metadata_json =
      match metadata with
      | Snapshot_metadata.Current metadata ->
          Data_encoding.Json.(
            construct Snapshot_metadata.metadata_encoding metadata)
      | Legacy metadata ->
          Data_encoding.Json.(
            construct Snapshot_metadata.legacy_metadata_encoding metadata)
    in
    let metadata_file =
      Naming.snapshot_metadata_file t.snapshot_tmp_dir |> Naming.file_path
    in
    let* () = Lwt_utils_unix.Json.write_file metadata_file metadata_json in
    let*! () =
      Onthefly.add_file_and_finalize
        t.tar
        ~file:metadata_file
        ~filename:(Filename.basename metadata_file)
    in
    return_unit

  let cleaner ?to_clean t =
    let open Lwt_syntax in
    let* () = Event.(emit cleaning_after_failure ()) in
    let paths =
      match to_clean with
      | Some paths -> paths
      | None -> [Naming.dir_path t.snapshot_tmp_dir]
    in
    clean_all paths

  let finalize t metadata =
    let open Lwt_result_syntax in
    let snapshot_filename =
      match t.snapshot_file with
      | Some path -> path
      | None -> default_snapshot_filename metadata
    in
    let* () = write_metadata t metadata in
    let*! () = Onthefly.close_out t.tar in
    protect
      ~on_error:(fun errors ->
        let*! () = cleaner ~to_clean:[Naming.dir_path t.snapshot_tmp_dir] t in
        Lwt.return (Error errors))
      (fun () ->
        let*! () =
          Lwt_unix.rename
            Naming.(snapshot_tmp_tar_file t.snapshot_tmp_dir |> file_path)
            snapshot_filename
        in
        let*! () =
          Lwt_utils_unix.remove_dir (Naming.dir_path t.snapshot_tmp_dir)
        in
        return snapshot_filename)
end

module type Snapshot_exporter = sig
  type t

  val export :
    ?snapshot_path:string ->
    ?rolling:bool ->
    block:Block_services.block ->
    store_dir:string ->
    context_dir:string ->
    chain_name:Distributed_db_version.Name.t ->
    progress_display_mode:Animation.progress_display_mode ->
    Genesis.t ->
    unit tzresult Lwt.t
end

module Make_snapshot_exporter (Exporter : EXPORTER) : Snapshot_exporter = struct
  type t = Exporter.t

  let init = Exporter.init

  let copy_cemented_blocks snapshot_exporter ~should_filter_indexes
      ~progress_display_mode
      (files : Cemented_block_store.cemented_blocks_file list) =
    let open Lwt_result_syntax in
    let open Cemented_block_store in
    let*! () = Event.(emit exporting_cemented_cycles) () in
    let nb_cycles = List.length files in
    (* Rebuild fresh indexes: cannot cp because of concurrent accesses *)
    let fresh_level_index, fresh_hash_index =
      Exporter.create_cemented_block_indexes snapshot_exporter
    in
    let* () =
      protect (fun () ->
          let* () =
            Animation.display_progress
              ~pp_print_step:(fun fmt i ->
                Format.fprintf
                  fmt
                  "Copying cemented blocks and populating indexes: %d/%d cycles"
                  i
                  nb_cycles)
              ~progress_display_mode
              (fun notify ->
                (* Bound the number of copying threads *)
                List.iter_es
                  (fun ({start_level; end_level; file} as cemented_file) ->
                    let* () =
                      Cemented_block_store.iter_cemented_file
                        (fun block ->
                          let hash = Block_repr.hash block in
                          let level = Block_repr.level block in
                          Cemented_block_level_index.replace
                            fresh_level_index
                            hash
                            level ;
                          Cemented_block_hash_index.replace
                            fresh_hash_index
                            level
                            hash ;
                          Lwt.return_unit)
                        cemented_file
                    in
                    let file_path = Naming.file_path file in
                    let*! () =
                      Exporter.copy_cemented_block
                        snapshot_exporter
                        ~file:file_path
                        ~start_level
                        ~end_level
                    in
                    let*! () = notify () in
                    return_unit)
                  files)
          in
          Cemented_block_level_index.close fresh_level_index ;
          Cemented_block_hash_index.close fresh_hash_index ;
          let*! () =
            Exporter.clear_cemented_block_indexes_lockfiles snapshot_exporter
          in
          if should_filter_indexes && files <> [] then
            Exporter.filter_cemented_block_indexes
              snapshot_exporter
              ~limit:
                (List.last_opt files |> WithExceptions.Option.get ~loc:__LOC__)
                  .end_level ;
          return_unit)
    in
    let*! () = Event.(emit cemented_cycles_exported) () in
    return_unit

  let write_floating_block fd (block : Block_repr.t) =
    let bytes = Data_encoding.Binary.to_bytes_exn Block_repr.encoding block in
    Lwt_utils_unix.write_bytes ~pos:0 ~len:(Bytes.length bytes) fd bytes

  let export_floating_blocks ~floating_ro_fd ~floating_rw_fd ~export_block =
    let open Lwt_result_syntax in
    let ((limit_hash, limit_level) as export_block_descr) =
      Store.Block.descriptor export_block
    in
    let stream, bpush = Lwt_stream.create_bounded 1000 in
    (* Retrieve first floating block *)
    let* first_block =
      let*! o = Block_repr_unix.read_next_block floating_ro_fd in
      match o with
      | Some (block, _length) -> return block
      | None -> (
          let*! o = Block_repr_unix.read_next_block floating_rw_fd in
          match o with
          | Some (block, _length) -> return block
          | None ->
              (* No block to read *)
              tzfail Empty_floating_store)
    in
    let first_block_level = Block_repr.level first_block in
    if Compare.Int32.(limit_level < first_block_level) then
      tzfail
        (Inconsistent_floating_store
           (export_block_descr, (Block_repr.hash first_block, first_block_level)))
    else
      let exception Done in
      let f block =
        (* FIXME: we also write potential branches, it will eventually
           be GCed *)
        if Compare.Int32.(Block_repr.level block >= limit_level) then
          if Block_hash.equal limit_hash (Block_repr.hash block) then raise Done
          else return_unit
        else
          let block = (* Prune everything  *) {block with metadata = None} in
          let*! () = bpush#push block in
          return_unit
      in
      let reading_thread =
        Lwt.finalize
          (fun () ->
            Lwt.catch
              (fun () ->
                let*! _ = Lwt_unix.lseek floating_ro_fd 0 Unix.SEEK_SET in
                let* () = Floating_block_store.iter_s_raw_fd f floating_ro_fd in
                let*! _ = Lwt_unix.lseek floating_rw_fd 0 Unix.SEEK_SET in
                let* () = Floating_block_store.iter_s_raw_fd f floating_rw_fd in
                tzfail (Missing_target_block export_block_descr))
              (function
                | Done -> return_unit
                | exn ->
                    tzfail (Cannot_read_floating_store (Printexc.to_string exn))))
          (fun () ->
            bpush#close ;
            Lwt.return_unit)
      in
      return (reading_thread, stream)

  (* Export the protocol table (info regarding the protocol transitions)
     as well as all the stored protocols *)
  let export_protocols snapshot_exporter export_block all_protocol_levels
      protocol_store_dir progress_display_mode =
    let open Lwt_syntax in
    let* () = Event.(emit exporting_protocols) () in
    let export_proto_level = Store.Block.proto_level export_block in
    (* Export only the protocols with a protocol level below the
       protocol's level of the targeted export block. *)
    let protocol_levels =
      Protocol_levels.filter
        (fun proto_level _ -> proto_level <= export_proto_level)
        all_protocol_levels
    in
    let* () =
      Exporter.write_protocols_table snapshot_exporter ~f:(fun fd ->
          let bytes =
            Data_encoding.Binary.to_bytes_exn
              Protocol_levels.encoding
              protocol_levels
          in
          Lwt_utils_unix.write_bytes ~pos:0 fd bytes)
    in
    let* dir_handle = Lwt_unix.opendir (Naming.dir_path protocol_store_dir) in
    let proto_to_export =
      List.map
        (fun (_, {Protocol_levels.protocol; _}) -> protocol)
        (Protocol_levels.bindings protocol_levels)
    in
    let nb_proto_to_export = List.length proto_to_export in
    Animation.display_progress
      ~pp_print_step:(fun fmt i ->
        Format.fprintf fmt "Copying protocols: %d/%d" i nb_proto_to_export)
      ~progress_display_mode
      (fun notify ->
        let rec copy_protocols () =
          Lwt.catch
            (fun () ->
              let* d = Lwt_unix.readdir dir_handle in
              match d with
              | filename
                when filename = Filename.current_dir_name
                     || filename = Filename.parent_dir_name ->
                  copy_protocols ()
              | filename -> (
                  match Protocol_hash.of_b58check_opt filename with
                  | None -> return_ok_unit
                  | Some ph ->
                      let src_protocol_file =
                        Naming.protocol_file protocol_store_dir ph
                      in
                      let* () =
                        if
                          List.mem ~equal:Protocol_hash.equal ph proto_to_export
                        then
                          let* () =
                            Exporter.copy_protocol
                              snapshot_exporter
                              ~src:(Naming.file_path src_protocol_file)
                              ~dst_ph:ph
                          in
                          notify ()
                        else Lwt.return_unit
                      in
                      copy_protocols ()))
            (function
              | End_of_file -> return_ok_unit | exn -> fail_with_exn exn)
        in
        Lwt.finalize
          (fun () -> copy_protocols ())
          (fun () ->
            let* () = Lwt_unix.closedir dir_handle in
            Event.(emit protocols_exported) ()))

  (* Ensures that the data needed to export the snapshot from the target
     block is available:
     - the target_block is not the genesis
     - the target_block and its predecessor are known
     - the context of the predecessor of the target_block must be known
     - at least max_op_ttl(target_block) headers must be available
  *)
  let check_export_block_validity chain_store block =
    let open Lwt_result_syntax in
    let block_hash, block_level = Store.Block.descriptor block in
    let*! is_known = Store.Block.is_known_valid chain_store block_hash in
    let* () =
      fail_unless
        is_known
        (Invalid_export_block {block = Some block_hash; reason = `Unknown})
    in
    let* () =
      fail_when
        (Store.Block.is_genesis chain_store block_hash)
        (Invalid_export_block {block = Some block_hash; reason = `Genesis})
    in
    let*! _, savepoint_level = Store.Chain.savepoint chain_store in
    let* () =
      fail_when
        Compare.Int32.(savepoint_level > block_level)
        (Invalid_export_block {block = Some block_hash; reason = `Pruned})
    in
    let* block = Store.Block.read_block chain_store block_hash in
    let* pred_block =
      let*! o = Store.Block.read_predecessor_opt chain_store block in
      match o with
      | None ->
          tzfail
            (Invalid_export_block
               {block = Some block_hash; reason = `Not_enough_pred})
      | Some pred_block -> return pred_block
    in
    (* Make sure that the predecessor's context is known *)
    let* pred_context_exists =
      let protocol_level = Store.Block.proto_level block in
      let* expect_predecessor_context_hash =
        Store.Chain.expect_predecessor_context_hash chain_store ~protocol_level
      in
      let*! exists =
        if expect_predecessor_context_hash then
          Store.Block.context_exists chain_store block
        else Store.Block.context_exists chain_store pred_block
      in
      return exists
    in
    (* We also need the predecessor not to be pruned *)
    let* () =
      fail_when
        Compare.Int32.(
          savepoint_level > Int32.pred block_level && not pred_context_exists)
        (Invalid_export_block {block = Some block_hash; reason = `Pruned_pred})
    in
    let* block_metadata =
      let*! o = Store.Block.get_block_metadata_opt chain_store block in
      match o with
      | None ->
          tzfail
            (Invalid_export_block {block = Some block_hash; reason = `Pruned})
      | Some block_metadata -> return block_metadata
    in
    let*! _, caboose_level = Store.Chain.caboose chain_store in
    (* We will need the following blocks
       [ (target_block - max_op_ttl(target_block)) ; ... ; target_block ] *)
    let block_max_op_ttl = Store.Block.max_operations_ttl block_metadata in
    let*! genesis_block = Store.Chain.genesis_block chain_store in
    let genesis_level = Store.Block.level genesis_block in
    let minimum_level_needed =
      Compare.Int32.(
        max genesis_level Int32.(sub block_level (of_int block_max_op_ttl)))
    in
    let* () =
      fail_when
        Compare.Int32.(minimum_level_needed < caboose_level)
        (Invalid_export_block
           {block = Some block_hash; reason = `Not_enough_pred})
    in
    return (pred_block, minimum_level_needed)

  (* Retrieves the block to export based on given block "as hint". As
     the checkpoint is provided as a default value, we must ensure
     that it is valid. It may be not the case when the checkpoint was
     set in the future. In this particular case, the last preserved
     block level of the current head is chosen. *)
  let retrieve_export_block chain_store block =
    let open Lwt_result_syntax in
    let* export_block =
      (match block with
      | `Genesis ->
          (* Exporting the genesis block does not make sense. *)
          tzfail
            (Invalid_export_block
               {
                 block = Some (Store.Chain.genesis chain_store).Genesis.block;
                 reason = `Genesis;
               })
      | `Alias (`Caboose, distance) when distance >= 0 ->
          (* With the caboose, we do not allow to use the ~/- as it is a
             non sense. Additionally, it is not allowed to export the
             caboose block. *)
          let*! hash, _ = Store.Chain.caboose chain_store in
          tzfail (Invalid_export_block {block = Some hash; reason = `Caboose})
      | _ -> Store.Chain.block_of_identifier chain_store block)
      |> trace (Invalid_export_block {block = None; reason = `Unknown})
    in
    let* pred_block, minimum_level_needed =
      check_export_block_validity chain_store export_block
    in
    return (export_block, pred_block, minimum_level_needed)

  (* Returns the list of cemented files to export and an optional list
     of remaining blocks. If the export block is cemented, we need to cut
     the cycle containing the export block accordingly and retrieve the
     extra blocks. *)
  let compute_cemented_table_and_extra_cycle chain_store ~src_cemented_dir
      ~export_block =
    let open Lwt_result_syntax in
    let* o = Cemented_block_store.load_table src_cemented_dir in
    match o with
    | None -> return ([], None)
    | Some table_arr -> (
        let table_len = Array.length table_arr in
        let table = Array.to_list table_arr in
        (* Check whether the export_block is in the cemented blocks *)
        let export_block_level = Store.Block.level export_block in
        let is_cemented =
          table_len > 0
          && Compare.Int32.(
               export_block_level
               <= table_arr.(table_len - 1).Cemented_block_store.end_level)
        in
        if not is_cemented then
          (* Return either an empty list or the list of all cemented files *)
          return (table, None)
        else
          let is_last_cemented_block =
            Compare.Int32.(
              export_block_level
              = table_arr.(table_len - 1).Cemented_block_store.end_level)
          in
          if is_last_cemented_block then return (table, Some [])
          else
            (* If the export block is cemented, cut the cycle containing the
               export block accordingly and retrieve the extra blocks *)
            let filtered_table, extra_cycles =
              List.partition
                (fun {Cemented_block_store.end_level; _} ->
                  Compare.Int32.(export_block_level > end_level))
                table
            in
            assert (extra_cycles <> []) ;
            let extra_cycle =
              List.hd extra_cycles |> WithExceptions.Option.get ~loc:__LOC__
            in
            (* If the export block is the last block in cycle, append the cycle *)
            if Compare.Int32.(export_block_level = extra_cycle.end_level) then
              return (filtered_table @ [extra_cycle], Some [])
            else
              let* first_block =
                let* first_block_in_cycle =
                  Store.Block.read_block_by_level
                    chain_store
                    extra_cycle.start_level
                in
                (* TODO explain this... *)
                if
                  Compare.Int32.(
                    Store.Block.level first_block_in_cycle > export_block_level)
                then
                  (* When the cycles are short, we may keep more blocks in the
                     floating store than in cemented *)
                  let*! _, caboose_level = Store.Chain.caboose chain_store in
                  Store.Block.read_block_by_level chain_store caboose_level
                else return first_block_in_cycle
              in
              let*! o =
                Store.Chain_traversal.path
                  chain_store
                  ~from_block:first_block
                  ~to_block:export_block
              in
              match o with
              | None -> tzfail Cannot_retrieve_block_interval
              | Some floating_blocks ->
                  (* Don't forget to add the first block as
                     [Chain_traversal.path] does not include the lower-bound
                     block *)
                  let floating_blocks = first_block :: floating_blocks in
                  return (filtered_table, Some floating_blocks))

  (* Ensures that the history mode requested to export is compatible
     with the current storage. *)
  let check_history_mode chain_store ~rolling =
    let open Lwt_result_syntax in
    match (Store.Chain.history_mode chain_store : History_mode.t) with
    | Archive | Full _ -> return_unit
    | Rolling _ when rolling -> return_unit
    | Rolling _ as stored ->
        tzfail (Incompatible_history_mode {stored; requested = Full None})

  let export_floating_block_stream snapshot_exporter floating_block_stream
      progress_display_mode =
    let open Lwt_syntax in
    let* () = Event.(emit exporting_floating_blocks) () in
    let f fd =
      let* is_empty = Lwt_stream.is_empty floating_block_stream in
      if is_empty then Lwt.return_unit
      else
        Animation.display_progress
          ~every:10
          ~pp_print_step:(fun fmt i ->
            Format.fprintf fmt "Copying floating blocks: %d blocks copied" i)
          (fun notify ->
            Lwt_stream.iter_s
              (fun b ->
                let* () = write_floating_block fd b in
                notify ())
              floating_block_stream)
          ~progress_display_mode
    in
    let* () = Exporter.write_floating_blocks snapshot_exporter ~f in
    let* () = Event.(emit floating_blocks_exported) () in
    return_ok_unit

  let export_context snapshot_exporter ~context_dir context_hash =
    let open Lwt_result_syntax in
    let*! () = Event.(emit exporting_context) () in
    let*! context_index = Context.init ~readonly:true context_dir in
    let is_gc_allowed = Context.is_gc_allowed context_index in
    let* () =
      if not is_gc_allowed then tzfail Cannot_export_snapshot_format
      else
        Animation.three_dots
          ~progress_display_mode:Auto
          ~msg:"Exporting context"
        @@ fun () ->
        Lwt.finalize
          (fun () ->
            Exporter.export_context snapshot_exporter context_index context_hash)
          (fun () -> Context.close context_index)
    in
    let*! () = Event.(emit context_exported) () in
    return_unit

  let export_rolling snapshot_exporter ~store_dir ~context_dir ~block ~rolling
      genesis =
    let open Lwt_result_syntax in
    let export_rolling_f chain_store =
      let* () = check_history_mode chain_store ~rolling in
      let* export_block, pred_block, lowest_block_level_needed =
        retrieve_export_block chain_store block
      in
      (* The number of additional cycles to export is fixed as the
         snapshot content must not rely on the local configuration. *)
      let export_mode = History_mode.Rolling None in
      let*! () =
        Event.(
          emit
            export_info
            ( Version.current_version,
              export_mode,
              Store.Block.descriptor export_block ))
      in
      (* Blocks *)
      (* Read the store to gather only the necessary blocks *)
      let* minimum_block =
        Store.Block.read_block_by_level chain_store lowest_block_level_needed
      in
      let* floating_blocks =
        let*! o =
          Store.Chain_traversal.path
            chain_store
            ~from_block:minimum_block
            ~to_block:pred_block
        in
        match o with
        | None -> tzfail Cannot_retrieve_block_interval
        | Some blocks ->
            (* Don't forget to add the first block as
               [Chain_traversal.path] does not include the
               lower-bound block *)
            return (minimum_block :: blocks)
      in
      (* Prune all blocks except for the export_block's predecessor *)
      let floating_block_stream =
        Lwt_stream.of_list
          (List.filter_map
             (fun b ->
               Some {(Store.Unsafe.repr_of_block b) with metadata = None})
             floating_blocks)
      in
      (* Protocols *)
      let*! protocol_levels = Store.Chain.all_protocol_levels chain_store in
      (* Filter protocols s.t. forall proto. proto.level >=
         caboose.proto_level. *)
      let protocol_levels =
        Protocol_levels.(
          filter
            (fun level {activation_block; _} ->
              let block = activation_block in
              level >= Store.Block.proto_level minimum_block
              || Store.Block.is_genesis chain_store (fst block))
            protocol_levels)
      in
      let* pred_resulting_context_hash =
        Store.Block.resulting_context_hash chain_store pred_block
      in
      let* resulting_context_hash =
        Store.Block.resulting_context_hash chain_store export_block
      in
      let* () =
        export_context
          snapshot_exporter
          ~context_dir
          pred_resulting_context_hash
      in
      return
        ( export_mode,
          export_block,
          resulting_context_hash,
          pred_block,
          protocol_levels,
          (return_unit, floating_block_stream) )
    in
    let* ( export_mode,
           export_block,
           resulting_context_hash,
           pred_block,
           protocol_levels,
           (return_unit, floating_block_stream) ) =
      Store.Unsafe.open_for_snapshot_export
        ~store_dir
        ~context_dir
        genesis
        ~locked_f:export_rolling_f
    in
    return
      ( export_mode,
        export_block,
        resulting_context_hash,
        pred_block,
        protocol_levels,
        (return_unit, floating_block_stream) )

  let export_full snapshot_exporter ~store_dir ~context_dir ~block ~rolling
      ~progress_display_mode genesis =
    let open Lwt_result_syntax in
    let export_full_f chain_store =
      let* () = check_history_mode chain_store ~rolling in
      let* export_block, pred_block, _lowest_block_level_needed =
        retrieve_export_block chain_store block
      in
      (* The number of additional cycles to export is fixed as the
         snapshot content must not rely on the local configuration. *)
      let export_mode = History_mode.Full None in
      let*! () =
        Event.(
          emit
            export_info
            ( Version.current_version,
              export_mode,
              Store.Block.descriptor export_block ))
      in
      let store_dir = Naming.store_dir ~dir_path:store_dir in
      let chain_id = Store.Chain.chain_id chain_store in
      let chain_dir = Naming.chain_dir store_dir chain_id in
      (* Open the floating FDs (in readonly) while the lock is present *)
      let ro_floating_blocks =
        Naming.floating_blocks_file (Naming.floating_blocks_dir chain_dir RO)
      in
      let rw_floating_blocks =
        Naming.floating_blocks_file (Naming.floating_blocks_dir chain_dir RW)
      in
      let*! ro_fd =
        Lwt_unix.openfile
          (Naming.file_path ro_floating_blocks)
          [Unix.O_RDONLY]
          snapshot_ro_file_perm
      in
      let*! rw_fd =
        Lwt_unix.openfile
          (Naming.file_path rw_floating_blocks)
          [Unix.O_RDONLY]
          snapshot_rw_file_perm
      in
      Lwt.catch
        (fun () ->
          let src_cemented_dir = Naming.cemented_blocks_dir chain_dir in
          (* Compute the necessary cemented table *)
          let* cemented_table, extra_floating_blocks =
            compute_cemented_table_and_extra_cycle
              chain_store
              ~src_cemented_dir
              ~export_block
          in
          let*! protocol_levels = Store.Chain.all_protocol_levels chain_store in
          let block_store = Store.Unsafe.get_block_store chain_store in
          let cemented_store = Block_store.cemented_block_store block_store in
          let should_filter_indexes =
            match
              Cemented_block_store.get_highest_cemented_level cemented_store
            with
            | None -> false
            | Some max_cemented_level ->
                Compare.Int32.(
                  max_cemented_level > Store.Block.level export_block)
          in
          let* pred_resulting_context_hash =
            Store.Block.resulting_context_hash chain_store pred_block
          in
          let* resulting_context_hash =
            Store.Block.resulting_context_hash chain_store export_block
          in
          let* () =
            export_context
              snapshot_exporter
              ~context_dir
              pred_resulting_context_hash
          in
          return
            ( export_mode,
              export_block,
              resulting_context_hash,
              pred_block,
              protocol_levels,
              cemented_table,
              (ro_fd, rw_fd),
              extra_floating_blocks,
              should_filter_indexes ))
        (fun exn ->
          let*! _ = Lwt_utils_unix.safe_close ro_fd in
          let*! _ = Lwt_utils_unix.safe_close rw_fd in
          fail_with_exn exn)
    in
    let* ( export_mode,
           export_block,
           pred_block,
           pred_resulting_context,
           protocol_levels,
           cemented_table,
           (floating_ro_fd, floating_rw_fd),
           extra_floating_blocks,
           should_filter_indexes ) =
      Store.Unsafe.open_for_snapshot_export
        ~store_dir
        ~context_dir
        genesis
        ~locked_f:export_full_f
    in
    let* () =
      copy_cemented_blocks
        snapshot_exporter
        ~should_filter_indexes
        cemented_table
        ~progress_display_mode
    in
    let finalizer () =
      let*! _ = Lwt_utils_unix.safe_close floating_ro_fd in
      let*! _ = Lwt_utils_unix.safe_close floating_rw_fd in
      Lwt.return_unit
    in
    let* reading_thread, floating_block_stream =
      match extra_floating_blocks with
      | Some floating_blocks ->
          let*! () = finalizer () in
          return
            ( return_unit,
              Lwt_stream.of_list
                (List.map Store.Unsafe.repr_of_block floating_blocks) )
      | None ->
          (* The export block is in the floating stores, copy all the
             floating stores until the block is reached *)
          let* reading_thread, floating_block_stream =
            export_floating_blocks ~floating_ro_fd ~floating_rw_fd ~export_block
          in
          let reading_thread =
            Lwt.finalize (fun () -> reading_thread) finalizer
          in
          return (reading_thread, floating_block_stream)
    in
    return
      ( export_mode,
        export_block,
        pred_block,
        pred_resulting_context,
        protocol_levels,
        (reading_thread, floating_block_stream) )

  let ensure_valid_export_chain_dir store_path chain_id =
    let open Lwt_result_syntax in
    let store_dir = Naming.store_dir ~dir_path:store_path in
    let chain_dir = Naming.chain_dir store_dir chain_id in
    let*! b = Lwt_unix.file_exists (Naming.dir_path chain_dir) in
    match b with
    | true -> return_unit
    | false ->
        tzfail
          (Invalid_chain_store_export (chain_id, Naming.dir_path store_dir))

  let export ?snapshot_path ?(rolling = false) ~block ~store_dir ~context_dir
      ~chain_name ~progress_display_mode genesis =
    let open Lwt_result_syntax in
    let chain_id = Chain_id.of_block_hash genesis.Genesis.block in
    let* () = ensure_valid_export_chain_dir store_dir chain_id in
    let* snapshot_exporter = init snapshot_path in
    (* Register a clean up callback to prevent export cancellation not
       to be correctly cleaned. *)
    let cleaner_id =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
          let*! () = Exporter.cleaner snapshot_exporter in
          Lwt.return_unit)
    in
    let* metadata =
      protect (fun () ->
          let* ( export_mode,
                 export_block,
                 resulting_context_hash,
                 pred_block,
                 protocol_levels,
                 (reading_thread, floating_block_stream) ) =
            if rolling then
              export_rolling
                snapshot_exporter
                ~store_dir
                ~context_dir
                ~block
                ~rolling
                genesis
            else
              export_full
                snapshot_exporter
                ~store_dir
                ~context_dir
                ~block
                ~rolling
                ~progress_display_mode
                genesis
          in
          (* Retrieve predecessor block metadata hash and operations
             metadata hash from the context of the exported block *)
          let predecessor_block_metadata_hash =
            Store.Block.block_metadata_hash pred_block
          in
          let predecessor_ops_metadata_hash =
            Store.Block.all_operations_metadata_hash pred_block
          in
          let*! () =
            Exporter.write_block_data
              snapshot_exporter
              ~predecessor_header:(Store.Block.header pred_block)
              ~predecessor_block_metadata_hash
              ~predecessor_ops_metadata_hash
              ~export_block
              ~resulting_context_hash
          in
          let* metadata =
            return
              (Snapshot_metadata.Current
                 {
                   chain_name;
                   history_mode = export_mode;
                   block_hash = Store.Block.hash export_block;
                   level = Store.Block.level export_block;
                   timestamp = Store.Block.timestamp export_block;
                 })
          in
          let* () =
            export_floating_block_stream
              snapshot_exporter
              floating_block_stream
              progress_display_mode
          in
          let* () = reading_thread in
          let* () =
            export_protocols
              snapshot_exporter
              export_block
              protocol_levels
              (Naming.protocol_store_dir (Naming.store_dir ~dir_path:store_dir))
              progress_display_mode
          in
          return metadata)
    in
    let* exported_snapshot_filename =
      Exporter.finalize snapshot_exporter metadata
    in
    let*! () = Event.(emit export_success exported_snapshot_filename) in
    Lwt_exit.unregister_clean_up_callback cleaner_id ;
    return_unit
end

module type LOADER = sig
  type t

  val load : string -> t Lwt.t

  val load_snapshot_header : t -> Snapshot_header.t tzresult Lwt.t

  val close : t -> unit Lwt.t
end

module Raw_loader : LOADER = struct
  type t = {snapshot_dir : [`Snapshot_dir] Naming.directory}

  let load snapshot_path =
    let snapshot_dir = Naming.snapshot_dir ~snapshot_path () in
    Lwt.return {snapshot_dir}

  let load_snapshot_version t =
    let open Lwt_result_syntax in
    let snapshot_file =
      Naming.(snapshot_version_file t.snapshot_dir |> file_path)
    in
    let read_json json = Data_encoding.Json.destruct Version.encoding json in
    let* json = Lwt_utils_unix.Json.read_file snapshot_file in
    return (read_json json)

  let load_snapshot_metadata t =
    let metadata_file =
      Naming.(snapshot_metadata_file t.snapshot_dir |> file_path)
    in
    Snapshot_metadata.read_metadata ~metadata_file

  let load_snapshot_legacy_metadata t =
    let metadata_file =
      Naming.(snapshot_metadata_file t.snapshot_dir |> file_path)
    in
    Snapshot_metadata.read_legacy_metadata ~metadata_file

  let load_snapshot_header t =
    let open Lwt_result_syntax in
    let* version = load_snapshot_version t in
    let* is_legacy_format = Version.is_legacy_format version in
    if is_legacy_format then
      let* legacy_metadata = load_snapshot_legacy_metadata t in
      return (Snapshot_header.Legacy (version, legacy_metadata))
    else
      let* metadata = load_snapshot_metadata t in
      return (Snapshot_header.Current (version, metadata))

  let close _ = Lwt.return_unit
end

module Tar_loader : LOADER = struct
  type t = {
    tar : Onthefly.i;
    snapshot_file : [`Snapshot_file] Naming.file;
    snapshot_tar : [`Tar_archive] Naming.directory;
  }

  let load snapshot_path =
    let open Lwt_syntax in
    let snapshot_dir =
      Naming.snapshot_dir ~snapshot_path:(Filename.dirname snapshot_path) ()
    in
    let snapshot_tar = Naming.snapshot_tar_root in
    let snapshot_file =
      Naming.snapshot_file
        ~snapshot_filename:(Filename.basename snapshot_path)
        snapshot_dir
    in
    let* tar = Onthefly.open_in ~file:(Naming.file_path snapshot_file) in
    Lwt.return {tar; snapshot_file; snapshot_tar}

  let load_snapshot_version t =
    let open Lwt_result_syntax in
    let filename = Naming.(snapshot_version_file t.snapshot_tar |> file_path) in
    let*! o =
      let*! o = Onthefly.find_file t.tar ~filename in
      match o with
      | Some file -> (
          let*! str = Onthefly.load_file t.tar file in
          match Data_encoding.Json.from_string str with
          | Ok json ->
              Lwt.return_some
                (Data_encoding.Json.destruct Version.encoding json)
          | Error _ -> Lwt.return_none)
      | None -> Lwt.return_none
    in
    match o with
    | Some version -> return version
    | None -> tzfail (Cannot_read {kind = `Version; path = filename})

  let load_snapshot_metadata t =
    let open Lwt_result_syntax in
    let filename =
      Naming.(snapshot_metadata_file t.snapshot_tar |> file_path)
    in
    let*! o =
      let*! o = Onthefly.find_file t.tar ~filename in
      match o with
      | Some file -> (
          let*! str = Onthefly.load_file t.tar file in
          match Data_encoding.Json.from_string str with
          | Ok json ->
              Lwt.return_some
                (Data_encoding.Json.destruct
                   Snapshot_metadata.metadata_encoding
                   json)
          | Error _ -> Lwt.return_none)
      | None -> Lwt.return_none
    in
    match o with
    | Some metadata -> return metadata
    | None -> tzfail (Cannot_read {kind = `Metadata; path = filename})

  let load_snapshot_legacy_metadata t =
    let open Lwt_result_syntax in
    let filename =
      Naming.(snapshot_metadata_file t.snapshot_tar |> file_path)
    in
    let*! o =
      let*! o = Onthefly.find_file t.tar ~filename in
      match o with
      | Some file -> (
          let*! str = Onthefly.load_file t.tar file in
          match Data_encoding.Json.from_string str with
          | Ok json ->
              Lwt.return_some
                (Data_encoding.Json.destruct
                   Snapshot_metadata.legacy_metadata_encoding
                   json)
          | Error _ -> Lwt.return_none)
      | None -> Lwt.return_none
    in
    match o with
    | Some metadata -> return metadata
    | None -> tzfail (Cannot_read {kind = `Metadata; path = filename})

  let load_snapshot_header t =
    let open Lwt_result_syntax in
    let* version = load_snapshot_version t in
    let* is_legacy_format = Version.is_legacy_format version in
    if is_legacy_format then
      let* legacy_metadata = load_snapshot_legacy_metadata t in
      return (Snapshot_header.Legacy (version, legacy_metadata))
    else
      let* metadata = load_snapshot_metadata t in
      return (Snapshot_header.Current (version, metadata))

  let close t = Onthefly.close_in t.tar
end

module type Snapshot_loader = sig
  type t

  val load_snapshot_header :
    snapshot_path:string -> Snapshot_header.t tzresult Lwt.t
end

module Make_snapshot_loader (Loader : LOADER) : Snapshot_loader = struct
  type t = Loader.t

  let load = Loader.load

  let close = Loader.close

  let load_snapshot_header ~snapshot_path =
    let open Lwt_syntax in
    let* loader = load snapshot_path in
    protect
      (fun () ->
        Lwt.finalize
          (fun () -> Loader.load_snapshot_header loader)
          (fun () -> close loader))
      ~on_error:(fun err ->
        let* () = close loader in
        Lwt.return_error err)
end

module type IMPORTER = sig
  type t

  val format : snapshot_format

  val init :
    snapshot_path:string ->
    dst_store_dir:[`Store_dir] Naming.directory ->
    Chain_id.t ->
    t tzresult Lwt.t

  val snapshot_version : t -> Version.t

  val snapshot_metadata : t -> Snapshot_metadata.t

  val load_block_data : t -> block_data tzresult Lwt.t

  val restore_context : t -> dst_context_dir:string -> unit tzresult Lwt.t

  val legacy_restore_context :
    t ->
    Context.index ->
    expected_context_hash:Context_hash.t ->
    nb_context_elements:int ->
    progress_display_mode:Animation.progress_display_mode ->
    unit tzresult Lwt.t

  val load_protocol_table :
    t -> Protocol_levels.protocol_info Protocol_levels.t tzresult Lwt.t

  val load_and_validate_protocol_filenames :
    t -> Protocol_hash.t list tzresult Lwt.t

  val copy_and_validate_protocol :
    t -> protocol_hash:Protocol_hash.t -> (unit, error trace) result Lwt.t

  val restore_cemented_indexes : t -> unit Lwt.t

  val load_cemented_files : t -> string list tzresult Lwt.t

  val restore_cemented_cycle : t -> file:string -> unit tzresult Lwt.t

  val restore_floating_blocks :
    t ->
    Block_hash.t ->
    (unit tzresult Lwt.t * Block_repr.block Lwt_stream.t) tzresult Lwt.t

  val close : t -> unit Lwt.t
end

module Raw_importer : IMPORTER = struct
  type t = {
    version : Version.t;
    metadata : Snapshot_metadata.t;
    snapshot_dir : [`Snapshot_dir] Naming.directory;
    snapshot_cemented_dir : [`Cemented_blocks_dir] Naming.directory;
    snapshot_protocol_dir : [`Protocol_dir] Naming.directory;
    dst_cemented_dir : [`Cemented_blocks_dir] Naming.directory;
    dst_protocol_dir : [`Protocol_dir] Naming.directory;
    dst_store_dir : [`Store_dir] Naming.directory;
    dst_chain_dir : [`Chain_dir] Naming.directory;
  }

  let format = Raw

  let load_snapshot_header ~snapshot_path =
    let (module Loader) =
      (module Make_snapshot_loader (Raw_loader) : Snapshot_loader)
    in
    Loader.load_snapshot_header ~snapshot_path

  let snapshot_version {version; _} = version

  let snapshot_metadata {metadata; _} = metadata

  let init ~snapshot_path ~dst_store_dir chain_id =
    let open Lwt_result_syntax in
    let snapshot_dir = Naming.snapshot_dir ~snapshot_path () in
    let snapshot_cemented_dir = Naming.cemented_blocks_dir snapshot_dir in
    let snapshot_protocol_dir = Naming.protocol_store_dir snapshot_dir in
    let dst_chain_dir = Naming.chain_dir dst_store_dir chain_id in
    let dst_cemented_dir = Naming.cemented_blocks_dir dst_chain_dir in
    let dst_protocol_dir = Naming.protocol_store_dir dst_store_dir in
    let* snapshot_header =
      load_snapshot_header ~snapshot_path:(snapshot_dir |> Naming.(dir_path))
    in
    return
      {
        version = Snapshot_header.get_version snapshot_header;
        metadata = Snapshot_header.get_metadata snapshot_header;
        snapshot_dir;
        snapshot_cemented_dir;
        snapshot_protocol_dir;
        dst_cemented_dir;
        dst_protocol_dir;
        dst_store_dir;
        dst_chain_dir;
      }

  let load_block_data t =
    let open Lwt_result_syntax in
    let file = Naming.(snapshot_block_data_file t.snapshot_dir |> file_path) in
    let*! block_data = Lwt_utils_unix.read_file file in
    match Data_encoding.Binary.of_string_opt block_data_encoding block_data with
    | Some block_data -> return block_data
    | None -> (
        let* is_legacy_format = Version.is_legacy_format t.version in
        let* res =
          if is_legacy_format then
            Data_encoding.Binary.of_string_opt
              legacy_block_data_encoding
              block_data
            |> Option.map
                 (fun
                   {
                     block_header;
                     operations;
                     predecessor_header;
                     predecessor_block_metadata_hash;
                     predecessor_ops_metadata_hash;
                   }
                 ->
                   {
                     block_header;
                     operations;
                     predecessor_header;
                     predecessor_block_metadata_hash;
                     predecessor_ops_metadata_hash;
                     resulting_context_hash = Context_hash.zero;
                   })
            |> return
          else
            return
            @@ Data_encoding.Binary.of_string_opt block_data_encoding block_data
        in
        match res with
        | Some v -> return v
        | None -> tzfail (Cannot_read {kind = `Block_data; path = file}))

  let restore_context t ~dst_context_dir =
    let open Lwt_result_syntax in
    let context_file_path =
      Naming.(snapshot_context_file t.snapshot_dir |> file_path)
    in
    let*! () = Lwt_utils_unix.copy_dir context_file_path dst_context_dir in
    return_unit

  let legacy_restore_context t context_index ~expected_context_hash
      ~nb_context_elements ~progress_display_mode =
    let open Lwt_result_syntax in
    let context_file_path =
      Naming.(snapshot_context_file t.snapshot_dir |> file_path)
    in
    let* fd =
      Lwt.catch
        (fun () ->
          let*! fd =
            Lwt_unix.openfile
              context_file_path
              Lwt_unix.[O_RDONLY]
              snapshot_ro_file_perm
          in
          return fd)
        (function
          | Unix.Unix_error (e, _, _) ->
              tzfail (Context.Cannot_open_file (Unix.error_message e))
          | exc ->
              let msg =
                Printf.sprintf "unknown error: %s" (Printexc.to_string exc)
              in
              tzfail (Context.Cannot_open_file msg))
    in
    Lwt.finalize
      (fun () ->
        let* () =
          Context.restore_context
            context_index
            ~expected_context_hash
            ~fd
            ~nb_context_elements
            ~in_memory:false
            ~progress_display_mode
        in
        let*! current = Lwt_unix.lseek fd 0 Lwt_unix.SEEK_CUR in
        let*! stats = Lwt_unix.fstat fd in
        let total = stats.Lwt_unix.st_size in
        if current = total then return_unit
        else tzfail (Context.Suspicious_file (total - current)))
      (fun () -> Lwt_unix.close fd)

  let load_protocol_table t =
    let open Lwt_result_syntax in
    let protocol_tbl_filename =
      Naming.(snapshot_protocol_levels_file t.snapshot_dir |> encoded_file_path)
    in
    let*! table_bytes = Lwt_utils_unix.read_file protocol_tbl_filename in
    let* res =
      let* is_legacy_format = Version.is_legacy_format t.version in
      if is_legacy_format then
        (* Use the legacy encoding *)
        match
          Data_encoding.Binary.of_string_opt
            Protocol_levels.Legacy.encoding
            table_bytes
        with
        | Some table ->
            let* res =
              Protocol_levels.Legacy.fold_es
                (fun proto_level activation_block map ->
                  let protocol_info =
                    {
                      Protocol_levels.protocol =
                        activation_block.Protocol_levels.Legacy.protocol;
                      activation_block = activation_block.block;
                      (* Only snapshot with legacy semantics will have
                         legacy encoding. *)
                      expect_predecessor_context = false;
                    }
                  in
                  return (Protocol_levels.add proto_level protocol_info map))
                table
                Protocol_levels.empty
            in
            return_some res
        | None -> return_none
      else
        Data_encoding.Binary.of_string_opt Protocol_levels.encoding table_bytes
        |> return
    in
    match res with
    | Some v -> return v
    | None ->
        tzfail
          (Cannot_read {kind = `Protocol_table; path = protocol_tbl_filename})

  let load_and_validate_protocol_filenames t =
    let open Lwt_result_syntax in
    let protocol_levels_file =
      Naming.snapshot_protocol_levels_file t.snapshot_dir
    in
    let stream =
      Lwt_unix.files_of_directory (Naming.dir_path t.snapshot_protocol_dir)
    in
    let*! files = Lwt_stream.to_list stream in
    let is_not_a_protocol =
      let protocol_levels_path =
        Naming.encoded_file_path protocol_levels_file
      in
      fun file ->
        file = Filename.current_dir_name
        || file = Filename.parent_dir_name
        || file = Filename.basename protocol_levels_path
    in
    let protocol_files =
      List.filter_map
        (function
          | file when is_not_a_protocol file -> None | file -> Some file)
        files
    in
    List.map_es
      (fun file ->
        match Protocol_hash.of_b58check_opt file with
        | Some ph -> return ph
        | None -> tzfail (Invalid_protocol_file file))
      protocol_files

  let copy_and_validate_protocol t ~protocol_hash =
    let open Lwt_result_syntax in
    let src =
      Filename.concat
        (Naming.dir_path t.snapshot_protocol_dir)
        (Protocol_hash.to_b58check protocol_hash)
    in
    let dst =
      Filename.concat
        (Naming.dir_path t.dst_protocol_dir)
        (Protocol_hash.to_b58check protocol_hash)
    in
    let*! () = Lwt_utils_unix.copy_file ~src ~dst in
    let*! protocol_sources = Lwt_utils_unix.read_file dst in
    match Protocol.of_string protocol_sources with
    | None -> tzfail (Cannot_decode_protocol protocol_hash)
    | Some p ->
        let hash = Protocol.hash p in
        fail_unless
          (Protocol_hash.equal protocol_hash hash)
          (Inconsistent_protocol_hash {expected = protocol_hash; got = hash})

  let restore_cemented_indexes t =
    let open Lwt_syntax in
    let src_level_dir =
      Naming.(
        cemented_blocks_level_index_dir t.snapshot_cemented_dir |> dir_path)
    in
    let src_hash_dir =
      Naming.(
        cemented_blocks_hash_index_dir t.snapshot_cemented_dir |> dir_path)
    in
    let* () =
      if Sys.file_exists src_level_dir then
        Lwt_utils_unix.copy_dir
          src_level_dir
          Naming.(
            cemented_blocks_level_index_dir t.dst_cemented_dir |> dir_path)
      else Lwt.return_unit
    in
    if Sys.file_exists src_hash_dir then
      Lwt_utils_unix.copy_dir
        src_hash_dir
        Naming.(cemented_blocks_hash_index_dir t.dst_cemented_dir |> dir_path)
    else Lwt.return_unit

  let load_cemented_files t =
    let open Lwt_result_syntax in
    let stream =
      Lwt_unix.files_of_directory (Naming.dir_path t.snapshot_cemented_dir)
    in
    let*! files = Lwt_stream.to_list stream in
    let is_not_cycle_file file =
      file = Filename.current_dir_name
      || file = Filename.parent_dir_name
      || file
         = Filename.basename
             (Naming.dir_path
                (Naming.cemented_blocks_hash_index_dir t.snapshot_cemented_dir))
      || file
         = Filename.basename
             (Naming.dir_path
                (Naming.cemented_blocks_level_index_dir t.snapshot_cemented_dir))
    in
    List.filter_es
      (function
        | file when is_not_cycle_file file -> return_false
        | file ->
            let is_valid =
              match String.split_on_char '_' file with
              | [s; e] ->
                  Int32.of_string_opt s <> None || Int32.of_string_opt e <> None
              | _ -> false
            in
            if not is_valid then tzfail (Invalid_cemented_file file)
            else return_true)
      files

  let restore_cemented_cycle t ~file =
    let open Lwt_syntax in
    let src = Filename.concat (Naming.dir_path t.snapshot_cemented_dir) file in
    let dst = Filename.concat (Naming.dir_path t.dst_cemented_dir) file in
    let* () = Lwt_utils_unix.copy_file ~src ~dst in
    return_ok_unit

  let restore_floating_blocks t genesis_hash =
    let open Lwt_result_syntax in
    let floating_blocks_file =
      Naming.(snapshot_floating_blocks_file t.snapshot_dir |> file_path)
    in
    if not (Sys.file_exists floating_blocks_file) then
      return (return_unit, Lwt_stream.of_list [])
    else
      let*! fd =
        Lwt_unix.openfile
          floating_blocks_file
          Unix.[O_RDONLY]
          snapshot_ro_file_perm
      in
      let stream, bounded_push = Lwt_stream.create_bounded 1000 in
      let rec loop ?pred_block nb_bytes_left =
        if nb_bytes_left < 0 then tzfail Corrupted_floating_store
        else if nb_bytes_left = 0 then return_unit
        else
          let*! block, len_read = Block_repr_unix.read_next_block_exn fd in
          let* () =
            Block_repr.check_block_consistency ~genesis_hash ?pred_block block
          in
          let*! () = bounded_push#push block in
          loop (nb_bytes_left - len_read)
      in
      let reading_thread =
        Lwt.finalize
          (fun () ->
            let*! eof_offset = Lwt_unix.lseek fd 0 Unix.SEEK_END in
            let*! _ = Lwt_unix.lseek fd 0 Unix.SEEK_SET in
            loop eof_offset)
          (fun () ->
            bounded_push#close ;
            let*! _ = Lwt_utils_unix.safe_close fd in
            Lwt.return_unit)
      in
      return (reading_thread, stream)

  let close _ = Lwt.return_unit
end

module Tar_importer : IMPORTER = struct
  type t = {
    version : Version.t;
    metadata : Snapshot_metadata.t;
    snapshot_file : [`Snapshot_file] Naming.file;
    snapshot_tar : [`Tar_archive] Naming.directory;
    snapshot_cemented_blocks_dir : [`Cemented_blocks_dir] Naming.directory;
    dst_store_dir : [`Store_dir] Naming.directory;
    dst_chain_dir : [`Chain_dir] Naming.directory;
    dst_cemented_dir : [`Cemented_blocks_dir] Naming.directory;
    dst_protocol_dir : [`Protocol_dir] Naming.directory;
    tar : Onthefly.i;
    (* Store the files of the archive to avoid re-reading them *)
    files : Onthefly.file list;
  }

  let format = Tar

  let load_snapshot_header ~snapshot_path =
    let (module Loader) =
      (module Make_snapshot_loader (Tar_loader) : Snapshot_loader)
    in
    Loader.load_snapshot_header ~snapshot_path

  let snapshot_version {version; _} = version

  let snapshot_metadata {metadata; _} = metadata

  let init ~snapshot_path ~dst_store_dir chain_id =
    let open Lwt_result_syntax in
    let snapshot_dir =
      Naming.snapshot_dir ~snapshot_path:(Filename.dirname snapshot_path) ()
    in
    let snapshot_tar = Naming.snapshot_tar_root in
    let snapshot_file =
      Naming.snapshot_file
        ~snapshot_filename:(Filename.basename snapshot_path)
        snapshot_dir
    in
    let snapshot_cemented_blocks_dir =
      Naming.cemented_blocks_dir snapshot_tar
    in
    let dst_chain_dir = Naming.chain_dir dst_store_dir chain_id in
    let dst_cemented_dir = Naming.cemented_blocks_dir dst_chain_dir in
    let dst_protocol_dir = Naming.protocol_store_dir dst_store_dir in
    let* snapshot_header =
      load_snapshot_header ~snapshot_path:(snapshot_file |> Naming.(file_path))
    in
    let*! tar = Onthefly.open_in ~file:(Naming.file_path snapshot_file) in
    let*! files = Onthefly.list_files tar in
    return
      {
        version = Snapshot_header.get_version snapshot_header;
        metadata = Snapshot_header.get_metadata snapshot_header;
        snapshot_file;
        snapshot_tar;
        snapshot_cemented_blocks_dir;
        dst_store_dir;
        dst_chain_dir;
        dst_cemented_dir;
        dst_protocol_dir;
        tar;
        files;
      }

  let load_block_data t =
    let open Lwt_result_syntax in
    let filename =
      Naming.(snapshot_block_data_file t.snapshot_tar |> file_path)
    in
    let*! o = Onthefly.load_from_filename t.tar ~filename in
    match o with
    | Some str -> (
        let* is_legacy_format = Version.is_legacy_format t.version in
        let* res =
          if is_legacy_format then
            Data_encoding.Binary.of_string_opt legacy_block_data_encoding str
            |> Option.map
                 (fun
                   {
                     block_header;
                     operations;
                     predecessor_header;
                     predecessor_block_metadata_hash;
                     predecessor_ops_metadata_hash;
                   }
                 ->
                   {
                     block_header;
                     operations;
                     predecessor_header;
                     predecessor_block_metadata_hash;
                     predecessor_ops_metadata_hash;
                     resulting_context_hash = Context_hash.zero;
                   })
            |> return
          else
            return @@ Data_encoding.Binary.of_string_opt block_data_encoding str
        in
        match res with
        | Some v -> return v
        | None -> tzfail (Cannot_read {kind = `Block_data; path = filename}))
    | None -> tzfail (Cannot_read {kind = `Block_data; path = filename})

  let restore_context t ~dst_context_dir =
    let open Lwt_result_syntax in
    let*! () = Lwt_unix.mkdir dst_context_dir snapshot_dir_perm in
    let index = Filename.concat dst_context_dir "index" in
    let*! () = Lwt_unix.mkdir index snapshot_dir_perm in
    let*! context_files =
      Onthefly.find_files_with_common_path t.tar ~pattern:"context"
    in
    let dst_dir = Filename.chop_suffix dst_context_dir "context" in
    let*! () =
      List.iter_s
        (fun file ->
          let filename = Onthefly.get_filename file in
          Onthefly.copy_to_file
            t.tar
            file
            ~dst:Filename.(concat dst_dir filename))
        context_files
    in
    return_unit

  let legacy_restore_context t context_index ~expected_context_hash
      ~nb_context_elements ~progress_display_mode =
    let open Lwt_result_syntax in
    let filename = Naming.(snapshot_context_file t.snapshot_tar |> file_path) in
    let* header =
      let*! o = Onthefly.get_file t.tar ~filename in
      match o with
      | Some header -> return header
      | None -> tzfail (Cannot_read {kind = `Context; path = filename})
    in
    let*! fd = Onthefly.read_raw t.tar header in
    Context.restore_context
      context_index
      ~expected_context_hash
      ~nb_context_elements
      ~fd
      ~in_memory:false
      ~progress_display_mode

  let load_protocol_table t =
    let open Lwt_result_syntax in
    let protocol_tbl_filename =
      Naming.(snapshot_protocol_levels_file t.snapshot_tar |> encoded_file_path)
    in
    let*! o =
      Onthefly.load_from_filename t.tar ~filename:protocol_tbl_filename
    in
    match o with
    | Some str -> (
        let* res =
          let* is_legacy_format = Version.is_legacy_format t.version in
          if is_legacy_format then
            (* Use the legacy encoding *)
            match
              Data_encoding.Binary.of_string_opt
                Protocol_levels.Legacy.encoding
                str
            with
            | Some table ->
                let* res =
                  Protocol_levels.Legacy.fold_es
                    (fun proto_level activation_block map ->
                      let protocol_info =
                        {
                          Protocol_levels.protocol =
                            activation_block.Protocol_levels.Legacy.protocol;
                          activation_block = activation_block.block;
                          (* Only snapshot with legacy semantics will have
                             legacy encoding. *)
                          expect_predecessor_context = false;
                        }
                      in
                      return (Protocol_levels.add proto_level protocol_info map))
                    table
                    Protocol_levels.empty
                in
                return_some res
            | None -> return_none
          else
            Data_encoding.Binary.of_string_opt Protocol_levels.encoding str
            |> return
        in
        match res with
        | Some v -> return v
        | None ->
            tzfail
              (Cannot_read
                 {kind = `Protocol_table; path = protocol_tbl_filename}))
    | None ->
        tzfail
          (Cannot_read {kind = `Protocol_table; path = protocol_tbl_filename})

  let load_and_validate_protocol_filenames t =
    let open Lwt_result_syntax in
    let protocol_tbl_filename =
      Naming.(snapshot_protocol_levels_file t.snapshot_tar |> encoded_file_path)
    in
    let*! protocol_dir_files =
      Onthefly.find_files_with_common_path
        t.tar
        ~pattern:Naming.(protocol_store_dir t.snapshot_tar |> dir_path)
    in
    let protocol_files =
      List.fold_left
        (fun acc file ->
          let filename = Filename.basename (Onthefly.get_filename file) in
          if filename <> protocol_tbl_filename then filename :: acc else acc)
        []
        protocol_dir_files
    in
    List.map_es
      (fun file ->
        match Protocol_hash.of_b58check_opt file with
        | Some ph -> return ph
        | None -> tzfail (Invalid_protocol_file file))
      protocol_files

  let copy_and_validate_protocol t ~protocol_hash =
    let open Lwt_result_syntax in
    let src =
      Filename.(
        concat
          Naming.(protocol_store_dir t.snapshot_tar |> dir_path)
          (Protocol_hash.to_b58check protocol_hash))
    in
    let* file =
      let*! o = Onthefly.get_file t.tar ~filename:src in
      match o with
      | Some file -> return file
      | None -> tzfail (Cannot_read {kind = `Protocol; path = src})
    in
    let dst =
      Filename.(
        concat
          (Naming.dir_path t.dst_protocol_dir)
          (Protocol_hash.to_b58check protocol_hash))
    in
    let*! () = Onthefly.copy_to_file t.tar file ~dst in
    let*! protocol_sources = Lwt_utils_unix.read_file dst in
    match Protocol.of_string protocol_sources with
    | None -> tzfail (Cannot_decode_protocol protocol_hash)
    | Some p ->
        let hash = Protocol.hash p in
        fail_unless
          (Protocol_hash.equal protocol_hash hash)
          (Inconsistent_protocol_hash {expected = protocol_hash; got = hash})

  let restore_cemented_indexes t =
    let open Lwt_syntax in
    let* cbl =
      Onthefly.find_files_with_common_path
        t.tar
        ~pattern:
          Naming.(
            cemented_blocks_level_index_dir t.snapshot_cemented_blocks_dir
            |> dir_path)
    in
    let* cbh =
      Onthefly.find_files_with_common_path
        t.tar
        ~pattern:
          Naming.(
            cemented_blocks_hash_index_dir t.snapshot_cemented_blocks_dir
            |> dir_path)
    in
    let cemented_indexes_paths = cbl @ cbh in
    if cemented_indexes_paths <> [] then
      let level_index_dir =
        Naming.(cemented_blocks_level_index_dir t.dst_cemented_dir |> dir_path)
      in
      let hash_index_dir =
        Naming.(cemented_blocks_hash_index_dir t.dst_cemented_dir |> dir_path)
      in
      let* () = Lwt_unix.mkdir level_index_dir snapshot_dir_perm in
      let* () = Lwt_unix.mkdir hash_index_dir snapshot_dir_perm in
      let* () =
        Lwt_unix.mkdir
          Filename.(concat level_index_dir "index")
          snapshot_dir_perm
      in
      let* () =
        Lwt_unix.mkdir
          Filename.(concat hash_index_dir "index")
          snapshot_dir_perm
      in
      List.iter_s
        (fun file ->
          Onthefly.copy_to_file
            t.tar
            file
            ~dst:
              (Filename.concat
                 (Naming.dir_path t.dst_chain_dir)
                 (Onthefly.get_filename file)))
        cemented_indexes_paths
    else Lwt.return_unit

  let load_cemented_files t =
    let open Lwt_syntax in
    let* cemented_files =
      Onthefly.find_files_with_common_path t.tar ~pattern:"\\d+_\\d+"
    in
    return_ok
      (List.map
         (fun file -> Filename.basename (Onthefly.get_filename file))
         cemented_files)

  let restore_cemented_cycle t ~file =
    let open Lwt_result_syntax in
    let filename =
      Filename.(
        concat Naming.(cemented_blocks_dir t.snapshot_tar |> dir_path) file)
    in
    let* tar_file =
      let*! o = Onthefly.get_file t.tar ~filename in
      match o with
      | Some file -> return file
      | None -> tzfail (Cannot_read {kind = `Cemented_cycle; path = filename})
    in
    let*! () =
      Onthefly.copy_to_file
        t.tar
        tar_file
        ~dst:
          (Filename.concat
             (Naming.dir_path t.dst_cemented_dir)
             (Filename.basename file))
    in
    return_unit

  let restore_floating_blocks t genesis_hash =
    let open Lwt_result_syntax in
    let*! o =
      Onthefly.get_file
        t.tar
        ~filename:
          Naming.(snapshot_floating_blocks_file t.snapshot_tar |> file_path)
    in
    match o with
    | Some floating_blocks_file ->
        let file_size = Onthefly.get_file_size floating_blocks_file in
        let floating_blocks_file_fd = Onthefly.get_raw_input_fd t.tar in
        let stream, bounded_push = Lwt_stream.create_bounded 1000 in
        let rec loop ?pred_block nb_bytes_left =
          if nb_bytes_left < 0L then tzfail Corrupted_floating_store
          else if nb_bytes_left = 0L then return_unit
          else
            let*! block, len_read =
              Block_repr_unix.read_next_block_exn floating_blocks_file_fd
            in
            let* () =
              Block_repr.check_block_consistency ~genesis_hash ?pred_block block
            in
            let*! () = bounded_push#push block in
            loop Int64.(sub nb_bytes_left (of_int len_read))
        in
        let reading_thread =
          Lwt.finalize
            (fun () ->
              let raw_data_ofs =
                Onthefly.get_raw_file_ofs floating_blocks_file
              in
              let*! _ =
                Lwt_unix.LargeFile.lseek
                  floating_blocks_file_fd
                  raw_data_ofs
                  Unix.SEEK_SET
              in
              loop file_size)
            (fun () ->
              bounded_push#close ;
              Lwt.return_unit)
        in
        return (reading_thread, stream)
    | None -> return (return_unit, Lwt_stream.of_list [])

  let close t = Onthefly.close_in t.tar
end

module type Snapshot_importer = sig
  type t

  val import :
    snapshot_path:string ->
    ?patch_context:
      (Tezos_protocol_environment.Context.t ->
      Tezos_protocol_environment.Context.t tzresult Lwt.t) ->
    ?block:Block_hash.t ->
    ?check_consistency:bool ->
    dst_store_dir:[`Store_dir] Naming.directory ->
    dst_context_dir:string ->
    chain_name:Distributed_db_version.Name.t ->
    configured_history_mode:History_mode.t option ->
    user_activated_upgrades:User_activated.upgrades ->
    user_activated_protocol_overrides:User_activated.protocol_overrides ->
    operation_metadata_size_limit:Shell_limits.operation_metadata_size_limit ->
    progress_display_mode:Animation.progress_display_mode ->
    Genesis.t ->
    (unit, error trace) result Lwt.t
end

module Make_snapshot_importer (Importer : IMPORTER) : Snapshot_importer = struct
  type t = Importer.t

  let init = Importer.init

  let close = Importer.close

  let restore_cemented_blocks ?(check_consistency = true) ~dst_chain_dir
      ~genesis_hash ~progress_display_mode snapshot_importer =
    let open Lwt_result_syntax in
    let*! () = Event.(emit restoring_cemented_indexes) () in
    let*! () = Importer.restore_cemented_indexes snapshot_importer in
    let* cemented_files = Importer.load_cemented_files snapshot_importer in
    let*! () = Event.(emit restoring_cemented_cycles) () in
    let nb_cemented_files = List.length cemented_files in
    let* () =
      if nb_cemented_files > 0 then
        Animation.display_progress
          ~pp_print_step:(fun fmt i ->
            Format.fprintf
              fmt
              "Copying cycles: %d/%d (%d%%)"
              i
              nb_cemented_files
              (100 * i / nb_cemented_files))
          ~progress_display_mode
          (fun notify ->
            List.iter_es
              (fun file ->
                let* () =
                  Importer.restore_cemented_cycle snapshot_importer ~file
                in
                let*! () = notify () in
                return_unit)
              cemented_files)
      else return_unit
    in
    let*! () = Event.(emit cemented_cycles_restored) () in
    let* cemented_store =
      Cemented_block_store.init
        ~log_size:cemented_import_log_size
        ~readonly:false
        dst_chain_dir
    in
    let*! () = Event.(emit checking_cycles_consistency) () in
    let* () =
      if check_consistency && nb_cemented_files > 0 then
        match Cemented_block_store.cemented_blocks_files cemented_store with
        | None -> failwith "unexpected empty set of cemented files"
        | Some stored_cemented_files ->
            let* () =
              List.iter_es
                (fun cemented_file ->
                  if
                    not
                      (Array.exists
                         (fun {Cemented_block_store.file; _} ->
                           Compare.String.equal
                             (Naming.file_path file |> Filename.basename)
                             cemented_file)
                         stored_cemented_files)
                  then tzfail (Missing_cemented_file cemented_file)
                  else return_unit)
                (List.sort compare cemented_files)
            in
            Animation.display_progress
              ~pp_print_step:(fun fmt i ->
                Format.fprintf
                  fmt
                  "Restoring cycles consistency: %d/%d (%d%%)"
                  i
                  nb_cemented_files
                  (100 * i / nb_cemented_files))
              ~progress_display_mode
              (fun notify ->
                Cemented_block_store.check_indexes_consistency
                  ~post_step:notify
                  ~genesis_hash
                  cemented_store)
      else return_unit
    in
    Cemented_block_store.close cemented_store ;
    let*! () = Event.(emit cycles_consistency_checked) () in
    return_unit

  let read_floating_blocks snapshot_importer ~genesis_hash =
    Importer.restore_floating_blocks snapshot_importer genesis_hash

  let restore_protocols snapshot_importer progress_display_mode =
    let open Lwt_result_syntax in
    let*! () = Event.(emit restoring_protocols) () in
    (* Import protocol table *)
    let* protocol_levels = Importer.load_protocol_table snapshot_importer in
    (* Retrieve protocol files *)
    let* protocols =
      Importer.load_and_validate_protocol_filenames snapshot_importer
    in
    let* () =
      Animation.display_progress
        ~pp_print_step:(fun fmt i ->
          Format.fprintf
            fmt
            "Copying protocols: %d/%d"
            i
            (List.length protocols))
        ~progress_display_mode
        (fun notify ->
          let validate_and_copy protocol_hash =
            let* () =
              Importer.copy_and_validate_protocol
                snapshot_importer
                ~protocol_hash
            in
            let*! () = notify () in
            return_unit
          in
          List.iter_es validate_and_copy protocols)
    in
    let*! () = Event.(emit protocols_restored) () in
    return protocol_levels

  let import_log_notice ~snapshot_version ~snapshot_metadata filename block =
    let open Lwt_syntax in
    let header =
      Format.asprintf
        "%a (snapshot version %d)"
        Snapshot_metadata.pp
        snapshot_metadata
        snapshot_version
    in
    let* () = Event.(emit import_info (filename, header)) in
    let* () =
      match block with
      | None -> Event.(emit import_unspecified_hash ())
      | Some _ -> Lwt.return_unit
    in
    Event.(emit import_loading ())

  let check_context_hash_consistency ~expected_context_hash validation_store =
    fail_unless
      (Context_hash.equal
         validation_store.Block_validation.resulting_context_hash
         expected_context_hash
      || (* This is needed as the former snapshot's version does not
            provide enough data to perform this check with the new
            context hash semantics (a block header contains the
            predecessor's context hash). In that particular case, we
            are not able to perform this validity check. *)
      Context_hash.equal expected_context_hash Context_hash.zero)
      (Inconsistent_context_hash
         {
           expected = expected_context_hash;
           got = validation_store.Block_validation.resulting_context_hash;
         })

  let apply_context context_index ~imported_context_hash chain_id ~block_header
      ~operations ~predecessor_header ~predecessor_block_metadata_hash
      ~predecessor_ops_metadata_hash ~user_activated_upgrades
      ~user_activated_protocol_overrides ~operation_metadata_size_limit =
    let open Lwt_result_syntax in
    let* predecessor_context =
      let*! o = Context.checkout context_index imported_context_hash in
      match o with
      | Some ch -> return ch
      | None -> tzfail (Inconsistent_context imported_context_hash)
    in
    let predecessor_context =
      Tezos_shell_context.Shell_context.wrap_disk_context predecessor_context
    in
    let apply_environment =
      {
        Block_validation.max_operations_ttl =
          Int32.to_int predecessor_header.Block_header.shell.level;
        chain_id;
        predecessor_block_header = predecessor_header;
        predecessor_context;
        predecessor_resulting_context_hash = imported_context_hash;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        operation_metadata_size_limit;
      }
    in
    let operations =
      List.map (List.map Block_validation.mk_operation) operations
    in
    let* {result = block_validation_result; _} =
      let*! r =
        Block_validation.apply
          apply_environment
          block_header
          operations
          ~cache:`Lazy
      in
      match r with
      | Ok block_validation_result -> return block_validation_result
      | Error errs ->
          Format.kasprintf
            (fun errs ->
              tzfail
                (Target_block_validation_failed
                   (Block_header.hash block_header, errs)))
            "%a"
            pp_print_trace
            errs
    in
    return block_validation_result

  let restore_and_apply_context snapshot_importer protocol_levels
      ?user_expected_block ~dst_context_dir ~user_activated_upgrades
      ~user_activated_protocol_overrides ~operation_metadata_size_limit
      ~progress_display_mode ~legacy ~patch_context ~check_consistency
      snapshot_metadata genesis chain_id =
    let open Lwt_result_syntax in
    let* ({
            block_header;
            resulting_context_hash;
            operations;
            predecessor_header;
            predecessor_block_metadata_hash;
            predecessor_ops_metadata_hash;
          } as block_data) =
      Importer.load_block_data snapshot_importer
    in
    (* Checks that the block hash imported from the snapshot is the one
       expected by the user's --block command line option *)
    let block_header_hash = Block_header.hash block_header in
    let* () =
      match user_expected_block with
      | Some bh ->
          fail_unless
            (Block_hash.equal bh block_header_hash)
            (Inconsistent_imported_block (block_header_hash, bh))
      | None -> return_unit
    in
    (* Checks that the block hash read from the snapshot metadata is the
       expected one *)
    let* () =
      let block_hash = Snapshot_metadata.get_block_hash snapshot_metadata in
      fail_unless
        (Block_hash.equal block_hash block_header_hash)
        (Inconsistent_imported_block (block_header_hash, block_hash))
    in
    let imported_context_hash =
      match
        Protocol_levels.find
          predecessor_header.shell.proto_level
          protocol_levels
      with
      | None -> Stdlib.failwith "unknown protocol"
      | Some {Protocol_levels.expect_predecessor_context; _} ->
          if expect_predecessor_context then
            block_header.Block_header.shell.context
          else predecessor_header.Block_header.shell.context
    in
    let* genesis_ctxt_hash, block_validation_result =
      if legacy then
        let*! context_index =
          Context.init
            ~readonly:false
            ~index_log_size:default_index_log_size
            ?patch_context
            dst_context_dir
        in
        let* genesis_ctxt_hash =
          Context.commit_genesis
            context_index
            ~chain_id
            ~time:genesis.Genesis.time
            ~protocol:genesis.protocol
        in
        let* context_elements =
          match snapshot_metadata with
          | Current _ ->
              tzfail (Cannot_read {kind = `Metadata; path = "snapshot's file"})
          | Legacy metadata -> return metadata.context_elements
        in
        let*! () = Event.(emit restoring_context) () in
        let* () =
          Importer.legacy_restore_context
            snapshot_importer
            context_index
            ~expected_context_hash:imported_context_hash
            ~nb_context_elements:context_elements
            ~progress_display_mode
        in
        let*! () = Event.(emit context_restored) () in
        let*! () = Event.(emit applying_target_block) () in
        let* block_validation_result =
          apply_context
            context_index
            ~imported_context_hash
            chain_id
            ~block_header
            ~operations
            ~predecessor_header
            ~predecessor_block_metadata_hash
            ~predecessor_ops_metadata_hash
            ~user_activated_upgrades
            ~user_activated_protocol_overrides
            ~operation_metadata_size_limit
        in
        let*! () = Event.(emit target_block_applied) () in
        let*! () = Context.close context_index in
        return (genesis_ctxt_hash, block_validation_result)
      else
        let* () =
          Animation.three_dots
            ~progress_display_mode:Auto
            ~msg:"Importing context"
          @@ fun () ->
          Importer.restore_context snapshot_importer ~dst_context_dir
        in
        let*! context_index =
          Context.init
            ~readonly:false
            ~index_log_size:default_index_log_size
            ?patch_context
            dst_context_dir
        in
        let* genesis_ctxt_hash =
          Context.commit_genesis
            context_index
            ~chain_id
            ~time:genesis.Genesis.time
            ~protocol:genesis.protocol
        in
        let*! () =
          if check_consistency then
            Animation.three_dots
              ~progress_display_mode:Auto
              ~msg:"Checking context integrity"
            @@ fun () ->
            Context.Checks.Pack.Integrity_check.run
              ?ppf:None
              ~root:dst_context_dir
              ~auto_repair:false
              ~always:false
              ~heads:(Some [Context_hash.to_b58check imported_context_hash])
              ()
          else Lwt.return_unit
        in
        let* block_validation_result =
          apply_context
            context_index
            ~imported_context_hash
            chain_id
            ~block_header
            ~operations
            ~predecessor_header
            ~predecessor_block_metadata_hash
            ~predecessor_ops_metadata_hash
            ~user_activated_upgrades
            ~user_activated_protocol_overrides
            ~operation_metadata_size_limit
        in
        let*! () = Context.close context_index in
        return (genesis_ctxt_hash, block_validation_result)
    in
    let* () =
      check_context_hash_consistency
        ~expected_context_hash:resulting_context_hash
        block_validation_result.validation_store
    in
    return (block_data, genesis_ctxt_hash, block_validation_result)

  (* TODO parallelise in another process *)
  (* TODO? remove patch context *)
  let import ~snapshot_path ?patch_context ?block:user_expected_block
      ?(check_consistency = true) ~dst_store_dir ~dst_context_dir ~chain_name
      ~configured_history_mode ~user_activated_upgrades
      ~user_activated_protocol_overrides ~operation_metadata_size_limit
      ~progress_display_mode (genesis : Genesis.t) =
    let open Lwt_result_syntax in
    let chain_id = Chain_id.of_block_hash genesis.Genesis.block in
    let* snapshot_importer = init ~snapshot_path ~dst_store_dir chain_id in
    let dst_store_dir = Naming.dir_path dst_store_dir in
    let* () =
      fail_when
        (Sys.file_exists dst_store_dir)
        (Directory_already_exists dst_store_dir)
    in
    let dst_store_dir = Naming.store_dir ~dir_path:dst_store_dir in
    let dst_protocol_dir = Naming.protocol_store_dir dst_store_dir in
    let chain_id = Chain_id.of_block_hash genesis.block in
    let dst_chain_dir = Naming.chain_dir dst_store_dir chain_id in
    let dst_cemented_dir = Naming.cemented_blocks_dir dst_chain_dir in
    (* Create directories *)
    let*! () =
      List.iter_s
        (Lwt_utils_unix.create_dir ~perm:snapshot_dir_perm)
        [
          Naming.dir_path dst_store_dir;
          Naming.dir_path dst_protocol_dir;
          Naming.dir_path dst_chain_dir;
          Naming.dir_path dst_cemented_dir;
        ]
    in
    let* () =
      fail_unless
        (Sys.file_exists snapshot_path)
        (Snapshot_file_not_found snapshot_path)
    in
    let snapshot_version = Importer.snapshot_version snapshot_importer in
    let snapshot_metadata = Importer.snapshot_metadata snapshot_importer in
    let* () =
      fail_unless
        (Version.is_supported snapshot_version)
        (Inconsistent_version_import
           {
             expected = List.map fst Version.supported_versions;
             got = snapshot_version;
           })
    in
    let*! () =
      if snapshot_version <= 6 && Importer.format = Tar then
        Event.(emit warn_tar_corruption snapshot_version)
      else Lwt.return_unit
    in
    let* () =
      let metadata_chain_name =
        Snapshot_metadata.get_chain_name snapshot_metadata
      in
      fail_unless
        (Distributed_db_version.Name.equal chain_name metadata_chain_name)
        (Inconsistent_chain_import
           {expected = metadata_chain_name; got = chain_name})
    in
    let* () =
      let history_mode = Snapshot_metadata.get_history_mode snapshot_metadata in
      match configured_history_mode with
      | Some stored ->
          let requested = history_mode in
          fail_unless
            (History_mode.mode_equality requested stored)
            (Inconsistent_history_mode_import {requested; stored})
      | None -> return_unit
    in
    let*! () =
      if not check_consistency then Event.(emit warn_no_check ())
      else Event.(emit suggest_no_check ())
    in
    let*! () =
      import_log_notice
        ~snapshot_version
        ~snapshot_metadata
        snapshot_path
        user_expected_block
    in
    let patch_context =
      Option.map
        (fun f ctxt ->
          let open Tezos_shell_context in
          let ctxt = Shell_context.wrap_disk_context ctxt in
          let+ ctxt = f ctxt in
          Shell_context.unwrap_disk_context ctxt)
        patch_context
    in
    (* Restore protocols *)
    let* protocol_levels =
      restore_protocols snapshot_importer progress_display_mode
    in
    let* legacy = Version.is_legacy_format snapshot_version in
    (* Restore context *)
    let* block_data, genesis_context_hash, block_validation_result =
      restore_and_apply_context
        snapshot_importer
        protocol_levels
        ?user_expected_block
        ~dst_context_dir
        ~user_activated_upgrades
        ~user_activated_protocol_overrides
        ~operation_metadata_size_limit
        ~progress_display_mode
        ~legacy
        ~patch_context
        ~check_consistency
        snapshot_metadata
        genesis
        chain_id
    in
    (* Restore store *)
    (* Restore cemented dir *)
    let* () =
      restore_cemented_blocks
        snapshot_importer
        ~check_consistency
        ~dst_chain_dir
        ~genesis_hash:genesis.block
        ~progress_display_mode
    in
    let* reading_thread, floating_blocks_stream =
      read_floating_blocks snapshot_importer ~genesis_hash:genesis.block
    in
    let {
      Block_validation.validation_store;
      block_metadata;
      ops_metadata;
      shell_header_hash = _;
    } =
      block_validation_result
    in
    let contents =
      {
        Block_repr.header = block_data.block_header;
        operations = block_data.operations;
        block_metadata_hash = snd block_metadata;
        operations_metadata_hashes =
          (match ops_metadata with
          | Block_validation.No_metadata_hash _ -> None
          | Block_validation.Metadata_hash ops_metadata ->
              Some (List.map (List.map snd) ops_metadata));
      }
    in
    let metadata =
      Some
        ({
           message = validation_store.message;
           max_operations_ttl = validation_store.max_operations_ttl;
           last_preserved_block_level =
             validation_store.last_preserved_block_level;
           block_metadata = fst block_metadata;
           operations_metadata =
             (match ops_metadata with
             | Block_validation.No_metadata_hash x -> x
             | Block_validation.Metadata_hash ops_metadata ->
                 List.map (List.map fst) ops_metadata);
         }
          : Block_repr.metadata)
    in
    let new_head_with_metadata =
      ({hash = Block_header.hash block_data.block_header; contents; metadata}
        : Block_repr.block)
    in
    (* Set the history mode with the default additional cycle
       offset. This is necessary as the snapshot content does not rely
       on a given offset. If the node was configured to run with the
       non default number of additional cycles, it will be
       automatically updated when running the node. *)
    let* history_mode =
      let open History_mode in
      match Snapshot_metadata.get_history_mode snapshot_metadata with
      | Archive -> assert false
      | Rolling _ -> return (Rolling None)
      | Full _ -> return (Full None)
    in
    let*! () = Event.(emit restoring_floating_blocks) () in
    let* () =
      Animation.display_progress
        ~every:100
        ~pp_print_step:(fun fmt i ->
          Format.fprintf fmt "Storing floating blocks: %d blocks written" i)
        ~progress_display_mode
        (fun notify ->
          Store.Unsafe.restore_from_snapshot
            ~notify
            dst_store_dir
            ~genesis
            ~genesis_context_hash
            ~floating_blocks_stream
            ~new_head_with_metadata
            ~new_head_resulting_context_hash:
              validation_store.resulting_context_hash
            ~predecessor_header:block_data.predecessor_header
            ~protocol_levels
            ~history_mode)
    in
    let* () = reading_thread in
    let*! () = Event.(emit floating_blocks_restored) () in
    let*! () = close snapshot_importer in
    let*! () = Event.(emit import_success snapshot_path) in
    return_unit
end

(* [snapshot_file_kind ~snapshot_path] returns the kind of a
   snapshot. We assume that a snapshot is valid if the medata can be
   read. *)
let snapshot_file_kind ~snapshot_path =
  let open Lwt_result_syntax in
  let is_valid_uncompressed_snapshot file =
    let (module Loader) =
      (module Make_snapshot_loader (Tar_loader) : Snapshot_loader)
    in
    Error_monad.catch_es (fun () ->
        let* _header =
          Loader.load_snapshot_header ~snapshot_path:(Naming.file_path file)
        in
        return_unit)
  in
  let is_valid_raw_snapshot snapshot_dir =
    let (module Loader) =
      (module Make_snapshot_loader (Raw_loader) : Snapshot_loader)
    in
    Error_monad.catch_es (fun () ->
        let* _header =
          Loader.load_snapshot_header
            ~snapshot_path:(Naming.dir_path snapshot_dir)
        in
        return_unit)
  in
  protect (fun () ->
      let*! is_dir = Lwt_utils_unix.is_directory snapshot_path in
      if is_dir then
        let snapshot_dir = Naming.snapshot_dir ~snapshot_path () in
        let* () = is_valid_raw_snapshot snapshot_dir in
        return Raw
      else
        let snapshot_file =
          Naming.snapshot_file
            ~snapshot_filename:(Filename.basename snapshot_path)
            Naming.(
              snapshot_dir ~snapshot_path:(Filename.dirname snapshot_path) ())
        in
        let* () = is_valid_uncompressed_snapshot snapshot_file in
        return Tar)

let export ?snapshot_path export_format ?rolling ~block ~store_dir ~context_dir
    ~chain_name ~progress_display_mode genesis =
  let (module Exporter) =
    match export_format with
    | Tar -> (module Make_snapshot_exporter (Tar_exporter) : Snapshot_exporter)
    | Raw -> (module Make_snapshot_exporter (Raw_exporter) : Snapshot_exporter)
  in
  Exporter.export
    ?snapshot_path
    ?rolling
    ~block
    ~store_dir
    ~context_dir
    ~chain_name
    ~progress_display_mode
    genesis

let read_snapshot_header ~snapshot_path =
  let open Lwt_result_syntax in
  let* kind = snapshot_file_kind ~snapshot_path in
  let (module Loader) =
    match kind with
    | Tar -> (module Make_snapshot_loader (Tar_loader) : Snapshot_loader)
    | Raw -> (module Make_snapshot_loader (Raw_loader) : Snapshot_loader)
  in
  Loader.load_snapshot_header ~snapshot_path

let import ~snapshot_path ?patch_context ?block ?check_consistency
    ~dst_store_dir ~dst_context_dir ~chain_name ~configured_history_mode
    ~user_activated_upgrades ~user_activated_protocol_overrides
    ~operation_metadata_size_limit ~progress_display_mode genesis =
  let open Lwt_result_syntax in
  let* kind = snapshot_file_kind ~snapshot_path in
  let (module Importer) =
    match kind with
    | Tar -> (module Make_snapshot_importer (Tar_importer) : Snapshot_importer)
    | Raw -> (module Make_snapshot_importer (Raw_importer) : Snapshot_importer)
  in
  let dst_store_dir = Naming.store_dir ~dir_path:dst_store_dir in
  Importer.import
    ~snapshot_path
    ?patch_context
    ?block
    ?check_consistency
    ~dst_store_dir
    ~dst_context_dir
    ~chain_name
    ~configured_history_mode
    ~user_activated_upgrades
    ~user_activated_protocol_overrides
    ~operation_metadata_size_limit
    ~progress_display_mode
    genesis
