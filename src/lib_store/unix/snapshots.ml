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
open Tezos_context_ops

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
  | Cannot_checkout_imported_context of Context_hash.t

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
      let get_chain_name_info chain_name =
        let chain_name =
          Format.asprintf "%a" Distributed_db_version.Name.pp chain_name
        in
        (* We assume the chain_name to be formatted as "TEZOS_<CHAIN-NAME>_<TIMESTAMP>". *)
        String.lowercase_ascii
        @@
        match String.split '_' chain_name with
        | _ :: chain_name :: _ -> chain_name
        | _ -> ""
      in
      let expected_chain_name = get_chain_name_info expected in
      let chain_name = get_chain_name_info got in
      Format.fprintf
        ppf
        "@[<v 2>The chain name contained in the snapshot file (%s) is not \
         consistent with the network configured in the targeted data directory \
         (%s). Please check your configuration file. If your configuration \
         file is not set, you should use the following command first:@,\
         'octez-node config init --network <NETWORK> --data-dir <DATA_DIR>'.@,\
         See documentation for help.@."
        expected_chain_name
        chain_name)
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
    (fun () -> Cannot_export_snapshot_format) ;
  register_error_kind
    `Permanent
    ~id:"Snapshot.cannot_checkout_imported_context"
    ~title:"Cannot checkout imported context"
    ~description:"Cannot checkout imported context"
    ~pp:(fun ppf h ->
      Format.fprintf
        ppf
        "Cannot checkout imported context %a. The imported data directory is \
         incorrect."
        Context_hash.pp
        h)
    (obj1 (req "context_hash" Context_hash.encoding))
    (function Cannot_checkout_imported_context h -> Some h | _ -> None)
    (fun h -> Cannot_checkout_imported_context h)

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
   * - 8: change cemented files offset format to 64 bits
   * - 9: export target predecessor contains metadata
   *)

  let v8_version = 8

  (* Used for old snapshot format versions *)
  let legacy_version = v8_version

  let current_version = 9

  (* List of versions that are supported *)
  let supported_versions =
    [(legacy_version, `Legacy_format); (current_version, `Current)]

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

(* The [cemented_buffer_size] is used to enhance performance while
   copying cemented cycles. This hardcoded value is designed to
   provide good performance across a wide range of architectures. *)
let cemented_buffer_size = 4096 * 1024

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

  type t = Current of metadata

  let pp ppf metadata =
    let chain_name, block_hash, level, history_mode, timestamp =
      match metadata with
      | Current {chain_name; block_hash; level; history_mode; timestamp} ->
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

  let get_block_hash = function Current {block_hash; _} -> block_hash

  let get_chain_name = function Current {chain_name; _} -> chain_name

  let get_history_mode = function Current {history_mode; _} -> history_mode

  let read_metadata ~metadata_file =
    let open Lwt_result_syntax in
    let read_json json = Data_encoding.Json.destruct metadata_encoding json in
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

  type t = Current of snapshot_header

  let pp ppf = function
    | Current (version, metadata) ->
        Format.fprintf
          ppf
          "%a (snapshot version %d)"
          Snapshot_metadata.pp
          (Snapshot_metadata.Current metadata)
          version

  let to_json = function
    | Current snapshot_header ->
        Data_encoding.Json.construct snapshot_header_encoding snapshot_header

  let get_version = function Current (version, _) -> version

  let get_metadata = function
    | Current (_, metadata) -> Snapshot_metadata.Current metadata
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

type block_data_legacy_v8 = {
  block_header : Block_header.t;
  operations : Operation.t list list;
  predecessor_header : Block_header.t;
  predecessor_block_metadata_hash : Block_metadata_hash.t option;
  predecessor_ops_metadata_hash : Operation_metadata_list_list_hash.t option;
  resulting_context_hash : Context_hash.t;
}

let block_data_legacy_v8_encoding =
  let open Data_encoding in
  conv
    (fun {
           block_header;
           operations;
           predecessor_header;
           predecessor_block_metadata_hash;
           predecessor_ops_metadata_hash;
           resulting_context_hash;
         }
       ->
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
           resulting_context_hash )
       ->
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

type block_data = {
  block_header : Block_header.t;
  operations : Operation.t list list;
  predecessor_header : Block_header.t;
  predecessor_max_operations_ttl : int;
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
           predecessor_max_operations_ttl;
           predecessor_block_metadata_hash;
           predecessor_ops_metadata_hash;
           resulting_context_hash;
         }
       ->
      ( operations,
        block_header,
        predecessor_header,
        predecessor_max_operations_ttl,
        predecessor_block_metadata_hash,
        predecessor_ops_metadata_hash,
        resulting_context_hash ))
    (fun ( operations,
           block_header,
           predecessor_header,
           predecessor_max_operations_ttl,
           predecessor_block_metadata_hash,
           predecessor_ops_metadata_hash,
           resulting_context_hash )
       ->
      {
        block_header;
        operations;
        predecessor_header;
        predecessor_max_operations_ttl;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
        resulting_context_hash;
      })
    (obj7
       (req "operations" (list (list (dynamic_size Operation.encoding))))
       (req "block_header" (dynamic_size Block_header.encoding))
       (req "predecessor_header" (dynamic_size Block_header.encoding))
       (req "predecessor_max_operations_ttl" int31)
       (opt "predecessor_block_metadata_hash" Block_metadata_hash.encoding)
       (opt
          "predecessor_ops_metadata_hash"
          Operation_metadata_list_list_hash.encoding)
       (req " resulting_context_hash" Context_hash.encoding))

let default_snapshot_filename (metadata : Snapshot_metadata.t) =
  let chain_name, block_hash, level, history_mode =
    match metadata with
    | Current {chain_name; block_hash; level; history_mode; _} ->
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
          (Cannot_remove_tmp_export_directory (Naming.dir_path snapshot_tmp_dir)))
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

module type EXPORTER = sig
  type t

  val init : string option -> t tzresult Lwt.t

  val write_block_data :
    t ->
    predecessor_header:Block_header.t ->
    predecessor_max_operations_ttl:int ->
    predecessor_block_metadata_hash:Block_metadata_hash.t option ->
    predecessor_ops_metadata_hash:Operation_metadata_list_list_hash.t option ->
    export_block:Store.Block.t ->
    resulting_context_hash:Context_hash.t ->
    unit Lwt.t

  val export_context :
    t -> Context_ops.index -> Context_hash.t -> unit tzresult Lwt.t

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

  let write_block_data t ~predecessor_header ~predecessor_max_operations_ttl
      ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash
      ~export_block ~resulting_context_hash =
    let open Lwt_syntax in
    let block_data =
      {
        block_header = Store.Block.header export_block;
        operations = Store.Block.operations export_block;
        predecessor_header;
        predecessor_max_operations_ttl;
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
      Context_ops.export_snapshot
        context_index
        context_hash
        ~path:tmp_context_path
    in
    return_unit

  let copy_cemented_block t ~file ~start_level ~end_level =
    let filename =
      Naming.(
        cemented_blocks_file t.snapshot_cemented_dir ~start_level ~end_level
        |> file_path)
    in
    Lwt_utils_unix.copy_file_raw
      ~buffer_size:cemented_buffer_size
      ~src:file
      ~dst:filename
      ()

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
    Lwt_utils_unix.copy_file ~src ~dst ()

  let write_metadata t (metadata : Snapshot_metadata.t) =
    let metadata_file =
      Naming.(snapshot_metadata_file t.snapshot_tmp_dir |> file_path)
    in
    let metadata_json =
      match metadata with
      | Current metadata ->
          Data_encoding.Json.(
            construct Snapshot_metadata.metadata_encoding metadata)
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
    tar : Octez_tar_helpers.o;
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
      Octez_tar_helpers.open_out ~file:(snapshot_tar_file |> Naming.file_path)
    in
    let version_file =
      Naming.snapshot_version_file snapshot_tmp_dir |> Naming.file_path
    in
    let version_json =
      Data_encoding.Json.construct Version.encoding Version.current_version
    in
    let* () = Lwt_utils_unix.Json.write_file version_file version_json in
    let*! () =
      Octez_tar_helpers.add_file_and_finalize
        tar
        ~file:version_file
        ~filename:(Filename.basename version_file)
        ~buffer_size:cemented_buffer_size
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

  let write_block_data t ~predecessor_header ~predecessor_max_operations_ttl
      ~predecessor_block_metadata_hash ~predecessor_ops_metadata_hash
      ~export_block ~resulting_context_hash =
    let block_data =
      {
        block_header = Store.Block.header export_block;
        operations = Store.Block.operations export_block;
        predecessor_header;
        predecessor_max_operations_ttl;
        predecessor_block_metadata_hash;
        predecessor_ops_metadata_hash;
        resulting_context_hash;
      }
    in
    let bytes =
      Data_encoding.Binary.to_bytes_exn block_data_encoding block_data
    in
    Octez_tar_helpers.add_raw_and_finalize
      t.tar
      ~f:(fun fd -> Lwt_utils_unix.write_bytes fd bytes)
      ~filename:Naming.(snapshot_block_data_file t.snapshot_tar |> file_path)

  let export_context t context_index context_hash =
    let open Lwt_result_syntax in
    let tmp_context_path =
      Naming.(snapshot_context_file t.snapshot_tmp_dir |> file_path)
    in
    let*! () =
      Context_ops.export_snapshot
        context_index
        context_hash
        ~path:tmp_context_path
    in
    let*! () =
      Octez_tar_helpers.add_directory_and_finalize
        ~archive_prefix:"" (* /context/ was already added *)
        t.tar
        ~dir_path:tmp_context_path
        ~buffer_size:cemented_buffer_size
    in
    let*! () = Lwt_utils_unix.remove_dir tmp_context_path in
    return_unit

  let copy_cemented_block t ~file ~start_level ~end_level =
    let cemented_filename =
      Naming.(
        cemented_blocks_file t.snapshot_cemented_dir ~start_level ~end_level
        |> file_path)
    in
    Octez_tar_helpers.add_file_and_finalize
      t.tar
      ~file
      ~filename:cemented_filename
      ~buffer_size:cemented_buffer_size

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
      Octez_tar_helpers.add_directory_and_finalize
        ~archive_prefix:(Naming.dir_path t.snapshot_cemented_dir)
        t.tar
        ~dir_path:
          Naming.(
            cemented_blocks_hash_index_dir t.snapshot_tmp_cemented_dir
            |> dir_path)
        ~buffer_size:cemented_buffer_size
    in
    Octez_tar_helpers.add_directory_and_finalize
      ~archive_prefix:(Naming.dir_path t.snapshot_cemented_dir)
      t.tar
      ~dir_path:
        Naming.(
          cemented_blocks_level_index_dir t.snapshot_tmp_cemented_dir
          |> dir_path)
      ~buffer_size:cemented_buffer_size

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
    Octez_tar_helpers.add_raw_and_finalize
      t.tar
      ~f
      ~filename:
        Naming.(snapshot_floating_blocks_file t.snapshot_tar |> file_path)

  let write_protocols_table t ~f =
    Octez_tar_helpers.add_raw_and_finalize
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
    Octez_tar_helpers.add_file_and_finalize
      t.tar
      ~file:src
      ~filename:dst
      ~buffer_size:cemented_buffer_size

  let write_metadata t metadata =
    let open Lwt_result_syntax in
    let metadata_json =
      match metadata with
      | Snapshot_metadata.Current metadata ->
          Data_encoding.Json.(
            construct Snapshot_metadata.metadata_encoding metadata)
    in
    let metadata_file =
      Naming.snapshot_metadata_file t.snapshot_tmp_dir |> Naming.file_path
    in
    let* () = Lwt_utils_unix.Json.write_file metadata_file metadata_json in
    let*! () =
      Octez_tar_helpers.add_file_and_finalize
        t.tar
        ~file:metadata_file
        ~filename:(Filename.basename metadata_file)
        ~buffer_size:cemented_buffer_size
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
    let*! () = Octez_tar_helpers.close_out t.tar in
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
    data_dir:string ->
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
      let export_pred_level = Int32.sub (Store.Block.level export_block) 1l in
      let f block =
        (* FIXME: we also write potential branches, it will eventually
           be GCed *)
        if Compare.Int32.(Block_repr.level block >= limit_level) then
          if Block_hash.equal limit_hash (Block_repr.hash block) then raise Done
          else return_unit
        else
          let block =
            (* Prune everything but the predecessor's metadata *)
            if Block_repr.level block = export_pred_level then block
            else {block with metadata = None}
          in
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
     - at least max_op_ttl(target_block) headers must be available for the
       predecessor of the target
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
    let* pred_block_metadata =
      let*! o = Store.Block.get_block_metadata_opt chain_store pred_block in
      match o with
      | None ->
          tzfail
            (Invalid_export_block {block = Some block_hash; reason = `Pruned})
      | Some block_metadata -> return block_metadata
    in
    let*! _, caboose_level = Store.Chain.caboose chain_store in
    (* We will need the following blocks
       [ (pred(target_block) - max_op_ttl(target_block)) ; ... ; pred(target_block) ] *)
    let block_max_op_ttl = Store.Block.max_operations_ttl pred_block_metadata in
    let*! genesis_block = Store.Chain.genesis_block chain_store in
    let genesis_level = Store.Block.level genesis_block in
    let minimum_level_needed =
      Compare.Int32.(
        max
          genesis_level
          Int32.(sub (sub block_level 1l) (of_int block_max_op_ttl)))
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

  let retrieve_floating_block_list chain_store ~from_block ~to_block =
    let open Lwt_result_syntax in
    (* Invariant: the list of blocks returned by the
       [Store.Chain_traversal.path] contains metadata for the block target and
       its predecessor as the metadata were read during the early steps of the
       snapshot export and are stored in the cache. Having metadata for theses
       blocks is mandatory. *)
    let*! o = Store.Chain_traversal.path chain_store ~from_block ~to_block in
    match o with
    | None -> tzfail Cannot_retrieve_block_interval
    | Some floating_blocks ->
        (* Don't forget to add the first block as
           [Chain_traversal.path] does not include the lower-bound
           block *)
        let floating_blocks = from_block :: floating_blocks in
        return_some floating_blocks

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
    | Some table_arr ->
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
            (* If the export block is the last block of the extra_cycle, we need
               to put the complete extra_cycle in the floating store to ensure
               block metadata. *)
            if Compare.Int32.(export_block_level = extra_cycle.end_level) then
              let block_store = Store.Unsafe.get_block_store chain_store in
              let cemented_store =
                Block_store.cemented_block_store block_store
              in
              let* from_block =
                let* res =
                  Cemented_block_store.get_cemented_block_by_level
                    ~read_metadata:true
                    cemented_store
                    extra_cycle.start_level
                in
                match res with
                | Some block -> Store.Unsafe.block_of_repr block |> return
                | None -> tzfail Cannot_retrieve_block_interval
              in
              let* to_block =
                let* res =
                  Cemented_block_store.get_cemented_block_by_level
                    ~read_metadata:true
                    cemented_store
                    extra_cycle.end_level
                in
                match res with
                | Some block -> Store.Unsafe.block_of_repr block |> return
                | None -> tzfail Cannot_retrieve_block_interval
              in
              let* floating_blocks =
                retrieve_floating_block_list chain_store ~from_block ~to_block
              in
              return (filtered_table, floating_blocks)
            else if Compare.Int32.(export_block_level = extra_cycle.start_level)
            then
              (* When the export level is set to the very first block of a
                 cycle, it's preceding block (that needs to be exported with
                 metadata) is stored in the preceding cycle -- without metadata.
                 To handle that, the two last cycles are stored in the floating
                 store of the exported snapshot. *)
              let block_store = Store.Unsafe.get_block_store chain_store in
              let cemented_store =
                Block_store.cemented_block_store block_store
              in
              let last_filtered_cycle, remaining_filtered_cycles =
                match List.rev filtered_table with
                | hd :: tl -> (hd, tl)
                | [] -> (* Invariant: the table is not empty here*) assert false
              in
              let* from_block =
                let* res =
                  Cemented_block_store.get_cemented_block_by_level
                    ~read_metadata:true
                    cemented_store
                    last_filtered_cycle.start_level
                in
                match res with
                | Some block -> Store.Unsafe.block_of_repr block |> return
                | None -> tzfail Cannot_retrieve_block_interval
              in
              let* to_block =
                let* res =
                  Cemented_block_store.get_cemented_block_by_level
                    ~read_metadata:true
                    cemented_store
                    export_block_level
                in
                match res with
                | Some block -> Store.Unsafe.block_of_repr block |> return
                | None -> tzfail Cannot_retrieve_block_interval
              in
              let* floating_blocks =
                retrieve_floating_block_list chain_store ~from_block ~to_block
              in
              return (remaining_filtered_cycles, floating_blocks)
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
              let* floating_blocks =
                retrieve_floating_block_list
                  chain_store
                  ~from_block:first_block
                  ~to_block:export_block
              in
              return (filtered_table, floating_blocks)

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

  let export_context snapshot_exporter ~data_dir context_hash =
    let open Lwt_result_syntax in
    let*! () = Event.(emit exporting_context) () in
    let* context_index =
      Context_ops.init ~kind:`Disk ~readonly:true ~data_dir ()
    in
    (* Is it the right test? *)
    let is_gc_allowed = Context_ops.is_gc_allowed context_index in
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
          (fun () -> Context_ops.close context_index)
    in
    let*! () = Event.(emit context_exported) () in
    return_unit

  let export_rolling snapshot_exporter ~store_dir ~data_dir ~block ~rolling
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
        let export_pred_level = Int32.sub (Store.Block.level export_block) 1l in
        Lwt_stream.of_list
          (List.filter_map
             (fun b ->
               if Store.Block.level b = export_pred_level then (
                 let b = Store.Unsafe.repr_of_block b in
                 (* Enforce metadata in predecessor's block. The invariant might
                    be broken in case of Store cache issue. *)
                 assert (Option.is_some (Block_repr.metadata b)) ;
                 Some b)
               else Some {(Store.Unsafe.repr_of_block b) with metadata = None})
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
      let* pred_block_metadata =
        Store.Block.get_block_metadata chain_store pred_block
      in
      let pred_max_operations_ttl =
        Store.Block.max_operations_ttl pred_block_metadata
      in
      let* () =
        export_context snapshot_exporter ~data_dir pred_resulting_context_hash
      in
      return
        ( export_mode,
          export_block,
          resulting_context_hash,
          pred_block,
          pred_max_operations_ttl,
          protocol_levels,
          (return_unit, floating_block_stream) )
    in
    let* ( export_mode,
           export_block,
           resulting_context_hash,
           pred_block,
           pred_max_operations_ttl,
           protocol_levels,
           (return_unit, floating_block_stream) ) =
      Store.Unsafe.open_for_snapshot_export
        ~store_dir
        ~data_dir
        genesis
        ~locked_f:export_rolling_f
    in
    return
      ( export_mode,
        export_block,
        resulting_context_hash,
        pred_block,
        pred_max_operations_ttl,
        protocol_levels,
        (return_unit, floating_block_stream) )

  let export_full snapshot_exporter ~store_dir ~data_dir ~block ~rolling
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
          let* pred_block_metadata =
            Store.Block.get_block_metadata chain_store pred_block
          in
          let pred_max_operations_ttl =
            Store.Block.max_operations_ttl pred_block_metadata
          in
          let* () =
            export_context
              snapshot_exporter
              ~data_dir
              pred_resulting_context_hash
          in
          return
            ( export_mode,
              export_block,
              resulting_context_hash,
              pred_block,
              pred_max_operations_ttl,
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
           pred_resulting_context,
           pred_block,
           pred_max_operations_ttl,
           protocol_levels,
           cemented_table,
           (floating_ro_fd, floating_rw_fd),
           extra_floating_blocks,
           should_filter_indexes ) =
      Store.Unsafe.open_for_snapshot_export
        ~store_dir
        ~data_dir
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
        pred_resulting_context,
        pred_block,
        pred_max_operations_ttl,
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

  let export ?snapshot_path ?(rolling = false) ~block ~store_dir ~data_dir
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
                 predecessor_max_operations_ttl,
                 protocol_levels,
                 (reading_thread, floating_block_stream) ) =
            if rolling then
              export_rolling
                snapshot_exporter
                ~store_dir
                ~data_dir
                ~block
                ~rolling
                genesis
            else
              export_full
                snapshot_exporter
                ~store_dir
                ~data_dir
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
              ~predecessor_max_operations_ttl
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

  let load_snapshot_header t =
    let open Lwt_result_syntax in
    let* version = load_snapshot_version t in
    let* metadata = load_snapshot_metadata t in
    return (Snapshot_header.Current (version, metadata))

  let close _ = Lwt.return_unit
end

module Tar_loader : LOADER = struct
  type t = {
    tar : Octez_tar_helpers.i;
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
    let* tar =
      Octez_tar_helpers.open_in ~file:(Naming.file_path snapshot_file)
    in
    Lwt.return {tar; snapshot_file; snapshot_tar}

  let load_snapshot_version t =
    let open Lwt_result_syntax in
    let filename = Naming.(snapshot_version_file t.snapshot_tar |> file_path) in
    let*! o =
      let*! o = Octez_tar_helpers.find_file t.tar ~filename in
      match o with
      | Some file -> (
          let*! str = Octez_tar_helpers.load_file t.tar file in
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
      let*! o = Octez_tar_helpers.find_file t.tar ~filename in
      match o with
      | Some file -> (
          let*! str = Octez_tar_helpers.load_file t.tar file in
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

  let load_snapshot_header t =
    let open Lwt_result_syntax in
    let* version = load_snapshot_version t in
    let* metadata = load_snapshot_metadata t in
    return (Snapshot_header.Current (version, metadata))

  let close t = Octez_tar_helpers.close_in t.tar
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

  val load_block_data : is_legacy_v8:bool -> t -> block_data tzresult Lwt.t

  val restore_context : t -> dst_data_dir:string -> unit tzresult Lwt.t

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

  let load_block_data ~is_legacy_v8 t =
    let open Lwt_result_syntax in
    let file = Naming.(snapshot_block_data_file t.snapshot_dir |> file_path) in
    let*! block_data = Lwt_utils_unix.read_file file in
    match Data_encoding.Binary.of_string_opt block_data_encoding block_data with
    | Some block_data -> return block_data
    | None -> (
        if is_legacy_v8 then
          let res =
            Data_encoding.Binary.of_string_opt
              block_data_legacy_v8_encoding
              block_data
          in
          match res with
          | Some
              {
                block_header;
                operations;
                predecessor_header;
                predecessor_block_metadata_hash;
                predecessor_ops_metadata_hash;
                resulting_context_hash;
              } ->
              return
                {
                  block_header;
                  operations;
                  predecessor_header;
                  predecessor_max_operations_ttl =
                    (* This is a rough approximation that is used for backward
                       compatibility only. *)
                    Int32.to_int block_header.Block_header.shell.level;
                  predecessor_block_metadata_hash;
                  predecessor_ops_metadata_hash;
                  resulting_context_hash;
                }
          | None -> tzfail (Cannot_read {kind = `Block_data; path = file})
        else
          let res =
            Data_encoding.Binary.of_string_opt block_data_encoding block_data
          in
          match res with
          | Some v -> return v
          | None -> tzfail (Cannot_read {kind = `Block_data; path = file}))

  let restore_context t ~dst_data_dir =
    let open Lwt_result_syntax in
    let context_file_path =
      Naming.(snapshot_context_file t.snapshot_dir |> file_path)
    in
    let*! () =
      Lwt_utils_unix.copy_dir
        context_file_path
        (Tezos_context_ops.Context_ops.context_dir dst_data_dir)
    in
    return_unit

  let load_protocol_table t =
    let open Lwt_result_syntax in
    let protocol_tbl_filename =
      Naming.(snapshot_protocol_levels_file t.snapshot_dir |> encoded_file_path)
    in
    let*! table_bytes = Lwt_utils_unix.read_file protocol_tbl_filename in
    let* res =
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
    let*! () = Lwt_utils_unix.copy_file ~src ~dst () in
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
    let open Lwt_result_syntax in
    let src = Filename.concat (Naming.dir_path t.snapshot_cemented_dir) file in
    let dst = Filename.concat (Naming.dir_path t.dst_cemented_dir) file in
    let*! () =
      Lwt_utils_unix.copy_file_raw
        ~buffer_size:cemented_buffer_size
        ~src
        ~dst
        ()
    in
    return_unit

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
    tar : Octez_tar_helpers.i;
    (* Store the files of the archive to avoid re-reading them *)
    files : Octez_tar_helpers.file list;
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
    let*! tar =
      Octez_tar_helpers.open_in ~file:(Naming.file_path snapshot_file)
    in
    let*! files = Octez_tar_helpers.list_files tar in
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

  let load_block_data ~is_legacy_v8 t =
    let open Lwt_result_syntax in
    let filename =
      Naming.(snapshot_block_data_file t.snapshot_tar |> file_path)
    in
    let*! o = Octez_tar_helpers.load_from_filename t.tar ~filename in
    match o with
    | Some block_data -> (
        if is_legacy_v8 then
          let res =
            Data_encoding.Binary.of_string_opt
              block_data_legacy_v8_encoding
              block_data
          in
          match res with
          | Some
              {
                block_header;
                operations;
                predecessor_header;
                predecessor_block_metadata_hash;
                predecessor_ops_metadata_hash;
                resulting_context_hash;
              } ->
              return
                {
                  block_header;
                  operations;
                  predecessor_header;
                  predecessor_max_operations_ttl =
                    (* This is a rough approximation that is used for backward
                       compatibility only. *)
                    Int32.to_int block_header.Block_header.shell.level;
                  predecessor_block_metadata_hash;
                  predecessor_ops_metadata_hash;
                  resulting_context_hash;
                }
          | None -> tzfail (Cannot_read {kind = `Block_data; path = filename})
        else
          let res =
            Data_encoding.Binary.of_string_opt block_data_encoding block_data
          in
          match res with
          | Some v -> return v
          | None -> tzfail (Cannot_read {kind = `Block_data; path = filename}))
    | None -> tzfail (Cannot_read {kind = `Block_data; path = filename})

  let restore_context t ~dst_data_dir =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_unix.mkdir
        (Tezos_context_ops.Context_ops.context_dir dst_data_dir)
        snapshot_dir_perm
    in
    let index =
      Filename.concat
        (Tezos_context_ops.Context_ops.context_dir dst_data_dir)
        "index"
    in
    let*! () = Lwt_unix.mkdir index snapshot_dir_perm in
    let*! context_files =
      Octez_tar_helpers.find_files_with_common_path t.tar ~pattern:"context"
    in
    let dst_dir = Tezos_context_ops.Context_ops.context_dir dst_data_dir in
    let*! () =
      List.iter_s
        (fun file ->
          let filename = Octez_tar_helpers.get_filename file in
          (* Remove context from the filename since we can
             restore a brassaia context and would want to
             store it in brassaia_context *)
          let dst =
            dst_dir
            ^ (String.remove_prefix ~prefix:"context" filename
              |> Option.value ~default:"")
          in
          Octez_tar_helpers.copy_to_file
            t.tar
            file
            ~dst
            ~buffer_size:cemented_buffer_size)
        context_files
    in
    return_unit

  let load_protocol_table t =
    let open Lwt_result_syntax in
    let protocol_tbl_filename =
      Naming.(snapshot_protocol_levels_file t.snapshot_tar |> encoded_file_path)
    in
    let*! o =
      Octez_tar_helpers.load_from_filename t.tar ~filename:protocol_tbl_filename
    in
    match o with
    | Some str -> (
        let* res =
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
      Octez_tar_helpers.find_files_with_common_path
        t.tar
        ~pattern:Naming.(protocol_store_dir t.snapshot_tar |> dir_path)
    in
    let protocol_files =
      List.fold_left
        (fun acc file ->
          let filename =
            Filename.basename (Octez_tar_helpers.get_filename file)
          in
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
      let*! o = Octez_tar_helpers.get_file t.tar ~filename:src in
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
    let*! () =
      Octez_tar_helpers.copy_to_file
        t.tar
        file
        ~dst
        ~buffer_size:cemented_buffer_size
    in
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
      Octez_tar_helpers.find_files_with_common_path
        t.tar
        ~pattern:
          Naming.(
            cemented_blocks_level_index_dir t.snapshot_cemented_blocks_dir
            |> dir_path)
    in
    let* cbh =
      Octez_tar_helpers.find_files_with_common_path
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
          Octez_tar_helpers.copy_to_file
            t.tar
            file
            ~dst:
              (Filename.concat
                 (Naming.dir_path t.dst_chain_dir)
                 (Octez_tar_helpers.get_filename file))
            ~buffer_size:cemented_buffer_size)
        cemented_indexes_paths
    else Lwt.return_unit

  let load_cemented_files t =
    let open Lwt_syntax in
    let* cemented_files =
      Octez_tar_helpers.find_files_with_common_path t.tar ~pattern:"\\d+_\\d+"
    in
    return_ok
      (List.map
         (fun file -> Filename.basename (Octez_tar_helpers.get_filename file))
         cemented_files)

  let restore_cemented_cycle t ~file =
    let open Lwt_result_syntax in
    let filename =
      Filename.(
        concat Naming.(cemented_blocks_dir t.snapshot_tar |> dir_path) file)
    in
    let* tar_file =
      let*! o = Octez_tar_helpers.get_file t.tar ~filename in
      match o with
      | Some file -> return file
      | None -> tzfail (Cannot_read {kind = `Cemented_cycle; path = filename})
    in
    let*! () =
      Octez_tar_helpers.copy_to_file
        t.tar
        tar_file
        ~dst:
          (Filename.concat
             (Naming.dir_path t.dst_cemented_dir)
             (Filename.basename file))
        ~buffer_size:cemented_buffer_size
    in
    return_unit

  let restore_floating_blocks t genesis_hash =
    let open Lwt_result_syntax in
    let*! o =
      Octez_tar_helpers.get_file
        t.tar
        ~filename:
          Naming.(snapshot_floating_blocks_file t.snapshot_tar |> file_path)
    in
    match o with
    | Some floating_blocks_file ->
        let file_size = Octez_tar_helpers.get_file_size floating_blocks_file in
        let floating_blocks_file_fd =
          Octez_tar_helpers.get_raw_input_fd t.tar
        in
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
                Octez_tar_helpers.get_raw_file_ofs floating_blocks_file
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

  let close t = Octez_tar_helpers.close_in t.tar
end

module type Snapshot_importer = sig
  type t

  val import :
    snapshot_path:string ->
    ?patch_context:
      (Tezos_context_ops.Context_ops.t ->
      Tezos_context_ops.Context_ops.t tzresult Lwt.t) ->
    ?block:Block_hash.t ->
    ?check_consistency:bool ->
    dst_store_dir:[`Store_dir] Naming.directory ->
    dst_data_dir:string ->
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
      ||
      (* This is needed as the former snapshot's version does not
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
      ~predecessor_ops_metadata_hash ~predecessor_max_operations_ttl
      ~user_activated_upgrades ~user_activated_protocol_overrides
      ~operation_metadata_size_limit =
    let open Lwt_result_syntax in
    let* predecessor_context =
      let*! o = Context_ops.checkout context_index imported_context_hash in
      match o with
      | Some ch -> return ch
      | None -> tzfail (Inconsistent_context imported_context_hash)
    in
    let max_operations_ttl = predecessor_max_operations_ttl in
    let apply_environment =
      {
        Block_validation.max_operations_ttl;
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
      ?user_expected_block ~dst_data_dir ~user_activated_upgrades
      ~user_activated_protocol_overrides ~operation_metadata_size_limit
      ~patch_context ~check_consistency ~is_legacy_v8 snapshot_metadata genesis
      chain_id =
    let open Lwt_result_syntax in
    let* ({
            block_header;
            resulting_context_hash;
            operations;
            predecessor_header;
            predecessor_max_operations_ttl;
            predecessor_block_metadata_hash;
            predecessor_ops_metadata_hash;
          } as block_data) =
      Importer.load_block_data ~is_legacy_v8 snapshot_importer
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
      let* () =
        Animation.three_dots
          ~progress_display_mode:Auto
          ~msg:"Importing context"
        @@ fun () -> Importer.restore_context snapshot_importer ~dst_data_dir
      in
      let*! () =
        if Context_ops.do_not_use__is_duo () then
          (* The way the context is imported make it so that only one context is created
             In duo mode this will always be the default one "context". We copy this directory
             in "brassaia_context" *)
          Lwt_utils_unix.copy_dir
            (* Copy the context directory in the brassaia_context directory *)
            (Tezos_context_ops.Context_ops.context_dir dst_data_dir)
            (Tezos_context_ops.Context_ops.do_not_use__brassaia_dir
               dst_data_dir)
        else Lwt.return_unit
      in
      let* context_index =
        Context_ops.init
          ~kind:`Disk
          ~readonly:false
          ~index_log_size:default_index_log_size
          ?patch_context
          ~data_dir:dst_data_dir
          ()
      in
      let* genesis_ctxt_hash =
        Context_ops.commit_genesis
          context_index
          ~chain_id
          ~time:genesis.Genesis.time
          ~protocol:genesis.protocol
      in
      let* () =
        (* As Irmin's integrity check is not actively checking that the
           requested context hash is well stored, we do it manually. *)
        let*! ctxt_opt =
          Context_ops.checkout context_index imported_context_hash
        in
        match ctxt_opt with
        | Some (_ : Context_ops.t) -> return_unit
        | None ->
            tzfail (Cannot_checkout_imported_context imported_context_hash)
      in
      let* () =
        if check_consistency then
          Animation.three_dots
            ~progress_display_mode:Auto
            ~msg:"Checking context integrity"
          @@ fun () ->
          let*! () =
            Context_ops.integrity_check
              ~ppf:Format.std_formatter
              ~root:dst_data_dir
              ~auto_repair:false
              ~always:false
              ~heads:(Some [Context_hash.to_b58check imported_context_hash])
              context_index
          in
          return_unit
        else return_unit
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
          ~predecessor_max_operations_ttl
          ~user_activated_upgrades
          ~user_activated_protocol_overrides
          ~operation_metadata_size_limit
      in
      let*! () = Context_ops.close context_index in
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
      ?(check_consistency = true) ~dst_store_dir ~dst_data_dir ~chain_name
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
    let* is_legacy_format = Version.is_legacy_format snapshot_version in
    let*! () =
      if is_legacy_format then
        Store_events.(emit import_legacy_snapshot_version snapshot_version)
      else Lwt.return_unit
    in
    (* TODO/FIXME: https://gitlab.com/tezos/tezos/-/issues/8005
       remove the v8 import backward compatibility as soon as v9
       (and v23) are mandatory.*)
    let is_v8_import =
      is_legacy_format && snapshot_version = Version.v8_version
    in
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
    let patch_context = Option.map (fun f ctxt -> f ctxt) patch_context in
    (* Restore protocols *)
    let* protocol_levels =
      restore_protocols snapshot_importer progress_display_mode
    in
    (* Restore context *)
    let* block_data, genesis_context_hash, block_validation_result =
      restore_and_apply_context
        snapshot_importer
        protocol_levels
        ?user_expected_block
        ~dst_data_dir
        ~user_activated_upgrades
        ~user_activated_protocol_overrides
        ~operation_metadata_size_limit
        ~patch_context
        ~check_consistency
        ~is_legacy_v8:is_v8_import
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
            ~history_mode
            ~is_v8_import)
    in
    let* () = reading_thread in
    let*! () = Event.(emit floating_blocks_restored) () in
    let*! () = close snapshot_importer in
    let*! () = Event.(emit import_success snapshot_path) in
    return_unit
end

(* [snapshot_file_kind ~snapshot_path] returns the kind of a snapshot. We assume
   that a snapshot is valid if the metadata can be read. *)
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

let export ?snapshot_path export_format ?rolling ~block ~store_dir ~data_dir
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
    ~data_dir
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
    ~dst_store_dir ~dst_data_dir ~chain_name ~configured_history_mode
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
    ~dst_data_dir
    ~chain_name
    ~configured_history_mode
    ~user_activated_upgrades
    ~user_activated_protocol_overrides
    ~operation_metadata_size_limit
    ~progress_display_mode
    genesis
