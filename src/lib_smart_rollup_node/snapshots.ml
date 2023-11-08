(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Snapshot_utils

let check_store_version store_dir =
  let open Lwt_result_syntax in
  let* store_version = Store_version.read_version_file ~dir:store_dir in
  let*? () =
    match store_version with
    | None -> error_with "Unversionned store, cannot produce snapshot."
    | Some v when v <> Store.version ->
        error_with
          "Incompatible store version %a, expected %a. Cannot produce \
           snapshot. Please restart your rollup node to migrate."
          Store_version.pp
          v
          Store_version.pp
          Store.version
    | Some _ -> Ok ()
  in
  return_unit

let check_head (store : _ Store.t) context =
  let open Lwt_result_syntax in
  let* head = Store.L2_head.read store.l2_head in
  let*? head =
    match head with
    | None ->
        error_with
          "There is no head in the rollup node store, cannot produce snapshot."
    | Some head -> Ok head
  in
  (* Ensure head context is available. *)
  let*! head_ctxt = Context.checkout context head.header.context in
  let*? () =
    error_when (Option.is_none head_ctxt)
    @@ error_of_fmt "Head context cannot be checkouted, won't produce snapshot."
  in
  return head

let pre_export_checks_and_get_snapshot_metadata ~data_dir =
  let open Lwt_result_syntax in
  let store_dir = Configuration.default_storage_dir data_dir in
  let context_dir = Configuration.default_context_dir data_dir in
  (* Load context and stores in read-only to check they are valid. *)
  let* () = check_store_version store_dir in
  let* metadata = Metadata.read_metadata_file ~dir:data_dir in
  let*? metadata =
    match metadata with
    | None -> error_with "No rollup node metadata in %S." data_dir
    | Some m -> Ok m
  in
  let*? () = Context.Version.check metadata.context_version in
  let* context = Context.load ~cache_size:1 Read_only context_dir in
  let* store =
    Store.load Read_only ~index_buffer_size:0 ~l2_blocks_cache_size:1 store_dir
  in
  let* history_mode = Store.History_mode.read store.history_mode in
  let*? history_mode =
    match history_mode with
    | None -> error_with "No history mode information in %S." data_dir
    | Some h -> Ok h
  in
  let* head = check_head store context in
  (* Closing context and stores after checks *)
  let*! () = Context.close context in
  let* () = Store.close store in
  return (history_mode, metadata.rollup_address, head.header.level)

let operator_local_file_regexp =
  Re.Str.regexp "^storage/\\(commitments_published_at_level.*\\|lpc$\\)"

let snapshotable_files_regexp =
  Re.Str.regexp
    "^\\(storage/.*\\|context/.*\\|wasm_2_0_0/.*\\|arith/.*\\|context/.*\\|metadata$\\)"

let export ~data_dir ~dest =
  let open Lwt_result_syntax in
  let* uncompressed_snapshot =
    Format.eprintf "Acquiring GC lock@." ;
    (* Take GC lock first in order to not prevent progression of rollup node. *)
    Utils.with_lockfile (Node_context.gc_lockfile_path ~data_dir) @@ fun () ->
    Format.eprintf "Acquiring process lock@." ;
    Utils.with_lockfile (Node_context.processing_lockfile_path ~data_dir)
    @@ fun () ->
    let* history_mode, address, head_level =
      pre_export_checks_and_get_snapshot_metadata ~data_dir
    in
    let dest_file_name =
      Format.asprintf
        "snapshot-%a-%ld.%s.uncompressed"
        Address.pp_short
        address
        head_level
        (Configuration.string_of_history_mode history_mode)
    in
    let dest_file =
      match dest with
      | Some dest -> Filename.concat dest dest_file_name
      | None -> dest_file_name
    in
    let*! () =
      let open Lwt_syntax in
      let* () = Option.iter_s Lwt_utils_unix.create_dir dest in
      let include_file ~relative_path =
        Re.Str.string_match snapshotable_files_regexp relative_path 0
        && not (Re.Str.string_match operator_local_file_regexp relative_path 0)
      in
      create
        stdlib_reader
        stdlib_writer
        ~dir:data_dir
        ~include_file
        ~dest:dest_file ;
      return_unit
    in
    return dest_file
  in
  let snapshot_file = compress ~snapshot_file:uncompressed_snapshot in
  return snapshot_file
