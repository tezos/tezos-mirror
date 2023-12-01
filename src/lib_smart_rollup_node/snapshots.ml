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
  return
    {
      history_mode;
      address = metadata.rollup_address;
      head_level = head.header.level;
      last_commitment = Sc_rollup_block.most_recent_commitment head.header;
    }

let first_available_level ~data_dir store =
  let open Lwt_result_syntax in
  let* gc_levels = Store.Gc_levels.read store.Store.gc_levels in
  match gc_levels with
  | Some {first_available_level; _} -> return first_available_level
  | None -> (
      let* metadata = Metadata.read_metadata_file ~dir:data_dir in
      match metadata with
      | None -> failwith "No metadata (needs rollup genesis info)."
      | Some {genesis_info = {level; _}; _} -> return level)

let check_some hash what = function
  | Some x -> Ok x
  | None ->
      error_with "Could not read %s at %a after export." what Block_hash.pp hash

let check_l2_chain ~data_dir (store : _ Store.t) context
    (head : Sc_rollup_block.t) =
  let open Lwt_result_syntax in
  let* first_available_level = first_available_level ~data_dir store in
  let rec check_block hash =
    let* b = Store.L2_blocks.read store.l2_blocks hash in
    let*? _b, header = check_some hash "L2 block" b in
    let* messages = Store.Messages.read store.messages header.inbox_witness in
    let*? _messages = check_some hash "messages" messages in
    let* inbox = Store.Inboxes.read store.inboxes header.inbox_hash in
    let*? _inbox = check_some hash "inbox" inbox in
    let* () =
      match header.commitment_hash with
      | None -> return_unit
      | Some commitment_hash ->
          let* commitment =
            Store.Commitments.read store.commitments commitment_hash
          in
          let*? _commitment = check_some hash "commitment" commitment in
          return_unit
    in
    (* Ensure head context is available. *)
    let*! head_ctxt = Context.checkout context header.context in
    let*? _head_ctxt = check_some hash "context" head_ctxt in
    if header.level <= first_available_level then return_unit
    else check_block header.predecessor
  in
  check_block head.header.block_hash

let post_import_checks ~dest =
  let open Lwt_result_syntax in
  let store_dir = Configuration.default_storage_dir dest in
  let context_dir = Configuration.default_context_dir dest in
  (* Load context and stores in read-only to run checks. *)
  let* () = check_store_version store_dir in
  let* context = Context.load ~cache_size:100 Read_only context_dir in
  let* store =
    Store.load
      Read_only
      ~index_buffer_size:1000
      ~l2_blocks_cache_size:100
      store_dir
  in
  let* head = check_head store context in
  let* () = check_l2_chain ~data_dir:dest store context head in
  let*! () = Context.close context in
  let* () = Store.close store in
  return_unit

let post_export_checks ~snapshot_file =
  Lwt_utils_unix.with_tempdir "snapshot_checks_" @@ fun dest ->
  extract gzip_reader stdlib_writer (fun _ -> ()) ~snapshot_file ~dest ;
  post_import_checks ~dest

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
    let* metadata = pre_export_checks_and_get_snapshot_metadata ~data_dir in
    let dest_file_name =
      Format.asprintf
        "snapshot-%a-%ld.%s.uncompressed"
        Address.pp_short
        metadata.address
        metadata.head_level
        (Configuration.string_of_history_mode metadata.history_mode)
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
        metadata
        ~dir:data_dir
        ~include_file
        ~dest:dest_file ;
      return_unit
    in
    return dest_file
  in
  let snapshot_file = compress ~snapshot_file:uncompressed_snapshot in
  let* () = post_export_checks ~snapshot_file in
  return snapshot_file
