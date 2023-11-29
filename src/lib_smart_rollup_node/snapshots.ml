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

let get_head (store : _ Store.t) =
  let open Lwt_result_syntax in
  let* head = Store.L2_head.read store.l2_head in
  let*? head =
    match head with
    | None ->
        error_with
          "There is no head in the rollup node store, cannot produce snapshot."
    | Some head -> Ok head
  in
  return head

let check_head (head : Sc_rollup_block.t) context =
  let open Lwt_result_syntax in
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
  let* store =
    Store.load Read_only ~index_buffer_size:0 ~l2_blocks_cache_size:1 store_dir
  in

  let* head = get_head store in
  let level = head.Sc_rollup_block.header.level in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_level_with_store store level
  in
  let (module C) = Plugin.Pvm.context metadata.kind in
  let* context = Context.load (module C) ~cache_size:1 Read_only context_dir in
  let* history_mode = Store.History_mode.read store.history_mode in
  let*? history_mode =
    match history_mode with
    | None -> error_with "No history mode information in %S." data_dir
    | Some h -> Ok h
  in

  let* head = check_head head context in
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
      error_with "Could not read %s at %a after import." what Block_hash.pp hash

let check_block_data_and_get_content (store : _ Store.t) context hash =
  let open Lwt_result_syntax in
  let* b = Store.L2_blocks.read store.l2_blocks hash in
  let*? _b, header = check_some hash "L2 block" b in
  let* messages = Store.Messages.read store.messages header.inbox_witness in
  let*? _messages, _ = check_some hash "messages" messages in
  let* inbox = Store.Inboxes.read store.inboxes header.inbox_hash in
  let*? inbox, () = check_some hash "inbox" inbox in
  let* commitment =
    match header.commitment_hash with
    | None -> return_none
    | Some commitment_hash ->
        let* commitment =
          Store.Commitments.read store.commitments commitment_hash
        in
        let*? commitment, () = check_some hash "commitment" commitment in
        return_some commitment
  in
  (* Ensure head context is available. *)
  let*! head_ctxt = Context.checkout context header.context in
  let*? head_ctxt = check_some hash "context" head_ctxt in
  return (header, inbox, commitment, head_ctxt)

let check_block_data_consistency (metadata : Metadata.t) (store : _ Store.t)
    context hash =
  let open Lwt_result_syntax in
  let* header, inbox, commitment, head_ctxt =
    check_block_data_and_get_content store context hash
  in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_level_with_store store header.level
  in
  let*! pvm_state = Context.PVMState.find head_ctxt in
  let*? pvm_state = check_some hash "pvm_state" pvm_state in
  let*! state_hash = Plugin.Pvm.state_hash metadata.kind pvm_state in
  let* () =
    match (commitment, header.commitment_hash) with
    | None, None -> return_unit
    | Some _, None | None, Some _ ->
        (* The commitment is fetched from the header value *)
        assert false
    | Some commitment, Some commitment_hash ->
        let hash_of_commitment = Commitment.hash commitment in
        let*? () =
          error_unless Commitment.Hash.(hash_of_commitment = commitment_hash)
          @@ error_of_fmt
               "Erroneous commitment hash %a for level %ld instead of %a."
               Commitment.Hash.pp
               hash_of_commitment
               header.level
               Commitment.Hash.pp
               commitment_hash
        in
        let*? () =
          error_unless State_hash.(state_hash = commitment.compressed_state)
          @@ error_of_fmt
               "Erroneous state hash %a for level %ld instead of %a."
               State_hash.pp
               state_hash
               header.level
               State_hash.pp
               commitment.compressed_state
        in
        let*? () =
          error_unless (commitment.inbox_level = header.level)
          @@ error_of_fmt
               "Erroneous inbox level %ld in commitment instead of level %ld."
               commitment.inbox_level
               header.level
        in
        let*? () =
          if header.level = metadata.genesis_info.level then Ok ()
          else
            error_unless
              Commitment.Hash.(
                header.previous_commitment_hash = commitment.predecessor)
            @@ error_of_fmt
                 "Erroneous previous commitment hash %a for level %ld instead \
                  of %a."
                 Commitment.Hash.pp
                 header.previous_commitment_hash
                 header.level
                 Commitment.Hash.pp
                 commitment.predecessor
        in
        return_unit
  in
  let hash_of_inbox = Inbox.hash inbox in
  let*? () =
    error_unless Inbox.Hash.(hash_of_inbox = header.inbox_hash)
    @@ error_of_fmt
         "Erroneous inbox %a for level %ld instead of %a."
         Inbox.Hash.pp
         hash_of_inbox
         header.level
         Inbox.Hash.pp
         header.inbox_hash
  in
  return header

let check_block_data (store : _ Store.t) context hash =
  let open Lwt_result_syntax in
  let* header, _inbox, _commitment, _head_ctxt =
    check_block_data_and_get_content store context hash
  in
  return header

let check_l2_chain ~message ~data_dir (store : _ Store.t) context
    (head : Sc_rollup_block.t) check_block =
  let open Lwt_result_syntax in
  let* first_available_level = first_available_level ~data_dir store in
  let blocks_to_check =
    Int32.sub head.header.level first_available_level |> Int32.to_int |> succ
  in
  let progress_bar =
    Progress_bar.progress_bar
      ~counter:`Int
      ~message
      ~color:(Terminal.Color.rgb 3 252 132)
      blocks_to_check
  in
  Progress_bar.Lwt.with_reporter progress_bar @@ fun count_progress ->
  let rec check_chain hash =
    let* header = check_block store context hash in
    let*! () = count_progress 1 in
    if header.Sc_rollup_block.level <= first_available_level then return_unit
    else check_chain header.predecessor
  in
  check_chain head.header.block_hash

let check_last_commitment head snapshot_metadata =
  let last_snapshot_commitment =
    Sc_rollup_block.most_recent_commitment head.Sc_rollup_block.header
  in
  error_unless
    Commitment.Hash.(
      snapshot_metadata.last_commitment = last_snapshot_commitment)
  @@ error_of_fmt
       "Last commitment in snapshot is %a but should be %a."
       Commitment.Hash.pp
       last_snapshot_commitment
       Commitment.Hash.pp
       snapshot_metadata.last_commitment

let check_last_commitment_published cctxt snapshot_metadata =
  let open Lwt_result_syntax in
  Error.trace_lwt_result_with
    "Last commitment of snapshot is not published on L1."
  @@ let* {current_protocol; _} =
       Tezos_shell_services.Shell_services.Blocks.protocols
         cctxt
         ~block:(`Head 0)
         ()
     in
     let*? (module Plugin) =
       Protocol_plugins.proto_plugin_for_protocol current_protocol
     in
     let* (_commitment : Commitment.t) =
       Plugin.Layer1_helpers.get_commitment
         cctxt
         snapshot_metadata.address
         snapshot_metadata.last_commitment
     in
     return_unit

let post_checks ~action ~message snapshot_metadata ~dest =
  let open Lwt_result_syntax in
  let store_dir = Configuration.default_storage_dir dest in
  let context_dir = Configuration.default_context_dir dest in
  (* Load context and stores in read-only to run checks. *)
  let* () = check_store_version store_dir in
  let* store =
    Store.load
      Read_only
      ~index_buffer_size:1000
      ~l2_blocks_cache_size:100
      store_dir
  in
  let* head = get_head store in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_level_with_store store head.header.level
  in
  let* metadata = Metadata.read_metadata_file ~dir:dest in
  let*? metadata =
    match metadata with
    | None -> error_with "No rollup node metadata in %S." dest
    | Some m -> Ok m
  in
  let (module C) = Plugin.Pvm.context metadata.kind in
  let* context =
    Context.load (module C) ~cache_size:100 Read_only context_dir
  in
  let* head = check_head head context in
  let* check_block_data =
    match action with
    | `Export -> return check_block_data
    | `Import -> (
        let* metadata = Metadata.read_metadata_file ~dir:dest in
        match metadata with
        | None ->
            (* We need the kind of the rollup to run the consistency checks in
               order to verify state hashes. *)
            failwith "No metadata (needs rollup kind)."
        | Some metadata ->
            let*? () = check_last_commitment head snapshot_metadata in
            return (check_block_data_consistency metadata))
  in
  let* () =
    check_l2_chain ~message ~data_dir:dest store context head check_block_data
  in
  let*! () = Context.close context in
  let* () = Store.close store in
  return_unit

let post_export_checks ~snapshot_file =
  let open Lwt_result_syntax in
  Lwt_utils_unix.with_tempdir "snapshot_checks_" @@ fun dest ->
  let* snapshot_metadata =
    extract
      gzip_reader
      stdlib_writer
      (fun _ -> return_unit)
      ~snapshot_file
      ~dest
  in
  post_checks
    ~action:`Export
    ~message:"Checking snapshot   "
    snapshot_metadata
    ~dest

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

let pre_import_checks cctxt ~data_dir snapshot_metadata =
  let open Lwt_result_syntax in
  let store_dir = Configuration.default_storage_dir data_dir in
  (* Load stores in read-only to make simple checks. *)
  let* store =
    Store.load
      Read_write
      ~index_buffer_size:1000
      ~l2_blocks_cache_size:100
      store_dir
  in
  let* metadata = Metadata.read_metadata_file ~dir:data_dir
  and* history_mode = Store.History_mode.read store.history_mode
  and* head = Store.L2_head.read store.l2_head in
  let* () = Store.close store in
  let*? () =
    let open Result_syntax in
    match (metadata, history_mode) with
    | None, _ | _, None ->
        (* The rollup node data dir was never initialized, i.e. the rollup node
           wasn't run yet. *)
        return_unit
    | Some {rollup_address; _}, Some history_mode ->
        let* () =
          error_unless Address.(rollup_address = snapshot_metadata.address)
          @@ error_of_fmt
               "The existing rollup node is for %a, but the snapshot is for \
                rollup %a."
               Address.pp
               rollup_address
               Address.pp
               snapshot_metadata.address
        in
        let a_history_str = function
          | Configuration.Archive -> "an archive"
          | Configuration.Full -> "a full"
        in
        error_unless (history_mode = snapshot_metadata.history_mode)
        @@ error_of_fmt
             "Cannot import %s snapshot into %s rollup node."
             (a_history_str snapshot_metadata.history_mode)
             (a_history_str history_mode)
  in
  let*? () =
    let open Result_syntax in
    match head with
    | None ->
        (* The rollup node has no L2 chain. *)
        return_unit
    | Some head ->
        error_when (snapshot_metadata.head_level <= head.header.level)
        @@ error_of_fmt
             "The rollup node is already at level %ld but the snapshot is only \
              for level %ld."
             head.header.level
             snapshot_metadata.head_level
  in
  let* () = check_last_commitment_published cctxt snapshot_metadata in
  return_unit

let import cctxt ~data_dir ~snapshot_file =
  let open Lwt_result_syntax in
  let*! () = Lwt_utils_unix.create_dir data_dir in
  let*! () = Event.acquiring_lock () in
  Utils.with_lockfile
    ~when_locked:`Fail
    (Node_context.global_lockfile_path ~data_dir)
  @@ fun () ->
  let* snapshot_metadata =
    extract
      gzip_reader
      stdlib_writer
      (pre_import_checks cctxt ~data_dir)
      ~snapshot_file
      ~dest:data_dir
  in
  post_checks
    ~action:`Import
    ~message:"Checking imported data"
    snapshot_metadata
    ~dest:data_dir
