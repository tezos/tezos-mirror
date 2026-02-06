(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Snapshot_utils

type compression = No | On_the_fly | After

module Header = struct
  type version = V0

  type t = {
    version : version;
    history_mode : Configuration.history_mode;
    address : Address.t;
    head_level : int32;
    last_commitment : Commitment.Hash.t;
  }

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"rollup_node.snapshot.header.v0"
          (Tag 0)
          (obj4
             (req "history_mode" Configuration.history_mode_encoding)
             (req "address" Address.encoding)
             (req "head_level" int32)
             (req "last_commitment" Commitment.Hash.encoding))
          (fun {
                 version = V0;
                 history_mode;
                 address;
                 head_level;
                 last_commitment;
               }
             -> Some (history_mode, address, head_level, last_commitment))
          (fun (history_mode, address, head_level, last_commitment) ->
            {version = V0; history_mode; address; head_level; last_commitment});
      ]
end

open Snapshot_utils.Make (Header)

let check_store_version store_dir =
  let open Lwt_result_syntax in
  let* store_version = Store_version.read_version_file ~dir:store_dir in
  let*? () =
    match store_version with
    | None -> Ok ()
    | Some v when v <> Store.version ->
        error_with
          "Incompatible store version %a, expected %a. Cannot produce snapshot."
          Store_version.pp
          v
          Store_version.pp
          Store.version
    | Some _ -> Ok ()
  in
  return_unit

let get_head (store : _ Store.t) =
  let open Lwt_result_syntax in
  let* head = Store.L2_blocks.find_head store in
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

let check_commitment_published cctxt address commitment =
  let open Lwt_result_syntax in
  Error.trace_lwt_result_with "Commitment of snapshot is not published on L1."
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
       Plugin.Layer1_helpers.get_commitment cctxt address commitment
     in
     return_unit

let pre_export_checks_and_get_snapshot_header cctxt ~no_checks ~data_dir =
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
  let* store = Store.init Read_only ~data_dir in
  let* head = get_head store in
  let level = head.Sc_rollup_block.header.level in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_level_with_store store level
  in
  let (module C) = Plugin.Pvm.context metadata.kind in
  let* context = Context.load (module C) ~cache_size:1 Read_only context_dir in
  let* history_mode = Store.State.History_mode.get store in
  let*? history_mode =
    match history_mode with
    | None -> error_with "No history mode information in %S." data_dir
    | Some h -> Ok h
  in
  let* head = check_head head context in
  let last_commitment_hash =
    Sc_rollup_block.most_recent_commitment head.header
  in
  let* () =
    unless no_checks @@ fun () ->
    let* last_commitment = Store.Commitments.find store last_commitment_hash in
    let last_commitment =
      WithExceptions.Option.get ~loc:__LOC__ last_commitment
    in
    (* Check if predecessor commitment exist on chain as a safety measure,
       because the very last one may not be included in a block yet. *)
    let pred_last_commitment = last_commitment.predecessor in
    check_commitment_published
      cctxt
      metadata.rollup_address
      pred_last_commitment
  in
  (* Closing context and stores after checks *)
  let*! () = Context.close context in
  let*! () = Store.close store in
  return
    {
      Header.version = V0;
      history_mode;
      address = metadata.rollup_address;
      head_level = head.header.level;
      last_commitment = last_commitment_hash;
    }

let first_available_level ~data_dir store =
  let open Lwt_result_syntax in
  let* first_available_level = Store.State.Last_gc_target.get store in
  match first_available_level with
  | Some first_available_level -> return first_available_level
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
  let* b = Store.L2_blocks.find store hash in
  let*? {header; _} = check_some hash "L2 block" b in
  let* messages = Store.Messages.find store header.inbox_witness in
  let*? _messages = check_some hash "messages" messages in
  let* inbox = Store.Inboxes.find store header.inbox_hash in
  let*? inbox = check_some hash "inbox" inbox in
  let* commitment =
    match header.commitment_hash with
    | None -> return_none
    | Some commitment_hash ->
        let* commitment = Store.Commitments.find store commitment_hash in
        let*? commitment = check_some hash "commitment" commitment in
        return_some commitment
  in
  (* Ensure head context is available. *)
  let*! head_ctxt = Context.checkout context header.context in
  let*? head_ctxt = check_some hash "context" head_ctxt in
  return (header, inbox, commitment, head_ctxt)

let get_pvm_state_from_store head_ctxt hash =
  let open Lwt_result_syntax in
  let*! pvm_state = Context.PVMState.find head_ctxt in
  let*? pvm_state = check_some hash "pvm_state" pvm_state in
  return pvm_state

let compute_pvm_state_for_genenis cctxt dest (store : _ Store.t) context
    (header : Sc_rollup_block.header) plugin ~apply_unsafe_patches =
  let open Lwt_result_syntax in
  let* current_protocol =
    Node_context.protocol_of_level_with_store store header.level
  in
  let (module Plugin : Protocol_plugin_sig.PARTIAL) = plugin in
  let* constants =
    Plugin.Layer1_helpers.retrieve_constants cctxt ~block:(`Level header.level)
  in
  let current_protocol =
    {
      Node_context.hash = current_protocol.protocol;
      proto_level = current_protocol.proto_level;
      constants;
    }
  in
  let* node_context =
    Node_context_loader.For_snapshots.create_node_context
      cctxt
      current_protocol
      store
      context
      ~data_dir:dest
      ~apply_unsafe_patches
  in
  Interpreter.genesis_state
    Both
    plugin
    node_context
    (Context.PVMState.empty context)

let check_genesis_pvm_state_and_return cctxt dest store context header
    (module Plugin : Protocol_plugin_sig.PARTIAL) (metadata : Metadata.t)
    head_ctxt hash ~apply_unsafe_patches =
  let open Lwt_result_syntax in
  let* (Both {patched = patched_pvm_state; original = pvm_state}) =
    compute_pvm_state_for_genenis
      cctxt
      dest
      store
      context
      header
      (module Plugin)
      ~apply_unsafe_patches
  in
  let* context_pvm_state = get_pvm_state_from_store head_ctxt hash in
  let*! context_state_hash =
    Plugin.Pvm.state_hash metadata.kind context_pvm_state
  in
  let*! patched_state_hash =
    Plugin.Pvm.state_hash metadata.kind patched_pvm_state
  in
  let*? () =
    error_unless State_hash.(context_state_hash = patched_state_hash)
    @@ error_of_fmt
         "Erroneous state hash %a for originated rollup (level %ld) instead of \
          %a. You might be missing unsafe pvm patches and/or the cli argument \
          to activate them, or the pvm state of the snapshot is corrupted for \
          the rollup genesis block."
         State_hash.pp
         context_state_hash
         header.level
         State_hash.pp
         patched_state_hash
  in
  return pvm_state

let check_block_data_consistency ~apply_unsafe_patches cctxt dest
    (metadata : Metadata.t) (store : _ Store.t) context hash next_commitment =
  let open Lwt_result_syntax in
  let* header, inbox, commitment, head_ctxt =
    check_block_data_and_get_content store context hash
  in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_level_with_store store header.level
  in
  let* pvm_state_of_commitment =
    if metadata.genesis_info.level = header.level then
      check_genesis_pvm_state_and_return
        ~apply_unsafe_patches
        cctxt
        dest
        store
        context
        header
        (module Plugin)
        metadata
        head_ctxt
        hash
    else get_pvm_state_from_store head_ctxt hash
  in
  let*! state_hash =
    Plugin.Pvm.state_hash metadata.kind pvm_state_of_commitment
  in
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
  let*? () =
    match (next_commitment, header.commitment_hash) with
    | None, _ | _, None ->
        (* If there is no commitment for this block there is no check to do. *)
        Ok ()
    | Some next_commitment, Some commitment_hash ->
        error_unless
          Commitment.Hash.(
            next_commitment.Commitment.predecessor = commitment_hash)
        @@ error_of_fmt
             "Commitment hash %a for level %ld was expected to be %a in the \
              chain of commitments."
             Commitment.Hash.pp
             commitment_hash
             header.level
             Commitment.Hash.pp
             next_commitment.predecessor
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
  return (header, commitment)

let check_block_data (store : _ Store.t) context hash _next_commitment =
  let open Lwt_result_syntax in
  let* header, _inbox, commitment, _head_ctxt =
    check_block_data_and_get_content store context hash
  in
  return (header, commitment)

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
  let rec check_chain hash next_commitment =
    let* header, commitment = check_block store context hash next_commitment in
    let*! () = count_progress 1 in
    if header.Sc_rollup_block.level <= first_available_level then return_unit
    else
      check_chain header.predecessor (Option.either commitment next_commitment)
  in
  check_chain head.header.block_hash None

let check_last_commitment head (snapshot_header : Header.t) =
  let last_snapshot_commitment =
    Sc_rollup_block.most_recent_commitment head.Sc_rollup_block.header
  in
  error_unless
    Commitment.Hash.(snapshot_header.last_commitment = last_snapshot_commitment)
  @@ error_of_fmt
       "Last commitment in snapshot is %a but should be %a."
       Commitment.Hash.pp
       last_snapshot_commitment
       Commitment.Hash.pp
       snapshot_header.last_commitment

let check_lcc metadata cctxt (store : _ Store.t) (head : Sc_rollup_block.t)
    (module Plugin : Protocol_plugin_sig.S) =
  let open Lwt_result_syntax in
  let* lcc =
    Plugin.Layer1_helpers.get_last_cemented_commitment
      cctxt
      metadata.Metadata.rollup_address
  in
  if lcc.level > head.header.level then
    (* The snapshot is older than the current LCC *)
    return_unit
  else
    let* lcc_block_hash = Store.L2_levels.find store lcc.level in
    let*? lcc_block_hash =
      match lcc_block_hash with
      | None -> error_with "No block for LCC level %ld" lcc.level
      | Some h -> Ok h
    in
    let* lcc_block = Store.L2_blocks.find store lcc_block_hash in
    match lcc_block with
    | None ->
        failwith
          "Unknown block %a for LCC level %ld"
          Block_hash.pp
          lcc_block_hash
          lcc.level
    | Some {header = {commitment_hash = None; _}; _} ->
        failwith
          "No commitment for block %a for LCC level %ld"
          Block_hash.pp
          lcc_block_hash
          lcc.level
    | Some {header = {commitment_hash = Some commitment_hash; _}; _} ->
        fail_unless Commitment.Hash.(lcc.commitment = commitment_hash)
        @@ error_of_fmt
             "Snapshot contains %a for LCC at level %ld but was expected to be \
              %a."
             Commitment.Hash.pp
             commitment_hash
             lcc.level
             Commitment.Hash.pp
             lcc.commitment

let hash_level_of_l2_block (b : Sc_rollup_block.t) =
  Layer1.{hash = b.header.block_hash; level = b.header.level}

let reconstruct_level_context ctxt ~predecessor (node_ctxt : _ Node_context.t)
    level =
  let open Lwt_result_syntax in
  let* block = Node_context.get_l2_block_by_level node_ctxt level in
  let* inbox = Node_context.get_inbox node_ctxt block.header.inbox_hash
  and* messages =
    Node_context.get_messages node_ctxt block.header.inbox_witness
  in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_level node_ctxt level
  in
  let* _num_messages, _num_ticks, _initial_tick =
    Interpreter.process_head
      (module Plugin)
      node_ctxt
      ctxt
      ~predecessor:(hash_level_of_l2_block predecessor)
      (hash_level_of_l2_block block)
      (inbox, messages)
  in
  let*! context_hash = Context.commit ctxt in
  assert (Smart_rollup_context_hash.(context_hash = block.header.context)) ;
  return (block, ctxt)

let with_modify_data_dir cctxt ~data_dir ~apply_unsafe_patches
    ?(skip_condition = fun _ _ ~head:_ -> Lwt_result.return false) f =
  let open Lwt_result_syntax in
  let store_dir = Configuration.default_storage_dir data_dir in
  let context_dir = Configuration.default_context_dir data_dir in
  let* () = check_store_version store_dir in
  let* store = Store.init Read_write ~data_dir in
  let* head = get_head store in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_level_with_store store head.header.level
  in
  let* metadata = Metadata.read_metadata_file ~dir:data_dir in
  let*? metadata =
    match metadata with
    | None -> error_with "No rollup node metadata in %S." data_dir
    | Some m -> Ok m
  in
  let (module C) = Plugin.Pvm.context metadata.kind in
  let* context =
    Context.load (module C) ~cache_size:100 Read_write context_dir
  in
  let* skip = skip_condition store context ~head in
  unless skip @@ fun () ->
  let* current_protocol =
    Node_context.protocol_of_level_with_store store head.header.level
  in
  let*? (module Plugin) =
    Protocol_plugins.proto_plugin_for_protocol current_protocol.protocol
  in
  let* constants =
    Plugin.Layer1_helpers.retrieve_constants
      cctxt
      ~block:(`Level head.header.level)
  in
  let current_protocol =
    {
      Node_context.hash = current_protocol.protocol;
      proto_level = current_protocol.proto_level;
      constants;
    }
  in
  let* node_ctxt =
    Node_context_loader.For_snapshots.create_node_context
      cctxt
      current_protocol
      store
      context
      ~data_dir
      ~apply_unsafe_patches
  in
  let* () = f node_ctxt ~head in
  let*! () = Context.close context in
  let*! () = Store.close store in
  return_unit

let reconstruct_context_from_first_available_level
    (node_ctxt : _ Node_context.t) ~(head : Sc_rollup_block.t) =
  let open Lwt_result_syntax in
  let* first_level = Node_context.first_available_level node_ctxt in
  let total = Int32.sub head.header.level first_level in
  let progress_bar =
    Progress_bar.progress_bar
      ~counter:`Int
      ~message:"Reconstructing context"
      ~color:(Terminal.Color.rgb 219 146 21)
      (Int32.to_int total)
  in
  Progress_bar.Lwt.with_reporter progress_bar @@ fun count_progress ->
  let* first_block = Node_context.get_l2_block_by_level node_ctxt first_level in
  let* first_ctxt =
    Node_context.checkout_context node_ctxt first_block.header.block_hash
  in
  let rec reconstruct_chain_from (block : Sc_rollup_block.t) rollup_ctxt =
    if block.header.level >= head.header.level then return_unit
    else
      let level = Int32.succ block.header.level in
      let* block, rollup_ctxt =
        reconstruct_level_context rollup_ctxt ~predecessor:block node_ctxt level
      in
      let*! () = count_progress 1 in
      reconstruct_chain_from block rollup_ctxt
  in
  reconstruct_chain_from first_block first_ctxt

let maybe_reconstruct_context cctxt ~data_dir ~apply_unsafe_patches =
  with_modify_data_dir
    cctxt
    ~data_dir
    ~apply_unsafe_patches
    ~skip_condition:(fun _store context ~head ->
      let open Lwt_result_syntax in
      let*! head_ctxt = Context.checkout context head.header.context in
      return (Option.is_some head_ctxt))
    reconstruct_context_from_first_available_level

let post_checks ?(apply_unsafe_patches = false) ~action ~message snapshot_header
    ~dest =
  let open Lwt_result_syntax in
  let store_dir = Configuration.default_storage_dir dest in
  let context_dir = Configuration.default_context_dir dest in
  (* Load context and stores in read-only to run checks. *)
  let* () = check_store_version store_dir in
  let* store = Store.init Read_only ~data_dir:dest in
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
  let*? () = check_last_commitment head snapshot_header in
  let* check_block_data =
    match action with
    | `Export -> return check_block_data
    | `Import cctxt -> (
        let* metadata = Metadata.read_metadata_file ~dir:dest in
        match metadata with
        | None ->
            (* We need the kind of the rollup to run the consistency checks in
               order to verify state hashes. *)
            failwith "No metadata (needs rollup kind)."
        | Some metadata ->
            let* () = check_lcc metadata cctxt store head (module Plugin) in
            return
              (check_block_data_consistency
                 ~apply_unsafe_patches
                 cctxt
                 dest
                 metadata))
  in
  let* () =
    check_l2_chain ~message ~data_dir:dest store context head check_block_data
  in
  let*! () = Context.close context in
  let*! () = Store.close store in
  return_unit

let post_export_checks ~snapshot_file =
  let open Lwt_result_syntax in
  Lwt_utils_unix.with_tempdir "snapshot_checks_" @@ fun dest ->
  let* snapshot_header =
    with_open_snapshot ~progress:false snapshot_file
    @@ fun header snapshot_input ->
    let*! () =
      extract snapshot_input ~display_progress:`Bar ~cancellable:false ~dest
    in
    return header
  in
  post_checks
    ~action:`Export
    ~message:"Checking snapshot   "
    snapshot_header
    ~dest

let snapshotable_files_regexp =
  Re.Str.regexp
    "^\\(storage/version\\|context/.*\\|wasm_2_0_0/.*\\|arith/.*\\|riscv/.*\\|context/.*\\|metadata$\\)"

let maybe_cancel_gc ~rollup_node_endpoint =
  let open Lwt_syntax in
  let open Tezos_rpc_http in
  let open Tezos_rpc_http_client_unix in
  match rollup_node_endpoint with
  | None -> return_unit
  | Some rollup_node_endpoint ->
      let rhttp_ctxt =
        new RPC_client_unix.http_ctxt
          {RPC_client_unix.default_config with endpoint = rollup_node_endpoint}
          Media_type.all_media_types
      in
      let* canceled =
        Rollup_node_services.Admin.(make_call cancel_gc) rhttp_ctxt () () ()
      in
      (match canceled with
      | Ok true -> Format.eprintf "Rollup node GC canceled@."
      | Ok false -> Format.eprintf "No ongoing GC in rollup node@."
      | Error trace ->
          Format.eprintf
            "Could not cancel rollup node GC (use --rollup-node-endpoint for \
             that) because of the following error: %a@ Continuing anyway.@."
            pp_print_trace
            trace) ;
      return_unit

let lock_processing ~data_dir =
  let open Lwt_result_syntax in
  Format.eprintf "Acquiring process lock@." ;
  let* lock_fd =
    Lwt_lock_file.lock
      ~when_locked:`Block
      ~filename:(Node_context.processing_lockfile_path ~data_dir)
  in
  let unlock () = Lwt_lock_file.unlock lock_fd in
  return unlock

let lock_all ~data_dir ~rollup_node_endpoint =
  let open Lwt_result_syntax in
  let*! () = maybe_cancel_gc ~rollup_node_endpoint in
  Format.eprintf
    "Acquiring GC lock\n\
     (specify --rollup-node-endpoint for the snapshot command to take \
     priority)@." ;
  (* Take GC lock first in order to not prevent progression of rollup node. *)
  let* gc_lock_fd =
    Lwt_lock_file.lock
      ~when_locked:`Block
      ~filename:(Node_context.gc_lockfile_path ~data_dir)
  in
  let* unlock_processing = lock_processing ~data_dir in
  let unlock () =
    let open Lwt_syntax in
    let* () = unlock_processing () in
    Lwt_lock_file.unlock gc_lock_fd
  in
  return unlock

let export_dir (header : Header.t) ~unlock ~compression ~data_dir ~dest
    ~filename =
  let open Lwt_result_syntax in
  let* snapshot_file =
    let dest_file_name =
      match filename with
      | Some f ->
          let suffix =
            match compression with
            | No | On_the_fly -> ""
            | After -> ".uncompressed"
          in
          f ^ suffix
      | None ->
          let suffix =
            match compression with
            | On_the_fly -> ""
            | No | After -> ".uncompressed"
          in
          Format.asprintf
            "snapshot-%a-%ld.%s%s"
            Address.pp_short
            header.address
            header.head_level
            (Configuration.string_of_history_mode header.history_mode)
            suffix
    in
    let dest_file =
      match dest with
      | Some dest -> Filename.concat dest dest_file_name
      | None -> dest_file_name
    in
    let* () =
      let*! () = Option.iter_s Lwt_utils_unix.create_dir dest in
      let include_file relative_path =
        Re.Str.string_match snapshotable_files_regexp relative_path 0
      in
      let files =
        Tezos_stdlib_unix.Utils.fold_files
          data_dir
          (fun relative_path acc ->
            if not (include_file relative_path) then acc
            else
              let full_path = Filename.concat data_dir relative_path in
              (full_path, relative_path) :: acc)
          []
      in
      Lwt_utils_unix.with_tempdir "rollup_node_sqlite_export_" @@ fun tmp_dir ->
      let output_db_file = Filename.concat tmp_dir Store.sqlite_file_name in
      let* () = Store.export_store ~data_dir ~output_db_file in
      let files = (output_db_file, Store.sqlite_file_name) :: files in
      let writer =
        match compression with
        | On_the_fly -> gzip_writer
        | No | After -> stdlib_writer
      in
      let*! () =
        create
          writer
          header
          ~files
          ~display_progress:`Bar
          ~cancellable:false
          ~dest:dest_file
          ()
      in
      return_unit
    in
    return dest_file
  in
  let*! () = unlock () in
  let*! snapshot_file =
    match compression with
    | No | On_the_fly -> Lwt.return snapshot_file
    | After ->
        compress ~cancellable:false ~display_progress:`Bar ~snapshot_file ()
  in
  return snapshot_file

let export ?rollup_node_endpoint cctxt ~no_checks ~compression ~data_dir ~dest
    ~filename =
  let open Lwt_result_syntax in
  let* unlock = lock_all ~data_dir ~rollup_node_endpoint in
  let* snapshot_header =
    pre_export_checks_and_get_snapshot_header cctxt ~no_checks ~data_dir
  in
  let* snapshot_file =
    export_dir snapshot_header ~unlock ~compression ~data_dir ~dest ~filename
  in
  let* () = unless no_checks @@ fun () -> post_export_checks ~snapshot_file in
  return snapshot_file

let export_compact cctxt ~no_checks ~compression ~data_dir ~dest ~filename =
  let open Lwt_result_syntax in
  let* unlock = lock_processing ~data_dir in
  let* snapshot_header =
    pre_export_checks_and_get_snapshot_header cctxt ~no_checks ~data_dir
  in
  Lwt_utils_unix.with_tempdir "snapshot_temp_" @@ fun tmp_dir ->
  let tmp_context_dir = Configuration.default_context_dir tmp_dir in
  let*! () = Lwt_utils_unix.create_dir tmp_context_dir in
  let context_dir = Configuration.default_context_dir data_dir in
  let* store = Store.init Read_only ~data_dir in
  let* metadata = Metadata.read_metadata_file ~dir:data_dir in
  let*? metadata =
    match metadata with
    | None -> error_with "No rollup node metadata in %S." data_dir
    | Some m -> Ok m
  in
  let* head = get_head store in
  let level = head.Sc_rollup_block.header.level in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_level_with_store store level
  in
  let (module C) = Plugin.Pvm.context metadata.kind in
  let* context = Context.load (module C) ~cache_size:1 Read_only context_dir in
  let* first_level = first_available_level ~data_dir store in
  let* first_block = Store.L2_blocks.find_by_level store first_level in
  let first_block = WithExceptions.Option.get first_block ~loc:__LOC__ in
  let* () =
    Progress_bar.Lwt.with_background_spinner
      ~message:
        (Format.sprintf
           "Exporting context snapshot with first level %ld"
           first_level)
    @@ Context.export_snapshot
         context
         first_block.header.context
         ~path:tmp_context_dir
  in
  let ( // ) = Filename.concat in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6857
     Use Lwt_utils_unix.copy_dir instead when file descriptors issue is fixed. *)
  let copy_dir a =
    let dir = data_dir // a in
    if Sys.file_exists dir && Sys.is_directory dir then
      Tezos_stdlib_unix.Utils.copy_dir dir (tmp_dir // a)
  in
  let copy_file a =
    let path = data_dir // a in
    if Sys.file_exists path then
      Tezos_stdlib_unix.Utils.copy_file ~src:path ~dst:(tmp_dir // a)
  in
  let output_db_file = Filename.concat tmp_dir Store.sqlite_file_name in
  let* () = Store.export_store ~data_dir ~output_db_file in
  copy_file "metadata" ;
  copy_dir "storage" ;
  copy_dir "wasm_2_0_0" ;
  copy_dir "arith" ;
  copy_dir "riscv" ;
  let compression =
    match compression with
    | After ->
        (* We've already copied data *)
        On_the_fly
    | _ -> compression
  in
  export_dir
    snapshot_header
    ~unlock
    ~compression
    ~data_dir:tmp_dir
    ~dest
    ~filename

let pre_import_checks cctxt ~no_checks ~data_dir (snapshot_header : Header.t) =
  let open Lwt_result_syntax in
  (* Load stores in read-only to make simple checks. *)
  let* store = Store.init Read_write ~data_dir in
  let* metadata = Metadata.read_metadata_file ~dir:data_dir in
  let* history_mode = Store.State.History_mode.get store in
  let* head = Store.L2_blocks.find_head store in
  let*! () = Store.close store in
  let*? () =
    let open Result_syntax in
    match (metadata, history_mode) with
    | None, _ | _, None ->
        (* The rollup node data dir was never initialized, i.e. the rollup node
           wasn't run yet. *)
        return_unit
    | Some {rollup_address; _}, Some history_mode -> (
        let* () =
          error_unless Address.(rollup_address = snapshot_header.address)
          @@ error_of_fmt
               "The existing rollup node is for %a, but the snapshot is for \
                rollup %a."
               Address.pp
               rollup_address
               Address.pp
               snapshot_header.address
        in
        let a_history_str = function
          | Configuration.Archive -> "an archive"
          | Configuration.Full -> "a full"
        in
        match (history_mode, snapshot_header.history_mode) with
        | Full, Archive -> Ok ()
        | _, _ ->
            error_unless (history_mode = snapshot_header.history_mode)
            @@ error_of_fmt
                 "Cannot import %s snapshot into %s rollup node."
                 (a_history_str snapshot_header.history_mode)
                 (a_history_str history_mode))
  in
  let*? () =
    let open Result_syntax in
    match head with
    | None ->
        (* The rollup node has no L2 chain. *)
        return_unit
    | Some head ->
        error_when (snapshot_header.head_level <= head.header.level)
        @@ error_of_fmt
             "The rollup node is already at level %ld but the snapshot is only \
              for level %ld."
             head.header.level
             snapshot_header.head_level
  in
  let* () =
    unless no_checks @@ fun () ->
    check_commitment_published
      cctxt
      snapshot_header.address
      snapshot_header.last_commitment
  in
  return (metadata, history_mode)

let check_data_dir_unpopulated data_dir () =
  let open Lwt_result_syntax in
  let store_dir = Configuration.default_storage_dir data_dir in
  let context_dir = Configuration.default_context_dir data_dir in
  let*! store_exists = Lwt_utils_unix.dir_exists store_dir in
  let*! context_exists = Lwt_utils_unix.dir_exists context_dir in
  if store_exists || context_exists then
    failwith
      "The rollup node data dir %s is already populated. If you want to \
       overwrite its non-local content use the --force option."
      data_dir
  else return_unit

let correct_history_mode ~data_dir (snapshot_header : Header.t)
    (original_history_mode : Configuration.history_mode option) =
  let open Lwt_result_syntax in
  match (original_history_mode, snapshot_header.history_mode) with
  | None, _ -> return_unit
  | Some Archive, Full ->
      (* Impossible because filtered out by pre_import_checks. *)
      assert false
  | Some Archive, Archive | Some Full, Full -> return_unit
  | Some Full, Archive ->
      let* store = Store.init Read_write ~data_dir in
      Store.State.History_mode.set store Full

let import ~apply_unsafe_patches ~no_checks ~force cctxt ~data_dir
    ~snapshot_file =
  let open Lwt_result_syntax in
  let* () = unless force (check_data_dir_unpopulated data_dir) in
  let*! () = Lwt_utils_unix.create_dir data_dir in
  let*! () = Event.acquiring_lock () in
  let lockfile = Node_context.global_lockfile_path ~data_dir in
  Lwt_lock_file.with_lock
    ~when_locked:(`Fail (Rollup_node_errors.Could_not_acquire_lock lockfile))
    ~filename:lockfile
  @@ fun () ->
  let* snapshot_header, original_history_mode =
    with_open_snapshot ~progress:true snapshot_file
    @@ fun header snapshot_input ->
    let* _original_metadata, original_history_mode =
      (pre_import_checks cctxt ~no_checks ~data_dir) header
    in
    let*! () =
      extract
        snapshot_input
        ~display_progress:`Bar
        ~cancellable:false
        ~dest:data_dir
    in
    return (header, original_history_mode)
  in
  let rm f =
    try Unix.unlink f with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  List.iter rm Store.extra_sqlite_files ;
  let* () = check_store_version (Configuration.default_storage_dir data_dir) in
  let* () = maybe_reconstruct_context cctxt ~data_dir ~apply_unsafe_patches in
  let* () =
    correct_history_mode ~data_dir snapshot_header original_history_mode
  in
  unless no_checks @@ fun () ->
  post_checks
    ~apply_unsafe_patches
    ~action:(`Import cctxt)
    ~message:"Checking imported data"
    snapshot_header
    ~dest:data_dir

let info ~snapshot_file =
  with_open_snapshot ~progress:false snapshot_file
  @@ fun snapshot_header snapshot_input ->
  let format = input_format snapshot_input in
  Lwt_result_syntax.return (snapshot_header, format)
