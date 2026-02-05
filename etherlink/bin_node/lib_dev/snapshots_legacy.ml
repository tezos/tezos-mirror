(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Snapshot_utils

type compression = No | On_the_fly | After

module Header = struct
  let magic_bytes = "OCTEZ_EVM_node_snapshot"

  let magic_bytes_encoding =
    let open Data_encoding in
    conv_with_guard
      (fun () -> magic_bytes)
      (fun s ->
        if s = magic_bytes then Ok ()
        else Error "Invalid magic bytes for evm node snapshot")
      (obj1 (req "magic_bytes" (Fixed.string (String.length magic_bytes))))

  type t =
    | V0_legacy of {
        rollup_address : Address.t;
        current_level : Ethereum_types.quantity;
      }
    | V1 of {
        rollup_address : Address.t;
        current_level : Ethereum_types.quantity;
        history_mode : Configuration.history_mode;
        first_level : Ethereum_types.quantity;
      }

  let cur_level_encoding =
    Data_encoding.conv
      Ethereum_types.encode_u256_le
      Ethereum_types.decode_number_le
      (Data_encoding.Fixed.bytes 32)

  let header_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"evm_node.snapshot_header.v0_legacy"
          (Tag 0)
          (obj2
             (req "rollup_address" Address.encoding)
             (req "current_level" cur_level_encoding))
          (function
            | V0_legacy {rollup_address; current_level} ->
                Some (rollup_address, current_level)
            | _ -> None)
          (fun (rollup_address, current_level) ->
            V0_legacy {rollup_address; current_level});
        case
          ~title:"evm_node.snapshot_header.v1"
          (Tag 1)
          (obj4
             (req "rollup_address" Address.encoding)
             (req "current_level" cur_level_encoding)
             (req "history_mode" Configuration.history_mode_encoding)
             (req "first_level" cur_level_encoding))
          (function
            | V1 {rollup_address; current_level; history_mode; first_level} ->
                Some (rollup_address, current_level, history_mode, first_level)
            | _ -> None)
          (fun (rollup_address, current_level, history_mode, first_level) ->
            V1 {rollup_address; current_level; history_mode; first_level});
      ]

  let encoding =
    let open Data_encoding in
    conv (fun h -> ((), h)) (fun ((), h) -> h)
    @@ merge_objs magic_bytes_encoding header_encoding
end

open Snapshot_utils.Make (Header)

let default_snapshot_file = "evm-%h-snapshot-%r-%l"

let export ?snapshot_file ~compression ~data_dir () =
  let open Lwt_result_syntax in
  let* () = Data_dir.lock ~data_dir in
  let evm_state_path = Data_dir.store_path ~data_dir in

  let evm_context_files =
    Tezos_stdlib_unix.Utils.fold_files
      evm_state_path
      (fun relative_path acc ->
        let full_path = Filename.concat evm_state_path relative_path in
        (full_path, Filename.concat "store" relative_path) :: acc)
      []
  in
  let files = evm_context_files in
  (* Export SQLite database *)
  Lwt_utils_unix.with_tempdir ~temp_dir:data_dir ".evm_node_sqlite_export_"
  @@ fun tmp_dir ->
  let output_db_file = Filename.concat tmp_dir Evm_store.sqlite_file_name in
  let* {
         rollup_address;
         current_number = current_level;
         history_mode;
         first_number = first_level;
       } =
    Data_dir.export_store ~data_dir ~output_db_file
  in
  let header =
    Header.(V1 {rollup_address; current_level; history_mode; first_level})
  in
  let files = (output_db_file, Evm_store.sqlite_file_name) :: files in
  let writer =
    match compression with
    | On_the_fly -> gzip_writer
    | No | After -> stdlib_writer
  in
  let snapshot_file =
    match (snapshot_file, compression) with
    | Some f, After -> f ^ ".uncompressed"
    | Some f, (No | On_the_fly) -> f
    | None, On_the_fly -> default_snapshot_file ^ ".gz"
    | None, After -> default_snapshot_file ^ ".gz.uncompressed"
    | None, No -> default_snapshot_file
  in
  let*? extract_dest_file =
    Snapshots.interpolate_snapshot_file
      current_level
      rollup_address
      history_mode
      snapshot_file
  in
  let dest_file =
    Option.value
      ~default:extract_dest_file
      (Filename.chop_suffix_opt ~suffix:".uncompressed" extract_dest_file)
  in
  let*! () = Events.exporting_snapshot dest_file in
  let tmp_dest = Filename.(Infix.(tmp_dir // basename extract_dest_file)) in
  let*! () =
    create
      ~cancellable:true
      ~display_progress:
        (`Periodic_event (Events.still_exporting_snapshot dest_file))
      writer
      header
      ~files
      ~dest:tmp_dest
      ()
  in
  let*! tmp_dest =
    match compression with
    | No | On_the_fly -> Lwt.return tmp_dest
    | After ->
        let*! () = Events.compressing_snapshot dest_file in
        compress
          ~cancellable:true
          ~display_progress:
            (`Periodic_event (Events.still_compressing_snapshot dest_file))
          ~snapshot_file:tmp_dest
          ()
  in

  let*! () = Lwt_utils_unix.create_dir (Filename.dirname dest_file) in
  let*! () = Lwt_unix.rename tmp_dest dest_file in
  let*! () = Events.finished_exporting_snapshot dest_file in
  return dest_file

let check_header ~populated ~data_dir (header : Header.t) : unit tzresult Lwt.t
    =
  let open Lwt_result_syntax in
  let*? header_rollup_address, Qty header_current_level, header_history =
    match header with
    | V0_legacy _ -> error_with "Support for legacy snapshot was dropped"
    | V1 {rollup_address; current_level; history_mode; first_level} ->
        Ok (rollup_address, current_level, Some (history_mode, first_level))
  in
  when_ populated @@ fun () ->
  let* store =
    Evm_store.init
      ~chain_family:L2_types.EVM
      ~data_dir
      ~perm:(Read_only {pool_size = 1})
      ()
  in
  Evm_store.use store @@ fun conn ->
  let* metadata = Evm_store.Metadata.find conn in
  let* () =
    match metadata with
    | None -> return_unit
    | Some {smart_rollup_address = r; history_mode = _} ->
        fail_unless
          Address.(header_rollup_address = r)
          (Snapshots.Incorrect_rollup (header_rollup_address, r))
  in
  let* () =
    match (header_history, metadata) with
    | None, _ | _, None -> return_unit
    | Some (header_hist, _), Some {history_mode; _}
      when header_hist <> history_mode ->
        failwith
          "Cannot import %a snapshot into %a EVM node."
          Configuration.pp_history_mode_debug
          header_hist
          Configuration.pp_history_mode_debug
          history_mode
    | _ -> (* Same history mode *) return_unit
  in
  let* first_context = Evm_store.Context_hashes.find_earliest conn in
  let* () =
    match (first_context, header_history) with
    | None, _ | _, None -> return_unit
    | Some (Qty first_number, _), Some (Archive, Qty header_first_level)
      when Z.Compare.(header_first_level > first_number) ->
        failwith
          "Snapshot history starts at %a but the archive node has history from \
           %a locally. Backup configuration and start from an empty data dir \
           if you want to proceed."
          Z.pp_print
          header_first_level
          Z.pp_print
          first_number
    | _ -> return_unit
  in
  let* latest_context = Evm_store.Context_hashes.find_latest conn in
  let* () =
    match latest_context with
    | None -> return_unit
    | Some (Qty current_number, _) ->
        fail_when
          Z.Compare.(header_current_level <= current_number)
          (Snapshots.Outdated_snapshot (header_current_level, current_number))
  in
  return_unit

let info ~snapshot_file =
  with_open_snapshot ~progress:false snapshot_file
  @@ fun snapshot_header snapshot_input ->
  let format = input_format snapshot_input in
  Lwt_result_syntax.return (snapshot_header, format)

let import_from ~force ?history_mode ~data_dir ~snapshot_file () =
  let open Lwt_result_syntax in
  let open Filename.Infix in
  Data_dir.use ~data_dir @@ fun () ->
  let*! () = Events.importing_snapshot snapshot_file in
  Lwt_utils_unix.with_tempdir ~temp_dir:data_dir ".octez_evm_node_import_"
  @@ fun dest ->
  with_open_snapshot ~progress:true snapshot_file
  @@ fun header snapshot_input ->
  let* store_history_mode =
    match (history_mode, header) with
    | Some h1, V1 {history_mode = h2; _} ->
        if Configuration.history_mode_partial_eq h1 h2 then return_some h1
        else tzfail (Snapshots.History_mode_mismatch (h1, h2))
    | _ -> return_none
  in
  let*! populated = Data_dir.populated ~data_dir in
  let*? () =
    error_when
      ((not force) && populated)
      (Snapshots.Data_dir_populated data_dir)
  in
  let*! () = Lwt_utils_unix.create_dir data_dir in
  let* () = Data_dir.lock ~data_dir in
  let* () =
    (* This is safe because we took the lock. This cannot be moved before
       `Data_dir.lock`. *)
    when_ populated @@ fun () ->
    Format.printf "Delete previous contents from the data-dir\n%!" ;
    let*! () = Lwt_utils_unix.remove_dir (Data_dir.store_path ~data_dir) in
    return_unit
  in
  let* () = check_header ~populated ~data_dir header in
  let*! is_tty = Lwt_unix.isatty Lwt_unix.stderr in
  (* We always emit the importing event for local files. For remote files, we
     only show the event if the output is not a TTY because if it is, the
     progress bar will already be shown, and if it's not, no progress indicator
     would be logged otherwise. *)
  let archive_name, emit_event =
    match input_source snapshot_input with
    | `Local s -> (s, true)
    | `Remote s -> (s, not is_tty)
    | `Stdin -> ("(stdin)", true)
  in
  let*! () =
    extract
      snapshot_input
      ~cancellable:true
      ~display_progress:
        (`Periodic_event
           (fun elapsed_time ->
             if emit_event then
               Events.import_snapshot_archive_in_progress
                 ~archive_name
                 ~elapsed_time
             else Lwt.return_unit))
        (* [progress] modifies the signal handlers, which are necessary for
           [Lwt_exit] to work. As a consequence, if we want to be
           cancellable, we cannot have display bar. *)
      ~dest
  in
  Unix.rename
    (Data_dir.store_path ~data_dir:dest)
    (Data_dir.store_path ~data_dir) ;
  let rm f =
    try Unix.unlink f with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  rm @@ (data_dir // Evm_store.sqlite_file_name) ^ "-shm" ;
  rm @@ (data_dir // Evm_store.sqlite_file_name) ^ "-wal" ;
  Unix.rename
    (dest // Evm_store.sqlite_file_name)
    (data_dir // Evm_store.sqlite_file_name) ;
  let*! () = Events.import_finished () in
  return store_history_mode
