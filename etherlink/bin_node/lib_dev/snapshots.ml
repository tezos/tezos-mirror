(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Snapshot_utils

type error +=
  | Data_dir_populated of string
  | File_not_found of string
  | Incorrect_rollup of Address.t * Address.t
  | Outdated_snapshot of Z.t * Z.t
  | Invalid_snapshot_file of string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_data_dir_populated"
    ~title:"Data dir already populated"
    ~description:"Raise an error when the data dir is already populated"
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "The EVM node data directory %s is already populated."
        path)
    Data_encoding.(obj1 (req "data_dir_already_populated" string))
    (function Data_dir_populated path -> Some path | _ -> None)
    (fun path -> Data_dir_populated path) ;
  register_error_kind
    `Permanent
    ~id:"file_not_found"
    ~title:"File not found"
    ~description:"Raise an error when a file is not found"
    ~pp:(fun ppf file -> Format.fprintf ppf "File %s is not found" file)
    Data_encoding.(obj1 (req "file_not_found" string))
    (function File_not_found file -> Some file | _ -> None)
    (fun file -> File_not_found file) ;
  register_error_kind
    `Permanent
    ~id:"evm_incorrect_rollup"
    ~title:"Snapshot for incorrect rollup"
    ~description:"Snapshot for incorrect rollup."
    ~pp:(fun ppf (snap_addr, exp_addr) ->
      Format.fprintf
        ppf
        "The existing EVM node is for the rollup %a whereas the snapshot is \
         for %a."
        Address.pp
        exp_addr
        Address.pp
        snap_addr)
    Data_encoding.(
      obj2
        (req "evm_node_rollup" Address.encoding)
        (req "snapshot_rollup" Address.encoding))
    (function Incorrect_rollup (a1, a2) -> Some (a1, a2) | _ -> None)
    (fun (a1, a2) -> Incorrect_rollup (a1, a2)) ;
  register_error_kind
    `Permanent
    ~id:"evm_outdated_snapshot"
    ~title:"Outdated snapshot"
    ~description:"Snapshot is outdated with respect to existing data."
    ~pp:(fun ppf (snap_level, exp_level) ->
      Format.fprintf
        ppf
        "The snapshot is outdated (for level %a) while the \n\
        \ existing EVM node is already at %a@."
        Z.pp_print
        snap_level
        Z.pp_print
        exp_level)
    Data_encoding.(obj2 (req "evm_node_level" z) (req "snapshot_level" z))
    (function Outdated_snapshot (a1, a2) -> Some (a1, a2) | _ -> None)
    (fun (a1, a2) -> Outdated_snapshot (a1, a2)) ;
  register_error_kind
    `Permanent
    ~id:"evm_invalid_snapshot_file"
    ~title:"Invalid snapshot file path"
    ~description:"The snapshot file path is invalid."
    ~pp:(fun ppf name ->
      Format.fprintf
        ppf
        "%s is not a valid snapshot file name. It is likely because you are \
         using an invalid string interpolation variable or have a trailing \
         percent"
        name)
    Data_encoding.(obj1 (req "snapshot_file" string))
    (function Invalid_snapshot_file name -> Some name | _ -> None)
    (fun name -> Invalid_snapshot_file name)

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

let interpolate_snapshot_file current_level rollup_address filename =
  let percent = ('%', "%") in
  let rollup_address_short = ('r', Address.to_short_b58check rollup_address) in
  let rollup_address_long = ('R', Address.to_b58check rollup_address) in
  let current_level =
    ('l', Format.asprintf "%a" Ethereum_types.pp_quantity current_level)
  in
  try
    Ok
      (Misc.interpolate
         filename
         [percent; rollup_address_short; rollup_address_long; current_level])
  with _ -> Result_syntax.tzfail (Invalid_snapshot_file filename)

let export ?snapshot_file ~compression ~data_dir () =
  let open Lwt_result_syntax in
  let* () = Data_dir.lock ~data_dir in
  let evm_state_path = Data_dir.store_path ~data_dir in
  let* dest_file =
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
    Lwt_utils_unix.with_tempdir "evm_node_sqlite_export_" @@ fun tmp_dir ->
    let output_db_file = Filename.concat tmp_dir Evm_store.sqlite_file_name in
    let* {
           rollup_address;
           current_number = current_level;
           legacy_block_storage;
           history_mode;
           first_number = first_level;
         } =
      Data_dir.export_store ~data_dir ~output_db_file
    in
    let header =
      if legacy_block_storage then
        Header.(V0_legacy {rollup_address; current_level})
      else
        Header.(V1 {rollup_address; current_level; history_mode; first_level})
    in
    let files = (output_db_file, Evm_store.sqlite_file_name) :: files in
    let writer =
      match compression with
      | On_the_fly -> gzip_writer
      | No | After -> stdlib_writer
    in
    let*? dest_file =
      let open Result_syntax in
      match snapshot_file with
      | Some f -> (
          let+ f = interpolate_snapshot_file current_level rollup_address f in
          match compression with
          | No | On_the_fly -> f
          | After -> f ^ ".uncompressed")
      | None ->
          let suffix =
            match compression with
            | On_the_fly -> ""
            | No | After -> ".uncompressed"
          in
          let filename =
            Format.asprintf
              "evm-snapshot-%a-%a%s"
              Address.pp_short
              rollup_address
              Ethereum_types.pp_quantity
              current_level
              suffix
          in
          return filename
    in
    let*! () = Lwt_utils_unix.create_dir (Filename.dirname dest_file) in
    create stdlib_reader writer header ~files ~dest:dest_file ;
    return dest_file
  in
  let snapshot_file =
    match compression with
    | No | On_the_fly -> dest_file
    | After -> compress ~snapshot_file:dest_file
  in
  return snapshot_file

let check_snapshot_exists snapshot_file =
  let open Lwt_result_syntax in
  let*! snapshot_file_exists = Lwt_unix.file_exists snapshot_file in
  fail_when (not snapshot_file_exists) (File_not_found snapshot_file)

let check_header ~populated ~data_dir (header : Header.t) : unit tzresult Lwt.t
    =
  let open Lwt_result_syntax in
  let header_rollup_address, Qty header_current_level, header_history =
    match header with
    | V0_legacy {rollup_address; current_level} ->
        (rollup_address, current_level, None)
    | V1 {rollup_address; current_level; history_mode; first_level} ->
        (rollup_address, current_level, Some (history_mode, first_level))
  in
  when_ populated @@ fun () ->
  let* store = Evm_store.init ~data_dir ~perm:`Read_only () in
  Evm_store.use store @@ fun conn ->
  let* metadata = Evm_store.Metadata.find conn in
  let* () =
    match metadata with
    | None -> return_unit
    | Some {smart_rollup_address = r; history_mode = _} ->
        fail_unless
          Address.(header_rollup_address = r)
          (Incorrect_rollup (header_rollup_address, r))
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
          (Outdated_snapshot (header_current_level, current_number))
  in
  return_unit

let import ~cancellable ~force ~data_dir ~snapshot_file =
  let open Lwt_result_syntax in
  let open Filename.Infix in
  let*! populated = Data_dir.populated ~data_dir in
  let*? () =
    error_when ((not force) && populated) (Data_dir_populated data_dir)
  in
  let* () = check_snapshot_exists snapshot_file in
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
  let reader =
    if Snapshot_utils.is_compressed_snapshot snapshot_file then gzip_reader
    else stdlib_reader
  in

  let display_progress = not cancellable in

  let start_time = Ptime_clock.now () in
  let rec periodic_emit () =
    let*! () = Lwt_unix.sleep 60.0 in
    let elapsed_time = Ptime.diff (Ptime_clock.now ()) start_time in
    let*! () =
      Events.extract_snapshot_archive_in_progress
        ~archive_name:snapshot_file
        ~elapsed_time
    in
    periodic_emit ()
  in
  let extract_snapshot_archive =
    Lwt_utils_unix.with_tempdir ~temp_dir:data_dir ".octez_evm_node_import_"
    @@ fun dest ->
    let* _snapshot_header, () =
      extract
        reader
        stdlib_writer
        (check_header ~populated ~data_dir)
        ~cancellable
        ~display_progress
          (* [progress] modifies the signal handlers, which are necessary for
             [Lwt_exit] to work. As a consequence, if we want to be
             cancellable, we cannot have display bar. *)
        ~snapshot_file
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
    return_unit
  in
  if display_progress then extract_snapshot_archive
  else Lwt.pick [extract_snapshot_archive; periodic_emit ()]

let info ~snapshot_file =
  let compressed = is_compressed_snapshot snapshot_file in
  let reader = if compressed then gzip_reader else stdlib_reader in
  let snapshot_header = read_header reader ~snapshot_file in
  (snapshot_header, if compressed then `Compressed else `Uncompressed)
