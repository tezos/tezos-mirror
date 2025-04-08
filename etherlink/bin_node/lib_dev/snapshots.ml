(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Snapshot_utils

type error +=
  | Data_dir_populated of string
  | Incorrect_rollup of Address.t * Address.t
  | Outdated_snapshot of Z.t * Z.t
  | Invalid_snapshot_file of string
  | History_mode_mismatch of
      Configuration.history_mode * Configuration.history_mode
  | Invalid_snapshot_provider of string

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
        "The snapshot is outdated (for level %a) while the existing EVM node \
         is already at %a"
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
    ~description:"The snapshot file path is invalid"
    ~pp:(fun ppf name ->
      Format.fprintf ppf "%s is not a valid snapshot file name." name)
    Data_encoding.(obj1 (req "snapshot_file" string))
    (function Invalid_snapshot_file name -> Some name | _ -> None)
    (fun name -> Invalid_snapshot_file name) ;
  register_error_kind
    `Permanent
    ~id:"evm_history_mode_mismatch"
    ~title:"History mode mismatch"
    ~description:"Configuration history mode does not match snapshot's."
    ~pp:(fun ppf (config_mode, snapshot_mode) ->
      Format.fprintf
        ppf
        "History mode values are: Configuration = %s; Snapshot = %s. Consider \
         running with `--history-mode`."
        (Configuration.string_of_history_mode_info config_mode)
        (Configuration.string_of_history_mode_info snapshot_mode))
    Data_encoding.(
      obj2
        (req "config_history_mode" Configuration.history_mode_encoding)
        (req "snapshot_history_mode" Configuration.history_mode_encoding))
    (function History_mode_mismatch (a1, a2) -> Some (a1, a2) | _ -> None)
    (fun (a1, a2) -> History_mode_mismatch (a1, a2)) ;
  register_error_kind
    `Permanent
    ~id:"evm_invalid_snapshot_provider"
    ~title:"Invalid snapshot provider"
    ~description:"The snapshot provider is invalid."
    ~pp:(fun ppf name ->
      Format.fprintf ppf "%s is not a valid snapshot provider name" name)
    Data_encoding.(obj1 (req "snapshot_provider" string))
    (function Invalid_snapshot_provider name -> Some name | _ -> None)
    (fun name -> Invalid_snapshot_provider name)

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

let interpolate_snapshot_file current_level rollup_address history_mode filename
    =
  let rollup_address_short =
    ('r', `Available (Address.to_short_b58check rollup_address))
  in
  let rollup_address_long =
    ('R', `Available (Address.to_b58check rollup_address))
  in
  let current_level =
    ( 'l',
      `Available (Format.asprintf "%a" Ethereum_types.pp_quantity current_level)
    )
  in
  let history_mode =
    ( 'h',
      `Available
        (Configuration.string_of_history_mode_info history_mode
        |> String.lowercase_ascii) )
  in
  record_trace (Invalid_snapshot_file filename)
  @@ Misc.interpolate
       filename
       [rollup_address_short; rollup_address_long; current_level; history_mode]

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
         legacy_block_storage;
         history_mode;
         first_number = first_level;
       } =
    Data_dir.export_store ~data_dir ~output_db_file
  in
  let header =
    if legacy_block_storage then
      Header.(V0_legacy {rollup_address; current_level})
    else Header.(V1 {rollup_address; current_level; history_mode; first_level})
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
    interpolate_snapshot_file
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

let check_header ~populated ~force ~data_dir (header : Header.t) :
    unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let ( header_rollup_address,
        Qty header_current_level,
        header_history,
        header_legacy ) =
    match header with
    | V0_legacy {rollup_address; current_level} ->
        (rollup_address, current_level, None, true)
    | V1 {rollup_address; current_level; history_mode; first_level} ->
        (rollup_address, current_level, Some (history_mode, first_level), false)
  in
  let* () =
    if (not populated) && header_legacy && not force then
      failwith
        "Snapshot uses legacy block storage, please import a snapshot \
         generated from a more recent node (or use --force to still import the \
         legacy snapshot)."
    else if header_legacy then
      let*! () = Events.importing_legacy_snapshot () in
      return_unit
    else return_unit
  in
  when_ populated @@ fun () ->
  let* store = Evm_store.init ~data_dir ~perm:`Read_only () in
  Evm_store.use store @@ fun conn ->
  let* metadata = Evm_store.Metadata.find conn in
  let* legacy = Evm_store.Block_storage_mode.legacy conn in
  let* () =
    match metadata with
    | None -> return_unit
    | Some {smart_rollup_address = r; history_mode = _} ->
        fail_unless
          Address.(header_rollup_address = r)
          (Incorrect_rollup (header_rollup_address, r))
  in
  let* () =
    match (header_legacy, legacy) with
    | true, _ ->
        (* Legacy block storage will be replaced with new block storage *)
        return_unit
    | false, false -> return_unit
    | false, true ->
        failwith
          "Snapshot uses legacy block storage but already populated with new \
           Sqlite3 block storage"
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
        else tzfail (History_mode_mismatch (h1, h2))
    | _ -> return_none
  in
  let*! populated = Data_dir.populated ~data_dir in
  let*? () =
    error_when ((not force) && populated) (Data_dir_populated data_dir)
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
  let* () = check_header ~force ~populated ~data_dir header in
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

let interpolate_snapshot_provider ?rollup_address ?network history_mode provider
    =
  let inferred_rollup_address = Option.map Constants.rollup_address network in
  let reason = "try specifying the network with --network" in
  let rollup_address_short, rollup_address_long =
    match (rollup_address, inferred_rollup_address) with
    | Some rollup_address, _ | None, Some rollup_address ->
        ( ( 'r',
            `Available
              (Tezos_crypto.Hashed.Smart_rollup_address.to_short_b58check
                 rollup_address) ),
          ( 'R',
            `Available
              (Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
                 rollup_address) ) )
    | None, None -> (('r', `Disabled reason), ('R', `Disabled reason))
  in
  let history_mode =
    ( 'h',
      `Available
        (Configuration.string_of_history_mode_info history_mode
        |> String.lowercase_ascii) )
  in
  let network =
    match
      (network, Option.bind rollup_address Constants.network_of_address)
    with
    | Some n, _ | None, Some n ->
        ( 'n',
          `Available (Format.asprintf "%a" Configuration.pp_supported_network n)
        )
    | None, None -> ('n', `Disabled reason)
  in

  record_trace (Invalid_snapshot_provider provider)
  @@ Misc.interpolate
       provider
       [rollup_address_short; rollup_address_long; history_mode; network]
