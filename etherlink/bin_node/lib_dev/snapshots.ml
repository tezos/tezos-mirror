(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Invalid_snapshot_file of string
  | Invalid_snapshot_provider of string
  | Data_dir_populated of string
  | History_mode_mismatch of
      Configuration.history_mode * Configuration.history_mode
  | Incorrect_rollup of Address.t * Address.t
  | Outdated_snapshot of Z.t * Z.t

let () =
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
    ~id:"evm_invalid_snapshot_provider"
    ~title:"Invalid snapshot provider"
    ~description:"The snapshot provider is invalid."
    ~pp:(fun ppf name ->
      Format.fprintf ppf "%s is not a valid snapshot provider name" name)
    Data_encoding.(obj1 (req "snapshot_provider" string))
    (function Invalid_snapshot_provider name -> Some name | _ -> None)
    (fun name -> Invalid_snapshot_provider name) ;
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
    (fun (a1, a2) -> Outdated_snapshot (a1, a2))

type metadata =
  | V1 of {
      rollup_address : Address.t;
      current_level : Ethereum_types.quantity;
      history_mode : Configuration.history_mode;
      first_level : Ethereum_types.quantity;
    }

let quantity_hum_encoding =
  let open Data_encoding in
  union
    [
      case
        Json_only
        ~title:"int"
        int31
        (fun (Ethereum_types.Qty z) ->
          if Z.fits_int z then Some (Z.to_int z) else None)
        (fun i -> Ethereum_types.quantity_of_z (Z.of_int i));
      case
        Json_only
        ~title:"z"
        z
        (fun (Ethereum_types.Qty z) -> Some z)
        (fun z -> Ethereum_types.quantity_of_z z);
    ]

let metadata_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"evm_node.snapshot_metadata.v1"
        (Tag 1)
        (obj5
           (req "version" (constant "evm_node.snapshot_metadata.v1"))
           (req "rollup_address" Address.encoding)
           (req "current_level" quantity_hum_encoding)
           (req "history_mode" Configuration.history_mode_encoding)
           (req "first_level" quantity_hum_encoding))
        (function
          | V1 {rollup_address; current_level; history_mode; first_level} ->
              Some ((), rollup_address, current_level, history_mode, first_level))
        (fun ((), rollup_address, current_level, history_mode, first_level) ->
          V1 {rollup_address; current_level; history_mode; first_level});
    ]

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

let default_index_file = "evm-%h-snapshot-%r-%l.caidx"

let backup_items =
  [
    "store";
    Evm_store.sqlite_file_name;
    Evm_store.sqlite_file_name ^ "-shm";
    Evm_store.sqlite_file_name ^ "-wal";
    "wasm_2_0_0";
  ]

let export ?desync_path ?chunk_size ?progress ~target_store ?target_dir
    ?(index_file = default_index_file) ~data_dir () =
  let open Lwt_result_syntax in
  let* () = Data_dir.lock ~data_dir in
  let* {
         rollup_address;
         current_number = current_level;
         history_mode;
         first_number = first_level;
       } =
    Data_dir.store_info ~data_dir
  in
  let metadata =
    V1 {rollup_address; current_level; history_mode; first_level}
  in
  let index_file =
    match target_dir with
    | None -> index_file
    | Some target_dir -> Filename.concat target_dir index_file
  in
  let*? index_file =
    interpolate_snapshot_file
      current_level
      rollup_address
      history_mode
      index_file
  in
  let*! () = Events.exporting_snapshot index_file in
  let* () =
    Desync_snapshots.export
      ?desync_path
      ?chunk_size
      ?progress
      ~metadata
      ~metadata_encoding
      ~target_store
      ~index_file
      ~backup_items
      data_dir
  in
  let*! () = Events.finished_exporting_snapshot index_file in
  return index_file

let info ?desync_path ~source_store ?index_dir ~index_file () =
  let open Lwt_result_syntax in
  let index_file =
    match index_dir with
    | None -> index_file
    | Some index_dir -> Filename.concat index_dir index_file
  in
  let* index_info = Desync_snapshots.info ?desync_path index_file in
  let* metadata =
    Desync_snapshots.read_metadata
      ?desync_path
      ~source_store
      ~index_file
      metadata_encoding
  in
  let size = Ezjsonm.find index_info ["size"] |> Ezjsonm.get_int in
  return (metadata, size)

let check_metadata ~populated ~data_dir metadata : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let open Ethereum_types in
  let metadata_rollup_address, Qty metadata_current_level, metadata_history =
    match metadata with
    | V1 {rollup_address; current_level; history_mode; first_level} ->
        (rollup_address, current_level, Some (history_mode, first_level))
  in
  when_ populated @@ fun () ->
  let* store = Evm_store.init ~data_dir ~perm:(Read_only {pool_size = 1}) () in
  Evm_store.use store @@ fun conn ->
  let* metadata = Evm_store.Metadata.find conn in
  let* () =
    match metadata with
    | None -> return_unit
    | Some {smart_rollup_address = r; history_mode = _} ->
        fail_unless
          (metadata_rollup_address = r)
          (Incorrect_rollup (metadata_rollup_address, r))
  in
  let* () =
    match (metadata_history, metadata) with
    | None, _ | _, None -> return_unit
    | Some (metadata_hist, _), Some {history_mode; _}
      when metadata_hist <> history_mode ->
        failwith
          "Cannot import %a snapshot into %a EVM node."
          Configuration.pp_history_mode_debug
          metadata_hist
          Configuration.pp_history_mode_debug
          history_mode
    | _ -> (* Same history mode *) return_unit
  in
  let* first_context = Evm_store.Context_hashes.find_earliest conn in
  let* () =
    match (first_context, metadata_history) with
    | None, _ | _, None -> return_unit
    | Some (Qty first_number, _), Some (Archive, Qty metadata_first_level)
      when Z.Compare.(metadata_first_level > first_number) ->
        failwith
          "Snapshot history starts at %a but the archive node has history from \
           %a locally. Backup configuration and start from an empty data dir \
           if you want to proceed."
          Z.pp_print
          metadata_first_level
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
          Z.Compare.(metadata_current_level <= current_number)
          (Outdated_snapshot (metadata_current_level, current_number))
  in
  return_unit

let import ?desync_path ~force ?history_mode ~source_store ?index_dir
    ~index_file ~data_dir () =
  let open Lwt_result_syntax in
  let open Filename.Infix in
  Data_dir.use ~data_dir @@ fun () ->
  let index_file =
    match index_dir with
    | None -> index_file
    | Some index_dir -> Filename.concat index_dir index_file
  in
  let*! () = Events.importing_snapshot index_file in
  Lwt_utils_unix.with_tempdir ~temp_dir:data_dir ".octez_evm_node_import_"
  @@ fun dest ->
  let* metadata, _size =
    info ?desync_path ~source_store ?index_dir ~index_file ()
  in
  let* store_history_mode =
    match (history_mode, metadata) with
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
  let* () = check_metadata ~populated ~data_dir metadata in
  let* _metadata =
    Desync_snapshots.import
      ?desync_path
      ~metadata_encoding
      ~source_store
      ~index_file
      dest
  in
  let*! () =
    List.iter_s
      (fun item ->
        let orig = data_dir // item in
        let dest = dest // item in
        let*! to_rename = Lwt_unix.file_exists dest in
        if not to_rename then Lwt.return_unit
        else
          let*! () =
            match Sys.is_directory orig with
            | false -> Lwt_unix.unlink orig
            | true -> Lwt_utils_unix.remove_dir orig
            | exception Sys_error _ -> Lwt.return_unit
          in
          Lwt_unix.rename dest orig)
      backup_items
  in
  let*! () = Events.import_finished () in
  return store_history_mode
