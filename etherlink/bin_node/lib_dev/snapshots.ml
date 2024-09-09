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
  | Outdated_snapshot of Ethereum_types.quantity * Ethereum_types.quantity
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
        Ethereum_types.pp_quantity
        snap_level
        Ethereum_types.pp_quantity
        exp_level)
    Data_encoding.(
      obj2
        (req "evm_node_level" Ethereum_types.quantity_encoding)
        (req "snapshot_level" Ethereum_types.quantity_encoding))
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
  type version = V0

  let magic_bytes = "OCTEZ_EVM_node_snapshot"

  let magic_bytes_encoding =
    let open Data_encoding in
    conv_with_guard
      (fun () -> magic_bytes)
      (fun s ->
        if s = magic_bytes then Ok ()
        else Error "Invalid magic bytes for evm node snapshot")
      (obj1 (req "magic_bytes" (Fixed.string (String.length magic_bytes))))

  type t = {
    version : version;
    rollup_address : Address.t;
    current_level : Ethereum_types.quantity;
  }

  let v0_encoding =
    let open Data_encoding in
    conv
      (fun {version = V0; rollup_address; current_level} ->
        (rollup_address, Ethereum_types.encode_u256_le current_level))
      (fun (rollup_address, current_level) ->
        {
          version = V0;
          rollup_address;
          current_level = Ethereum_types.decode_number_le current_level;
        })
    @@ obj2
         (req "rollup_address" Address.encoding)
         (req "current_level" (Fixed.bytes 32))

  let header_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"evm_node.snapshot_header.v0"
          (Tag 0)
          v0_encoding
          (fun ({version = V0; _} as h) -> Some h)
          (fun h -> h);
      ]

  let encoding =
    let open Data_encoding in
    conv (fun h -> ((), h)) (fun ((), h) -> h)
    @@ merge_objs magic_bytes_encoding header_encoding

  let size =
    Data_encoding.Binary.fixed_length encoding
    |> WithExceptions.Option.get ~loc:__LOC__
end

open Snapshot_utils.Make (Header)

let interpolate_snapshot_file current_level rollup_address filename =
  let percent = ('%', "%") in
  let rollup_address = ('r', Address.to_b58check rollup_address) in
  let current_level =
    ('l', Format.asprintf "%a" Ethereum_types.pp_quantity current_level)
  in
  try Ok (Misc.interpolate filename [percent; rollup_address; current_level])
  with _ -> Result_syntax.tzfail (Invalid_snapshot_file filename)

let export ?snapshot_file ~compression ~data_dir () =
  let open Lwt_result_syntax in
  let* () = Evm_context.lock_data_dir ~data_dir in
  let evm_state_path = Evm_context.State.store_path ~data_dir in
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
    let* {rollup_address; current_number = current_level} =
      Evm_context.export_store ~data_dir ~output_db_file
    in
    let header = Header.{version = V0; rollup_address; current_level} in
    let files = (output_db_file, Evm_store.sqlite_file_name) :: files in
    let writer =
      match compression with
      | On_the_fly -> gzip_writer
      | No | After -> stdlib_writer
    in
    let*? dest_file =
      match snapshot_file with
      | Some f ->
          interpolate_snapshot_file header.current_level header.rollup_address f
      | None ->
          let suffix =
            match compression with
            | On_the_fly -> ".compressed"
            | No | After -> ".uncompressed"
          in
          let filename =
            Format.asprintf
              "evm-snapshot-%a-%a%s"
              Address.pp_short
              header.rollup_address
              Ethereum_types.pp_quantity
              header.current_level
              suffix
          in
          Ok filename
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

let data_dir_populated ~data_dir =
  let open Lwt_syntax in
  let store_file = Filename.concat data_dir Evm_store.sqlite_file_name in
  let state_dir = Evm_context.State.store_path ~data_dir in
  let* store_exists = Lwt_unix.file_exists store_file in
  let* state_exists = Lwt_utils_unix.dir_exists state_dir in
  return (store_exists || state_exists)

let check_snapshot_exists snapshot_file =
  let open Lwt_result_syntax in
  let*! snapshot_file_exists = Lwt_unix.file_exists snapshot_file in
  fail_when (not snapshot_file_exists) (File_not_found snapshot_file)

let check_header ~populated ~data_dir (header : Header.t) : unit tzresult Lwt.t
    =
  let open Lwt_result_syntax in
  when_ populated @@ fun () ->
  let* store = Evm_store.init ~data_dir ~perm:`Read_only () in
  Evm_store.use store @@ fun conn ->
  let* rollup_address = Evm_store.Metadata.find conn in
  let* () =
    match rollup_address with
    | None -> return_unit
    | Some r ->
        fail_unless
          Address.(header.rollup_address = r)
          (Incorrect_rollup (header.rollup_address, r))
  in
  let* latest_context = Evm_store.Context_hashes.find_latest conn in
  let* () =
    match latest_context with
    | None -> return_unit
    | Some (current_number, _) ->
        fail_when
          Z.Compare.(
            Ethereum_types.Qty.to_z header.current_level
            <= Ethereum_types.Qty.to_z current_number)
          (Outdated_snapshot (header.current_level, current_number))
  in
  return_unit

let import ~force ~data_dir ~snapshot_file =
  let open Lwt_result_syntax in
  let open Filename.Infix in
  let*! populated = data_dir_populated ~data_dir in
  let*? () =
    error_when ((not force) && populated) (Data_dir_populated data_dir)
  in
  let* () = check_snapshot_exists snapshot_file in
  let*! () = Lwt_utils_unix.create_dir data_dir in
  let* () = Evm_context.lock_data_dir ~data_dir in
  let* () =
    (* This is safe because we took the lock. This cannot be moved before
       `Evm_context.lock_data_dir`. *)
    when_ populated @@ fun () ->
    Format.printf "Delete previous contents from the data-dir\n%!" ;
    let*! () =
      Lwt_utils_unix.remove_dir (Evm_context.State.store_path ~data_dir)
    in
    return_unit
  in
  let reader =
    if Snapshot_utils.is_compressed_snapshot snapshot_file then gzip_reader
    else stdlib_reader
  in
  let* () =
    Lwt_utils_unix.with_tempdir ~temp_dir:data_dir ".octez_evm_node_import_"
    @@ fun dest ->
    let* _snapshot_header, () =
      extract
        reader
        stdlib_writer
        (check_header ~populated ~data_dir)
        ~snapshot_file
        ~dest
    in
    Unix.rename
      (Evm_context.State.store_path ~data_dir:dest)
      (Evm_context.State.store_path ~data_dir) ;
    Unix.rename
      (dest // Evm_store.sqlite_file_name)
      (data_dir // Evm_store.sqlite_file_name) ;
    return_unit
  in
  return_unit

let info ~snapshot_file =
  let compressed = is_compressed_snapshot snapshot_file in
  let reader = if compressed then gzip_reader else stdlib_reader in
  let snapshot_header = read_header reader ~snapshot_file in
  (snapshot_header, if compressed then `Compressed else `Uncompressed)
