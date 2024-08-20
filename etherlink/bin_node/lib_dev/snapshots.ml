(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Snapshot_utils

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

let export ?dest ?filename ~compression ~data_dir () =
  let open Lwt_result_syntax in
  let* () = Evm_context.lock_data_dir ~data_dir in
  let dest_file_name =
    match filename with
    | Some f -> f
    | None ->
        let suffix =
          match compression with
          | On_the_fly -> ".compressed"
          | No | After -> ".uncompressed"
        in
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/7433
           name based on header *)
        Format.asprintf "evm-snapshot%s" suffix
  in
  let dest_file =
    match dest with
    | Some dest -> Filename.concat dest dest_file_name
    | None -> dest_file_name
  in
  let*! () = Lwt_utils_unix.create_dir (Filename.dirname dest_file) in
  let evm_state_path = Evm_context.State.store_path ~data_dir in
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
  create stdlib_reader writer header ~files ~dest:dest_file ;
  let snapshot_file =
    match compression with
    | No | On_the_fly -> dest_file
    | After -> compress ~snapshot_file:dest_file
  in
  return snapshot_file
