(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Snapshot_utils

module Event = struct
  open Internal_event.Simple

  let section = ["evm_node"; "snapshot"]

  let take_evm_stats_lock =
    declare_0
      ~section
      ~name:"evm_stats_lock"
      ~msg:"Acquiring EVM state lock"
      ~level:Info
      ()

  let emit_evm_stats_lock = emit take_evm_stats_lock
end

type compression = No | On_the_fly | After

module Header = struct
  type version = V0

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7433
     header with more information. *)
  type t = {version : version}

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"evm_node.snapshot_header.v0"
          (Tag 0)
          Data_encoding.unit
          (fun {version = V0} -> Some ())
          (fun () -> {version = V0});
      ]

  let size =
    Data_encoding.Binary.fixed_length encoding
    |> WithExceptions.Option.get ~loc:__LOC__
end

open Snapshot_utils.Make (Header)

let export ?dest ?filename ~compression ~data_dir () =
  let open Lwt_result_syntax in
  let header = Header.{version = V0} in
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
  let lockfile_name =
    Evm_context.State.lockfile_path ~store_path:evm_state_path
  in
  let* () =
    let*! () = Event.emit_evm_stats_lock () in
    Lwt_lock_file.with_lock ~when_locked:`Block ~filename:lockfile_name
    @@ fun () ->
    let evm_context_files =
      Tezos_stdlib_unix.Utils.fold_files
        evm_state_path
        (fun relative_path acc ->
          let full_path = Filename.concat evm_state_path relative_path in
          if full_path = lockfile_name then acc
          else (full_path, Filename.concat "store" relative_path) :: acc)
        []
    in
    let files = evm_context_files in
    (* TODO: add store.sql *)
    (* TODO: add wasm_2_0_0 *)
    let writer =
      match compression with
      | On_the_fly -> gzip_writer
      | No | After -> stdlib_writer
    in
    create stdlib_reader writer header ~files ~dest:dest_file ;
    return_unit
  in
  let snapshot_file =
    match compression with
    | No | On_the_fly -> dest_file
    | After -> compress ~snapshot_file:dest_file
  in
  return snapshot_file
