(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type store_info = {
  rollup_address : Address.t;
  current_number : Ethereum_types.quantity;
  history_mode : Configuration.history_mode;
  first_number : Ethereum_types.quantity;
}

let store_path ~data_dir = Filename.Infix.(data_dir // "store")

let global_lockfile_path ~data_dir = Filename.Infix.(data_dir // "lock")

type error += Data_dir_locked of string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.datadir_is_locked"
    ~title:"Data directory of EVM node is locked"
    ~description:"Data directory of EVM node is locked by another process."
    ~pp:(fun ppf dir ->
      Format.fprintf
        ppf
        "The data directory %s of the EVM node is locked by another process."
        dir)
    Data_encoding.(obj1 (req "data_dir" string))
    (function Data_dir_locked dir -> Some dir | _ -> None)
    (fun dir -> Data_dir_locked dir)

let lock ~data_dir =
  let open Lwt_result_syntax in
  let*! () = Lwt_utils_unix.create_dir data_dir in
  let filename = global_lockfile_path ~data_dir in
  (* It's okay not to release the lock because once you have taken it,
     you don't want the data directory to be overwritten while the node
     is running. The lock will be released once the node stops. *)
  let* _fd =
    Lwt_lock_file.lock ~when_locked:(`Fail (Data_dir_locked data_dir)) ~filename
  in
  return_unit

let store_info conn =
  let open Lwt_result_syntax in
  let* metadata = Evm_store.Metadata.get conn in
  let* current = Evm_store.Context_hashes.find_latest conn in
  let* first = Evm_store.Context_hashes.find_earliest conn in
  let*? current_number, first_number =
    match (current, first) with
    | None, _ | _, None -> error_with "No data in store, cannot export."
    | Some (cur, _), Some (first, _) -> Ok (cur, first)
  in
  return
    {
      rollup_address = metadata.smart_rollup_address;
      current_number;
      history_mode = metadata.history_mode;
      first_number;
    }

let export_store ~data_dir ~output_db_file =
  let open Lwt_result_syntax in
  let* store =
    Evm_store.init
      ~chain_family:L2_types.EVM
      ~data_dir
      ~perm:(Read_only {pool_size = 1})
      ()
  in
  Evm_store.use store @@ fun conn ->
  let* info = store_info conn in
  let* () = Evm_store.vacuum ~conn ~output_db_file in
  return info

let store_info ~data_dir =
  let open Lwt_result_syntax in
  let* store =
    Evm_store.init
      ~chain_family:L2_types.EVM
      ~data_dir
      ~perm:(Read_only {pool_size = 1})
      ()
  in
  Evm_store.use store store_info

let use ~data_dir k =
  let open Lwt_result_syntax in
  let*! data_dir_exists = Lwt_utils_unix.dir_exists data_dir in
  let on_failure () =
    let open Lwt_syntax in
    (* TODO: [remove_dir] will remove [data_dir], but not its parents
       created by [create_dir]. For instance, if [/a/b/c] is the requested
       data-dir, and only [/a] exists, then [/a/b] will remain. *)
    if not data_dir_exists then Lwt_utils_unix.remove_dir data_dir
    else return_unit
  in
  Lwt_utils_unix.safe_cancel_on_exit @@ fun () ->
  let*! () = Lwt_utils_unix.create_dir data_dir in
  protect
    ~on_error:(fun err ->
      let*! () = on_failure () in
      fail err)
    k

let populated ~data_dir =
  let open Lwt_syntax in
  let store_file = Filename.concat data_dir Evm_store.sqlite_file_name in
  let state_dir = store_path ~data_dir in
  let* store_exists = Lwt_unix.file_exists store_file in
  let* state_exists = Lwt_utils_unix.dir_exists state_dir in
  return (store_exists || state_exists)
