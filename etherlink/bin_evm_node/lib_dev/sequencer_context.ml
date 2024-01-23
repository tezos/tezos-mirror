(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Bare_context = struct
  module Tree = Irmin_context.Tree

  type t = Irmin_context.rw

  type index = Irmin_context.rw_index

  type nonrec tree = Irmin_context.tree

  let init ?patch_context:_ ?readonly:_ ?index_log_size:_ path =
    let open Lwt_syntax in
    let* res = Irmin_context.load ~cache_size:100_000 Read_write path in
    match res with
    | Ok res -> return res
    | Error _ -> Lwt.fail_with "could not initialize the context"

  let empty index = Irmin_context.empty index
end

type evm_state = Irmin_context.PVMState.value

type t = {
  data_dir : string;
  context : Irmin_context.rw;
  kernel : string;
  preimages : string;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  mutable next_blueprint_number : Ethereum_types.quantity;
}

type metadata = {
  checkpoint : Context_hash.t;
  next_blueprint_number : Ethereum_types.quantity;
}

let store_path ~data_dir = Filename.Infix.(data_dir // "store")

let metadata_path ~data_dir = Filename.Infix.(data_dir // "metadata")

let metadata_encoding =
  let open Data_encoding in
  conv
    (fun {checkpoint; next_blueprint_number} ->
      (checkpoint, next_blueprint_number))
    (fun (checkpoint, next_blueprint_number) ->
      {checkpoint; next_blueprint_number})
    (obj2
       (req "checkpoint" Context_hash.encoding)
       (req "next_blueprint_number" Ethereum_types.quantity_encoding))

let store_metadata ~data_dir metadata =
  let json = Data_encoding.Json.construct metadata_encoding metadata in
  Lwt_utils_unix.Json.write_file (metadata_path ~data_dir) json

let load_metadata ~data_dir index =
  let open Lwt_result_syntax in
  let path = metadata_path ~data_dir in
  let*! exists = Lwt_unix.file_exists path in
  if exists then
    let* content = Lwt_utils_unix.Json.read_file path in
    let {checkpoint; next_blueprint_number} =
      Data_encoding.Json.destruct metadata_encoding content
    in
    let*! context = Irmin_context.checkout_exn index checkpoint in
    return (context, next_blueprint_number, true)
  else
    let context = Irmin_context.empty index in
    return (context, Ethereum_types.Qty Z.zero, false)

let init ~data_dir ~kernel ~preimages ~smart_rollup_address =
  let open Lwt_result_syntax in
  let* index =
    Irmin_context.load ~cache_size:100_000 Read_write (store_path ~data_dir)
  in
  let* context, next_blueprint_number, loaded = load_metadata ~data_dir index in
  let smart_rollup_address =
    Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn smart_rollup_address
  in
  return
    ( {
        context;
        data_dir;
        kernel;
        preimages;
        smart_rollup_address;
        next_blueprint_number;
      },
      loaded )

let commit (ctxt : t) evm_state =
  let open Lwt_result_syntax in
  let*! context = Irmin_context.PVMState.set ctxt.context evm_state in
  let*! checkpoint = Irmin_context.commit context in
  let* () =
    store_metadata
      ~data_dir:ctxt.data_dir
      {checkpoint; next_blueprint_number = ctxt.next_blueprint_number}
  in
  return {ctxt with context}

let sync ctxt =
  let open Lwt_result_syntax in
  let* index =
    Irmin_context.load
      ~cache_size:100_000
      Read_write
      (store_path ~data_dir:ctxt.data_dir)
  in
  let* context, next_blueprint_number, _loaded =
    load_metadata ~data_dir:ctxt.data_dir index
  in
  return {ctxt with context; next_blueprint_number}

let evm_state {context; _} = Irmin_context.PVMState.get context
