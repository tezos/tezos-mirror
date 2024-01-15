(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Context = Tezos_context_disk.Context_binary

type index = Context.index

type store = Context.t

type evm_state = Context.tree

type t = {
  data_dir : string;
  index : index;
  store : store;
  evm_state : evm_state;
  kernel : string;
  preimages : string;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  mutable next_blueprint_number : Ethereum_types.quantity;
}

(** The EVM/PVM local state used by the sequencer. *)
module EVMState = struct
  let key = ["evm_state"]

  let get store =
    let open Lwt_result_syntax in
    let*! tree_opt = Context.find_tree store key in
    match tree_opt with
    | Some tree -> return tree
    | None -> failwith "The EVM state was not found"

  let set ctxt tree =
    let open Lwt_syntax in
    let* store = Context.add_tree ctxt.store key tree in
    return {ctxt with store}
end

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
    let*! store = Context.checkout_exn index checkpoint in
    let* evm_state = EVMState.get store in
    return (store, evm_state, next_blueprint_number, true)
  else
    let store = Context.empty index in
    let evm_state = Context.Tree.empty store in
    return (store, evm_state, Ethereum_types.Qty Z.zero, false)

let init ~data_dir ~kernel ~preimages ~smart_rollup_address =
  let open Lwt_result_syntax in
  let*! index = Context.init (store_path ~data_dir) in
  let* store, evm_state, next_blueprint_number, loaded =
    load_metadata ~data_dir index
  in
  let smart_rollup_address =
    Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn smart_rollup_address
  in
  return
    ( {
        index;
        store;
        data_dir;
        evm_state;
        kernel;
        preimages;
        smart_rollup_address;
        next_blueprint_number;
      },
      loaded )

let commit ctxt evm_state =
  let open Lwt_result_syntax in
  let*! ctxt = EVMState.set ctxt evm_state in
  let*! checkpoint = Context.commit ~time:Time.Protocol.epoch ctxt.store in
  let* () =
    store_metadata
      ~data_dir:ctxt.data_dir
      {checkpoint; next_blueprint_number = ctxt.next_blueprint_number}
  in
  return {ctxt with evm_state}

let sync ctxt =
  let open Lwt_result_syntax in
  let*! () = Context.sync ctxt.index in
  let* store, evm_state, next_blueprint_number, _loaded =
    load_metadata ~data_dir:ctxt.data_dir ctxt.index
  in
  return {ctxt with store; evm_state; next_blueprint_number}
