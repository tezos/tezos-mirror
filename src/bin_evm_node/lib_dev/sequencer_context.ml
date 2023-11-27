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

let store_path ~data_dir = Filename.Infix.(data_dir // "store")

let checkpoint_path ~data_dir = Filename.Infix.(data_dir // "checkpoint")

let checkpoint_encoding =
  Data_encoding.(obj1 (req "checkpoint" Context_hash.encoding))

let store_checkpoint ~data_dir commit =
  let json = Data_encoding.Json.construct checkpoint_encoding commit in
  Lwt_utils_unix.Json.write_file (checkpoint_path ~data_dir) json

let load_checkpoint ~data_dir index =
  let open Lwt_result_syntax in
  let path = checkpoint_path ~data_dir in
  let*! exists = Lwt_unix.file_exists path in
  if exists then
    let* content = Lwt_utils_unix.Json.read_file path in
    let checkpoint = Data_encoding.Json.destruct checkpoint_encoding content in
    let*! store = Context.checkout_exn index checkpoint in
    let* evm_state = EVMState.get store in
    return (store, evm_state, true)
  else
    let store = Context.empty index in
    let evm_state = Context.Tree.empty store in
    return (store, evm_state, false)

let init ~data_dir ~kernel ~preimages =
  let open Lwt_result_syntax in
  let*! index = Context.init (store_path ~data_dir) in
  let* store, evm_state, loaded = load_checkpoint ~data_dir index in
  return ({index; store; data_dir; evm_state; kernel; preimages}, loaded)

let commit ctxt evm_state =
  let open Lwt_result_syntax in
  let*! ctxt = EVMState.set ctxt evm_state in
  let*! commit = Context.commit ~time:Time.Protocol.epoch ctxt.store in
  let* () = store_checkpoint ~data_dir:ctxt.data_dir commit in
  return {ctxt with evm_state}

let sync ctxt =
  let open Lwt_result_syntax in
  let*! () = Context.sync ctxt.index in
  let* store, evm_state, _loaded =
    load_checkpoint ~data_dir:ctxt.data_dir ctxt.index
  in
  return {ctxt with store; evm_state}
