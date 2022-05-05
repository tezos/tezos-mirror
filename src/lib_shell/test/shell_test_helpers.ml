(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Shell
    Subject:      Setup mocks for testing shell, notably state and protocol
                  validation.
*)

open Filename.Infix

(** Basic blocks *)

let genesis_block_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol_hash =
  Protocol_hash.of_b58check_exn
    "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

let genesis_time = Time.Protocol.of_seconds 0L

let genesis_protocol =
  match Registered_protocol.get genesis_protocol_hash with
  | None -> assert false
  | Some genesis_protocol -> genesis_protocol

module Genesis_proto = (val genesis_protocol)

let state_genesis_block =
  {
    Genesis.time = genesis_time;
    block = genesis_block_hash;
    protocol = genesis_protocol_hash;
  }

let genesis : Genesis.t =
  {
    time = genesis_time;
    block = genesis_block_hash;
    protocol = genesis_protocol_hash;
  }

let chain_id = Chain_id.of_block_hash genesis_block_hash

let patch_context ctxt =
  let open Lwt_syntax in
  let* v = Context.add ctxt ["version"] (Bytes.of_string "demo_noops") in
  return_ok v

(** [init_chain base_dir] with working directory [base_dir] returns a new state
    with a single genesis block *)
let init_chain ?(history_mode = History_mode.Archive) base_dir =
  let open Lwt_syntax in
  let store_dir = base_dir // "store" in
  let context_dir = base_dir // "context" in
  let* r =
    Store.init
      ~store_dir
      ~context_dir
      ~history_mode
      ~allow_testchains:true
      ~patch_context
      state_genesis_block
  in
  match r with
  | Error err ->
      Format.kasprintf Lwt.fail_with "init error: %a" pp_print_trace err
  | Ok store -> Lwt.return store

(** [init_mock_p2p] initializes a mock p2p *)
let init_mock_p2p chain_name =
  let open Connection_metadata in
  let peer_metadata_cfg : _ P2p_params.peer_meta_config =
    {
      peer_meta_encoding = Peer_metadata.encoding;
      peer_meta_initial = Peer_metadata.empty;
      score = Peer_metadata.score;
    }
  in
  let message_cfg = Distributed_db_message.cfg chain_name in
  let c_meta = {disable_mempool = true; private_node = true} in
  Lwt.return_ok (P2p.faked_network message_cfg peer_metadata_cfg c_meta)
