(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Profiler

let mempool_profiler = unplugged ()

let store_profiler = unplugged ()

(** [merge_profiler] needs to be distinct from {!store_profiler} because
    {!Block_store.merge_stores} creates an asynchronous thread where
    [merge_profiler] is used. *)
let merge_profiler = unplugged ()

let chain_validator_profiler = unplugged ()

let block_validator_profiler = unplugged ()

let peer_validator_profiler = unplugged ()

let rpc_server_profiler = unplugged ()

let p2p_reader_profiler = unplugged ()

let requester_profiler = unplugged ()

let create_reset_block_section =
  section_maker Block_hash.equal Block_hash.to_b58check

let all_profilers =
  [
    ("mempool", mempool_profiler);
    ("store", store_profiler);
    ("chain_validator", chain_validator_profiler);
    ("block_validator", block_validator_profiler);
    ("peer_validator", peer_validator_profiler);
    ("merge", merge_profiler);
    ("p2p_reader", p2p_reader_profiler);
    ("requester", requester_profiler);
    ("rpc_server", rpc_server_profiler);
  ]

let activate_all ~profiler_maker =
  List.iter
    (fun (name, p) ->
      match profiler_maker ~name with
      | Some instance -> plug p instance
      | None -> ())
    all_profilers
