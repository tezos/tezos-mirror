(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Profiler

let nonce_profiler = unplugged ()

let operation_worker_profiler = unplugged ()

let dal_attestable_slots_worker_profiler = unplugged ()

let node_rpc_profiler = unplugged ()

(* This is the main profiler for the baker *)
let baker_profiler = unplugged ()

(* This environment profiler was added to get insights on the signature checking. *)
let environment_profiler =
  Tezos_protocol_environment.Environment_profiler.environment_profiler

let all_profilers =
  [
    ("nonce", [nonce_profiler]);
    ("op_worker", [operation_worker_profiler]);
    ("dal_slots_worker", [dal_attestable_slots_worker_profiler]);
    ("node_rpc", [node_rpc_profiler]);
    ("baker", [baker_profiler; environment_profiler]);
  ]

let activate_all ~profiler_maker =
  List.iter
    (fun (name, profilers) ->
      Option.iter
        (fun instance -> List.iter (fun p -> plug p instance) profilers)
        (profiler_maker ~name))
    all_profilers

let create_reset_block_section =
  Profiler.section_maker Block_hash.equal Block_hash.to_b58check

module Baker_profiler = (val Profiler.wrap baker_profiler)

module Node_rpc_Profiler = struct
  include (val Profiler.wrap node_rpc_profiler)

  let[@warning "-32"] reset_block_section =
    create_reset_block_section node_rpc_profiler
end

module RPC_profiler = struct
  include (val Tezos_profiler.Profiler.wrap RPC_profiler.rpc_client_profiler)

  let[@warning "-32"] reset_block_section =
    RPC_profiler.create_reset_block_section RPC_profiler.rpc_client_profiler
end

module Operation_worker_profiler = (val Profiler.wrap operation_worker_profiler)

module Nonce_profiler = (val Profiler.wrap nonce_profiler)
