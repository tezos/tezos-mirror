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
