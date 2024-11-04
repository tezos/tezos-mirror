(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Profiler

let nonce_profiler = unplugged ()

let operation_worker_profiler = unplugged ()

let node_rpc_profiler = unplugged ()

(* This is the main profiler for the baker *)
let baker_profiler = unplugged ()

let all_profilers =
  [
    ("nonce", nonce_profiler);
    ("op_worker", operation_worker_profiler);
    ("node_rpc", node_rpc_profiler);
    ("baker", baker_profiler);
  ]

let activate_all ~profiler_maker =
  List.iter
    (fun (name, p) ->
      match profiler_maker ~name with
      | Some instance ->
          plug p instance ;
          (* This environment profiler was added to get insights on the signature checking. *)
          if name = "baker" then
            plug
              Tezos_protocol_environment.Environment_profiler
              .environment_profiler
              instance
      | None -> ())
    all_profilers

let create_reset_block_section =
  Profiler.section_maker Block_hash.equal Block_hash.to_b58check
