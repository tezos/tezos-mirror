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

let init profiler_maker =
  let baker_instance = profiler_maker ~name:"baker" in
  plug baker_profiler baker_instance ;
  (* This environment profiler was added to get insights on the signature checking. *)
  plug
    Tezos_protocol_environment.Environment_profiler.environment_profiler
    baker_instance ;
  plug nonce_profiler (profiler_maker ~name:"nonce") ;
  plug node_rpc_profiler (profiler_maker ~name:"node_rpc") ;
  plug operation_worker_profiler (profiler_maker ~name:"op_worker")

let create_reset_block_section profiler =
  let last_block = ref None in
  fun b ->
    match !last_block with
    | None ->
        record profiler (Block_hash.to_b58check b) ;
        last_block := Some b
    | Some b' when Block_hash.equal b' b -> ()
    | Some _ ->
        stop profiler ;
        record profiler (Block_hash.to_b58check b) ;
        last_block := Some b
