(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*                                                                           *)
(*****************************************************************************)

open Profiler

let rpc_client_profiler = unplugged ()

let init ~profiling_config profiler_maker =
  match profiler_maker ~profiling_config ~name:"rpc_client" with
  | Some instance -> plug rpc_client_profiler instance
  | None -> ()

let create_reset_block_section =
  Profiler.section_maker Block_hash.equal Block_hash.to_b58check
