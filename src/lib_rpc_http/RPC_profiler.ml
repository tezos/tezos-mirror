(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*                                                                           *)
(*****************************************************************************)

open Profiler

let rpc_client_profiler = unplugged ()

let init profiler_maker =
  plug rpc_client_profiler (profiler_maker ~name:"rpc_client")

let create_reset_block_section =
  Profiler.section_maker Block_hash.equal Block_hash.to_b58check
