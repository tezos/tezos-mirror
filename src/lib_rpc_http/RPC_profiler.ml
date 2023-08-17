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
