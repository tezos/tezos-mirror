(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Profiler

let gossipsub_profiler = unplugged ()

let init profiler_maker =
  match profiler_maker ~name:"gossipsub" with
  | Some instance -> plug gossipsub_profiler instance
  | None -> ()

let create_reset_block_section =
  Profiler.section_maker Block_hash.equal Block_hash.to_b58check
