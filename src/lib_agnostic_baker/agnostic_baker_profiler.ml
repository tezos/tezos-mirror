(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)
open Profiler

let agnostic_baker_profiler = unplugged ()

let init profiler_maker =
  match profiler_maker ~name:"agnostic_baker" with
  | Some instance -> plug agnostic_baker_profiler instance
  | None -> ()

let create_reset_block_section =
  section_maker Block_hash.equal Block_hash.to_b58check
