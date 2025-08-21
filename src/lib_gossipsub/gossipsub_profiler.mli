(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Profiler

(** Unplugged GossipSub worker profiler. *)
val gossipsub_profiler : profiler

(** Plug the GossipSub worker profiler given its name and Profiler instance option. *)
val init : (name:string -> instance option) -> unit

(** Creates a function to reset the block section *)
val create_reset_block_section : profiler -> Block_hash.t * metadata -> unit
