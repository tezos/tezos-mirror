(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Unplugged agnostic baker profiler. *)
val agnostic_baker_profiler : Profiler.profiler

(** Plug the agnostic baker profiler given its name and Profiler instance option. *)
val init : (name:string -> Profiler.instance option) -> unit

(** Creates a function to reset the block section *)
val create_reset_block_section :
  Profiler.profiler -> Block_hash.t * Profiler.metadata -> unit
