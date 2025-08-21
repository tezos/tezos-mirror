(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Profiler

(** Unplugged DAL node profiler. *)
val dal_profiler : profiler

(** Plug the DAL node profiler given its name and Profiler instance option. *)
val init : (name:string -> instance option) -> unit

(** Creates a function to reset the block section *)
val create_reset_block_section : profiler -> Block_hash.t * metadata -> unit
