(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*                                                                           *)
(*****************************************************************************)

(** Unplugged RPC client profiler. *)
val rpc_client_profiler : Profiler.profiler

(** Plug the RPC client profiler given its name and Profiler instance. *)
val init : (name:string -> Profiler.instance) -> unit

(** Creates a function to reset the block section *)
val create_reset_block_section :
  Profiler.profiler -> Block_hash.t * Profiler.metadata -> unit
