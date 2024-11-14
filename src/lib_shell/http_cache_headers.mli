(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(**  Expose helper tools used for Http cache header middleware. *)

type tools = {
  (* [get_estimated_time_to_next_level ()] gets the estimated time
     to the next level of the main chain *)
  get_estimated_time_to_next_level : unit -> Ptime.span option Lwt.t;
  (* [get_block_hash block_alias] gets the block hash given a block
     alias string *)
  get_block_hash : string -> Block_hash.t option Lwt.t;
}

val make_tools : (unit -> Store.chain_store option) -> tools
