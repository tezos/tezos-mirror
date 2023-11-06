(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*****************************************************************************)

(** RPC: [GET global/block/<block>/ticks] gets the number of ticks for the PVM 
    for the [block] (default ["head"]). *)
val get_global_block_ticks : ?block:string -> unit -> int RPC_core.t

(** RPC: [GET global/block/<block>/state_hash] gets the corresponding PVM state
    hash for the [block] (default ["head"]). *)
val get_global_block_state_hash : ?block:string -> unit -> string RPC_core.t

(** RPC: [GET global/block/<block>/total_ticks] gets the total number of ticks 
    for the PVM. *)
val get_global_block_total_ticks : ?block:string -> unit -> int RPC_core.t

(** RPC: [GET global/block/<block>/state_current_level] gets the corresponding 
    PVM state current level for the [block] (default ["head"]). *)
val get_global_block_state_current_level :
  ?block:string -> unit -> int RPC_core.t
