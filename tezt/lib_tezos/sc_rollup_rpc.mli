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

(** RPC: [GET global/block/<block>/status]  gets the corresponding PVM status for
    the [block] (default ["head"]). *)
val get_global_block_status : ?block:string -> unit -> string RPC_core.t

(** RPC: [GET global/block/<block>/outbox/<outbox_level>/messages] gets the rollup
   outbox of [outbox_level] as known to the [block] (default ["cemented"] which
   is the block corresponding to the last cemented level). *)
val get_global_block_outbox :
  ?block:string -> outbox_level:int -> unit -> JSON.t RPC_core.t

(** RPC: [GET global/smart_rollup_address] returns the smart contract rollup
    address of the node. *)
val get_global_smart_rollup_address : unit -> string RPC_core.t

(** RPC: [GET global/block/<block>]. *)
val get_global_block : ?block:string -> unit -> JSON.t RPC_core.t

(** RPC: [GET global/block/<block>/inbox]. *)
val get_global_block_inbox : ?block:string -> unit -> JSON.t RPC_core.t

(** RPC: [GET global/block/<block>/hash]. *)
val get_global_block_hash : ?block:string -> unit -> JSON.t RPC_core.t

(** RPC: [GET global/block/<block>/level]. *)
val get_global_block_level : ?block:string -> unit -> JSON.t RPC_core.t

(** RPC: [GET global/block/<block>/num_messages]. *)
val get_global_block_num_messages : ?block:string -> unit -> JSON.t RPC_core.t

(** RPC: [GET global/tezos_head]. *)
val get_global_tezos_head : unit -> JSON.t RPC_core.t

(** RPC: [GET global/tezos_level]. *)
val get_global_tezos_level : unit -> JSON.t RPC_core.t

type slot_header = {level : int; commitment : string; index : int}

(** RPC: [GET global/block/<block>/dal/slot_headers] returns the dal slot headers
    of the [block] (default ["head"]).  *)
val get_global_block_dal_slot_headers :
  ?block:string -> unit -> slot_header list RPC_core.t

(** RPC: [GET local/batcher/queue] returns the queue of messages, as pairs of message
    hash and binary message, in the batcher. *)
val get_local_batcher_queue : unit -> (string * string) list RPC_core.t

(** RPC: [GET local/batcher/queue/<msg_hash>] fetches the message whose hash is [hash]
    from the queue. It returns the message together with the full JSON response
    including the status.*)
val get_local_batcher_queue_msg_hash :
  msg_hash:string -> (string * string) RPC_core.t

type simulation_result = {
  state_hash : string;
  status : string;
  output : JSON.t;
  inbox_level : int;
  num_ticks : int;
  insights : string option list;
}

(** RPC: [POST global/block/<block>/simulate] simulates the evaluation of input
    [messages] for the rollup PVM at [block] (default ["head"]). [reveal_pages]
    can be used to provide data to be used for the revelation ticks.
    [insight_request] can be used to look at a list of keys in the PVM state after
    the simulation. *)
val post_global_block_simulate :
  ?block:string ->
  ?reveal_pages:string list ->
  ?insight_requests:
    [< `Durable_storage_key of string list | `Pvm_state_key of string list] list ->
  string list ->
  simulation_result RPC_core.t

(** RPC: [GET global/block/<block>/dal/processed_slots] returns the slots indices
    that have been marked by the rollup node as confirmed or unconfirmed for block
    [block] (default ["head"]), with their statuses.  *)
val get_global_block_dal_processed_slots :
  ?block:string -> unit -> (int * string) list RPC_core.t
