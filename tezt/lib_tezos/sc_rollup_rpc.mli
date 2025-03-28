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

type outbox = Transactions of JSON.t list

(** Rollup node block type. Incomplete type, to be completed when
    needs arise. *)
type block = {
  block_hash : string;
  previous_commitment_hash : string;
  level : int;
  inbox : RPC.smart_rollup_inbox;
  messages : string list;
  outbox : outbox list;
}

(** RPC: [GET global/block/<block>?outbox]. *)
val get_global_block : ?block:string -> ?outbox:bool -> unit -> block RPC_core.t

(** RPC: [GET global/block/<block>/inbox]. *)
val get_global_block_inbox :
  ?block:string -> unit -> RPC.smart_rollup_inbox RPC_core.t

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

(** RPC: [GET local/batcher/queue] returns the queue of messages, as pairs of
    message id and binary message, in the batcher. *)
val get_local_batcher_queue : unit -> (string * string) list RPC_core.t

(** RPC: [GET local/batcher/queue/<msg_id>] fetches the message whose id is
    [msg_id] from the queue. It returns the message together with the full JSON
    response including the status.*)
val get_local_batcher_queue_msg_id :
  msg_id:string -> (string * string) RPC_core.t

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

type commitment_and_hash = {
  commitment : RPC.smart_rollup_commitment;
  hash : string;
}

type commitment_info = {
  commitment_and_hash : commitment_and_hash;
  first_published_at_level : int option;
  published_at_level : int option;
}

(** [commitment_info_from_json] parses a commitment, its hash and
    the levels when the commitment was first published (if any) and included,
    from the JSON representation. *)
val commitment_info_from_json : JSON.t -> commitment_info

(** RPC: [GET global/last_stored_commitment] gets the last commitment with its hash
    stored by the rollup node.  *)
val get_global_last_stored_commitment : unit -> commitment_and_hash RPC_core.t

(** RPC: [GET local/last_published_commitment] gets the last commitment published by the
    rollup node, with its hash and level when the commitment was first published
    and the level it was included. *)
val get_local_last_published_commitment : unit -> commitment_info RPC_core.t

(** RPC: [GET local/commitments] gets commitment by its [hash] from the rollup node,
    with its hash and level when the commitment was first published and the
    level it was included. *)
val get_local_commitments :
  commitment_hash:string -> unit -> commitment_info RPC_core.t

type gc_info = {
  first_available_level : int;
  last_gc_started_at : int option;
  last_context_split_level : int option;
  last_successful_gc_target : int option;
}

(** RPC: [GET local/gc_info] returns garbage collection information. *)
val get_local_gc_info : unit -> gc_info RPC_core.t

(** RPC: [GET global/block/<block>/state] gets the corresponding PVM state value
    mapped to [key] for the [block] (default ["head"]). *)
val get_global_block_state :
  ?block:string -> key:string -> unit -> bytes RPC_core.t

type 'output_type durable_state_operation =
  | Value : string option durable_state_operation
  | Length : int64 option durable_state_operation
  | Subkeys : string list durable_state_operation
  | Values : (string * string) list durable_state_operation

(** RPC: [GET global/block/<block>/durable/<pvm_kind>/<operation>] gets the
    corresponding durable PVM state information (depending on [operation]) mapped to [key] for the [block]
    (default ["head"]). *)
val get_global_block_durable_state_value :
  ?block:string ->
  pvm_kind:string ->
  operation:'a durable_state_operation ->
  key:string ->
  unit ->
  'a RPC_core.t

(** RPC: [POST local/batcher/injection] injects the [messages] in the
    queue the rollup node's batcher and returns the list of message
    hashes injected. *)
val post_local_batcher_injection :
  ?order:int ->
  ?drop_duplicate:bool ->
  messages:string list ->
  unit ->
  string list RPC_core.t

(** RPC: [POST local/dal/batcher/injection] injects the given
    [messages] in the rollup node's DAL queue. *)
val post_local_dal_batcher_injection : messages:string list -> unit RPC_core.t

(** RPC: [POST local/dal/slot/indices] sets the given DAL [slot_indices] to be
    used when injecting DAL slots. *)
val post_dal_slot_indices : slot_indices:int list -> unit RPC_core.t

val get_dal_injected_operations_statuses : unit -> JSON.t list RPC_core.t

type outbox_proof = {commitment_hash : string; proof : string}

(** RPC: [GET global/block/<block>/helpers/proofs/outbox/<outbox_level>/messages] *)
val outbox_proof_simple :
  ?block:string ->
  outbox_level:int ->
  message_index:int ->
  unit ->
  outbox_proof option RPC_core.t

type outbox_msg = {message_index : int; message : JSON.t}

(** RPC: [GET /local/outbox/pending/executable] *)
val get_local_outbox_pending_executable :
  unit -> (int * outbox_msg list) list RPC_core.t

(** RPC: [GET /local/outbox/pending/unexecutable] *)
val get_local_outbox_pending_unexecutable :
  unit -> (int * outbox_msg list) list RPC_core.t

(** RPC: [DELETE /admin/batcher/queue] *)
val delete_admin_batcher_queue :
  ?order_below:int -> ?drop_no_order:bool -> unit -> unit RPC_core.t

(** RPC: [GET /admin/injector/queues] *)
val get_admin_injector_queues :
  ?tag:string -> unit -> (string list * JSON.t list) list RPC_core.t

(** RPC: [GET /admin/injector/queues/total] *)
val get_admin_injector_queues_total :
  ?tag:string -> unit -> ((string list * int) list * int) RPC_core.t

(** RPC: [DELETE /admin/injector/queues] *)
val delete_admin_injector_queues :
  ?operation_tag:string ->
  ?order_below:int ->
  ?drop_no_order:bool ->
  unit ->
  unit RPC_core.t
