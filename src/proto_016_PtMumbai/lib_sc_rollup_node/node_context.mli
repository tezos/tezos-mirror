(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** This module describes the execution context of the node. *)

open Protocol
open Alpha_context

type lcc = {commitment : Sc_rollup.Commitment.Hash.t; level : Raw_level.t}

(** Abstract type for store to force access through this module. *)
type 'a store constraint 'a = [< `Read | `Write > `Read]

type debug_logger = string -> unit Lwt.t

type 'a t = {
  cctxt : Protocol_client_context.full;
      (** Client context used by the rollup node. *)
  dal_cctxt : Dal_node_client.cctxt option;
      (** DAL client context to query the dal node, if the rollup node supports
          the DAL. *)
  data_dir : string;  (** Node data dir. *)
  l1_ctxt : Layer1.t;
      (** Layer 1 context to fetch blocks and monitor heads, etc.*)
  rollup_address : Sc_rollup.t;  (** Smart rollup tracked by the rollup node. *)
  mode : Configuration.mode;
      (** Mode of the node, see {!Configuration.mode}. *)
  operators : Configuration.operators;
      (** Addresses of the rollup node operators by purposes. *)
  genesis_info : Sc_rollup.Commitment.genesis_info;
      (** Origination information of the smart rollup. *)
  injector_retention_period : int;
      (** Number of blocks the injector will keep information about included
          operations. *)
  block_finality_time : int;
      (** Deterministic block finality time for the layer 1 protocol. *)
  kind : Sc_rollup.Kind.t;  (** Kind of the smart rollup. *)
  fee_parameters : Configuration.fee_parameters;
      (** Fee parameters to use when injecting operations in layer 1. *)
  protocol_constants : Constants.t;
      (** Protocol constants retrieved from the Tezos node. *)
  loser_mode : Loser_mode.t;
      (** If different from [Loser_mode.no_failures], the rollup node
          issues wrong commitments (for tests). *)
  lockfile : Lwt_unix.file_descr;
      (** A lock file acquired when the node starts. *)
  store : 'a store;  (** The store for the persistent storage. *)
  context : 'a Context.index;
      (** The persistent context for the rollup node. *)
  lcc : ('a, lcc) Reference.t;  (** Last cemented commitment and its level. *)
  lpc : ('a, Sc_rollup.Commitment.t option) Reference.t;
      (** The last published commitment, i.e. commitment that the operator is
          staked on. *)
  kernel_debug_logger : debug_logger;
      (** Logger used for writing [kernel_debug] messages *)
  finaliser : unit -> unit Lwt.t;
      (** Aggregation of finalisers to run when the node context closes *)
}

(** Read/write node context {!t}. *)
type rw = [`Read | `Write] t

(** Read only node context {!t}. *)
type ro = [`Read] t

(** [get_operator cctxt purpose] returns the public key hash for the operator
    who has purpose [purpose], if any.
*)
val get_operator :
  _ t ->
  Configuration.purpose ->
  Tezos_crypto.Signature.Public_key_hash.t option

(** [is_operator cctxt pkh] returns [true] if the public key hash [pkh] is an
    operator for the node (for any purpose). *)
val is_operator : _ t -> Tezos_crypto.Signature.Public_key_hash.t -> bool

(** [is_accuser node_ctxt] returns [true] if the rollup node runs in accuser
    mode.  *)
val is_accuser : _ t -> bool

(** [is_loser node_ctxt] returns [true] if the rollup node runs has some
    failures planned. *)
val is_loser : _ t -> bool

(** [get_fee_parameter cctxt purpose] returns the fee parameter to inject an
    operation for a given [purpose]. If no specific fee parameters were
    configured for this purpose, returns the default fee parameter for this
    purpose.
*)
val get_fee_parameter : _ t -> Configuration.purpose -> Injection.fee_parameter

(** [init cctxt ~data_dir mode configuration] initializes the rollup
    representation. The rollup origination level and kind are fetched via an RPC
    call to the layer1 node that [cctxt] uses for RPC requests.
*)
val init :
  Protocol_client_context.full ->
  data_dir:string ->
  ?log_kernel_debug_file:string ->
  'a Store_sigs.mode ->
  Configuration.t ->
  'a t tzresult Lwt.t

(** Closes the store, context and Layer 1 monitor. *)
val close : _ t -> unit tzresult Lwt.t

(** [checkout_context node_ctxt block_hash] returns the context at block
    [block_hash]. *)
val checkout_context : 'a t -> Block_hash.t -> 'a Context.t tzresult Lwt.t

(** [metadata node_ctxt] creates a {Sc_rollup.Metadata.t} using the information
    stored in [node_ctxt]. *)
val metadata : _ t -> Sc_rollup.Metadata.t

(** Returns [true] if the rollup node supports the DAL and if DAL is enabled for
    the current protocol. *)
val dal_supported : _ t -> bool

(** [readonly node_ctxt] returns a read only version of the node context
    [node_ctxt].  *)
val readonly : _ t -> ro

(** Monad for values with delayed write effects in the node context. *)
type 'a delayed_write = ('a, rw) Delayed_write_monad.t

(** {2 Abstraction over store} *)

(** {3 Layer 2 blocks} *)

(** [is_processed store hash] returns [true] if the block with [hash] has
    already been processed by the daemon. *)
val is_processed : _ t -> Block_hash.t -> bool tzresult Lwt.t

(** [get_l2_block t hash] returns the Layer 2 block known by the rollup node for
    Layer 1 block [hash]. *)
val get_l2_block : _ t -> Block_hash.t -> Sc_rollup_block.t tzresult Lwt.t

(** Same as {!get_l2_block} but returns [None] when the Layer 2 block is not
    available. *)
val find_l2_block :
  _ t -> Block_hash.t -> Sc_rollup_block.t option tzresult Lwt.t

(** Same as {!get_l2_block} but retrieves the Layer 2 block by its level. *)
val get_l2_block_by_level : _ t -> int32 -> Sc_rollup_block.t tzresult Lwt.t

(** Same as {!get_l2_block_by_level} but returns [None] when the Layer 2 block
    is not available. *)
val find_l2_block_by_level :
  _ t -> int32 -> Sc_rollup_block.t option tzresult Lwt.t

(** [get_full_l2_block node_ctxt hash] returns the full L2 block for L1 block
    hash [hash]. The result contains the L2 block and its content (inbox,
    messages, commitment). *)
val get_full_l2_block :
  _ t -> Block_hash.t -> Sc_rollup_block.full tzresult Lwt.t

(** [save_level t head] registers the correspondences [head.level |->
    head.hash] in the store. *)
val save_level : rw -> Layer1.head -> unit tzresult Lwt.t

(** [save_l2_head t l2_block] remembers that the [l2_block.head] is
    processed. The system should not have to come back to it. *)
val save_l2_head : rw -> Sc_rollup_block.t -> unit tzresult Lwt.t

(** [last_processed_head_opt store] returns the last processed head if it
    exists. *)
val last_processed_head_opt : _ t -> Sc_rollup_block.t option tzresult Lwt.t

(** [mark_finalized_head store head] remembers that the [head] is finalized. By
    construction, every block whose level is smaller than [head]'s is also
    finalized. *)
val mark_finalized_head : rw -> Block_hash.t -> unit tzresult Lwt.t

(** [last_finalized_head_opt store] returns the last finalized head if it exists. *)
val get_finalized_head_opt : _ t -> Sc_rollup_block.t option tzresult Lwt.t

(** [hash_of_level node_ctxt level] returns the current block hash for a given
    [level]. *)
val hash_of_level : _ t -> int32 -> Block_hash.t tzresult Lwt.t

(** [hash_of_level_opt] is like {!hash_of_level} but returns [None] if the
    [level] is not known. *)
val hash_of_level_opt : _ t -> int32 -> Block_hash.t option tzresult Lwt.t

(** [level_of_hash node_ctxt hash] returns the level for Tezos block hash [hash]
    if it is known by the Tezos Layer 1 node. *)
val level_of_hash : _ t -> Block_hash.t -> int32 tzresult Lwt.t

(** [get_predecessor_opt state head] returns the predecessor of block [head],
    when [head] is not the genesis block. *)
val get_predecessor_opt :
  _ t -> Layer1.head -> Layer1.head option tzresult Lwt.t

(** [get_predecessor state head] returns the predecessor block of [head]. *)
val get_predecessor : _ t -> Layer1.head -> Layer1.head tzresult Lwt.t

(** [nth_predecessor n head] return [block, history] where [block] is the nth
    predecessor of [head] and [history] is the list of blocks between [block]
    (excluded) and [head] (included) on the chain. *)
val nth_predecessor :
  _ t -> int -> Layer1.head -> (Layer1.head * Layer1.head list) tzresult Lwt.t

(** [get_tezos_reorg_for_new_head node_ctxt old_head new_head] returns the L1
    reorganization between [old_head] and [new_head]. *)
val get_tezos_reorg_for_new_head :
  _ t ->
  [`Head of Layer1.head | `Level of int32] ->
  Layer1.head ->
  Layer1.head Reorg.t tzresult Lwt.t

(** [block_with_tick store ~max_level tick] returns [Some b] where [b] is the
    last layer 2 block which contains the [tick] before [max_level]. If no such
    block exists (the tick happened after [max_level], or we are too late), the
    function returns [None]. *)
val block_with_tick :
  _ t ->
  max_level:Raw_level.t ->
  Sc_rollup.Tick.t ->
  Sc_rollup_block.t option tzresult Lwt.t

(** {3 Commitments} *)

(** [get_commitment t hash] returns the commitment with [hash] stored by the
    rollup node. *)
val get_commitment :
  _ t -> Sc_rollup.Commitment.Hash.t -> Sc_rollup.Commitment.t tzresult Lwt.t

(** Same as {!get_commitment} but returns [None] if this commitment hash is not
    known by the rollup node. *)
val find_commitment :
  _ t ->
  Sc_rollup.Commitment.Hash.t ->
  Sc_rollup.Commitment.t option tzresult Lwt.t

(** [commitment_exists t hash] returns [true] if the commitment with [hash] is
    known (i.e. stored) by the rollup node. *)
val commitment_exists :
  _ t -> Sc_rollup.Commitment.Hash.t -> bool tzresult Lwt.t

(** [save_commitment t commitment] saves a commitment in the store an returns is
    hash. *)
val save_commitment :
  rw -> Sc_rollup.Commitment.t -> Sc_rollup.Commitment.Hash.t tzresult Lwt.t

(** [commitment_published_at_level t hash] returns the levels at which the
    commitment was first published and the one at which it was included by in a
    Layer 1 block. It returns [None] if the commitment is not known by the
    rollup node or if it was never published by the rollup node (and included on
    L1). *)
val commitment_published_at_level :
  _ t ->
  Sc_rollup.Commitment.Hash.t ->
  Store.Commitments_published_at_level.element option tzresult Lwt.t

(** [save_commitment_published_at_level t hash levels] saves the
    publication/inclusion information for a commitment with [hash]. *)
val set_commitment_published_at_level :
  rw ->
  Sc_rollup.Commitment.Hash.t ->
  Store.Commitments_published_at_level.element ->
  unit tzresult Lwt.t

type commitment_source = Anyone | Us

(** [commitment_was_published t hash] returns [true] if the commitment is known
    as being already published on L1. The [source] indicates if we want to know
    the publication status for commitments we published ourselves [`Us] or that
    [`Anyone] published. *)
val commitment_was_published :
  _ t ->
  source:commitment_source ->
  Sc_rollup.Commitment.Hash.t ->
  bool tzresult Lwt.t

(** {3 Inboxes} *)

type messages_info = {
  predecessor : Block_hash.t;
  predecessor_timestamp : Timestamp.t;
  messages : Sc_rollup.Inbox_message.t list;
}

(** [get_inbox t inbox_hash] retrieves the inbox whose hash is [inbox_hash] from
    the rollup node's storage. *)
val get_inbox :
  _ t -> Sc_rollup.Inbox.Hash.t -> Sc_rollup.Inbox.t tzresult Lwt.t

(** Same as {!get_inbox} but returns [None] if this inbox is not known. *)
val find_inbox :
  _ t -> Sc_rollup.Inbox.Hash.t -> Sc_rollup.Inbox.t option tzresult Lwt.t

(** [save_inbox t inbox] remembers the [inbox] in the storage. It is associated
    to its hash which is returned. *)
val save_inbox :
  rw -> Sc_rollup.Inbox.t -> Sc_rollup.Inbox.Hash.t tzresult Lwt.t

(** [inbox_of_head node_ctxt block] returns the latest inbox at the given
    [block]. This function always returns [inbox] for all levels at and
    after the rollup genesis. NOTE: It requires the L2 block for [block.hash] to
    have been saved. *)
val inbox_of_head : _ t -> Layer1.head -> Sc_rollup.Inbox.t tzresult Lwt.t

(** Same as {!get_inbox} but uses the Layer 1 block hash for this inbox instead. *)
val get_inbox_by_block_hash :
  _ t -> Block_hash.t -> Sc_rollup.Inbox.t tzresult Lwt.t

(** [genesis_inbox t] is the genesis inbox for the rollup [t.sc_rollup_address]. *)
val genesis_inbox : _ t -> Sc_rollup.Inbox.t tzresult Lwt.t

(** [get_messages t witness_hash] retrieves the messages for the merkelized
    payloads hash [witness_hash] stored by the rollup node. *)
val get_messages :
  _ t ->
  Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t ->
  messages_info tzresult Lwt.t

(** Same as {!get_messages} but returns [None] if the payloads hash is not known. *)
val find_messages :
  _ t ->
  Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t ->
  messages_info option tzresult Lwt.t

(** [get_num_messages t witness_hash] retrieves (without reading all the messages
    from disk) the number of messages for the inbox witness [witness_hash]
    stored by the rollup node. *)
val get_num_messages :
  _ t -> Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t -> int tzresult Lwt.t

(** [save_messages t payloads_hash messages] associates the list of [messages]
    to the [payloads_hash]. The payload hash must be computed by calling,
    e.g. {!Sc_rollup.Inbox.add_all_messages}. *)
val save_messages :
  rw ->
  Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t ->
  messages_info ->
  unit tzresult Lwt.t

(** {3 DAL} *)

(** [get_slot_header t ~published_in_block_hash slot_index] returns the slot
    header for the [slot_index] that was published in the provided block hash on
    Layer 1. *)
val get_slot_header :
  _ t ->
  published_in_block_hash:Block_hash.t ->
  Dal.Slot_index.t ->
  Dal.Slot.Header.t tzresult Lwt.t

(** [get_all_slot_headers t ~published_in_block_hash] returns the slot headers
    for all the slots that were published in the provided block hash on Layer
    1. *)
val get_all_slot_headers :
  _ t ->
  published_in_block_hash:Block_hash.t ->
  Dal.Slot.Header.t list tzresult Lwt.t

(** [get_slot_indexes t ~published_in_block_hash] returns the slot indexes whose
    headers were published in the provided block hash on Layer 1. *)
val get_slot_indexes :
  _ t ->
  published_in_block_hash:Block_hash.t ->
  Dal.Slot_index.t list tzresult Lwt.t

(** [save_slot_header t ~published_in_block_hash header] saves the [header] as
    being published for its index in the provided block hash on Layer 1. *)
val save_slot_header :
  rw ->
  published_in_block_hash:Block_hash.t ->
  Dal.Slot.Header.t ->
  unit tzresult Lwt.t

(** [processed_slot t ~confirmed_in_block_hash index] returns [None] if the slot
    pages was never processed nor downloaded, [Some `Unconfirmed] if the slot
    was not confirmed and [Some `Confirmed] if the slot is confirmed and the
    associated pages are available. *)
val processed_slot :
  _ t ->
  confirmed_in_block_hash:Block_hash.t ->
  Dal.Slot_index.t ->
  [`Unconfirmed | `Confirmed] option tzresult Lwt.t

(** [list_slot_pages t ~confirmed_in_block_hash] lists all slots and their pages
    that were confirmed and stored by the rollup node for
    [confirmed_in_block_hash]. *)
val list_slot_pages :
  _ t ->
  confirmed_in_block_hash:Block_hash.t ->
  ((Dal.Slot_index.t * int) * bytes) list tzresult Lwt.t

(** [find_slot_page t ~confirmed_in_block_hash slot_index page_index] retrieves
    a pages (with index [page_index]) for a slot [slot_index] that was confirmed
    in the provided block hash. It returns [None] if the slot was not processed
    or if the page index is out of bounds. *)
val find_slot_page :
  _ t ->
  confirmed_in_block_hash:Block_hash.t ->
  slot_index:Dal.Slot_index.t ->
  page_index:int ->
  bytes option tzresult Lwt.t

(** [save_unconfirmed_slot node_ctxt hash slot_index] saves in [node_ctxt.store]
    that [slot_index] is unconfirmed in the block with hash in [node_ctxt.store].
*)
val save_unconfirmed_slot :
  rw -> Block_hash.t -> Dal.Slot_index.t -> unit tzresult Lwt.t

(** [save_confirmed_slot node_ctxt hash slot_index] saves in [node_ctxt.store]
    that [slot_index] is confirmed in the block with hashin [node_ctxt.store].
    The contents of the slot are set to [pages] in [node_ctxt.store]. *)
val save_confirmed_slot :
  rw -> Block_hash.t -> Dal.Slot_index.t -> bytes list -> unit tzresult Lwt.t

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4636
   Missing docstrings. *)

val find_confirmed_slots_history :
  _ t -> Block_hash.t -> Dal.Slots_history.t option tzresult Lwt.t

val save_confirmed_slots_history :
  rw -> Block_hash.t -> Dal.Slots_history.t -> unit tzresult Lwt.t

val find_confirmed_slots_histories :
  _ t -> Block_hash.t -> Dal.Slots_history.History_cache.t option tzresult Lwt.t

val save_confirmed_slots_histories :
  rw -> Block_hash.t -> Dal.Slots_history.History_cache.t -> unit tzresult Lwt.t

(**/**)

module Internal_for_tests : sig
  (** Create a node context which really stores data on disk but does not
      connect to any layer 1 node. It is meant to be used in unit tests for the
      rollup node functions. *)
  val create_node_context :
    Protocol_client_context.full ->
    ?constants:Constants.Parametric.t ->
    data_dir:string ->
    Sc_rollup.Kind.t ->
    Store_sigs.rw t tzresult Lwt.t
end
