(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

type lcc = {commitment : Commitment.Hash.t; level : int32}

type genesis_info = Metadata.genesis_info = {
  level : int32;
  commitment_hash : Commitment.Hash.t;
}

(** Abstract type for store to force access through this module. *)
type 'a store constraint 'a = [< `Read | `Write > `Read]

(** Exposed functions to manipulate Node_context store outside of this module *)
module Node_store : sig
  val init : 'a Access_mode.t -> data_dir:string -> 'a store tzresult Lwt.t

  (** [close store] closes the store *)
  val close : 'a store -> unit Lwt.t

  (** [check_and_set_history_mode store history_mode] checks the
    compatibility between given history mode and that of the store.
    History mode can be converted from Archive to Full. Trying to
    convert from Full to Archive will trigger an error.*)
  val check_and_set_history_mode :
    'a Access_mode.t ->
    'a store ->
    Configuration.history_mode option ->
    unit tzresult Lwt.t

  val of_store : 'a Store.t -> 'a store
end

type debug_logger = string -> unit Lwt.t

type current_protocol = {
  hash : Protocol_hash.t;  (** Hash of the current protocol. *)
  proto_level : int;
      (** Protocol supported by this rollup node (represented as a protocol
          level). *)
  constants : Rollup_constants.protocol_constants;
      (** Protocol constants retrieved from the Tezos node. *)
}

type last_whitelist_update = {message_index : int; outbox_level : Int32.t}

type private_info = {
  last_whitelist_update : last_whitelist_update;
  last_outbox_level_searched : int32;
      (** If the rollup is private then the last search outbox level
          when looking at whitelist update to execute. This is to
          reduce the folding call at each cementation. If the rollup
          is public then it's None. *)
}

type sync_info = {
  on_synchronized : unit Lwt_condition.t;
  mutable processed_level : int32;
  sync_level_input : int32 Lwt_watcher.input;
}

type 'a t = {
  config : Configuration.t;  (** Inlined configuration for the rollup node. *)
  cctxt : Client_context.full;  (** Client context used by the rollup node. *)
  degraded : bool Reference.rw;  (** Track if node running in degraded mode. *)
  dal_cctxt : Tezos_dal_node_lib.Dal_node_client.cctxt option;
      (** DAL client context to query the dal node, if the rollup node supports
          the DAL. *)
  data_dir : string;  (** Node data dir. *)
  l1_ctxt : Layer1.t;
      (** Layer 1 context to fetch blocks and monitor heads, etc.*)
  genesis_info : genesis_info;
      (** Origination information of the smart rollup. *)
  injector_retention_period : int;
      (** Number of blocks the injector will keep information about included
          operations. *)
  block_finality_time : int;
      (** Deterministic block finality time for the layer 1 protocol. *)
  kind : Kind.t;  (** Kind of the smart rollup. *)
  unsafe_patches : Pvm_patches.t;  (** Patches to apply to the PVM. *)
  lockfile : Lwt_unix.file_descr;
      (** A lock file acquired when the node starts. *)
  store : 'store store;  (** The store for the persistent storage. *)
  context : 'context Context.t;
      (** The persistent context for the rollup node. *)
  lcc : lcc Reference.rw;
      (** Last cemented commitment on L1 (independently of synchronized status
          of rollup node) and its level. *)
  lpc : Commitment.t option Reference.rw;
      (** The last published commitment on L1, i.e. commitment that the operator
          is staked on (even if the rollup node is not synchronized). *)
  private_info : private_info option Reference.rw;
      (** contains information for the rollup when it's private.*)
  kernel_debug_logger : debug_logger;
      (** Logger used for writing [kernel_debug] messages *)
  finaliser : unit -> unit Lwt.t;
      (** Aggregation of finalisers to run when the node context closes *)
  current_protocol : current_protocol Reference.rw;
      (** Information about the current protocol. This value is changed in place
          on protocol upgrades. *)
  global_block_watcher : Sc_rollup_block.t Lwt_watcher.input;
      (** Watcher for the L2 chain, which enables RPC services to access
          a stream of L2 blocks. *)
  finalized_block_watcher : Sc_rollup_block.t Lwt_watcher.input;
      (** Watcher for the finalized L2 blocks. *)
  sync : sync_info;  (** Synchronization status with respect to the L1 node.  *)
}
  constraint 'a = < store : 'store ; context : 'context >

(** Read/write node context {!t}. *)
type rw = < store : [`Read | `Write] ; context : [`Read | `Write] > t

(** Read only node context {!t}. *)
type ro = < store : [`Read] ; context : [`Read] > t

(** Node_context with read/write access to store. *)
type 'a rw_store = < store : [`Read | `Write] ; context : 'a > t

(** Node_context with read/write access to context. *)
type 'a rw_context = < store : 'a ; context : [`Read | `Write] > t

(** [get_operator cctxt purpose] returns the public key hash for the operator
    who has purpose [purpose], if any.
*)
val get_operator : _ t -> 'a Purpose.t -> 'a Purpose.operator option

(** [is_operator cctxt pkh] returns [true] if the public key hash [pkh] is an
    operator for the node (for any purpose). *)
val is_operator : _ t -> Signature.Public_key_hash.t -> bool

(** [is_accuser node_ctxt] returns [true] if the rollup node runs in accuser
    mode.  *)
val is_accuser : _ t -> bool

(** [is_bailout node_ctxt] returns [true] if the rollup node runs in bailout
    mode.  *)
val is_bailout : _ t -> bool

(** [is_loser node_ctxt] returns [true] if the rollup node runs has some
    failures planned. *)
val is_loser : _ t -> bool

(** [can_inject config op_kind] determines if a given operation kind can
    be injected based on the configuration settings. *)
val can_inject : _ t -> Operation_kind.t -> bool

(** [check_op_in_whitelist_or_bailout_mode node_ctxt whitelist] Checks
    when the rollup node is operating to determine if the operator is in the
    whitelist or if the rollup node is in bailout mode. Bailout mode
    does not publish any commitments but still defends previously
    committed one. *)
val check_op_in_whitelist_or_bailout_mode :
  _ t -> Signature.Public_key_hash.t list -> unit tzresult

(** [get_fee_parameter cctxt purpose] returns the fee parameter to inject an
    operation for a given [purpose]. If no specific fee parameters were
    configured for this purpose, returns the default fee parameter for this
    purpose.
*)
val get_fee_parameter : _ t -> Operation_kind.t -> Injector_common.fee_parameter

(** The path for the lockfile used when starting and running the node. *)
val global_lockfile_path : data_dir:string -> string

(** The path for the lockfile used in block processing. *)
val processing_lockfile_path : data_dir:string -> string

(** The path for the lockfile used in garbage collection. *)
val gc_lockfile_path : data_dir:string -> string

(** [checkout_context node_ctxt block_hash] returns the context at block
    [block_hash]. *)
val checkout_context :
  < store : _ ; context : 'a > t -> Block_hash.t -> 'a Context.t tzresult Lwt.t

(** Returns [true] if the rollup node supports the DAL and if DAL is enabled for
    the current protocol. *)
val dal_supported : _ t -> bool

(** [readonly node_ctxt] returns a read only version of the node context
    [node_ctxt].  *)
val readonly : _ t -> ro

val readonly_store :
  < store : _ ; context : 'a > t -> < store : Access_mode.ro ; context : 'a > t

val readonly_context :
  < store : 'a ; context : _ > t -> < store : 'a ; context : Access_mode.ro > t

(** {2 Abstraction over store} *)

(** [get_history_mode t] returns the current history mode for the rollup
    node. *)
val get_history_mode : _ t -> Configuration.history_mode tzresult Lwt.t

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

(** [get_full_l2_block ?get_outbox_messages node_ctxt hash] returns the full L2
    block for L1 block hash [hash]. The result contains the L2 block and its
    content (inbox, messages, commitment). If a function to retrieve outbox
    messages is provided, the result also contains the outbox for this block.

    NOTE: When given, [get_outbox_messages] is always instantiated with the
    appropriate [Pvm_plugin.get_outbox_messages] function which is dependent on
    the protocol. Passing this function around allows to break a dependency
    cycle between {!Node_context} and {!Protocol_plugins}. *)
val get_full_l2_block :
  ?get_outbox_messages:
    ('a t ->
    Context.pvmstate ->
    outbox_level:int32 ->
    (int * Outbox_message.summary) list Lwt.t) ->
  'a t ->
  Block_hash.t ->
  Sc_rollup_block.full tzresult Lwt.t

(** [save_level t head] registers the correspondences [head.level |->
    head.hash] in the store. *)
val save_level : _ rw_store -> Layer1.head -> unit tzresult Lwt.t

(** [save_l2_block t l2_block] remembers that the [l2_block] is processed. The
    system should not have to come back to it. *)
val save_l2_block : _ rw_store -> Sc_rollup_block.t -> unit tzresult Lwt.t

(** [set_l2_head t l2_block] sets [l2_block] as the new head of the L2 chain. *)
val set_l2_head : _ rw_store -> Sc_rollup_block.t -> unit tzresult Lwt.t

(** [last_processed_head_opt store] returns the last processed head if it
    exists. *)
val last_processed_head_opt : _ t -> Sc_rollup_block.t option tzresult Lwt.t

(** [mark_finalized_head store hash level] remembers that the block with [hash]
    at [level] is finalized. By construction, every block whose level is smaller
    than [level] is also finalized. *)
val set_finalized : _ rw_store -> Block_hash.t -> int32 -> unit tzresult Lwt.t

(** [get_finalized_level t] returns the last finalized level. *)
val get_finalized_level : _ t -> int32 tzresult Lwt.t

(** [get_finalized_head_opt store] returns the last finalized head if it exists. *)
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

(** Returns the block header for a given hash using the L1 node. *)
val header_of_hash : _ t -> Block_hash.t -> Layer1.header tzresult Lwt.t

(** [get_predecessor_opt state head] returns the predecessor of block [head],
    when [head] is not the genesis block. *)
val get_predecessor_opt :
  _ t -> Layer1.head -> Layer1.head option tzresult Lwt.t

(** [get_predecessor state head] returns the predecessor block of [head]. *)
val get_predecessor : _ t -> Layer1.head -> Layer1.head tzresult Lwt.t

(** Same as {!get_predecessor} with headers. *)
val get_predecessor_header :
  _ t -> Layer1.header -> Layer1.header tzresult Lwt.t

(** [get_tezos_reorg_for_new_head node_ctxt old_head new_head] returns the L1
    reorganization between [old_head] and [new_head]. *)
val get_tezos_reorg_for_new_head :
  _ t ->
  [`Head of Layer1.head | `Level of int32] ->
  Layer1.head ->
  Layer1.head Reorg.t tzresult Lwt.t

(** [block_with_tick store ?min_level ~max_level tick] returns [Some b] where
    [b] is the last layer 2 block which contains the [tick] before
    [max_level]. If no such block exists (the tick happened after [max_level],
    or we are too late), the function returns [None]. Fails if the tick happened
    before [min_level]. *)
val block_with_tick :
  _ t ->
  ?min_level:int32 ->
  max_level:int32 ->
  Z.t ->
  Sc_rollup_block.t option tzresult Lwt.t

(** [tick_offset_of_commitment_period node_ctxt commtient] returns the global
    initial tick (since genesis) of the PVM for the state at the beginning of the
    commitment period that [commitment] concludes. *)
val tick_offset_of_commitment_period : _ t -> Commitment.t -> Z.t tzresult Lwt.t

(** {3 Commitments} *)

(** [get_commitment t hash] returns the commitment with [hash] stored by the
    rollup node. *)
val get_commitment : _ t -> Commitment.Hash.t -> Commitment.t tzresult Lwt.t

(** Same as {!get_commitment} but returns [None] if this commitment hash is not
    known by the rollup node. *)
val find_commitment :
  _ t -> Commitment.Hash.t -> Commitment.t option tzresult Lwt.t

(** [commitment_exists t hash] returns [true] if the commitment with [hash] is
    known (i.e. stored) by the rollup node. *)
val commitment_exists : _ t -> Commitment.Hash.t -> bool tzresult Lwt.t

(** [save_commitment t commitment] saves a commitment in the store an returns is
    hash. *)
val save_commitment :
  _ rw_store -> Commitment.t -> Commitment.Hash.t tzresult Lwt.t

(** [commitment_published_at_level t hash] returns the levels at which the
    commitment was first published and the one at which it was included by in a
    Layer 1 block. It returns [None] if the commitment is not known by the
    rollup node or if it was never published by the rollup node (and included on
    L1). *)
val commitment_published_at_level :
  _ t ->
  Commitment.Hash.t ->
  Store.Commitments_published_at_levels.publication_levels option tzresult Lwt.t

(** [save_commitment_published_at_level t hash levels] saves the
    publication/inclusion information for a commitment with [hash]. *)
val set_commitment_published_at_level :
  _ rw_store ->
  Commitment.Hash.t ->
  Store.Commitments_published_at_levels.publication_levels ->
  unit tzresult Lwt.t

type commitment_source = Anyone | Us

(** [commitment_was_published t hash] returns [true] if the commitment is known
    as being already published on L1. The [source] indicates if we want to know
    the publication status for commitments we published ourselves [`Us] or that
    [`Anyone] published. *)
val commitment_was_published :
  _ t -> source:commitment_source -> Commitment.Hash.t -> bool tzresult Lwt.t

(** [set_lcc t lcc] saves the LCC both on disk and in the node context. It's written in the context iff [lcc] is is younger than its current value. *)
val set_lcc : _ rw_store -> lcc -> unit tzresult Lwt.t

(** [register_published_commitment t c ~first_published_at_level ~level
    ~published_by_us] saves the publishing information for commitment [c] both
    on disk and in the node context. We remember the first publication level
    and the level the commitment was published by us. *)
val register_published_commitment :
  _ rw_store ->
  Commitment.t ->
  first_published_at_level:int32 ->
  level:int32 ->
  published_by_us:bool ->
  unit tzresult Lwt.t

(** {3 Inboxes} *)

(** [get_inbox t inbox_hash] retrieves the inbox whose hash is [inbox_hash] from
    the rollup node's storage. *)
val get_inbox :
  _ t ->
  Octez_smart_rollup.Inbox.Hash.t ->
  Octez_smart_rollup.Inbox.t tzresult Lwt.t

(** Same as {!get_inbox} but returns [None] if this inbox is not known. *)
val find_inbox :
  _ t ->
  Octez_smart_rollup.Inbox.Hash.t ->
  Octez_smart_rollup.Inbox.t option tzresult Lwt.t

(** [save_inbox t inbox] remembers the [inbox] in the storage. It is associated
    to its hash which is returned. *)
val save_inbox :
  _ rw_store ->
  Octez_smart_rollup.Inbox.t ->
  Octez_smart_rollup.Inbox.Hash.t tzresult Lwt.t

(** [inbox_of_head node_ctxt block] returns the latest inbox at the given
    [block]. This function always returns [inbox] for all levels at and
    after the rollup genesis. NOTE: It requires the L2 block for [block.hash] to
    have been saved. *)
val inbox_of_head :
  _ t -> Layer1.head -> Octez_smart_rollup.Inbox.t tzresult Lwt.t

(** Same as {!get_inbox} but uses the Layer 1 block hash for this inbox instead. *)
val get_inbox_by_block_hash :
  _ t -> Block_hash.t -> Octez_smart_rollup.Inbox.t tzresult Lwt.t

(** Same as {!get_inbox} but uses the inbox level for this inbox instead. *)
val get_inbox_by_level : _ t -> int32 -> Inbox.t tzresult Lwt.t

(** Returns messages for a payload hash, including protocol messages. *)
val find_messages :
  _ t -> Merkelized_payload_hashes_hash.t -> string list option tzresult Lwt.t

(** Same as {!find_messages} but fails if not messages are stored. *)
val get_messages :
  _ t -> Merkelized_payload_hashes_hash.t -> string list tzresult Lwt.t

(** [get_num_messages t witness_hash] retrieves the number of messages for the
    inbox witness [witness_hash] stored by the rollup node. *)
val get_num_messages :
  _ t -> Merkelized_payload_hashes_hash.t -> int tzresult Lwt.t

(** [save_messages t payloads_hash ~level messages] associates the list of
    [messages] to the [payloads_hash]. The payload hash must be computed by
    calling, e.g. {!Sc_rollup.Inbox.add_all_messages}. *)
val save_messages :
  _ rw_store ->
  Merkelized_payload_hashes_hash.t ->
  level:int32 ->
  string list ->
  unit tzresult Lwt.t

(** Return values for {!protocol_of_level}. *)
type proto_info = {
  proto_level : int;
      (** Protocol level for operations of block (can be different from L1
          header value in the case of a migration block). *)
  first_level_of_protocol : bool;
      (** [true] if the level is the first of the protocol. *)
  protocol : Protocol_hash.t;
      (** Hash of the {e current} protocol for this level. *)
}

(** {3 Outbox messages} *)

(** Marks the outbox message at index for a given outbox level as executed in
    the persistent storage. *)
val set_outbox_message_executed :
  _ rw_store -> outbox_level:int32 -> index:int -> unit tzresult Lwt.t

(** [register_outbox_messages node_ctxt ~outbox_level ~indexes] registers the
    messages indexes for the [outbox_level]. If messages were already registered
    for this level, they are overwritten. Messages marked as executed are
    preserved. *)
val register_outbox_messages :
  _ rw_store -> outbox_level:int32 -> indexes:int list -> unit tzresult Lwt.t

(** Returns the pending messages (i.e. unexecuted) that can now be executed.
    The returned list contains outbox levels and indexes for each level (in
    order). *)
val get_executable_pending_outbox_messages :
  ?outbox_level:int32 -> _ t -> (int32 * int list) list tzresult Lwt.t

(** Returns the pending messages (i.e. unexecuted) that cannot be executed yet.
    The returned list contains outbox levels and indexes for each level (in
    order). *)
val get_unexecutable_pending_outbox_messages :
  ?outbox_level:int32 -> _ t -> (int32 * int list) list tzresult Lwt.t

(** Returns all pending messages with their status. *)
val get_pending_outbox_messages :
  ?outbox_level:int32 ->
  _ t ->
  ((int32 * int list) * [`Executable | `Lost | `Pending]) list tzresult Lwt.t

(** {3 Protocol} *)

(** [protocol_of_level_with_store store level] returns the protocol of block level [level]. *)
val protocol_of_level_with_store :
  _ Store.t -> int32 -> proto_info tzresult Lwt.t

(** [protocol_of_level t level] returns the protocol of block level [level]. *)
val protocol_of_level : _ t -> int32 -> proto_info tzresult Lwt.t

(** Returns the last protocol seen by the rollup node. *)
val last_seen_protocol : _ t -> Protocol_hash.t option tzresult Lwt.t

(** Returns the activation level of a protocol or fails if the protocol was
    never seen by the rollup node. *)
val protocol_activation_level :
  _ t -> Protocol_hash.t -> Store.Protocols.level tzresult Lwt.t

(** [save_protocol_info t block ~predecessor] saves to disk the protocol
    information associated to the [block], if there is a protocol change
    between [block] and [predecessor]. *)
val save_protocol_info :
  _ rw_store ->
  Layer1.header ->
  predecessor:Layer1.header ->
  unit tzresult Lwt.t

(** Save the protocol activation levels from L1 if possible. *)
val save_protocols_from_l1 : _ rw_store -> unit tzresult Lwt.t

(** {3 DAL} *)

(** [get_slot_header t ~published_in_block_hash slot_index] returns the slot
    header for the [slot_index] that was published in the provided block hash on
    Layer 1. *)
val get_slot_header :
  _ t ->
  published_in_block_hash:Block_hash.t ->
  Dal.Slot_index.t ->
  Dal.Slot_header.t tzresult Lwt.t

(** [get_all_slot_headers t ~published_in_block_hash] returns the slot headers
    for all the slots that were published in the provided block hash on Layer
    1. *)
val get_all_slot_headers :
  _ t ->
  published_in_block_hash:Block_hash.t ->
  Dal.Slot_header.t list tzresult Lwt.t

(** [get_slot_indexes t ~published_in_block_hash] returns the slot indexes whose
    headers were published in the provided block hash on Layer 1. *)
val get_slot_indexes :
  _ t ->
  published_in_block_hash:Block_hash.t ->
  Dal.Slot_index.t list tzresult Lwt.t

(** [save_slot_header t ~published_in_block_hash header] saves the [header] as
    being published for its index in the provided block hash on Layer 1. *)
val save_slot_header :
  _ rw_store ->
  published_in_block_hash:Block_hash.t ->
  Dal.Slot_header.t ->
  unit tzresult Lwt.t

(** [find_slot_status t ~confirmed_in_block_hash index] returns [None] if the
    slot's block is not processed yet, [Some `Unconfirmed] if the slot was not
    confirmed and [Some `Confirmed] if the slot is confirmed and the associated
    pages are available. *)
val find_slot_status :
  _ t ->
  confirmed_in_block_hash:Block_hash.t ->
  Dal.Slot_index.t ->
  [`Unconfirmed | `Confirmed] option tzresult Lwt.t

(** [list_slots_statuses t ~confirmed_in_block_hash] lists the list of
    slots indices with their respective statuses that are saved for the given
    [confirmed_in_block_hash] *)
val list_slots_statuses :
  _ t ->
  confirmed_in_block_hash:Block_hash.t ->
  (Dal.Slot_index.t * [`Confirmed | `Unconfirmed]) list tzresult Lwt.t

(** [save_slot_status node_ctxt hash slot_index status] saves in
    [node_ctxt.store] that [slot_index] has status [status] in the
    block with hash in [node_ctxt.store].
*)
val save_slot_status :
  _ rw_store ->
  Block_hash.t ->
  Dal.Slot_index.t ->
  [`Confirmed | `Unconfirmed] ->
  unit tzresult Lwt.t

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4636
   Missing docstrings. *)

(** [gc ?wait_finished ?force node_ctxt level] triggers garbage collection for
    the node in accordance with [node_ctxt.config.gc_parameters]. Upon
    completion, all data for L2 levels lower than [level] will be removed. If
    [wait_finished] is [true], the call blocks until the GC completes, otherwise
    it is run asynchronously. When [force = true], a GC is triggered
    independently of [node_ctxt.config.gc_parameters]. *)
val gc :
  ?wait_finished:bool -> ?force:bool -> rw -> level:int32 -> unit tzresult Lwt.t

(** [cancel_gc t] stops any currently ongoing GC. It returns [true] if a GC was
    canceled. *)
val cancel_gc : _ rw_context -> bool Lwt.t

type gc_level = {gc_triggered_at : int32; gc_target : int32}

(** [get_gc_info node_ctxt step] returns information about the garbage
    collected levels. If [step] is [`Started], it returns information for the
    last started GC and if it's [`Successful], it returns information for the
    last successful GC. *)
val get_gc_info :
  _ t -> [`Started | `Successful] -> gc_level option tzresult Lwt.t

(** The first non garbage collected level available in the node. *)
val first_available_level : _ t -> int32 tzresult Lwt.t

(** [check_level_available node_ctxt level] resolves with an error if the
    [level] is before the first non garbage collected level. *)
val check_level_available : _ t -> int32 -> unit tzresult Lwt.t

(** The splitting period for the context, as defined in the configuration or
    challenge window / 5. *)
val splitting_period : _ t -> int

val get_last_context_split_level : _ t -> int32 option tzresult Lwt.t

val save_context_split_level : _ rw_store -> int32 -> unit tzresult Lwt.t

(** {2 Synchronization tracking} *)

(** [is_synchronized node_ctxt] returns [true] iff the rollup node has processed
    the latest available L1 head. *)
val is_synchronized : _ t -> bool

(** [wait_synchronized node_ctxt] is a promise that resolves when the rollup
    node whose state is [node_ctxt] is synchronized with L1. If the node is
    already synchronized, it resolves immediately. *)
val wait_synchronized : _ t -> unit Lwt.t

(** {2 Kernel tracing} *)

(** Reset the kernel tracing with the current time and provided scope. Call this
    function before executing the PVM/kernel. *)
val reset_kernel_tracing : Opentelemetry.Scope.t -> unit

(** [make_kernel_logger ~enable_tracing ?log_kernel_debug_file ~logs_dir config
    event] returns two functions [kernel_debug_logger] and [finaliser], to be
    used in the node context. [kernel_debug_logger] writes kernel logs to
    [logs_dir/log_kernel_debug_file] and emits them with the [event]
    function. In addition if [enable_tracing = true] then information is
    extracted from the kernel logs to emit Opentelemetry traces for
    Etherlink. *)
val make_kernel_logger :
  enable_tracing:bool ->
  ?log_kernel_debug_file:string ->
  logs_dir:string ->
  Configuration.t ->
  (string -> unit Lwt.t) ->
  ((string -> unit Lwt.t) * (unit -> unit Lwt.t)) Lwt.t

module Internal_for_tests : sig
  val write_protocols_in_store :
    [> `Write] store -> Store.Protocols.proto_info list -> unit tzresult Lwt.t

  (** Extract the underlying store from the node context. This function is
           unsafe to use outside of tests as it breaks the abstraction barrier
           provided by the [Node_context]. *)
  val unsafe_get_store : < store : 'a ; .. > t -> 'a Store.t
end
