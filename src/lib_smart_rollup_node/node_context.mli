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

type genesis_info = {level : int32; commitment_hash : Commitment.Hash.t}

(** Abstract type for store to force access through this module. *)
type 'a store constraint 'a = [< `Read | `Write > `Read]

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

type 'a t = {
  config : Configuration.t;  (** Inlined configuration for the rollup node. *)
  cctxt : Client_context.full;  (** Client context used by the rollup node. *)
  dal_cctxt : Dal_node_client.cctxt option;
      (** DAL client context to query the dal node, if the rollup node supports
          the DAL. *)
  dac_client : Dac_observer_client.t option;
      (** DAC observer client to optionally pull in preimages *)
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
  lockfile : Lwt_unix.file_descr;
      (** A lock file acquired when the node starts. *)
  store : 'a store;  (** The store for the persistent storage. *)
  context : 'a Context.index;
      (** The persistent context for the rollup node. *)
  lcc : ('a, lcc) Reference.t;
      (** Last cemented commitment on L1 (independently of synchronized status
          of rollup node) and its level. *)
  lpc : ('a, Commitment.t option) Reference.t;
      (** The last published commitment on L1, i.e. commitment that the operator
          is staked on (even if the rollup node is not synchronized). *)
  private_info : ('a, private_info option) Reference.t;
      (** contains information for the rollup when it's private.*)
  kernel_debug_logger : debug_logger;
      (** Logger used for writing [kernel_debug] messages *)
  finaliser : unit -> unit Lwt.t;
      (** Aggregation of finalisers to run when the node context closes *)
  mutable current_protocol : current_protocol;
      (** Information about the current protocol. This value is changed in place
          on protocol upgrades. *)
  global_block_watcher : Sc_rollup_block.t Lwt_watcher.input;
      (** Watcher for the L2 chain, which enables RPC services to access
          a stream of L2 blocks. *)
}

(** Read/write node context {!t}. *)
type rw = [`Read | `Write] t

(** Read only node context {!t}. *)
type ro = [`Read] t

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

(** [init cctxt ~data_dir mode l1_ctxt genesis_info protocol configuration]
    initializes the rollup representation. The rollup origination level and kind
    are fetched via an RPC call to the layer1 node that [cctxt] uses for RPC
    requests.
*)
val init :
  #Client_context.full ->
  data_dir:string ->
  irmin_cache_size:int ->
  index_buffer_size:int ->
  ?log_kernel_debug_file:string ->
  ?last_whitelist_update:Z.t * Int32.t ->
  'a Store_sigs.mode ->
  Layer1.t ->
  genesis_info ->
  lcc:lcc ->
  lpc:Commitment.t option ->
  Kind.t ->
  current_protocol ->
  Configuration.t ->
  'a t tzresult Lwt.t

(** Closes the store, context and Layer 1 monitor. *)
val close : _ t -> unit tzresult Lwt.t

(** The path for the lockfile used in block processing. *)
val processing_lockfile_path : data_dir:string -> string

(** The path for the lockfile used in garbage collection. *)
val gc_lockfile_path : data_dir:string -> string

(** [checkout_context node_ctxt block_hash] returns the context at block
    [block_hash]. *)
val checkout_context : 'a t -> Block_hash.t -> 'a Context.t tzresult Lwt.t

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

(** [save_l2_block t l2_block] remembers that the [l2_block] is processed. The
    system should not have to come back to it. *)
val save_l2_block : rw -> Sc_rollup_block.t -> unit tzresult Lwt.t

(** [set_l2_head t l2_block] sets [l2_block] as the new head of the L2 chain. *)
val set_l2_head : rw -> Sc_rollup_block.t -> unit tzresult Lwt.t

(** [last_processed_head_opt store] returns the last processed head if it
    exists. *)
val last_processed_head_opt : _ t -> Sc_rollup_block.t option tzresult Lwt.t

(** [mark_finalized_head store head] remembers that the [head] is finalized. By
    construction, every block whose level is smaller than [head]'s is also
    finalized. *)
val mark_finalized_level : rw -> int32 -> unit tzresult Lwt.t

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

(** Same as {!get_predecessor_opt} with headers. *)
val get_predecessor_header_opt :
  _ t -> Layer1.header -> Layer1.header option tzresult Lwt.t

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

(** [block_with_tick store ~max_level tick] returns [Some b] where [b] is the
    last layer 2 block which contains the [tick] before [max_level]. If no such
    block exists (the tick happened after [max_level], or we are too late), the
    function returns [None]. *)
val block_with_tick :
  _ t -> max_level:int32 -> Z.t -> Sc_rollup_block.t option tzresult Lwt.t

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
val save_commitment : rw -> Commitment.t -> Commitment.Hash.t tzresult Lwt.t

(** [commitment_published_at_level t hash] returns the levels at which the
    commitment was first published and the one at which it was included by in a
    Layer 1 block. It returns [None] if the commitment is not known by the
    rollup node or if it was never published by the rollup node (and included on
    L1). *)
val commitment_published_at_level :
  _ t ->
  Commitment.Hash.t ->
  Store.Commitments_published_at_level.element option tzresult Lwt.t

(** [save_commitment_published_at_level t hash levels] saves the
    publication/inclusion information for a commitment with [hash]. *)
val set_commitment_published_at_level :
  rw ->
  Commitment.Hash.t ->
  Store.Commitments_published_at_level.element ->
  unit tzresult Lwt.t

type commitment_source = Anyone | Us

(** [commitment_was_published t hash] returns [true] if the commitment is known
    as being already published on L1. The [source] indicates if we want to know
    the publication status for commitments we published ourselves [`Us] or that
    [`Anyone] published. *)
val commitment_was_published :
  _ t -> source:commitment_source -> Commitment.Hash.t -> bool tzresult Lwt.t

(** [set_lcc t lcc] saves the LCC both on disk and in the node context. It's written in the context iff [lcc] is is younger than its current value. *)
val set_lcc : rw -> lcc -> unit tzresult Lwt.t

(** [register_published_commitment t c ~first_published_at_level ~level
    ~published_by_us] saves the publishing information for commitment [c] both
    on disk and in the node context. We remember the first publication level
    and the level the commitment was published by us. *)
val register_published_commitment :
  rw ->
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
  rw ->
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

(** Returns messages as they are stored in the store, unsafe to use because all
    messages may not be present. Use {!Messages.get} instead.  *)
val unsafe_find_stored_messages :
  _ t ->
  Merkelized_payload_hashes_hash.t ->
  (string list * Block_hash.t) option tzresult Lwt.t

(** [get_num_messages t witness_hash] retrieves the number of messages for the
    inbox witness [witness_hash] stored by the rollup node. *)
val get_num_messages :
  _ t -> Merkelized_payload_hashes_hash.t -> int tzresult Lwt.t

(** [save_messages t payloads_hash ~predecessor messages] associates the list of
    [messages] to the [payloads_hash]. The payload hash must be computed by
    calling, e.g. {!Sc_rollup.Inbox.add_all_messages}. *)
val save_messages :
  rw ->
  Merkelized_payload_hashes_hash.t ->
  predecessor:Block_hash.t ->
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
  rw -> Layer1.header -> predecessor:Layer1.header -> unit tzresult Lwt.t

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
  rw ->
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
  rw ->
  Block_hash.t ->
  Dal.Slot_index.t ->
  [`Confirmed | `Unconfirmed] ->
  unit tzresult Lwt.t

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4636
   Missing docstrings. *)

val find_confirmed_slots_history :
  _ t -> Block_hash.t -> Dal.Slot_history.t option tzresult Lwt.t

val save_confirmed_slots_history :
  rw -> Block_hash.t -> Dal.Slot_history.t -> unit tzresult Lwt.t

val find_confirmed_slots_histories :
  _ t -> Block_hash.t -> Dal.Slot_history_cache.t option tzresult Lwt.t

val save_confirmed_slots_histories :
  rw -> Block_hash.t -> Dal.Slot_history_cache.t -> unit tzresult Lwt.t

(** [gc node_ctxt level] triggers garbage collection for the node in accordance
    with [node_ctxt.config.gc_parameters]. Upon completion, all data for L2
    levels lower than [level] will be removed. *)
val gc : [> `Write] t -> level:int32 -> unit tzresult Lwt.t

(** [get_gc_levels node_ctxt] returns information about the garbage collected
    levels. *)
val get_gc_levels : _ t -> Store.Gc_levels.levels tzresult Lwt.t

(** [check_level_available node_ctxt level] resolves with an error if the
    [level] is before the first non garbage collected level. *)
val check_level_available : _ t -> int32 -> unit tzresult Lwt.t

(** {2 Helpers} *)

(** [make_kernel_logger event ?log_kernel_debug_file logs_dir] returns two
    functions [kernel_debug_logger] and [finaliser], to be used in the node
    context. [kernel_debug_logger] writes kernel logs to
    [logs_dir/log_kernel_debug_file] and emits them with the [event]. *)
val make_kernel_logger :
  (string -> unit Lwt.t) ->
  ?log_kernel_debug_file:string ->
  string ->
  ((string -> unit Lwt.t) * (unit -> unit Lwt.t)) Lwt.t

(**/**)

module Internal_for_tests : sig
  (** Create a node context which really stores data on disk but does not
      connect to any layer 1 node. It is meant to be used in unit tests for the
      rollup node functions. *)
  val create_node_context :
    #Client_context.full ->
    current_protocol ->
    data_dir:string ->
    Kind.t ->
    Store_sigs.rw t tzresult Lwt.t

  (** Extract the underlying store from the node context. This function is
      unsafe to use outside of tests as it breaks the abstraction barrier
      provided by the [Node_context]. *)
  val unsafe_get_store : 'a t -> 'a Store.t

  (** Create a dummy context to generate OpenAPI specification. *)
  val openapi_context :
    #Client_context.full -> Protocol_hash.t -> Store_sigs.rw t tzresult Lwt.t
end
