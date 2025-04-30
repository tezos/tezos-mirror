(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Type of store (a handler to the underlying database). *)
type 'a t

(** Read/write store {!t}. *)
type rw = Store_sigs.rw t

(** Read only store {!t}. *)
type ro = Store_sigs.ro t

(** Name of SQLite file in data directory. *)
val sqlite_file_name : string

(** Other files that make up the store. *)
val extra_sqlite_files : string list

(** [init mode ~data_dir] initializes the store and returns it. *)
val init : 'a Store_sigs.mode -> data_dir:string -> 'a t tzresult Lwt.t

(** Close the store by freeing all resources and closing database
    connections. *)
val close : _ t -> unit Lwt.t

(** Returns a read-only version of the store. *)
val readonly : _ t -> ro

(** [gc store ~level] garbage collects data that relate to levels below [level]
    (by removing information from the database). *)
val gc : rw -> level:int32 -> unit tzresult Lwt.t

(** [export_store ~data_dir ~output_db_file] exports the store database with
    data from the [data_dir] into the [output_db_file]. This function also
    removes data that is specific to the operator. This function is meant to be
    used to produce snapshots. *)
val export_store :
  data_dir:string -> output_db_file:string -> unit tzresult Lwt.t

(** [with_transaction store f] executes [f] with a single connection to the
    database in a transaction. I.e., if [f] returns an error or raises an
    exception, the store will not be modified.  *)
val with_transaction :
  _ t -> (Sqlite.conn -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

(** {2 Modules for storing data in the store}

    Each module maps to a table in the database. Every read and write function
    has two parameters: [store] and an optional [?conn].

    When [~conn] is provided, the database connection is used directly to
    execute the queries. If not, a new (or old) connection is taken from the
    pool of [store].
*)

(** Storage containing commitments and corresponding commitment hashes that the
    rollup node has knowledge of. *)
module Commitments : sig
  (** [store ?conn s commitment] stores a new [commitment] associated to its
      hash and returns it. *)
  val store :
    ?conn:Sqlite.conn -> rw -> Commitment.t -> Commitment.Hash.t tzresult Lwt.t

  (** Retrieve a commitment by its hash. *)
  val find :
    ?conn:Sqlite.conn ->
    _ t ->
    Commitment.Hash.t ->
    Commitment.t option tzresult Lwt.t

  (** Retrieve a commitment corresponding to the LCC, the last cemented
      commitment. *)
  val find_lcc :
    ?conn:Sqlite.conn -> _ t -> unit -> Commitment.t option tzresult Lwt.t

  (** Retrieve the last commitment published by the rollup node
      (as known from the handled L1 blocks). *)
  val find_lpc : ?conn:Sqlite.conn -> _ t -> Commitment.t option tzresult Lwt.t
end

(** Storage mapping commitment hashes to the level when they were published by
    the rollup node. *)
module Commitments_published_at_levels : sig
  type publication_levels = {
    first_published_at_level : int32;
        (** The level at which this commitment was first published. *)
    published_at_level : int32 option;
        (** The level at which we published this commitment. If
            [first_published_at_level <> published_at_level] it means that the
            commitment is republished. *)
  }

  (** [register ?conn s hash levels] stores the publication levels for
      commitment whose hash is [hash].  *)
  val register :
    ?conn:Sqlite.conn ->
    rw ->
    Commitment.Hash.t ->
    publication_levels ->
    unit tzresult Lwt.t

  (** Retrieve the publication levels for a commitment by its hash. *)
  val get :
    ?conn:Sqlite.conn ->
    _ t ->
    Commitment.Hash.t ->
    publication_levels option tzresult Lwt.t

  (** Retrieve the level at which a commitment was first published. *)
  val get_first_published_level :
    ?conn:Sqlite.conn -> _ t -> Commitment.Hash.t -> int32 option tzresult Lwt.t
end

(** Aggregated collection of messages from the L1 inbox *)
module Inboxes : sig
  (** [store ?conn s inbox] stores the [inbox], associated to its hash and
      returns it. *)
  val store : ?conn:Sqlite.conn -> rw -> Inbox.t -> Inbox_hash.t tzresult Lwt.t

  (** Retrieve an inbox by its hash. *)
  val find :
    ?conn:Sqlite.conn -> _ t -> Inbox_hash.t -> Inbox.t option tzresult Lwt.t

  (** Retrieve an inbox by its the block hash in which it appeared. *)
  val find_by_block_hash :
    ?conn:Sqlite.conn -> _ t -> Block_hash.t -> Inbox.t option tzresult Lwt.t
end

(** Storage for persisting messages downloaded from the L1 node. *)
module Messages : sig
  (** [store ?conn s ~level hash messages] stores the [messages] who's payload
      hash is [hash] that appeared in inbox level [~level]. *)
  val store :
    ?conn:Sqlite.conn ->
    rw ->
    level:int32 ->
    Merkelized_payload_hashes_hash.t ->
    string list ->
    unit tzresult Lwt.t

  (** Retrieve messages by their payload hash. *)
  val find :
    ?conn:Sqlite.conn ->
    _ t ->
    Merkelized_payload_hashes_hash.t ->
    string list option tzresult Lwt.t
end

(** Storage for persisting outbox messages. *)
module Outbox_messages : sig
  (** [pending ?conn s ~min_level ~max_level] returns all pending (i.e. non
      executed) outbox messages between outbox levels [min_level] and
      [max_level]. The result is given as a list of pairs whose first component
      is the outbox level and the second is the message indexes list. *)
  val pending :
    ?conn:Sqlite.conn ->
    _ t ->
    min_level:int32 ->
    max_level:int32 ->
    (int32 * int list) list tzresult Lwt.t

  (** Register outbox messages for a given outbox level by its indexes. *)
  val register_outbox_messages :
    ?conn:Sqlite.conn ->
    rw ->
    outbox_level:int32 ->
    indexes:Bitset.t ->
    unit tzresult Lwt.t

  (** Register an outbox message as executed by its outbox level and its index
      in the outbox. *)
  val set_outbox_message_executed :
    ?conn:Sqlite.conn ->
    rw ->
    outbox_level:int32 ->
    index:int ->
    unit tzresult Lwt.t
end

(** Storage for protocol activation levels. *)
module Protocols : sig
  type level = First_known of int32 | Activation_level of int32

  (** Each element of this type represents information we have about a Tezos
      protocol regarding its activation. *)
  type proto_info = {
    level : level;
        (** The level at which we have seen the protocol for the first time,
            either because we saw its activation or because the first block we
            saw (at the origination of the rollup) was from this protocol. *)
    proto_level : int;
        (** The protocol level, i.e. its number in the sequence of protocol
            activations on the chain. *)
    protocol : Protocol_hash.t;  (** The protocol this information concerns. *)
  }

  (** Store a new protocol with its activation level. NOTE: if the protocol hash
      is already registered, it will be overwritten.  *)
  val store : ?conn:Sqlite.conn -> rw -> proto_info -> unit tzresult Lwt.t

  (** Retrieve protocol information by protocol hash. *)
  val find :
    ?conn:Sqlite.conn ->
    _ t ->
    Protocol_hash.t ->
    proto_info option tzresult Lwt.t

  (** [proto_of_level ?conn s level] returns the protocol in which [level]
      appears. It returns [None] if [level] is before the activation of the
      first known protocol. *)
  val proto_of_level :
    ?conn:Sqlite.conn -> _ t -> int32 -> proto_info option tzresult Lwt.t

  (** Returns the last protocol by activation level. *)
  val last : ?conn:Sqlite.conn -> _ t -> proto_info option tzresult Lwt.t
end

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/7952

   Remove Dal_slots_headers and Dal_slots_statuses tables.
*)

(** Published slot headers per block hash, stored as a list of bindings from
    [Dal_slot_index.t] to [Dal.Slot.t]. The encoding function converts this list
    into a [Dal.Slot_index.t]-indexed map. *)
module Dal_slots_headers : sig
  (** [store ?conn s block slot_header] stores the slot header [slot_header] as
      being published in the L1 block whose hash is [block]. *)
  val store :
    ?conn:Sqlite.conn ->
    rw ->
    Block_hash.t ->
    Dal.Slot_header.t ->
    unit tzresult Lwt.t

  (** Retrieve a published slot header by its publication block and slot
      index. *)
  val find_slot_header :
    ?conn:Sqlite.conn ->
    _ t ->
    Block_hash.t ->
    slot_index:Dal.Slot_index.t ->
    Dal.Slot_header.t option tzresult Lwt.t

  (** List all slot headers published in a block. *)
  val list_slot_headers :
    ?conn:Sqlite.conn ->
    _ t ->
    Block_hash.t ->
    Dal.Slot_header.t list tzresult Lwt.t

  (** List all indexes of slot headers published in a block. *)
  val list_slot_indexes :
    ?conn:Sqlite.conn ->
    _ t ->
    Block_hash.t ->
    Dal.Slot_index.t list tzresult Lwt.t
end

(** [Dal_slots_statuses] is used to store the attestation status of DAL
    slots. The values of this storage module have type `[`Confirmed |
    `Unconfirmed]`, depending on whether the content of the slot has been
    attested on L1 or not. If an entry is not present for a [(block_hash,
    slot_index)], this means that the corresponding block is not processed yet.
*)
module Dal_slots_statuses : sig
  (** [store ?conn s block slot_index status] store the attestation status of
      slot published in [block] and index [slot_index]. *)
  val store :
    ?conn:Sqlite.conn ->
    rw ->
    Block_hash.t ->
    Dal.Slot_index.t ->
    [`Confirmed | `Unconfirmed] ->
    unit tzresult Lwt.t

  (** Retrieve the attestation status of a DAL slot. *)
  val find_slot_status :
    ?conn:Sqlite.conn ->
    _ t ->
    Block_hash.t ->
    slot_index:Dal.Slot_index.t ->
    [`Confirmed | `Unconfirmed] option tzresult Lwt.t

  (** List attestation statuses of all slots published in a given L1 block. *)
  val list_slot_statuses :
    ?conn:Sqlite.conn ->
    _ t ->
    Block_hash.t ->
    (Dal.Slot_index.t * [`Confirmed | `Unconfirmed]) list tzresult Lwt.t
end

(** Storage for associating levels to block hashes  *)
module L2_levels : sig
  (** [store ?conn s level block] associates the block hash [block] to
      [level]. *)
  val store :
    ?conn:Sqlite.conn -> rw -> int32 -> Block_hash.t -> unit tzresult Lwt.t

  (** Retrieve the block hash associated to a given level. *)
  val find :
    ?conn:Sqlite.conn -> _ t -> int32 -> Block_hash.t option tzresult Lwt.t
end

(** Storage for L2 blocks contracted by the rollup node. *)
module L2_blocks : sig
  (** Store an L2 block. *)
  val store :
    ?conn:Sqlite.conn -> rw -> Sc_rollup_block.t -> unit tzresult Lwt.t

  (** Retrieve an L2 block by the L1 block hash. *)
  val find :
    ?conn:Sqlite.conn ->
    _ t ->
    Block_hash.t ->
    Sc_rollup_block.t option tzresult Lwt.t

  (** Retrieve an L2 block by its level. *)
  val find_by_level :
    ?conn:Sqlite.conn -> _ t -> int32 -> Sc_rollup_block.t option tzresult Lwt.t

  (** Retrieve the level of an L2 block with its hash. *)
  val find_level :
    ?conn:Sqlite.conn -> _ t -> Block_hash.t -> int32 option tzresult Lwt.t

  (** Retrieve the context hash for an L2 block with its hash. *)
  val find_context :
    ?conn:Sqlite.conn ->
    _ t ->
    Block_hash.t ->
    Smart_rollup_context_hash.t option tzresult Lwt.t

  (** Retrieve the current head of the L2 chain. *)
  val find_head :
    ?conn:Sqlite.conn -> _ t -> Sc_rollup_block.t option tzresult Lwt.t

  (** Retrieve the currently last finalized block of the L2 chain. *)
  val find_finalized :
    ?conn:Sqlite.conn -> _ t -> Sc_rollup_block.t option tzresult Lwt.t

  (** Returns the predecessor, and its level, of an L2 block. *)
  val find_predecessor :
    ?conn:Sqlite.conn ->
    _ t ->
    Block_hash.t ->
    (Block_hash.t * int32) option tzresult Lwt.t

  (** Returns a full L2 block (i.e. with all information) with a single database
      query. NOTE: The result does not contain the outbox. *)
  val find_full :
    ?conn:Sqlite.conn ->
    _ t ->
    Block_hash.t ->
    Sc_rollup_block.full option tzresult Lwt.t
end

(** Storage of single values. *)
module State : sig
  module type S = sig
    type value

    val set : ?conn:Sqlite.conn -> rw -> value -> unit tzresult Lwt.t

    val get : ?conn:Sqlite.conn -> _ t -> value option tzresult Lwt.t
  end

  type history_mode =
    | Archive
        (** The whole history of the rollup (starting at its genesis) is kept *)
    | Full
        (** Only the history necessary to play refutation games is kept
          (i.e. after the LCC only) *)

  module Finalized_level : S with type value = Block_hash.t * int32

  module LCC : S with type value = Commitment.Hash.t * int32

  module LPC : S with type value = Commitment.Hash.t * int32

  module Last_gc_target : S with type value = int32

  module Last_gc_triggered_at : S with type value = int32

  module Last_successful_gc_target : S with type value = int32

  module Last_successful_gc_triggered_at : S with type value = int32

  module Last_context_split : S with type value = int32

  module History_mode : S with type value = history_mode

  module L2_head : S with type value = Block_hash.t * int32
end
