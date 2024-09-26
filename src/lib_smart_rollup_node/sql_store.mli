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

(** [init mode ~data_dir] initializes the store and returns it. *)
val init : 'a Store_sigs.mode -> data_dir:string -> 'a t tzresult Lwt.t

(** Close the store by freeing all resources and closing database
    connections. *)
val close : _ t -> unit Lwt.t

(** Returns a read-only version of the store. *)
val readonly : _ t -> ro

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
    (int32 * string list) option tzresult Lwt.t
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
