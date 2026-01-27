(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module handles the on-disk storage of the DAL node. We rely
    on the [Key_value_store] module from lib_stdlib_unix. For shards storage, we
    rely on a cache mechanism in the case of non-prover profiles. *)

open Cryptobox

(** Store directory names within the DAL node's data directory. *)
module Stores_dirs : sig
  val shard : string

  val slot : string

  val skip_list_cells : string
end

module Shards_disk : sig
  (** Low-level access to shards stored on disk. *)

  (** [get_file_layout slot_id] returns a function to be used as the
      [file_layout] of the Key_value_store for the given [slot_id] in the root
      directory. This function might fail if there is no layout registered for the
      given [slot_id].

      Beware that cryptographic parameters must be initialized via
      {!Node_context.init_cryptobox} before using this function. *)
  val get_file_layout :
    slot_id:Types.slot_id ->
    (root_dir:string ->
    Types.slot_id ->
    (int, Cryptobox.share) Key_value_store.layout)
    tzresult
    Lwt.t

  (** [add_file_layout] adds a new file layout that will be used starting at the
      given level. This should be called when a new protocol is activated,
      introducing a new layout related to DAL protocol parameters changes. *)
  val add_file_layout :
    int32 -> Cryptobox.parameters -> (unit, [> `Fail of string]) result
end

module Shards : sig
  (** A shard of some slot id consist of a shard index (a number
      between 0 and the number_of_shards protocol parameter) and a
      share. The shard store is a mapping associating 0 or 1 share to
      each (slot_id, shard index) pair. *)

  type t

  (** [number_of_shards_available store slot_id shard_indices] returns the
      number of shards stored among the ones given by indexes in [shard_indices]
      for the given [slot_id]. *)
  val number_of_shards_available :
    t -> Types.slot_id -> int list -> int tzresult Lwt.t

  (** [write_all store slot_id shards] adds to the shard store all the
      given shards of the given slot id. *)
  val write_all :
    t -> Types.slot_id -> shard Seq.t -> (unit, [> Errors.other]) result Lwt.t

  (** [read store slot_id shard_id] gets the shard associated to
    [slot_id] at the range [shard_id]. *)
  val read :
    t ->
    Types.slot_id ->
    int ->
    (Cryptobox.shard, [> Errors.not_found | Errors.other]) result Lwt.t

  (** Same as [read_values] but for all possible shards of the given slot id. *)
  val read_all :
    ?from_bytes:bytes ->
    t ->
    Types.slot_id ->
    number_of_shards:int ->
    (Types.slot_id * int * share tzresult) Seq_s.t tzresult Lwt.t

  (** [count_values store slot_id] returns the number of shards
      which are stored for the given slot id. *)
  val count_values : t -> Types.slot_id -> int tzresult Lwt.t

  (** [remove store slot_id] removes the shards associated to the given
      slot id from the store. *)
  val remove : t -> Types.slot_id -> unit tzresult Lwt.t
end

module Slots : sig
  (** A store of slots, indexed by slot id. *)

  type t

  (** [get_file_layout slot_id] returns a function to be used as the
      [file_layout] of the Key_value_store for the given [slot_id] in the root
      directory. This function might fail if there is no layout registered for the
      given [slot_id]. *)
  val get_file_layout :
    slot_id:Types.slot_id ->
    (root_dir:string -> Types.slot_id -> (unit, bytes) Key_value_store.layout)
    tzresult
    Lwt.t

  (** [add_slot store slot_content slot_id] adds a mapping from the given slot
      id to the given slot content. *)
  val add_slot :
    t -> bytes -> Types.slot_id -> (unit, [> Errors.other]) result Lwt.t

  (** [find_slot store slot_id] returns the slot associated to some slot id or
      [Error `Not_found] if no slot is associated. *)
  val find_slot :
    t ->
    Types.slot_id ->
    (bytes, [> Errors.other | Errors.not_found]) result Lwt.t

  val remove_slot : t -> Types.slot_id -> unit tzresult Lwt.t

  (** [add_file_layout] adds a new file layout that will be used starting at the
      given level. This aims to be called when a new protocol is activated,
      introducing a new layout related to DAL protocol parameters changes. *)
  val add_file_layout : int32 -> Cryptobox.parameters -> unit
end

module Slot_id_cache : sig
  type t

  val add : number_of_slots:int -> t -> Dal_plugin.slot_header -> unit

  val find_opt : t -> Types.slot_id -> commitment option
end

module Statuses_cache : sig
  (** A cache keeping the attestation status of slot ids. *)

  type t

  (** [update_slot_header_status store slot_id status] updates the status of the
      [slot_id] to [status]. *)
  val update_slot_header_status :
    t -> Types.slot_id -> Types.header_status -> unit tzresult

  (** [get_slot_status cache ~slot_id] returns the status associated
      to the given [slot_id], if any. *)
  val get_slot_status : t -> Types.slot_id -> Types.header_status option
end

module Commitment_indexed_cache : sig
  type 'a t

  (** Returns the element associated to the commitment in the cache,
      or [None] if there is none. *)
  val find_opt : 'a t -> commitment -> 'a option
end

module Traps : sig
  (** A cache for trap data that stores up to [Constants.traps_cache_size] levels
      in memory. *)
  type t

  (** [add ~slot_id ~shard_index ~delegate ~share ~shard_proof] adds
      trap data to the cache. The cache maintains a maximum of
      [Constants.traps_cache_size] levels. Data is expected to be
      added in ascending order within a window of
      [proto_parameters.attestation_lag]. When the cache reaches its
      capacity, the oldest trap data (relative to the highest stored
      level) is removed when adding new entries. *)
  val add :
    t ->
    slot_id:Types.Slot_id.t ->
    shard_index:Types.shard_index ->
    delegate:Signature.public_key_hash ->
    share:Cryptobox.share ->
    shard_proof:Cryptobox.shard_proof ->
    unit

  (** [find t ~level] retrieves all trap data associated with the given
      level. Returns an empty list if no traps exist for that level. *)
  val find : t -> level:Types.level -> Types.trap list
end

module Chain_id : Single_value_store.S with type value = Chain_id.t

module Last_processed_level : Single_value_store.S with type value = int32

module First_seen_level : Single_value_store.S with type value = int32

(** Storage backend for storage components supporting multiple backends. *)
module Storage_backend : sig
  type kind
end

(** The DAL node store. *)
type t

(** [not_yet_published_cache t] returns the cache for not-yet-published data
    associated with the store [t]. *)
val not_yet_published_cache :
  t -> (slot * share array * shard_proof array) Commitment_indexed_cache.t

(** [first_seen_level t] returns the first seen level store associated
    with the store [t]. *)
val first_seen_level : t -> First_seen_level.rw First_seen_level.t

(** [finalized_commitments t] returns the cache of commitments indexed
    by level and then by slot id associated with the store [t]. The
    maximum number of levels is given by {!Constants.slot_id_cache_size}.
    No more than [number_of_slots] commitments can be stored per level. *)
val finalized_commitments : t -> Slot_id_cache.t

(** [chain_id t] returns the chain_id store associated with the store [t]. *)
val chain_id : t -> Chain_id.rw Chain_id.t

(** [last_processed_level t] returns the last processed level store
    associated with the store [t]. *)
val last_processed_level : t -> Last_processed_level.rw Last_processed_level.t

(** [shards t] returns the shards store associated with the store
    [t]. *)
val shards : t -> Shards.t

(** [skip_list_cells t] returns the skip list cells store associated
    with the store [t]. *)
val skip_list_cells : t -> Dal_store_sqlite3.Skip_list_cells.t

(** [statuses_cache t] returns the statuses cache associated with the store
    [t]. *)
val statuses_cache : t -> Statuses_cache.t

(** [slots t] returns the slots store associated with the store
    [t]. *)
val slots : t -> Slots.t

(** [traps t] returns the traps store associated with the store
    [t]. *)
val traps : t -> Traps.t

(** [cache_entry store commitment entry] adds or replace an entry to
    the not-yet-published cache with key [commitment]. *)
val cache_not_yet_published_entry :
  t ->
  commitment ->
  Cryptobox.slot ->
  Cryptobox.share array ->
  Cryptobox.shard_proof array ->
  unit

(** [init config profile_ctxt proto_parameters] inits the store on the filesystem using the
    given [config], [profile_ctxt] and [proto_parameters]. *)
val init :
  Configuration_file.t ->
  Profile_manager.t ->
  Types.proto_parameters ->
  t tzresult Lwt.t

(** [add_slot_headers ~number_of_slots ~block_level slot_headers store]
    processes the [slot_headers] published at [block_level]. Concretely, for
    each slot header successfully applied in the L1 block,

    - It is added to disk store {!slot_header_statuses} with a
    [`Waiting_attestation] status (indexed by slot_id);

    - It is added to the 2D memory cache {!finalized_commitments}, indexed by
    publication level and slots indices.
*)
val add_slot_headers :
  number_of_slots:int ->
  block_level:int32 ->
  Dal_plugin.slot_header list ->
  t ->
  unit Lwt.t

(** [Skip_list_cells] manages the storage of [Skip_list_cell.t] *)
module Skip_list_cells : sig
  open Dal_proto_types

  (** [find_opt ?conn store hash] returns the cell associated to [hash] in the
      [store], if any. *)
  val find_opt :
    ?conn:Dal_store_sqlite3.conn ->
    t ->
    Skip_list_hash.t ->
    Skip_list_cell.t option tzresult Lwt.t

  (** [find_by_slot_id_opt ?conn store slot_id] returns the cell and the
      attestation lag associated to ([slot_id.slot_level], [slot_id.slot_index])
      in the [store], if any. *)
  val find_by_slot_id_opt :
    ?conn:Sqlite.conn ->
    t ->
    Types.slot_id ->
    (Dal_proto_types.Skip_list_cell.t * int) option tzresult Lwt.t

  (** See {!Dal_store_sqlite3.Skip_list_cells.find_by_level}. *)
  val find_by_level :
    ?conn:Sqlite.conn ->
    t ->
    published_level:int32 ->
    (Dal_proto_types.Skip_list_cell.t
    * Dal_proto_types.Skip_list_hash.t
    * Types.slot_index)
    list
    tzresult
    Lwt.t

  (** [insert ?conn store ~published_level ~attestation_lag values extract]
      inserts the given list of [values] associated to the given
      [published_level] in the [store], using [extract] for extracting needed
      data from [values]. Any existing value is overridden. *)
  val insert :
    ?conn:Dal_store_sqlite3.conn ->
    t ->
    attested_level:int32 ->
    'a list ->
    ('a ->
    Skip_list_hash.t
    * Skip_list_cell.t
    * Types.slot_index
    * Types.attestation_lag) ->
    unit tzresult Lwt.t

  (** [remove ?conn store ~published_level] removes any data related to [published_level]
      from the [store]. *)
  val remove :
    ?conn:Dal_store_sqlite3.conn ->
    t ->
    published_level:int32 ->
    unit tzresult Lwt.t

  (** [schemas data_dir] returns the list of SQL statements allowing
      to recreate the tables of the DAL skip list cells store. *)
  val schemas : string -> string list tzresult Lwt.t
end
