(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module handles the on-disk storage of the DAL node. We rely
    on the [Key_value_store] module from lib_stdlib_unix. *)

open Cryptobox

module Shards : sig
  (** A shard of some slot id consist in a shard index (a number
      between 0 and the number_of_shards protocol parameter) and a
      share. The shard store is a mapping associating 0 or 1 share to
      each (slot_id, shard index) pair.  *)

  type t

  (** [number_of_shards_available store slot_id shard_indices] returns the
      number of shards stored among the ones given by indexes in [shard_indices]
      for the given [slot_id]. *)
  val number_of_shards_available :
    t -> Types.slot_id -> int list -> int tzresult Lwt.t

  (** [write_all store slot_id shards] adds to the shard store all the given
      shards of the given slot id. *)
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
    t ->
    Types.slot_id ->
    number_of_shards:int ->
    (Types.slot_id * int * share tzresult) Seq_s.t

  (** [count_values store slot_id] returns the number of shards
      which are stored for the given slot id. *)
  val count_values : t -> Types.slot_id -> int tzresult Lwt.t

  (** [remove store slot_id] removes the shards associated to the given
      slot id from the store *)
  val remove : t -> Types.slot_id -> unit tzresult Lwt.t
end

module Slots : sig
  (** A store of slots, indexed by slot id. *)

  type t

  (** [add_slot store ~slot_size slot_content slot_id] adds a mapping from the
      given slot id to the given slot content. *)
  val add_slot :
    t ->
    slot_size:int ->
    bytes ->
    Types.slot_id ->
    (unit, [> Errors.other]) result Lwt.t

  (** [find_slot store ~slot_size slot_id] returns the slot associated to some
      slot id or [Error `Not_found] if no slot is associated. *)
  val find_slot :
    t ->
    slot_size:int ->
    Types.slot_id ->
    (bytes, [> Errors.other | Errors.not_found]) result Lwt.t

  val remove_slot : t -> slot_size:int -> Types.slot_id -> unit tzresult Lwt.t
end

module Slot_id_cache : sig
  type t

  val add : number_of_slots:int -> t -> Dal_plugin.slot_header -> unit

  val find_opt : t -> Types.slot_id -> commitment option
end

module Statuses : sig
  (** A store keeping the attestation status of slot ids. *)

  type t

  (** [update_selected_slot_headers_statuses ~block_level
      ~attestation_lag ~number_of_slots attested is_attested store]
      updates the status of all accepted slots at level [block_level -
      attestation_lag] to either `Attested ([attested] returns [true])
      or `Unattested (when [attested] returns [false]). *)
  val update_selected_slot_headers_statuses :
    block_level:int32 ->
    attestation_lag:int ->
    number_of_slots:int ->
    (int -> bool) ->
    t ->
    unit tzresult Lwt.t

  (** [get_slot_status ~slot_id store] returns the status associated
      to the given accepted [slot_id], or [None] if no status is
      associated to the [slot_id]. *)
  val get_slot_status :
    slot_id:Types.slot_id ->
    t ->
    (Types.header_status, [> Errors.other | Errors.not_found]) result Lwt.t

  (** [remove_level_status ~level store] removes the status of all the
      slot ids published at the given level. *)
  val remove_level_status : level:int32 -> t -> unit tzresult Lwt.t
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

module Last_processed_level : Single_value_store.S with type value = int32

module First_seen_level : Single_value_store.S with type value = int32

(** Storage backend for storage components supporting multiple backends. *)
module Storage_backend : sig
  type kind
end

(** The DAL node store. *)
type t

(** [cache t] returns the cache associated with the store [t]. *)
val cache :
  t -> (slot * share array * shard_proof array) Commitment_indexed_cache.t

(** [first_seen_level t] returns the first seen level store associated
    with the store [t]. *)
val first_seen_level : t -> First_seen_level.t

(** [finalized_commitments t] returns the cache of commitments indexed
    by level and then by slot id associated with the store [t]. The
    maximum number of levels is given by {!Constants.slot_id_cache_size}.
    No more than [number_of_slots] commitments can be stored per level. *)
val finalized_commitments : t -> Slot_id_cache.t

(** [last_processed_level t] returns the last processed level store
    associated with the store [t]. *)
val last_processed_level : t -> Last_processed_level.t

(** [shards t] returns the shards store associated with the store
    [t]. *)
val shards : t -> Shards.t

(** [skip_list_cells t] returns the skip list cells store associated
    with the store [t]. *)
val skip_list_cells : t -> Dal_store_sqlite3.Skip_list_cells.t

(** [slot_header_statuses t] returns the statuses store  associated with the store
    [t]. *)
val slot_header_statuses : t -> Statuses.t

(** [slots t] returns the slots store associated with the store
    [t]. *)
val slots : t -> Slots.t

(** [traps t] returns the traps store associated with the store
    [t]. *)
val traps : t -> Traps.t

(** [cache_entry store commitment entry] adds or replace an entry to
    the cache with key [commitment]. *)
val cache_entry :
  t ->
  commitment ->
  Cryptobox.slot ->
  Cryptobox.share array ->
  Cryptobox.shard_proof array ->
  unit

val init : Configuration_file.t -> t tzresult Lwt.t

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
  unit tzresult Lwt.t

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

  (** [find_by_slot_id_opt ?conn store ~attested_level ~slot_index] returns the
      cell associated to ([attested_level], [slot_index]) in the [store], if
      any. *)
  val find_by_slot_id_opt :
    ?conn:Sqlite.conn ->
    t ->
    attested_level:int32 ->
    slot_index:int ->
    Dal_proto_types.Skip_list_cell.t option tzresult Lwt.t

  (** [insert ?conn store ~attested_level values] inserts the given list of [values]
      associated to the given [attested_level] in the [store]. Any existing value
      is overridden. *)
  val insert :
    ?conn:Dal_store_sqlite3.conn ->
    t ->
    attested_level:int32 ->
    (Skip_list_hash.t * Skip_list_cell.t) list ->
    unit tzresult Lwt.t

  (** [remove ?conn store ~attested_level] removes any data related to [attested_level]
      from the [store]. *)
  val remove :
    ?conn:Dal_store_sqlite3.conn ->
    t ->
    attested_level:int32 ->
    unit tzresult Lwt.t

  (** [schemas data_dir] returns the list of SQL statements allowing
      to recreate the tables of the DAL skip list cells store. *)
  val schemas : string -> string list tzresult Lwt.t
end
