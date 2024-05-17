(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module handles the on-disk storage of the DAL node. We use
    two key-value stores: the [Key_value_store] module from
    lib_stdlib_unix (for storing shards) and Irmin (for everything
    else). *)

open Cryptobox

type irmin (* This is the Irmin part of the store *)

module Value_size_hooks : sig
  (** [set_share_size size] sets the size of shard shares. This
      function must be called once, before [init] is used. *)
  val set_share_size : int -> unit
end

module Shards : sig
  (** A shard of some slot id consist in a shard index (a number
      between 0 and the number_of_shards protocol parameter) and a
      share. The shard store is a mapping associating 0 or 1 share to
      each (slot_id, shard index) pair.  *)

  type t

  (** [are_shards_available store slot_id shard_indices] returns
      true IFF a share is stored for each given shard index of the
      given slot id.  *)
  val are_shards_available :
    t -> Types.slot_id -> int list -> bool tzresult Lwt.t

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
  (** A store of slots, indexed by slot size and commitment. *)

  type t

  (** [add_slot_by_commitment store ~slot_size slot_content commitment]
      adds a mapping from the given commitment to the given slot
      content. *)
  val add_slot_by_commitment :
    t ->
    slot_size:int ->
    bytes ->
    commitment ->
    (unit, [> Errors.other]) result Lwt.t

  (** [exists_slot_by_commitment store ~slot_size commitment] returns
      true IFF a slot is associated to the given commitment. *)
  val exists_slot_by_commitment :
    t -> slot_size:int -> commitment -> (bool, [> Errors.other]) result Lwt.t

  (** [find_slot_by_commitment store ~slot_size commitment] returns
      the slot associated to some commitment or [Error `Not_found] if
      no slot is associated. *)
  val find_slot_by_commitment :
    t ->
    slot_size:int ->
    commitment ->
    (bytes, [> Errors.other | Errors.not_found]) result Lwt.t

  val remove_slot_by_commitment :
    t -> slot_size:int -> commitment -> unit tzresult Lwt.t
end

module Commitment_indexed_cache : sig
  type 'a t

  (** Returns the element associated to the commitment in the cache,
      or [None] if there is none. *)
  val find_opt : 'a t -> commitment -> 'a option
end

(** Shard proofs are not stored on disk because we can recompute
    them. This computation is quite costly though so we cache the
    result in memory. *)
module Shard_proofs_cache = Commitment_indexed_cache

(** A cache in which we keep the shards of the slots which we have
    received via RPC but are not yet published so we cannot yet assign
    a slot id to them. *)
module Shard_cache = Commitment_indexed_cache

(** A cache in which we keep the slots which we have received via RPC
    but are not yet published so we cannot yet assign a slot id to
    them. *)
module Slot_cache = Commitment_indexed_cache

type t = private {
  store : irmin;  (** The Irmin-based part of the store *)
  shards : Shards.t;  (** Shards store *)
  slots : Slots.t;  (** Slots store *)
  in_memory_shard_proofs : shard_proof array Shard_proofs_cache.t;
      (* The length of the array is the number of shards per slot *)
  not_yet_published_shards : Cryptobox.share array Shard_cache.t;
      (** Cache of shards *)
  not_yet_published_slots : Cryptobox.slot Slot_cache.t;  (** Cache of slots *)
}

(** [cache_shard_proofs store commitment shard_proofs] replaces in the
    shard proof cache all the shard proofs for the given commitment
    with the given ones. *)
val cache_shard_proofs : t -> commitment -> shard_proof array -> unit

(** [cache_shards store commitment shards] adds [shards] to the shard
    cache with key [commitment]. *)
val cache_shards : t -> commitment -> Cryptobox.share array -> unit

(** [cache_slot store commitment slot] adds [slot] to the slot cache
    with key [commitment]. *)
val cache_slot : t -> commitment -> slot -> unit

val init : Configuration_file.t -> t tzresult Lwt.t

module Legacy : sig
  (** The Irmin part of the storage is considered legacy because we
      want to move everything to the KV store. *)

  (**
     We have two concise ways to refer to a slot:

     - a commitment uniquely identifies the content of the slot, it is
       useful to prove properties about the slot length and the shards.

     - a slot identifier gives at which level and at which index the
       slot was published. It only makes sense for slots which have been
       published in some block or at least whose publication in a block
       has been attempted.

     The Irmin part of the store provides a bidirectional mapping
     between commitments and slot identifiers, it also maps to these
     identifiers the following pieces of information:

     - the content of the slot,
     - the status of the publication operation (accepted or not),
     - the attestation status (see Types.header_status).

  *)

  (** [add_slot_headers ~number_of_slots ~block_level slot_headers
      store] adds all the given slot headers at the given block level,
      updating the bidirectional mapping between commitments and slot
      identifiers. For each slot header, the associated operation
      application result indicates if the slot header should be
      considered as accepted (the publication operation has succeeded)
      or not. In the accepted case, the associated status is
      [`Waiting_attestation], otherwise it is [`Not_selected]. *)
  val add_slot_headers :
    number_of_slots:int ->
    block_level:int32 ->
    (Dal_plugin.slot_header * Dal_plugin.operation_application_result) list ->
    t ->
    unit tzresult Lwt.t

  (** [update_selected_slot_headers_statuses ~block_level
      ~attestation_lag ~number_of_slots attested store] updates the
      status of all accepted slots at level [block_level -
      attestation_lag] to either `Attested (when present in the
      [attested] list) or `Unattested (when absent). *)
  val update_selected_slot_headers_statuses :
    block_level:int32 ->
    attestation_lag:int ->
    number_of_slots:int ->
    int list ->
    t ->
    unit Lwt.t

  (** [get_slot_commitment ~level ~slot_index store] returns the
      commitment associated to the given slot identifier in the
      bidirectional mapping. *)
  val get_slot_commitment :
    level:int32 ->
    slot_index:int ->
    t ->
    (commitment, [> Errors.other | Errors.not_found]) result Lwt.t

  (** [get_slot_status ~slot_id store] returns the status associated
      to the given accepted [slot_id], or [None] if no status is
      associated to the [slot_id]. *)
  val get_slot_status :
    slot_id:Types.slot_id ->
    t ->
    (Types.header_status, [> Errors.other | Errors.not_found]) result Lwt.t
end
