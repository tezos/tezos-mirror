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

module Statuses : sig
  (** A store keeping the attestation status of slot ids. *)

  type t

  (** [get_slot_status ~slot_id store] returns the status associated
      to the given accepted [slot_id], or [None] if no status is
      associated to the [slot_id]. *)
  val get_slot_status :
    slot_id:Types.slot_id ->
    t ->
    (Types.header_status, [> Errors.other | Errors.not_found]) result Lwt.t
end

module Commitment_indexed_cache : sig
  type 'a t

  (** Returns the element associated to the commitment in the cache,
      or [None] if there is none. *)
  val find_opt : 'a t -> commitment -> 'a option
end

type t = private {
  slot_header_statuses : Statuses.t;
  store : irmin;  (** The Irmin-based part of the store *)
  shards : Shards.t;  (** Shards store *)
  slots : Slots.t;  (** Slots store *)
  cache :
    (Cryptobox.slot * Cryptobox.share array * Cryptobox.shard_proof array)
    Commitment_indexed_cache.t;
      (* The length of the array is the number of shards per slot *)
}

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
    unit tzresult Lwt.t

  (** [get_slot_status ~slot_id store] returns the status associated
      to the given accepted [slot_id], or [None] if no status is
      associated to the [slot_id]. *)
  val get_slot_status :
    slot_id:Types.slot_id ->
    t ->
    (Types.header_status, [> Errors.other | Errors.not_found]) result Lwt.t
end
