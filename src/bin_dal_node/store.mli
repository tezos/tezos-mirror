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

type t (* This is the Irmin part of the store *)

module Value_size_hooks : sig
  (** [set_share_size size] sets the size of shard shares. This
      function must be called once, before [init] is used. *)
  val set_share_size : int -> unit
end

module Shards : sig
  (** A shard of some commitment consist in a shard index (a number
      between 0 and the number_of_shards protocol parameter) and a
      share. The shard store is a mapping associating 0 or 1 share to
      each (commitment, shard index) pair.  *)

  type t

  (** [are_shards_available store commitment shard_indices] returns
      true IFF a share is stored for each given shard index of the
      given commitment.  *)
  val are_shards_available : t -> commitment -> int list -> bool tzresult Lwt.t

  (** [save_and_notify store watcher commitment shards] adds to the
      shard store all the given shards of the given commitment. The
      [watcher] is notified. *)
  val save_and_notify :
    t ->
    commitment Lwt_watcher.input ->
    commitment ->
    shard Seq.t ->
    (unit, [> Errors.other]) result Lwt.t

  (** [read_value store commitment shard_index] returns the associated
     share when the shard is available or an error when it is not.  *)
  val read_value : t -> commitment -> int -> share tzresult Lwt.t

  (** Same as [read_value] but for a sequence of shards. *)
  val read_values :
    t -> (commitment * int) Seq.t -> (commitment * int * share tzresult) Seq_s.t

  (** Same as [read_values] but for all possible shards of the given commitment. *)
  val read_all :
    t ->
    commitment ->
    number_of_shards:int ->
    (commitment * int * share tzresult) Seq_s.t

  (** [count_values store commitment] returns the number of shards
      which are stored for the given commitment. *)
  val count_values : t -> commitment -> int tzresult Lwt.t
end

module Shard_proofs_cache : sig
  (** Shard proofs are not stored on disk because we can recompute
      them. This computation is quite costly though so we cache the
      result in memory. *)

  type 'a t

  (** Returns the element associated to the commitment in the shard proofs cache, or [None] if
      there is none. *)
  val find_opt : 'a t -> commitment -> 'a option
end

type node_store = private {
  store : t; (* The Irmin part *)
  shard_store : Shards.t;
  shards_watcher : commitment Lwt_watcher.input;
  in_memory_shard_proofs : shard_proof array Shard_proofs_cache.t;
      (* The length of the array is the number of shards per slot *)
}

val open_shards_stream :
  node_store -> commitment Lwt_stream.t * Lwt_watcher.stopper

(** [save_shard_proofs store commitment shard_proofs] replaces in
      the shard proof cache all the shard proofs for the given
      commitment with the given ones. *)
val save_shard_proofs : node_store -> commitment -> shard_proof array -> unit

val init : Configuration_file.t -> node_store tzresult Lwt.t

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

  (** [add_slot_by_commitment store cryptobox slot_content commitment]
      adds a mapping from the given commitment to the given slot
      content. The given cryptobox is only used to get the size of the
      slot content, no cryptographic verification is performed by this
      function. *)
  val add_slot_by_commitment :
    node_store -> Cryptobox.t -> bytes -> commitment -> unit Lwt.t

  (** [associate_slot_id_with_commitment store commitment slot_id]
      adds an entry to the bidirectional mapping between commitments
      and slot identifiers. The status is initialized to
      [`Unseen_or_not_finalized].

      This function is only used by RPCs.
  *)
  val associate_slot_id_with_commitment :
    node_store -> commitment -> Types.slot_id -> unit Lwt.t

  (** [exists_slot_by_commitment store cryptobox commitment] returns
      true IFF a slot is associated to the given commitment. *)
  val exists_slot_by_commitment :
    node_store -> Cryptobox.t -> commitment -> bool Lwt.t

  (** [find_slot_by_commitment store cryptobox commitment] returns the
      slot associated to some commitment or an error if no slot is
      associated. *)
  val find_slot_by_commitment :
    node_store ->
    Cryptobox.t ->
    commitment ->
    (bytes option, [> `Decoding_failed of Types.Store.kind * tztrace]) result
    Lwt.t

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
    node_store ->
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
    node_store ->
    unit Lwt.t

  (** [get_commitment_by_published_level_and_index ~level ~slot_index
      store] returns the commitment associated to the given slot
      identifier in the bidirectional mapping. *)
  val get_commitment_by_published_level_and_index :
    level:int32 ->
    slot_index:int ->
    node_store ->
    ( commitment,
      [> `Decoding_failed of Types.Store.kind * tztrace | `Not_found] )
    result
    Lwt.t

  (** [get_commitment_headers commitment ?slot_level ?slot_index
      store] returns all the slot headers (slot identifiers and
      statuses) associated to the given commitment. The optional
      arguments can be used for filtering; when set, only the
      slot headers for the provided level or slot index are
      considered. *)
  val get_commitment_headers :
    commitment ->
    ?slot_level:int32 ->
    ?slot_index:int ->
    node_store ->
    ( Types.slot_header list,
      [> `Decoding_failed of Types.Store.kind * tztrace] )
    result
    Lwt.t

  (** [get_published_level_headers ~published_level ?header_status
      store] returns all the slot headers (commitments, slot
      identifiers, and statuses) of the slots published at the given
      level. The optional argument ?header_status can be used to
      restrict the output to only the headers with the given status. *)
  val get_published_level_headers :
    published_level:int32 ->
    ?header_status:Types.header_status ->
    node_store ->
    ( Types.slot_header list,
      [> `Decoding_failed of Types.Store.kind * tztrace] )
    result
    Lwt.t
end
