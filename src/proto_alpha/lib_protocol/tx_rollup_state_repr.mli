(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(** The state of a transaction rollup is a set of variables whose values vary
    in time, as the rollup progresses. *)
type t

(** [initial_state pre_allocated_storage] returns the initial state of
    a transaction rollup (after its origination) with
    [pre_allocated_storage] bytes of storage already paid for. *)
val initial_state : pre_allocated_storage:Z.t -> t

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

(** [update_burn_per_byte state ~elapsed ~factor ~final_size
    ~hard_limit] updates the cost per byte to be paid for each message
    submitted to the rollup.  This is done by computing a moving
    average for [factor] snapshots. Each snapshot being the size of the
    total messages for the rollup. Hence each snapshot contributes to
    [1/(1 + factor)] to the average.

    It may happen that the rollup does not receive any message for
    some period of time. The parameter [elapsed] allows that to be taken
    into account. If [elapsed=n] with [n>=1] it is similar as if
    [update_burn_per_byte] was called [n] times with [final_size=0].

    Once the exponential moving average [ema] is computed, we use the
    [hard limit] to know whether the cost per byte should be updated:

    1. If [ema <= 80] then the cost per byte is decreased

    2. If [80 < ema <= 90] then the cost per byte is stable

    3. If [90 < ema] then the cost ber byte is increased

    The rationale behind this mechanics is to adapt the cost of a
    transactional rollup depending on its activity. This can be used
    to prevent from some spamming attacks. *)
val update_burn_per_byte :
  t -> elapsed:int -> factor:int -> final_size:int -> hard_limit:int -> t

(** [burn_cost ~limit state size] computes the burn to be paid to submit
    [size] bytes in the inbox of the transactional rollup.

    Returns [Tx_rollup_submit_batch_burn_exceeded] if the (computed) burn
    exceeds [limit].
*)
val burn_cost : limit:Tez_repr.t option -> t -> int -> Tez_repr.t tzresult

(** [has_valid_commitment_at state level] returns [true] iff there is
    a valid commitment for [level] in the layer-1 storage.

    On the contrary, if there is not commitment for [level] in the
    layer-1 storage, or if there exists an orphan commitment (that is,
    a commitment which has been rejected, or with one of its ancestors
    that has been rejected) at [level], this function returns
    [false]. *)
val has_valid_commitment_at : t -> Tx_rollup_level_repr.t -> bool

(** [uncommitted_inboxes_count state] returns the number of inboxes
    the rollup current has in the storage which did not receive a
    commitment yet. *)
val uncommitted_inboxes_count : t -> int

(** [commitments_count t] returns the number of commitment still in
    the layer-1 context. *)
val commitments_count : t -> int

(** [inboxes_count state] returns the number of inboxes the rollup
    current has in the storage. *)
val inboxes_count : t -> int

(** [next_commitment_to_finalize state] returns the rollup level of
    the next commitment to be finalized. *)
val next_commitment_to_finalize : t -> Tx_rollup_level_repr.t option

(** [next_commitment_to_remove state] returns the rollup level of the
    next commitment to be removed from the layer-1 context. *)
val next_commitment_to_remove : t -> Tx_rollup_level_repr.t option

(** [finalized_commitment_oldest_level state] returns the rollup level
    of the oldest finalized commitment. *)
val finalized_commitment_oldest_level : t -> Tx_rollup_level_repr.t option

(** [next_commitment_level current_level state] returns the expected
    level of the next valid commitment.

    This function can return the error [No_uncommitted_inbox] if
    there is no inbox awaiting a commitment. *)
val next_commitment_level :
  t -> Raw_level_repr.t -> Tx_rollup_level_repr.t tzresult

(** [next_commitment_predecessor state] returns the expected
    predecessor hash of the next valid commitment. *)
val next_commitment_predecessor : t -> Tx_rollup_commitment_repr.Hash.t option

(** [record_inbox_creation state level] updates the state of a rollup
    to take into account the creation of of a new inbox at the given
    Tezos [level], and returns the rollup level to associate to this
    inbox and the number of bytes allocated for the inbox.

    This function may return an [Internal_error] iff an inbox has
    already been created at a level greater (or equal) than
    [level]. It is the responsibility of the caller to avoid that. *)
val record_inbox_creation :
  t -> Raw_level_repr.t -> (t * Tx_rollup_level_repr.t * Z.t) tzresult

(** [record_inbox_deletion state level] updates [state] to take into
    account the deletion of the inbox stored at Tezos [level] from the
    storage.

    This function returns an [Internal_error] iff there is no inbox
    in the storage of the layer-1, or if [level] is not the oldest
    level of rollup. *)
val record_inbox_deletion : t -> Tx_rollup_level_repr.t -> t tzresult

(** [record_commitment_creation state level] updates [state] to take
    into account the creation of a commitment at a given Tezos
    [level].

    This function returns an [Internal_error] if [level] is not the
    successor level of the current commitment head, or if [level] is
    greater than the inbox head. *)
val record_commitment_creation :
  t -> Tx_rollup_level_repr.t -> Tx_rollup_commitment_repr.Hash.t -> t tzresult

(** [record_commitment_rejection state level pred_hash] updates
    [state] to take into account the fact that the commitment for the
    inbox at [level] has been rejected.

    The caller is expected to provide the predecessor hash the next
    valid commitment needs to use. It can be omitted under two
    circumstances: if [level = root], or if the commitment identified
    by [pred_hash] is no longer in the layer-1 context. *)
val record_commitment_rejection :
  t ->
  Tx_rollup_level_repr.t ->
  Tx_rollup_commitment_repr.Hash.t option ->
  t tzresult

(** [record_commitment_deletion state level msg_hash commitment_hash]
    updates [state] to take into account the deletion of a commitment
    at a given rollup [level], and of given [commitment_hash] and
    whose last message commitment is [msg_hash].

    This function returns an [Internal_error] if [level] is not the
    commitment tail, that is the oldest finalized commitment. *)
val record_commitment_deletion :
  t ->
  Tx_rollup_level_repr.t ->
  Tx_rollup_commitment_repr.Hash.t ->
  Tx_rollup_message_result_hash_repr.t ->
  t tzresult

(** [finalized_commitments_range state] returns the window of finalized
    commitments that have not yet been cleaned out

    This function returns an [Internal_error] if the state is inconsistent,
    which should not be possible. *)
val finalized_commitments_range :
  t -> (Tx_rollup_level_repr.t * Tx_rollup_level_repr.t) option

(** [check_level_can_be_rejected state level] raises
    [Cannot_reject_level] iff there does not exist a commitment at
    [level] that is not yet finalized. *)
val check_level_can_be_rejected : t -> Tx_rollup_level_repr.t -> unit tzresult

(** [last_removed_commitment_hashes state] returns two hashes
    associated to the last removed commitment: the message result
    hash and the last commitment hash. *)
val last_removed_commitment_hashes :
  t ->
  (Tx_rollup_message_result_hash_repr.t * Tx_rollup_commitment_repr.Hash.t)
  option

(** [head_levels state] returns the level of the last inbox which has
    been created in the layer-1 context, along with the Tezos level at
    which said inbox has been created. *)
val head_levels : t -> (Tx_rollup_level_repr.t * Raw_level_repr.t) option

(** [adjust_storage_allocation state ~delta] accounts for a change in
    [delta] number of bytes used storage space by a transaction rollup.

    A positive [delta] indicates that the occupied storage of the
    rollup increased. A negative [delta] indicates that the
    occupied storage of the rollup decreased.

    Along with an updated state, a diff of storage space
    is returned. The diff is
    [max(0, allocated_storage - (occupied_storage + delta))].
    That is, 0 if no new storage was allocated, and the number of bytes
    allocated otherwise.

    This function returns [Tx_rollup_errors.Internal_error] if
    submitted [delta] would make [occupied_storage] negative. *)
val adjust_storage_allocation : t -> delta:Z.t -> (t * Z.t) tzresult

module Internal_for_tests : sig
  (** [make] returns a state for tests *)
  val make :
    ?burn_per_byte:Tez_repr.t ->
    ?inbox_ema:int ->
    ?last_removed_commitment_hashes:
      Tx_rollup_message_result_hash_repr.t * Tx_rollup_commitment_repr.Hash.t ->
    ?finalized_commitments:Tx_rollup_level_repr.t * Tx_rollup_level_repr.t ->
    ?unfinalized_commitments:Tx_rollup_level_repr.t * Tx_rollup_level_repr.t ->
    ?uncommitted_inboxes:Tx_rollup_level_repr.t * Tx_rollup_level_repr.t ->
    ?commitment_newest_hash:Tx_rollup_commitment_repr.Hash.t ->
    ?tezos_head_level:Raw_level_repr.t ->
    ?occupied_storage:Z.t ->
    ?commitments_watermark:Tx_rollup_level_repr.t ->
    allocated_storage:Z.t ->
    unit ->
    t

  val get_inbox_ema : t -> int

  val get_occupied_storage : t -> Z.t

  val set_occupied_storage : Z.t -> t -> t

  val get_allocated_storage : t -> Z.t

  val set_allocated_storage : Z.t -> t -> t

  val reset_commitments_watermark : t -> t

  val get_commitments_watermark : t -> Tx_rollup_level_repr.t option
end
