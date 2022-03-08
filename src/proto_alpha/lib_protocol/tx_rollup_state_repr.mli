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

(** The initial value of a transaction rollup state, after its origination. *)
val initial_state : t

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

(** [update_burn_per_byte state ~final_size ~hard_limit] updates the
    burn to be paid for each byte submitted to a transaction rollup
    inbox, based on the ratio of the [hard_limit] maximum amount of
    byte an inbox can use and the [final_size] amount of bytes it uses
    at the end of the construction of a Tezos block.

    In a nutshell, if the ratio is lesser than 80%, the burn per byte
    are reduced. If the ratio is somewhere between 80% and 90%, the
    burn per byte remain constant. If the ratio is greater than 90%,
    then the burn per byte are increased.

    The rationale behind this mechanics is to reduce the activity of a
    transaction rollup in case it becomes too intense. *)
val update_burn_per_byte : t -> final_size:int -> hard_limit:int -> t

(** [burn_cost ~limit state size] computes the burn to be paid to submit
    [size] bytes in the inbox of the transactional rollup.

    Returns [Tx_rollup_submit_batch_burn_excedeed] if the (computed) burn
    exceeds [limit].
*)
val burn_cost : limit:Tez_repr.t option -> t -> int -> Tez_repr.t tzresult

(** [head_level state] returns the rollup level of the most recent
    inbox —if it exists— long with the Tezos level at which this inbox
    was created. *)
val head_level : t -> (Tx_rollup_level_repr.t * Raw_level_repr.t) option

(** [commitment_head_level state] returns the rollup level of the most
    recent unfinalized commitment, if it exists. *)
val commitment_head_level : t -> Tx_rollup_level_repr.t option

(** [commitment_tail_level state] returns the rollup level of the
    oldest finalized commitment still in the layer-1 context, if it
    exists. *)
val commitment_tail_level : t -> Tx_rollup_level_repr.t option

(** [uncommitted_inboxes_count state] returns the number of inboxes
    the rollup current has in the storage which did not receive a
    commitment yet. *)
val uncommitted_inboxes_count : t -> int

(** [finalized_commitments_count t] returns the number of finalized
    commitment still in the layer-1 context. *)
val finalized_commitments_count : t -> int

(** [inboxes_count state] returns the number of inboxes the rollup
    current has in the storage. *)
val inboxes_count : t -> int

(** [oldest_inbox_level state] returns the level of the oldest inbox
    of a rollup that is still in the layer-1 context, if it exists. *)
val oldest_inbox_level : t -> Tx_rollup_level_repr.t option

(** [next_commitment_level state] returns the expected level of the
    next valid commitment.

    This function can return the error [No_uncommitted_inbox] if
    there is no inbox awaiting a commitment. *)
val next_commitment_level : t -> Tx_rollup_level_repr.t tzresult

(** [next_commitment_predecessor state] returns the expected
    predecessor hash of the next valid commitment. *)
val next_commitment_predecessor :
  t -> Tx_rollup_commitment_repr.Commitment_hash.t option

(** [record_inbox_creation state level] updates the state of a rollup
    to take into account the creation of of a new inbox at the given
    Tezos [level], and returns the rollup level to associate to this
    inbox.

    This function may return an [Internal_error] iff an inbox has
    already been created at a level greater (or equal) than
    [level]. It is the responsibility of the caller to avoid that. *)
val record_inbox_creation :
  t -> Raw_level_repr.t -> (t * Tx_rollup_level_repr.t) tzresult

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
  t ->
  Tx_rollup_level_repr.t ->
  Tx_rollup_commitment_repr.Commitment_hash.t ->
  t tzresult

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
  Tx_rollup_commitment_repr.Commitment_hash.t option ->
  t tzresult

(** [record_commitment_deletion state level hash] updates [state] to
    take into account the deletion of a commitment at a given rollup
    [level], and of given [hash].

    This function returns an [Internal_error] if [level] is not the
    commitment tail, that is the oldest finalized commitment. *)
val record_commitment_deletion :
  t ->
  Tx_rollup_level_repr.t ->
  Tx_rollup_commitment_repr.Commitment_hash.t ->
  t tzresult

(** [finalized_commitments_range state] returns the window of finalized
    commitments that have not yet been cleaned out

    This function returns an [Internal_error] if the state is inconsistent, 
    which should not be possible. *)
val finalized_commitments_range :
  t -> (Tx_rollup_level_repr.t * Tx_rollup_level_repr.t) option tzresult

module Internal_for_tests : sig
  (** [make] returns a state for tests *)
  val make :
    ?burn_per_byte:Tez_repr.t ->
    ?inbox_ema:int ->
    ?last_removed_commitment_hash:Tx_rollup_commitment_repr.Commitment_hash.t ->
    ?commitment_tail_level:Tx_rollup_level_repr.t ->
    ?oldest_inbox_level:Tx_rollup_level_repr.t ->
    ?commitment_head_level:
      Tx_rollup_level_repr.t * Tx_rollup_commitment_repr.Commitment_hash.t ->
    ?head_level:Tx_rollup_level_repr.t * Raw_level_repr.t ->
    unit ->
    t

  val get_inbox_ema : t -> int
end
