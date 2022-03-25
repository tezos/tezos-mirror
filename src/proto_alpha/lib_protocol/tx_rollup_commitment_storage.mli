(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2021 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

(** This module introduces various functions to manipulate the storage related
    to commitments for transaction rollups. *)

(** [check_commitment_level current_tezos_level state commitment] fails if [commitment]
    does not target the expected level. *)
val check_commitment_level :
  Raw_level_repr.t ->
  Tx_rollup_state_repr.t ->
  Tx_rollup_commitment_repr.t ->
  unit tzresult
(* FIXME: move in Tx_rollup_commitment_repr *)

(** [add_commitment context tx_rollup contract commitment] adds a
    commitment to a rollup. It returns the new context, the new
    state, and the storage size diff.

    This function returns the errors

    {ul {li [Level_already_has_commitment] iff there is already a
            commitment at this level.}
        {li [Missing_commitment_predecessor] iff the predecessor does
            not match the already-stored predecessor commitment.}
        {li [Wrong_commitment_predecessor_level] iff there is no
            predecessor level, but a predecessor commitment is
            provided (or no predecessor commitment is provided but
            there is a precessor level)}
        {li [Wrong_batch_count] iff the number of batches does not
            equal the length of the inbox.}} *)
val add_commitment :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_state_repr.t ->
  Signature.Public_key_hash.t ->
  Tx_rollup_commitment_repr.t ->
  (Raw_context.t * Tx_rollup_state_repr.t * Z.t) tzresult Lwt.t

(** [remove_bond context state tx_rollup contract] removes the bond for an
    implicit contract. This will fail if either the bond does not exist,
    or if the bond is currently in use. *)
val remove_bond :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Signature.public_key_hash ->
  Raw_context.t tzresult Lwt.t

(** [slash_bond ctxt tx_rollup contract] removes the bond counter for
    an implicit contract if it exists. Besides, it returns a boolean
    to determine if this counter was strictly superior to 0. *)
val slash_bond :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Signature.public_key_hash ->
  (Raw_context.t * bool) tzresult Lwt.t

(** [find context tx_rollup state level] returns the commitment for a
    level, if any exists and is not orphan (that is, one of its
    ancestors has been rejected).  If the rollup does not exist, the
    error [Tx_rollup_does_not_exist] is returned. *)
val find :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_state_repr.t ->
  Tx_rollup_level_repr.t ->
  (Raw_context.t * Tx_rollup_commitment_repr.Submitted_commitment.t option)
  tzresult
  Lwt.t

(** [get context tx_rollup state level] returns the commitment for a
    level, if any exists.  If the rollup does not exist, the error
    [Tx_rollup_does_not_exist] is returned. If there is no commitment
    in the storage, or if a commitment exists but it is orphan (that
    is, one of its ancestors has been rejected), then
    [Commitment_does_not_exist] is returned. *)
val get :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_state_repr.t ->
  Tx_rollup_level_repr.t ->
  (Raw_context.t * Tx_rollup_commitment_repr.Submitted_commitment.t) tzresult
  Lwt.t

(** [get_finalized context tx_rollup level] returns the
    commitment for a level, if any exists and is finalized. If the rollup does not
    exist, the error [Tx_rollup_does_not_exist] is returned. If the commitment
    is not finalized the error [Tx_rollup_commitment_not_final] is returned *)
val get_finalized :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_level_repr.t ->
  (Raw_context.t * Tx_rollup_commitment_repr.Submitted_commitment.t) tzresult
  Lwt.t

(** [pending_bonded_commitments ctxt tx_rollup contract] returns the
    number of commitments that [contract] has made that are still
    pending (that is, still subject to rejection). *)
val pending_bonded_commitments :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Signature.public_key_hash ->
  (Raw_context.t * int) tzresult Lwt.t

(** [has_bond ctxt tx_rollup contract] returns true if we have
    already collected a bond for [contract] for commitments on
    [tx_rollup]. *)
val has_bond :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Signature.public_key_hash ->
  (Raw_context.t * bool) tzresult Lwt.t

(** [finalize_commitment ctxt tx_rollup state] marks the commitment of
    the oldest inbox as final, if the commitment exists and if it is
    old enough. Otherwise, this function returns the error
    [No_commitment_to_finalize].

    The state of the rollup is adjusted accordingly, and the finalized
    level is returned. Besides, the inbox at said level is removed
    from the context. This function returns the new context, the new
    state, and the storage size diff. *)
val finalize_commitment :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_state_repr.t ->
  (Raw_context.t * Tx_rollup_state_repr.t * Tx_rollup_level_repr.t * Z.t)
  tzresult
  Lwt.t

(** [remove_commitment ctxt tx_rollup state] tries to remove the
    oldest finalized commitment from the layer-1 storage, if it
    exists, and if it is old enough. Otherwise, this functions returns
    the error [No_commitment_to_remove].

    The state of the rollup is adjusted accordingly. *)
val remove_commitment :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_state_repr.t ->
  (Raw_context.t * Tx_rollup_state_repr.t * Tx_rollup_level_repr.t) tzresult
  Lwt.t

(** [reject_commitment context tx_rollup state level] removes the
    commitment at [level].  It should only be called after a
    successful rejection operation. The [state] is updated to reflect
    the rejection, and returned. *)
val reject_commitment :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_state_repr.t ->
  Tx_rollup_level_repr.t ->
  (Raw_context.t * Tx_rollup_state_repr.t) tzresult Lwt.t

(** [get_before_and_after_results tx_rollup commitment
    ~message_position state] returns the before and after roots for a
    given [message_position], from [commitment] on [tx_rollup]. *)
val get_before_and_after_results :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_commitment_repr.Submitted_commitment.t ->
  message_position:int ->
  Tx_rollup_state_repr.t ->
  (Raw_context.t
  * Tx_rollup_commitment_repr.Message_result_hash.t
  * Tx_rollup_commitment_repr.Message_result_hash.t)
  tzresult
  Lwt.t
