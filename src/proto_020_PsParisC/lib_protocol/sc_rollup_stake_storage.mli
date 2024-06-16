(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** [remove_staker context rollup staker] forcibly removes the given [staker]
    and confiscates their frozen deposits.

    Removes [staker] from the list of active stakers on the [rollup] and
    clean its metadata.
*)
val remove_staker :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** [publish_commitment context rollup staker commitment] published [commitment].

    Starts by depositing a stake for [staker] if [staker] is not a known
    staker of [rollup]. Then, [staker] will use its stake to stake on
    [commitment].

    For publishing to succeed, the following must hold:
    {ol
      {li A deposit exists (or can be deposited) for [staker].}
      {li The commitment respects the commitment period and is not published
           in advance.}
      {li The commitment is not past the curfew, i.e., stakers has a limit on the
          available time to publish, if a staker already published for this
          inbox level.}
      {li The [commitment.predecessor] exists.}
      {li The [commitment] contains at least one tick.}
    }

    Returns the hash of the given commitment, the level when the commitment
    was first published by some staker, the modified context and the balance
    updates if a stake was deposited.

    This function does not authenticate the staker.
*)
val publish_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Sc_rollup_commitment_repr.t ->
  (Sc_rollup_commitment_repr.Hash.t
  * Raw_level_repr.t
  * Raw_context.t
  * Receipt_repr.balance_updates)
  tzresult
  Lwt.t

(** [cement context rollup] tries to cement the next inbox level commitment,
    that is, the LCC's successor. Returns the cemented commitment hash and
    its hash.

    For cementing to succeed, we need to have **one** commitment respecting
    the following properties:
    {ol
      {li The deadline for [commitment] must have passed.}
      {li The predecessor of [commitment] must be the Last Cemented Commitment.}
      {li There must be at least one staker.}
      {li All stakers must be indirectly staked on [commitment].}
    }

    If successful, Last Cemented commitment is set to the found commitment,
    and deallocate the old cemented commitment accordingly to the number
    of stored cemented commitments.

    Clean the storage for the metadata added for this inbox level.
*)
val cement_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Raw_context.t
  * Sc_rollup_commitment_repr.t
  * Sc_rollup_commitment_repr.Hash.t)
  tzresult
  Lwt.t

(** [find_staker context rollup staker] returns the most recent commitment
    [staker] staked on, or [None] if its last staked commitment is older
    or equal than the last cemented commitment. *)
val find_staker :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (Raw_context.t * Sc_rollup_commitment_repr.Hash.t option) tzresult Lwt.t

(** [is_staked_on context rollup staker commitment_hash] returns true
    iff [staker] is an active staker and has staked on [commitment_hash]. *)
val is_staked_on :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Sc_rollup_commitment_repr.Hash.t ->
  (Raw_context.t * bool) tzresult Lwt.t

(** [commitments_uncarbonated ctxt ~rollup ~inbox_level] returns the
    list of commitments associated to [rollup] at [inbox_level] *)
val commitments_uncarbonated :
  Raw_context.t ->
  rollup:Sc_rollup_repr.t ->
  inbox_level:Raw_level_repr.t ->
  Sc_rollup_commitment_repr.Hash.t list option tzresult Lwt.t

(** [stakers_ids_uncarbonated ctxt ~rollup ~commitment] returns the
    list of stakers' indexes associated to [rollup] for a specific
    [commitment] *)
val stakers_ids_uncarbonated :
  Raw_context.t ->
  rollup:Sc_rollup_repr.t ->
  commitment:Sc_rollup_commitment_repr.Hash.t ->
  Sc_rollup_staker_index_repr.t list tzresult Lwt.t

(** [staker_id_uncarbonated ctxt ~rollup ~pkh] returns the staker's
    index associated to the public key hash [pkh] *)
val staker_id_uncarbonated :
  Raw_context.t ->
  rollup:Sc_rollup_repr.t ->
  pkh:Signature.public_key_hash ->
  Sc_rollup_staker_index_repr.t tzresult Lwt.t

(** [stakers_pkhs_uncarbonated ctxt ~rollup] returns the public key hashes 
    of stakers that are currently actively staking on [rollup] *)
val stakers_pkhs_uncarbonated :
  Raw_context.t ->
  rollup:Sc_rollup_repr.t ->
  Signature.public_key_hash list Lwt.t

(** [withdraw_stake context rollup staker] removes [staker] and cleans
    its metadata. [staker] is allowed to withdraw if it latest staked
    commitment is older than the last cemented commitment.
*)
val withdraw_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** [commitments_of_inbox_level ctxt rollup inbox_level] returns the list
    of commitments for [inbox_level]. *)
val commitments_of_inbox_level :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Raw_level_repr.t ->
  (Raw_context.t * Sc_rollup_commitment_repr.Hash.t list) tzresult Lwt.t

(** [stakers_of_commitment ctxt rollup commitment_hash] returns the list
    of stakers staking on [commitment_hash]. *)
val stakers_of_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_commitment_repr.Hash.t ->
  (Raw_context.t * Sc_rollup_staker_index_repr.t list) tzresult Lwt.t

(** [find_commitment_of_staker_in_commitments ctxt rollup staker_index commitments]
    selects in [commitments] the hash of the commitment staked by
    [staker_index], if any. *)
val find_commitment_of_staker_in_commitments :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_staker_index_repr.t ->
  Sc_rollup_commitment_repr.Hash.t list ->
  (Raw_context.t * Sc_rollup_commitment_repr.Hash.t option) tzresult Lwt.t

(**/**)

module Internal_for_tests : sig
  (** [deposit_stake context rollup staker] stakes [staker] on the [rollup] by
      freezing [sc_rollup_stake_amount] from [staker]'s account balance and
      initializing [staker]'s metadata.

      This should usually be followed by [refine_stake] to stake on a
      specific commitment.

      Returns the modified context, the balance updates of the frozen
      deposit and the index created for [staker].
  *)
  val deposit_stake :
    Raw_context.t ->
    Sc_rollup_repr.t ->
    Sc_rollup_repr.Staker.t ->
    (Raw_context.t
    * Receipt_repr.balance_updates
    * Sc_rollup_staker_index_repr.t)
    tzresult
    Lwt.t

  (** [refine_stake context rollup commitment staker] makes [staker]
      stakes on [commitment].

      Because we do not assume any form of coordination between validators, we
      do not distinguish between {i adding new} commitments and {i staking on
      existing commitments}. The storage of commitments is content-addressable
      to minimize storage duplication.

      The first time a commitment hash is staked on, it is assigned a deadline,
      which is counted in Tezos blocks (levels). Further stakes on the block does
      not affect the deadline. The commitment can not be cemented before the
      deadline has expired. Note that if a commitment is removed due to disputes
      and then re-entered, a later deadline may be assigned. Assuming one honest
      staker is always available, this only affects invalid commitments.

      See {!publish_commitment} to see the list of properties this function
      enforces.

      Returns the hashed commitment, at the first level this commitment was
      published, and the modified context.
  *)
  val refine_stake :
    Raw_context.t ->
    Sc_rollup_repr.t ->
    Sc_rollup_repr.Staker.t ->
    Sc_rollup_commitment_repr.t ->
    (Sc_rollup_commitment_repr.Hash.t * Raw_level_repr.t * Raw_context.t)
    tzresult
    Lwt.t

  (** The maximum storage size requirement (in bytes) of a commitment *)
  val max_commitment_storage_size_in_bytes : int
end
