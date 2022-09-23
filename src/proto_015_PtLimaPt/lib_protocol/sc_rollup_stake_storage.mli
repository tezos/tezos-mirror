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

    Any commitments no longer staked on are removed and storage reclaimed by
    [remove_staker]. Because of this there is no need to explicitly reject
    commitments.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_not_staked] if [staker] is not staked}
      {li [Sc_rollup_remove_lcc] if [staker] is staked on a cemented commitment}
    } *)
val remove_staker :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** This is a wrapper around [deposit_stake] and [refine_stake] that
    deposits a stake and then refines it to the specified commitment,
    creating that commitment if necessary. Before calling
    [deposit_stake] it checks that the staker is not already staked, and
    if so will skip that step and go straight to calling [refine_stake].

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_too_far_ahead] if [staker] would be more than
        [sc_rollup_max_future_commitments] ahead of the Last Cemented Commitment}
      {li [Sc_rollup_bad_inbox_level] if [commitment]'s predecessor is
        less than [sc_rollup_commitment_period] blocks ahead}
      {li [Sc_rollup_staker_backtracked] if [staker] is not staked on an ancestor
        of [commitment]}
      {li [Sc_rollup_unknown_commitment] if the parent of the given commitment
        does not exist}
      {li [Sc_rollup_staker_funds_too_low] if [staker] is not previously a staker, and does not have enough funds
        to cover the deposit}
    }

    Returns the hash of the given commitment, and the level when the commitment
    was first published by some staker.

    This function does not authenticate the staker. *)
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

(** [cement_commitment context rollup commitment] cements the given
    commitment whose hash is given (and returns the corresponding commitment).

    Subsequent calls to [refine_stake] and [cement_commitment] must use
    a [context] with greater level, or behavior is undefined.

    For cementing to succeed, the following must hold:
    {ol
      {li The deadline for [commitment] must have passed.}
      {li The predecessor of [commitment] must be the Last Cemented Commitment.}
      {li There must be at least one staker.}
      {li All stakers must be indirectly staked on [commitment].}
    }

    If successful, [last_cemented_commitment] is set to the given [commitment] and
    the appropriate amount of inbox messages is consumed. The old LCC is also
    deallocated.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_unknown_commitment] if [commitment] does not exist}
      {li [Sc_rollup_parent_not_lcc] if [commitment] is not the child of the last cemented commitment}
      {li [Sc_rollup_commitment_too_recent] if [commitment] has not passed its deadline}
      {li [Sc_rollup_no_stakers] if there are zero stakers}
      {li [Sc_rollup_disputed] if at least one staker is not staked on [commitment]}
    } *)
val cement_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_commitment_repr.Hash.t ->
  (Raw_context.t * Sc_rollup_commitment_repr.t) tzresult Lwt.t

(** [find_staker_unsafe ctxt rollup staker] returns the branch on which the stake
    is deposited for the [rollup]'s [staker].
    This function *must* be called only after they have checked for the existence
    of the rollup, and therefore it is not necessary for it to check for the
    existence of the rollup again. Otherwise, use the safe function
    {!find_staker}.

    May fail with [Sc_rollup_not_staked] if [staker] is not staked. *)
val find_staker_unsafe :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (Sc_rollup_commitment_repr.Hash.t * Raw_context.t) tzresult Lwt.t

(** Same as {!find_staker_unsafe} but checks for the existence of the [rollup]
    before. *)
val find_staker :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (Sc_rollup_commitment_repr.Hash.t * Raw_context.t) tzresult Lwt.t

(** The storage size requirement (in bytes) of a commitment *)
val commitment_storage_size_in_bytes : int

(** [withdraw_stake context rollup staker] removes [staker] and returns
      any deposit previously frozen by [deposit_stake].

      May fail with:
      {ul
        {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
        {li [Sc_rollup_not_staked_on_lcc] if [staker] is not staked on the last
            cemented commitment}
      }

      Note that it is not possible to be staked on a Cemented commitment other
      than the Last, because of Cementation Rule #4. See [cement_commitment]
      for details.

      By design, the operation wrapping this might {i not} be authenticated,
      as it may be necessary for nodes on the honest branch to refund stakers on
      the LCC. They must do so by using [withdraw_stake] as they are implicitly
      staked on the LCC and can not dispute it. *)
val withdraw_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

(**/**)

module Internal_for_tests : sig
  (** [deposit_stake context rollup staker] stakes [staker] at the last
      cemented commitment, freezing [sc_rollup_stake_amount] from [staker]'s
      account balance. It also returns the last cemented commitment of the
      [rollup] on which the staker just deposited.

      Warning: must be called only if [rollup] exists and [staker] is not to be
      found in {!Store.Stakers.}

      May fail with:
      {ul
        {li [Sc_rollup_staker_funds_too_low] if [staker] does not have enough
            funds to cover the deposit}
      }

      This should usually be followed by [refine_stake] to stake on a
      specific commitment.

      This function does not authenticate the staker. *)
  val deposit_stake :
    Raw_context.t ->
    Sc_rollup_repr.t ->
    Sc_rollup_repr.Staker.t ->
    (Raw_context.t
    * Receipt_repr.balance_updates
    * Sc_rollup_commitment_repr.Hash.t)
    tzresult
    Lwt.t

  (** [refine_stake context rollup staker ?staked_on commitment] moves the stake
      of [staker] on [?staked_on] to [commitment]. The function exposed
      in [Internal_for_tests] allows [staked_on] to be [None] and fetches
      the real value from the storage, but, the production code uses the
      already existing commitment on which the staker is staked.

      Because we do not assume any form of coordination between validators, we
      do not distinguish between {i adding new} commitments and {i staking on
      existing commitments}. The storage of commitments is content-addressable
      to minimize storage duplication.

      Subsequent calls to [refine_stake] and [cement_commitment] must use
      a [context] with greater level, or this function call will fail.

      The first time a commitment hash is staked on, it is assigned a deadline,
      which is counted in Tezos blocks (levels). Further stakes on the block does
      not affect the deadline. The commitment can not be cemented before the
      deadline has expired. Note that if a commitment is removed due to disputes
      and then re-entered, a later deadline may be assigned. Assuming one honest
      staker is always available, this only affects invalid commitments.

      May fail with:
      {ul
        {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
        {li [Sc_rollup_too_far_ahead] if [staker] would be more than
          [sc_rollup_max_future_commitments] ahead of the Last Cemented Commitment}
        {li [Sc_rollup_bad_inbox_level] if [commitment]'s predecessor is
          less than [sc_rollup_commitment_period] blocks ahead}
        {li [Sc_rollup_not_staked] if [staker] is not staked}
        {li [Sc_rollup_staker_backtracked] if [staker] is not staked on an ancestor of [commitment]}
        {li [Sc_rollup_unknown_commitment] if the parent of the given commitment does not exist}
      }

      Returns the hash of the given commitment.

      This function does not authenticate the staker. *)
  val refine_stake :
    Raw_context.t ->
    Sc_rollup_repr.t ->
    Sc_rollup_repr.Staker.t ->
    ?staked_on:Sc_rollup_commitment_repr.Hash.t ->
    Sc_rollup_commitment_repr.t ->
    (Sc_rollup_commitment_repr.Hash.t * Raw_level_repr.t * Raw_context.t)
    tzresult
    Lwt.t
end
