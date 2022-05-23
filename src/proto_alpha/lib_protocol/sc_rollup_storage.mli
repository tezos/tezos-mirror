(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** [originate context ~kind ~boot_sector] produces an address [a] for
   a smart contract rollup using the origination nonce found in
   [context]. This function also initializes the storage with a new
   entry indexed by [a] to remember the [kind] of the rollup at
   address [a] and also to remember its [boot_sector].

   Also returns the number of allocated bytes.  *)
val originate :
  Raw_context.t ->
  kind:Sc_rollup_repr.Kind.t ->
  boot_sector:string ->
  (Sc_rollup_repr.Address.t * Z.t * Raw_context.t) tzresult Lwt.t

(** [kind context address] returns [Some kind] iff [address] is an
    existing rollup of some [kind]. Returns [None] if [address] is
    not the address of an existing rollup. *)
val kind :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Kind.t option tzresult Lwt.t

(** [deposit_stake context rollup staker] stakes [staker] at the last
    cemented commitment, freezing [sc_rollup_deposit] from [staker]'s account
    balance.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_already_staked] if [staker] is already staked}
      {li [Sc_rollup_staker_funds_too_low] if [staker] does not have enough funds to cover the deposit}
    }

    This should usually be followed by [refine_stake] to stake on a
    specific commitment.

    This function does not authenticate the staker. *)
val deposit_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Raw_context.t tzresult Lwt.t

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

    By design, the operation wrapping this should {i not} be authenticated,
    as it may be necessary for nodes on the honest branch to refund stakers on
    the LCC. They must do so by using [withdraw_stake] as they are implicitly
    staked on the LCC and can not dispute it. *)
val withdraw_stake :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Raw_context.t tzresult Lwt.t

(** [refine_stake context rollup staker commitment] moves the stake of
    [staker] to [commitment]. Because we do not assume any form of coordination
    between validators, we do not distinguish between {i adding new}
    commitments and {i staking on existing commitments}.  The storage of
    commitments is content-addressable to minimize storage duplication.

    Subsequent calls to [refine_stake] and [cement_commitment] must use
    a [context] with greater level, or behavior is undefined.

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
  Sc_rollup_repr.Commitment.t ->
  (Sc_rollup_repr.Commitment_hash.t * Raw_level_repr.t * Raw_context.t) tzresult
  Lwt.t

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
  Sc_rollup_repr.Commitment.t ->
  (Sc_rollup_repr.Commitment_hash.t * Raw_level_repr.t * Raw_context.t) tzresult
  Lwt.t

(** [cement_commitment context rollup commitment] cements the given
    commitment.

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
      {li [Sc_rollup_too_recent] if [commitment] has not passed its deadline}
      {li [Sc_rollup_no_stakers] if there are zero stakers}
      {li [Sc_rollup_disputed] if at least one staker is not staked on [commitment]}
    } *)
val cement_commitment :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Commitment_hash.t ->
  Raw_context.t tzresult Lwt.t

type conflict_point =
  Sc_rollup_repr.Commitment_hash.t * Sc_rollup_repr.Commitment_hash.t

(** [get_conflict_point context rollup staker1 staker2] returns the first point
    of disagreement between the given stakers. The returned commitments are
    distinct, and have the same [parent] commitment.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_no_conflict] if [staker1] is staked on an ancestor of the
         commitment staked on by [staker2], or vice versa}
      {li [Sc_rollup_not_staked] if one of the stakers is not staked}
    } *)
val get_conflict_point :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  Sc_rollup_repr.Staker.t ->
  (conflict_point * Raw_context.t) tzresult Lwt.t

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
  Raw_context.t tzresult Lwt.t

(** [list context] returns a list of all rollups that have been originated. *)
val list : Raw_context.t -> Sc_rollup_repr.t list tzresult Lwt.t

(** [initial_level ctxt sc_rollup] returns the level at which a [sc_rollup] was
   originated. *)
val initial_level :
  Raw_context.t -> Sc_rollup_repr.t -> Raw_level_repr.t tzresult Lwt.t

(** [get_boot_sector ctxt sc_rollup] retrieves the boot sector for [sc_rollup]. *)
val get_boot_sector : Raw_context.t -> Sc_rollup_repr.t -> string tzresult Lwt.t

(** [get_or_init_game ctxt rollup refuter defender] returns the current
    game between the two stakers [refuter] and [defender] if it exists.

    If it does not already exist, it creates one with [refuter] as the
    first player to move. The initial state of the game will be obtained
    from the commitment pair belonging to [defender] at the conflict
    point. See [Sc_rollup_game_repr.initial] for documentation on how a
    pair of commitments is turned into an initial game state.

    This also deals with the other bits of data in the storage around
    the game. It checks neither staker is already in a game (and also
    marks them as in a game once the new game is created). The reason we
    only allow a staker to play one game at a time is to keep the
    end-of-game logic simple---this way, a game can't end suddenly in
    the middle because one player lost their stake in another game, it
    can only end due to it's own moves or timeouts.

    It also initialises the timeout level to the current level plus
    [timeout_period_in_blocks] (which will become a protocol constant
    soon) to mark the block level at which it becomes possible for
    anyone to end the game by timeout.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_no_conflict] if [refuter] is staked on an ancestor of
         the commitment staked on by [defender], or vice versa}
      {li [Sc_rollup_not_staked] if one of the [refuter] or [defender] is
         not actually staked}
      {li [Sc_rollup_staker_in_game] if one of the [refuter] or [defender]
         is already playing a game}
    } *)
val get_or_init_game :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  refuter:Sc_rollup_repr.Staker.t ->
  defender:Sc_rollup_repr.Staker.t ->
  (Sc_rollup_game_repr.t * Raw_context.t) tzresult Lwt.t

(** [update_game ctxt rollup player opponent refutation] handles the
    storage-side logic for when one of the players makes a move in the
    game. It initializes the game if necessary (the first move looks
    much like any other). It checks that [player] is the player whose
    turn it is; if so, it applies [refutation] using the [play] function.

    If the result is a new game, this is stored and the timeout level is
    updated.

    If the result is an [outcome], this will be returned.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_no_conflict] if [player] is staked on an ancestor of
         the commitment staked on by [opponent], or vice versa}
      {li [Sc_rollup_not_staked] if one of the [player] or [opponent] is
         not actually staked}
      {li [Sc_rollup_staker_in_game] if one of the [player] or [opponent]
         is already playing a game}
      {li [Sc_rollup_wrong_turn] if a player is trying to move out of
         turn}
    } *)
val update_game :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  player:Sc_rollup_repr.Staker.t ->
  opponent:Sc_rollup_repr.Staker.t ->
  Sc_rollup_game_repr.refutation ->
  (Sc_rollup_game_repr.outcome option * Raw_context.t) tzresult Lwt.t

(* TODO: #2902 update reference to timeout period in doc-string *)

(** [timeout ctxt rollup stakers] checks that the timeout has
    elapsed and if this function returns a game outcome that punishes whichever
    of [stakers] is supposed to have played a move.

    The timeout period is currently defined in
    [timeout_period_in_blocks]. This should become a protocol constant
    soon.

    May fail with:
    {ul
      {li [Sc_rollup_no_game] if the game does not in fact exist}
      {li [Sc_rollup_timeout_level_not_reached] if the player still has
         time in which to play}
    }

    Note: this function takes the two stakers as a pair rather than
    separate arguments. This reflects the fact that for this function
    the two players are symmetric. This function will normalize the
    order of the players if necessary to get a valid game index, so the
    argument [stakers] doesn't have to be in normal form. *)
val timeout :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t * Sc_rollup_repr.Staker.t ->
  (Sc_rollup_game_repr.outcome * Raw_context.t) tzresult Lwt.t

(** [apply_outcome ctxt rollup outcome] takes an [outcome] produced
    by [timeout] or [update_game] and performs the necessary end-of-game
    cleanup: remove the game itself from the store and punish the losing
    player by removing their stake.

    It also translates the 'internal' type to represent the end of the
    game, [outcome], into the [status] type that makes sense to the
    outside world.

    This is mostly just calling [remove_staker], so it can fail with the
    same errors as that. However, if it is called on an [outcome]
    generated by [update_game] or [timeout] it should not fail.

    Note: this function takes the two stakers as a pair rather than
    separate arguments. This reflects the fact that for this function
    the two players are symmetric. This function will normalize the
    order of the players if necessary to get a valid game index, so the
    argument [stakers] doesn't have to be in normal form. *)
val apply_outcome :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t * Sc_rollup_repr.Staker.t ->
  Sc_rollup_game_repr.outcome ->
  (Sc_rollup_game_repr.status * Raw_context.t) tzresult Lwt.t

(** A module for managing state concerning a rollup's outbox. *)
module Outbox : sig
  (** [record_applied_message ctxt rollup level ~message_index] marks the
      message in the outbox of rollup [rollup] at level [level] and position
      [message_index] as processed. Returns the size diff resulting from
      adding an entry. The size diff may be 0 if an entry already exists, or
      negative if an index is replaced with a new level.

      An attempt to apply an old level that has already been replaced
      fails with an [Sc_rollup_outbox_level_expired] error.

      In case a message has already been applied for the given level and message
      index, the function fails with an
      [Sc_rollup_outbox_message_already_applied]  error.  *)
  val record_applied_message :
    Raw_context.t ->
    Sc_rollup_repr.t ->
    Raw_level_repr.t ->
    message_index:int ->
    (Z.t * Raw_context.t) tzresult Lwt.t
end
