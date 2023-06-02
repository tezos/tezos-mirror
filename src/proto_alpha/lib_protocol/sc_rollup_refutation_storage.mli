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

module Commitment_hash = Sc_rollup_commitment_repr.Hash

type point = {
  commitment : Sc_rollup_commitment_repr.t;
  hash : Commitment_hash.t;
}

type conflict_point = point * point

(** [get_ongoing_games_for_staker ctxt rollup staker] returns [games],
   the list of refutation games currently played by [staker] in the
   [rollup]. *)
val get_ongoing_games_for_staker :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  ((Sc_rollup_game_repr.t * Sc_rollup_game_repr.Index.t) list * Raw_context.t)
  tzresult
  Lwt.t

(** [get_game ctxt rollup stakers] returns the [game] between
    [stakers.alice] and [stakers.bob]. *)
val find_game :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_game_repr.Index.t ->
  (Raw_context.t * Sc_rollup_game_repr.t option) tzresult Lwt.t

(** A conflict between a staker and an [other] staker. The conflict is
   about the commitment that follows the [parent_commitment]:
   [their_commitment] and [our_commitment] are distinct, hence in
   conflict. *)
type conflict = {
  other : Sc_rollup_repr.Staker.t;
  their_commitment : Sc_rollup_commitment_repr.t;
  our_commitment : Sc_rollup_commitment_repr.t;
  parent_commitment : Sc_rollup_commitment_repr.Hash.t;
}

val conflict_encoding : conflict Data_encoding.t

(** [conflicting_stakers_uncarbonated rollup staker] returns the list
   of conflicts with [staker] in [rollup].

   Notice that this operation can be expensive as it is proportional
   to the number of stakers multiplied by the number of commitments in
   the staked branches. Fortunately, this operation is only useful as
   an RPC for the rollup node to look for a new conflict to solve. *)
val conflicting_stakers_uncarbonated :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_repr.Staker.t ->
  conflict list tzresult Lwt.t

(** [start_game ctxt rollup ~player:(player, player_commitment_hash)
    ~opponent:(opponent, opponent_commitment_hash)] initiates a refutation game
    between [player] and [opponent] in the given [rollup] as they are in
    conflict with [commitment] and [opponent_commitment]. Where [commitment] is
    the commitment in the storage with hash [player_commitment_hash]
    (resp. [opponent_commitment] with [opponent_commitment_hash]). *)
val start_game :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  player:Signature.public_key_hash * Sc_rollup_commitment_repr.Hash.t ->
  opponent:Signature.public_key_hash * Sc_rollup_commitment_repr.Hash.t ->
  Raw_context.t tzresult Lwt.t

(** [game_move ctxt rollup player opponent refutation]
    handles the storage-side logic for when one of the players makes a
    move in the game. It checks the game already exists. Then it checks
    that [player] is the player whose turn it is; if so, it applies
    [refutation] using the [play] function.

    If the result is a new game, this is stored and the timeout is updated.

    May fail with:
    {ul
      {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
      {li [Sc_rollup_no_game] if [is_opening_move] is [false] but the
         game does not exist}
      {li [Sc_rollup_game_already_started] if [is_opening_move] is [true]
         but the game already exists}
      {li [Sc_rollup_no_conflict] if [player] is staked on an ancestor of
         the commitment staked on by [opponent], or vice versa}
      {li [Sc_rollup_not_staked] if one of the [player] or [opponent] is
         not actually staked}
      {li [Sc_rollup_staker_in_game] if one of the [player] or [opponent]
         is already playing a game}
      {li [Sc_rollup_wrong_turn] if a player is trying to move out of
         turn}
    }

    The [is_opening_move] argument is included here to make sure that an
    operation intended to start a refutation game is never mistaken for
    an operation to play the second move of the game---this may
    otherwise happen due to non-deterministic ordering of L1 operations.
    With the [is_opening_move] parameter, the worst case is that the
    operation simply fails. Without it, the operation would be mistaken
    for an invalid move in the game and the staker would lose their
    stake! *)
val game_move :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  player:Sc_rollup_repr.Staker.t ->
  opponent:Sc_rollup_repr.Staker.t ->
  step:Sc_rollup_game_repr.step ->
  choice:Sc_rollup_tick_repr.t ->
  (Sc_rollup_game_repr.game_result option * Raw_context.t) tzresult Lwt.t

(** [timeout ctxt rollup stakers] checks that the timeout has
    elapsed and if this function returns a game result that punishes whichever
    of [stakers] is supposed to have played a move.

    The timeout period is defined a protocol constant, see
    {!Constants_storage.sc_rollup_timeout_period_in_blocks}.

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
  Sc_rollup_game_repr.Index.t ->
  (Sc_rollup_game_repr.game_result * Raw_context.t) tzresult Lwt.t

(** [get_timeout ctxt rollup stakers] returns the current timeout values of both
    players. *)
val get_timeout :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_game_repr.Index.t ->
  (Sc_rollup_game_repr.timeout * Raw_context.t) tzresult Lwt.t

(** [apply_game_result ctxt rollup game_result] takes a [game_result] produced
    by [timeout] or [game_move] and performs the necessary end-of-game
    cleanup: remove the game itself from the store and punish the losing
    player by removing their stake. In the case where the game ended in
    a draw, both players are slashed.

    This is mostly just calling [remove_staker], so it can fail with the
    same errors as that. However, if it is called on an [game_result]
    generated by [game_move] or [timeout] it should not fail.

    Note: this function takes the two stakers as a pair rather than
    separate arguments. This reflects the fact that for this function
    the two players are symmetric. This function will normalize the
    order of the players if necessary to get a valid game index, so the
    argument [stakers] doesn't have to be in normal form. *)
val apply_game_result :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Sc_rollup_game_repr.Index.t ->
  Sc_rollup_game_repr.game_result ->
  (Sc_rollup_game_repr.status * Raw_context.t * Receipt_repr.balance_updates)
  tzresult
  Lwt.t

(**/**)

module Internal_for_tests : sig
  (** [get_conflict_point context rollup staker1 staker2] returns the
      first point of disagreement between the [staker1] and the
      [staker2]
      The returned commitments are distinct, and have the same [parent]
      commitment; hence the same inbox level.

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
end
