(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** The smart contract rollup refutation game types are defined here, as
    well as the basic pure logic for:

    - how to create a new game from a pair of commits in the commit tree;

    - how to update a game or complete a game when a move is played.

    This game logic is used by the protocol when two commitments are in
    conflict to determine which one of the commitments is wrong.

    Game state and moves
    ====================

    At any given moment, the game stores a list [dissection] of state
    hashes and tick counts. These are the claims about the PVM history
    made by the player who has just moved.

    The next player to move will specify a tick count which appears in
    the [dissection]; this is the last of the state hashes which she
    agrees with. She will then either:

    - provide a new [dissection] by giving a list of state hashes and
    tick counts that starts at the chosen tick count and ends at the
    next tick count in the previous [dissection]. It must agree at the
    start but disagree with the final state.

    - if the tick difference between this state and the next is one,
    there is no 'room' for a new [dissection]. In this case she must
    provide a Merkle proof that shows the step in the current
    [dissection] is invalid.

    Initializing a game
    ===================

    In order to trigger the start of a game, one player must publish a
    first move.

    The [initial] function is called at this point. It converts a
    parent-child pair of commitments (belonging to the other player) into
    an initial [dissection]. The first move is immediately applied to
    this to give the first state of the game.

    Note: it is quite possible for the game to end immediately after
    this first move, either if the commitment has a tick count of one or
    more probably if the refutation proves that the commitment was
    'premature' (the state is not blocked---there are further
    computation steps to do or more inbox messages to read).

    Expected properties
    ===================

    P1 - If [dissection] is honest, the next move must be dishonest:

      There is only one honest state hash for a given tick count. The
      next player must provide a different hash to the honest hash in
      the [dissection].

    P2 - If [dissection] is dishonest, there is a strategy for a player
    equipped with a perfect PVM to play an honest next move:

      The player with a perfect PVM can calculate honest hashes until
      one disagrees with the [dissection], and challenge the dissection
      at that point, publishing either an honest [dissection] or an
      honest [Proof].

    Each [dissection] has a maximum tick count step shorter than the
    last, so by induction using P1 and P2 we have

    P1' - If [dissection] is honest, the last player has a winning
    strategy.

    P2' - If [dissection] is dishonest, the next player has a winning
    strategy.

    This allows us to see the following. (We use [refuter] to mean the
    first player to move, and [defender] to mean the other player.)

    Honest refuter wins:
      An honest refuter will be refuting a dishonest commitment, because
      there is only one honest state possible per level. Therefore the
      initial [dissection] will be dishonest. By P2' the refuter has a
      winning strategy.

    Honest defender wins:
      An honest defender will have made an honest commitment which will
      be translated into an honest initial [dissection]. By P1' the
      defender has a winning strategy.

    In the case that both players are dishonest the outcome is slightly
    harder to determine---it basically comes down to which player gets
    to play the final move. That means that in this case the refutation
    game is equivalent to a finite game of nim (by the Sprague-Grundy
    Theorem, see en.wikipedia.org/wiki/Sprague-Grundy_theorem). However,
    this fact is completely irrelevant, because if the rollup is working
    correctly the winner will subsequently be defeated by some honest
    opponent.
*)

open Sc_rollup_repr

(** Currently, [Proof] is a dummy type with the structure we need, set
    up to allow testing. *)
module Proof : sig
  (** There are three cases for a refutation game proof:

      [Computation_step]: a simple step in the PVM that doesn't involve
      any interaction with the inbox.

      [Input_step]: a step in which the PVM 'reads' from the inbox. This
      will include a proof that the machine is in a blocked state and a
      proof that the next message to be read is correct. The inbox proof
      part of this will refer to the [inbox_snapshot] stored in the game
      type (see {!Sc_rollup_game_repr.t}).

      [Blocked_step]: similar to an input step, this is a step where the
      machine is in a blocked state. However, it includes a proof that
      there are no further messages in the inbox at the current level.
      This means the machine is genuinely blocked and the [stop] state
      of this [Proof] will be [None]. *)
  type t =
    | Computation_step of {
        valid : bool;
        start : State_hash.t;
        stop : State_hash.t;
      }
    | Input_step of {valid : bool; start : State_hash.t; stop : State_hash.t}
    | Blocked_step of {valid : bool; start : State_hash.t}

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  (** The state hash of the machine before the step. *)
  val start : t -> State_hash.t

  (** The state hash of the machine after the step. *)
  val stop : t -> State_hash.t option

  (** Check the validity of a proof *)
  val valid : t -> bool
end

(** The two stakers index the game in the storage as a pair of public
    key hashes which is in lexical order. We use [Alice] and [Bob] to
    represent the first and second player in the pair respectively. *)
type player = Alice | Bob

(**
   A game state is characterized by:

    - [turn], the player that must provide the next move.

    - [inbox_snapshot], a snapshot of the inbox state at the moment the
      game is created. This is only used when checking [Input_step] and
      [Blocked_step] proofs; it makes the proofs easier to create---
      otherwise they would have a 'moving target' because the actual
      inbox may be updated continuously.

    - [dissection], a list of states with tick counts. The current
      player will specify, in the next move, a tick count that
      indicates the last of these states that she agrees with.

    Invariants:
    -----------
    - [dissection] must contain at least 3 values
    - only the last value in [dissection] may be [None]
    - [inbox_snapshot] never changes once the game is created
*)
type t = {
  turn : player;
  inbox_snapshot : Sc_rollup_inbox_repr.t;
  dissection : (State_hash.t option * Sc_rollup_tick_repr.t) list;
}

(** Return the other player *)
val opponent : player -> player

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

module Index : sig
  type t = Staker.t * Staker.t

  (** [to_path i p] returns a new path with the path to the game indexed
      by [i] added as a prefix to path [p]. See [Path_encoding] module. *)
  val to_path : t -> string list -> string list

  val of_path : string list -> t option

  val path_length : int

  val rpc_arg : t RPC_arg.t

  val encoding : t Data_encoding.t

  val compare : t -> t -> int

  (** The 'normal form' for indices is when the two stakers appear in
      the pair in lexical order (we just use [Staker.compare]). *)
  val normalize : t -> t

  (** Given an index in normal form, resolve a given [player] ([Alice]
      or [Bob]) to the actual staker they represent. *)
  val staker : Staker.t * Staker.t -> player -> Staker.t
end

(** To begin a game, first the conflict point in the commit tree is
    found, and then this function is applied.

    [initial inbox parent child refuter defender] will construct an
    initial game where [refuter] is next to play. The game has
    [dissection] with three states:

      - firstly, the state (with tick zero) of [parent], the commitment
      that both stakers agree on.

      - secondly, the state and tick count of [child], the commitment
      that [defender] has staked on.

      - thirdly, a [None] state which is a single tick after the [child]
      commitment. This represents the claim, implicit in the commitment,
      that the state given is blocked.

    This gives [refuter] a binary choice: she can refute the commit
    itself by providing a new dissection between the two committed
    states, or she can refute the claim that the [child] commit is a
    blocked state by immediately providing a proof of a single tick
    increment from that state to its successor. *)
val initial :
  Sc_rollup_inbox_repr.t ->
  parent:Commitment.t ->
  child:Commitment.t ->
  refuter:Staker.t ->
  defender:Staker.t ->
  t

(** A [step] in the game is either a new dissection (if there are
    intermediate ticks remaining to put in it) or a proof. *)
type step =
  | Dissection of (State_hash.t option * Sc_rollup_tick_repr.t) list
  | Proof of Proof.t

(** A [refutation] is a move in the game. [choice] is the final tick
    in the current dissection at which the two players agree. *)
type refutation = {choice : Sc_rollup_tick_repr.t; step : step}

val pp_refutation : Format.formatter -> refutation -> unit

val refutation_encoding : refutation Data_encoding.t

(** A game ends for one of three reasons: the conflict has been
    resolved via a proof, a player has been timed out, or a player has
    forfeited because of attempting to make an invalid move. *)
type reason = Conflict_resolved | Invalid_move | Timeout

val pp_reason : Format.formatter -> reason -> unit

val reason_encoding : reason Data_encoding.t

(** A type that represents the current game status in a way that is
    useful to the outside world (using actual [Staker.t] values
    instead of the internal [player] type).

    The [Staker.t] in the [Ended] case is the loser of the game: the
    staker who will have their stake slashed.

    Used in operation result types. *)
type status = Ongoing | Ended of (reason * Staker.t)

val pp_status : Format.formatter -> status -> unit

val status_encoding : status Data_encoding.t

(** A game ends with a single [loser] and the [reason] for the game
    ending. This type uses [Alice] or [Bob] to refer to the players
    without knowing which stakers they are---so it cannot identify an
    actual staker who should be punished without the associated game
    index. *)
type outcome = {loser : player; reason : reason}

val pp_outcome : Format.formatter -> outcome -> unit

val outcome_encoding : outcome Data_encoding.t

(** Applies the move [refutation] to the game. Checks the move is
    valid and returns an [Invalid_move] outcome if not.

    In the case of the game continuing, this swaps the current
    player and updates the [dissection]. In the case of a [Proof]
    being provided this returns an [outcome]. *)
val play : t -> refutation -> (outcome, t) Either.t
