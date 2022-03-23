(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module provides a refutation game logic for smart contract rollups.

   It is in fact a functor that takes a PVM semantic implemented by a
   {!Sc_rollup_PVM_sem.S} module and produces a game logic.

   This game logic is used by the protocol when two commitments are in conflict
   to determine which one of the commitments is wrong.

   A commitment characterizes a section of the PVM execution trace. When
   two commitments are in conflict, they agree on the first state of the
   section and disagree on the last one. The game runs in two steps: first,
   the two players alternatively refine the section they disagree on until
   they reach an atomic section of length 1, that is a single execution step
   of the PVM ; second, the player must provide a Merkle proof that a
   valid execution step corresponds to her final state.

   Important invariants
   ====================

   - The committer and the refuter agree on the first state of the current
     section and disagree on its final state. If they agree on both then
     whoever plays loses by [InvalidMove].

   Expected properties
   ===================

   Honest-committer-wins:
   - If a committer has posted a valid commit and has a perfect PVM at hand,
     there is a winning strategy which consists in choosing the first section
     of the current dissection and in producing a regular dissection until the
     conflict is reached.

   Honest-refuter-wins:
   - If a refuter has detected an invalid commit and has a perfect PVM at hand,
     the same strategy is also winning.

   Here "winning strategy" means that the player actually wins (a draw is not
   enough).

   Remarks
   =======

   There are several subtle cornercases:

   - If the refuter and the committer both post only invalid states, the
     game may end in a conflict state where both are wrong. By convention,
     we decide that these games have no winner.

   - If the refuter and the committer both post only states, the
     game never has any conflicts. By convention,
     we decide that in this case the commiter wins by [InvalidMove].

   - If the refuter and the committer both post valid and invalid states,
     all outcomes are possible. This means that if a committer wins a
     game we have no guarantee that she has posted a valid commit.


*)

module Make : functor (PVM : Sc_rollup_PVM_sem.S) -> sig
  module PVM :
    Sc_rollup_PVM_sem.S
      with type state = PVM.state
       and type proof = PVM.proof
       and type context = PVM.context

  type tick = Sc_rollup_tick_repr.t

  module Section : sig
    type section = {
      section_start_state : PVM.hash;
      section_start_at : tick;
      section_stop_state : PVM.hash;
      section_stop_at : tick;
    }

    val section_encoding : section Data_encoding.t

    val pp_section : Format.formatter -> section -> unit

    (** A [dissection] is a split of a section in several smaller
        sections that form a partition of this section. *)
    type dissection

    val dissection_encoding : dissection option Data_encoding.t

    val pp_dissection : Format.formatter -> dissection -> unit

    (** [empty_dissection] is an empty set. *)
    val empty_dissection : dissection

    (** [add_section section dissection] contains the sections of
        [dissection] plus a new [section]. *)
    val add_section : section -> dissection -> dissection

    (** [find_section section dissection] returns [Some section]
        if [section] is a member of [dissection].
        Returns [None] otherwise. *)
    val find_section : section -> dissection -> section option

    (** [fold_over_dissection f dissection i] traverses [dissection]
        from the section of lowest tick to the section of highest ticks
        applying [f] to compute the next value of the accumulator,
        initially set to [i]. *)
    val fold_over_dissection :
      (tick -> section -> 'a -> 'a) -> dissection -> 'a -> 'a

    (** [dissection_cardinal dissection] returns the number of
        sections in the given [dissection]. *)
    val dissection_cardinal : dissection -> int

    (** [last_section dissection] returns [Some (tick, section)]
        if the last [section] of [dissection] starts at [tick].
        Returns [None] if [dissection] is empty. *)
    val last_section : dissection -> (tick * section) option

    (** A section is valid if its [start_at] tick is smaller than its
       [stop_at] tick. *)
    val valid_section : section -> bool

    (** [valid_dissection section dissection] returns [true] iff
        [dissection] is a partition of [section]. *)
    val valid_dissection : section -> dissection -> bool
  end

  (** The game has two players. The refuter starts. *)
  type player = Committer | Refuter

  val player_encoding : player Data_encoding.t

  val pp_player : Format.formatter -> player -> unit

  (** [opponent player] is the other player. *)
  val opponent : player -> player

  (**

     A game state is characterized by:

      - [turn], the player that must provide the next move.

      - [start_state/start_at], the state the two players agree on.

      - [player_stop_state] and [opponent_stop_state] are distinct as
        the two players disagree on the final state.

      - [stop_at] is the position of the final state.

      - If the cardinal of [current_dissection] is strictly greater than 1,
        the current player must choose one of its section and produce a
        dissection of this section.

      - If the cardinal of [current_dissection] is equal to 1,
        the game has reached its second phase and the current player
        must provide a proof of the execution step at [start_at].

      Invariants:
      -----------
      - [current_dissection] cannot be empty.
      - [start_at] is strictly less than [stop_at].
      - [player_stop_state] is distinct from [opponent_stop_state].

  *)
  type t = {
    turn : player;
    start_state : PVM.hash;
    start_at : tick;
    player_stop_state : PVM.hash;
    opponent_stop_state : PVM.hash;
    stop_at : tick;
    current_dissection : Section.dissection option;
  }

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  (** The conflict resolution is done in two phases: the refinement
      phase composed of refinement steps and a final step, the
      conclusion. *)
  type conflict_resolution_step =
    | Refine of {stop_state : PVM.hash; next_dissection : Section.dissection}
        (** Assuming a current [dissection], a [section] has been chosen and
            [next_dissection] is a dissection of this section. [stop_state]
            is a state which is different from the one exposed in for the
            [section] in the current [dissection]. *)
    | Conclude of Sc_rollup_PVM_sem.input option * PVM.proof
        (** When the current [dissection] contains a single section of
            length 1, the conflict tick has been found. The player provides
            a [PVM.proof] to demonstrate that her final state is the
            correct one.

            If the current state is an input state (see
            {!Sc_rollup_PVM_sem}), the player must provide the [input]
            received by the PVM. Notice that the validity of this [input]
            is externally verified using the {!Sc_rollup_inbox} proofs. *)

  (** A game move is a conflict resolution step performed in a given
      section of the PVM execution trace. *)
  type move =
    | ConflictInside of {
        choice : Section.section;
        conflict_resolution_step : conflict_resolution_step;
      }

  (** To start the game, the committer must have made a section public... *)
  type commit = Commit of Section.section

  (** ... and the refuter must have started a dispute. *)
  type refutation = RefuteByConflict of conflict_resolution_step

  (** The game can stop for two reasons: *)
  type reason =
    | InvalidMove
        (** One of the player did not respect the game rules, typically
            by providing a refinement step which is inconsistent with
            the current dissection. *)
    | ConflictResolved
        (** The game reached the conclusion and the arbiter was able to
            determine how the conflict must be resolved in conformance
            with the PVM semantics. *)

  val pp_reason : Format.formatter -> reason -> unit

  (** The game can end with no winner if both players have wrong final
      states at the conflicting step. *)
  type outcome = {winner : player option; reason : reason}

  val pp_outcome : Format.formatter -> outcome -> unit

  (** After one game move, the game can be over or ongoing. *)
  type move_result = Over of outcome | Ongoing of t

  val pp_move : Format.formatter -> move -> unit

  (** [initial] is the initial game state from the commit and the
     refutation. The first player to play is the refuter, this
     function also returns refuter's first move. *)
  val initial : commit -> conflict_resolution_step -> t * move

  (** [play game move] returns the result of the given [move] applied
      on [game]. *)
  val play : t -> move -> move_result Lwt.t
end
