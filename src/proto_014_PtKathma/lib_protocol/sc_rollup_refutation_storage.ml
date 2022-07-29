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

open Sc_rollup_errors
module Store = Storage.Sc_rollup
module Commitment = Sc_rollup_commitment_repr
module Commitment_storage = Sc_rollup_commitment_storage
module Commitment_hash = Commitment.Hash
module Stake_storage = Sc_rollup_stake_storage

type point = {
  commitment : Sc_rollup_commitment_repr.t;
  hash : Commitment_hash.t;
}

type conflict_point = point * point

(** TODO: #2902 replace with protocol constant and consider good value. *)
let timeout_period_in_blocks = 500

let timeout_level ctxt =
  let level = Raw_context.current_level ctxt in
  Raw_level_repr.add level.level timeout_period_in_blocks

(** [goto_inbox_level ctxt rollup inbox_level commit] Follows the predecessors of [commit] until it
    arrives at the exact [inbox_level]. The result is the commit hash at the given inbox level. *)
let goto_inbox_level ctxt rollup inbox_level commit =
  let open Lwt_tzresult_syntax in
  let rec go ctxt commit =
    let* info, ctxt =
      Commitment_storage.get_commitment_unsafe ctxt rollup commit
    in
    if Raw_level_repr.(info.Commitment.inbox_level <= inbox_level) then (
      (* Assert that we're exactly at that level. If this isn't the case, we're most likely in a
         situation where inbox levels are inconsistent. *)
      assert (Raw_level_repr.(info.inbox_level = inbox_level)) ;
      return (commit, ctxt))
    else (go [@ocaml.tailcall]) ctxt info.predecessor
  in
  go ctxt commit

let get_conflict_point ctxt rollup staker1 staker2 =
  let open Lwt_tzresult_syntax in
  (* Ensure the LCC is set. *)
  let* lcc, ctxt = Commitment_storage.last_cemented_commitment ctxt rollup in
  (* Find out on which commitments the competitors are staked. *)
  let* commit1, ctxt = Stake_storage.find_staker ctxt rollup staker1 in
  let* commit2, ctxt = Stake_storage.find_staker ctxt rollup staker2 in
  let* () =
    fail_when
      Commitment_hash.(
        (* If PVM is in pre-boot state, there might be stakes on the zero commitment. *)
        commit1 = zero || commit2 = zero
        (* If either commit is the LCC, that also means there can't be a conflict. *)
        || commit1 = lcc
        || commit2 = lcc)
      Sc_rollup_no_conflict
  in
  let* commit1_info, ctxt =
    Commitment_storage.get_commitment_unsafe ctxt rollup commit1
  in
  let* commit2_info, ctxt =
    Commitment_storage.get_commitment_unsafe ctxt rollup commit2
  in
  (* Make sure that both commits are at the same inbox level. In case they are not move the commit
     that is farther ahead to the exact inbox level of the other.

     We do this instead of an alternating traversal of either commit to ensure the we can detect
     wonky inbox level increases. For example, if the inbox levels decrease in different intervals
     between commits for either history, we risk going past the conflict point and accidentally
     determined that the commits are not in conflict by joining at the same commit. *)
  let target_inbox_level =
    Raw_level_repr.min commit1_info.inbox_level commit2_info.inbox_level
  in
  let* commit1, ctxt =
    goto_inbox_level ctxt rollup target_inbox_level commit1
  in
  let* commit2, ctxt =
    goto_inbox_level ctxt rollup target_inbox_level commit2
  in
  (* The inbox level of a commitment increases by a fixed amount over the preceding commitment.
     We use this fact in the following to efficiently traverse both commitment histories towards
     the conflict points. *)
  let rec traverse_in_parallel ctxt commit1 commit2 =
    (* We know that commit1 <> commit2 at the first call and during recursive calls
       as well. *)
    let* commit1_info, ctxt =
      Commitment_storage.get_commitment_unsafe ctxt rollup commit1
    in
    let* commit2_info, ctxt =
      Commitment_storage.get_commitment_unsafe ctxt rollup commit2
    in
    (* This assert should hold because:
       - We call function [traverse_in_parallel] with two initial commitments
       whose levels are equal to [target_inbox_level],
       - In recursive calls, the commitments are replaced by their respective
       predecessors, and we know that successive commitments in a branch are
       spaced by [sc_rollup_commitment_period_in_blocks] *)
    assert (Raw_level_repr.(commit1_info.inbox_level = commit2_info.inbox_level)) ;
    if Commitment_hash.(commit1_info.predecessor = commit2_info.predecessor)
    then
      (* Same predecessor means we've found the conflict points. *)
      return
        ( ( {hash = commit1; commitment = commit1_info},
            {hash = commit2; commitment = commit2_info} ),
          ctxt )
    else
      (* Different predecessors means they run in parallel. *)
      (traverse_in_parallel [@ocaml.tailcall])
        ctxt
        commit1_info.predecessor
        commit2_info.predecessor
  in
  let* () =
    fail_when
      (* This case will most dominantly happen when either commit is part of the other's history.
         It occurs when the commit that is farther ahead gets dereferenced to its predecessor often
         enough to land at the other commit. *)
      Commitment_hash.(commit1 = commit2)
      Sc_rollup_no_conflict
  in
  traverse_in_parallel ctxt commit1 commit2

let get_game ctxt rollup stakers =
  let open Lwt_tzresult_syntax in
  let* ctxt, game = Store.Game.find (ctxt, rollup) stakers in
  match game with Some g -> return (g, ctxt) | None -> fail Sc_rollup_no_game

(** [init_game ctxt rollup refuter defender] initialises the game or
    if it already exists fails with `Sc_rollup_game_already_started`.

    The game is created with `refuter` as the first player to move. The
    initial state of the game will be obtained from the commitment pair
    belonging to [defender] at the conflict point. See
    [Sc_rollup_game_repr.initial] for documentation on how a pair of
    commitments is turned into an initial game state.

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
let init_game ctxt rollup ~refuter ~defender =
  let open Lwt_tzresult_syntax in
  let stakers = Sc_rollup_game_repr.Index.make refuter defender in
  let* ctxt, game = Store.Game.find (ctxt, rollup) stakers in
  match game with
  | Some _ -> fail Sc_rollup_game_already_started
  | None ->
      let* ctxt, opp_1 = Store.Opponent.find (ctxt, rollup) refuter in
      let* ctxt, opp_2 = Store.Opponent.find (ctxt, rollup) defender in
      let* _ =
        match (opp_1, opp_2) with
        | None, None -> return ()
        | Some _refuter_opponent, None ->
            fail (Sc_rollup_staker_in_game (`Refuter refuter))
        | None, Some _defender_opponent ->
            fail (Sc_rollup_staker_in_game (`Defender defender))
        | Some _refuter_opponent, Some _defender_opponent ->
            fail (Sc_rollup_staker_in_game (`Both (refuter, defender)))
      in
      let* ( ( {hash = _refuter_commit; commitment = _info},
               {hash = _defender_commit; commitment = child_info} ),
             ctxt ) =
        get_conflict_point ctxt rollup refuter defender
      in
      let* parent_info, ctxt =
        Commitment_storage.get_commitment_unsafe
          ctxt
          rollup
          child_info.predecessor
      in
      let* ctxt, inbox = Store.Inbox.get ctxt rollup in
      let* kind = Store.PVM_kind.get ctxt rollup in
      let game =
        Sc_rollup_game_repr.initial
          inbox
          ~pvm_name:(Sc_rollups.Kind.name_of kind)
          ~parent:parent_info
          ~child:child_info
          ~refuter
          ~defender
      in
      let* ctxt, _ = Store.Game.init (ctxt, rollup) stakers game in
      let* ctxt, _ =
        Store.Game_timeout.init (ctxt, rollup) stakers (timeout_level ctxt)
      in
      let* ctxt, _ = Store.Opponent.init (ctxt, rollup) refuter defender in
      let* ctxt, _ = Store.Opponent.init (ctxt, rollup) defender refuter in
      return (game, ctxt)

let game_move ctxt rollup ~player ~opponent refutation ~is_opening_move =
  let open Lwt_tzresult_syntax in
  let ({alice; bob} as stakers : Sc_rollup_game_repr.Index.t) =
    Sc_rollup_game_repr.Index.make player opponent
  in
  let* game, ctxt =
    if is_opening_move then
      init_game ctxt rollup ~refuter:player ~defender:opponent
    else get_game ctxt rollup stakers
  in
  let* () =
    fail_unless
      (let turn = match game.turn with Alice -> alice | Bob -> bob in
       Sc_rollup_repr.Staker.equal turn player)
      Sc_rollup_wrong_turn
  in
  let* move_result =
    Lwt.map Result.ok @@ Sc_rollup_game_repr.play game refutation
  in
  match move_result with
  | Either.Left outcome -> return (Some outcome, ctxt)
  | Either.Right new_game ->
      let* ctxt, _ = Store.Game.update (ctxt, rollup) stakers new_game in
      let* ctxt, _ =
        Store.Game_timeout.update (ctxt, rollup) stakers (timeout_level ctxt)
      in
      return (None, ctxt)

let timeout ctxt rollup stakers =
  let open Lwt_tzresult_syntax in
  let level = (Raw_context.current_level ctxt).level in
  let* ctxt, game = Store.Game.find (ctxt, rollup) stakers in
  match game with
  | None -> fail Sc_rollup_no_game
  | Some game ->
      let* ctxt, timeout_level =
        Store.Game_timeout.get (ctxt, rollup) stakers
      in
      let* () =
        fail_unless
          Raw_level_repr.(level > timeout_level)
          Sc_rollup_timeout_level_not_reached
      in
      return (Sc_rollup_game_repr.{loser = game.turn; reason = Timeout}, ctxt)

let apply_outcome ctxt rollup stakers (outcome : Sc_rollup_game_repr.outcome) =
  let open Lwt_tzresult_syntax in
  let losing_staker = Sc_rollup_game_repr.Index.staker stakers outcome.loser in
  let* ctxt, balance_updates =
    Stake_storage.remove_staker ctxt rollup losing_staker
  in
  let* ctxt, _, _ = Store.Game.remove (ctxt, rollup) stakers in
  let* ctxt, _, _ = Store.Game_timeout.remove (ctxt, rollup) stakers in
  let* ctxt, _, _ = Store.Opponent.remove (ctxt, rollup) stakers.alice in
  let* ctxt, _, _ = Store.Opponent.remove (ctxt, rollup) stakers.bob in
  return
    ( Sc_rollup_game_repr.Ended (outcome.reason, losing_staker),
      ctxt,
      balance_updates )

module Internal_for_tests = struct
  let get_conflict_point = get_conflict_point
end
