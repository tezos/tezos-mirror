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

(** [initial_timeout ctxt] set the initial timeout of players. The initial
    timeout of each player is equal to [sc_rollup_timeout_period_in_blocks]. *)
let initial_timeout ctxt =
  let last_turn_level = (Raw_context.current_level ctxt).level in
  let timeout_period_in_blocks =
    Constants_storage.sc_rollup_timeout_period_in_blocks ctxt
  in
  Sc_rollup_game_repr.
    {
      alice = timeout_period_in_blocks;
      bob = timeout_period_in_blocks;
      last_turn_level;
    }

(** [update_timeout ctxt rollup game idx] update the timeout left for the
    current player [game.turn]. Her new timeout is equal to [nb_of_block_left -
    (current_level - last_turn_level)] where [nb_of_block_left] is her current
    timeout. *)
let update_timeout ctxt rollup (game : Sc_rollup_game_repr.t) idx =
  let open Lwt_tzresult_syntax in
  let* ctxt, timeout = Store.Game_timeout.get (ctxt, rollup) idx in
  let current_level = (Raw_context.current_level ctxt).level in
  let sub_block_left nb_of_block_left =
    nb_of_block_left
    - Int32.to_int (Raw_level_repr.diff current_level timeout.last_turn_level)
  in
  let new_timeout =
    match game.turn with
    | Alice ->
        let nb_of_block_left = sub_block_left timeout.alice in
        {timeout with last_turn_level = current_level; alice = nb_of_block_left}
    | Bob ->
        let nb_of_block_left = sub_block_left timeout.bob in
        {timeout with last_turn_level = current_level; bob = nb_of_block_left}
  in
  let* ctxt, _ = Store.Game_timeout.update (ctxt, rollup) idx new_timeout in
  return ctxt

let get_ongoing_game ctxt rollup staker1 staker2 =
  let open Lwt_tzresult_syntax in
  let stakers = Sc_rollup_game_repr.Index.make staker1 staker2 in
  let* ctxt, game = Store.Game.find (ctxt, rollup) stakers in
  let answer = Option.map (fun game -> (game, stakers)) game in
  return (answer, ctxt)

let get_ongoing_game_for_staker ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let* ctxt, opponent = Store.Opponent.find (ctxt, rollup) staker in
  match opponent with
  | Some opponent -> get_ongoing_game ctxt rollup staker opponent
  | None -> return (None, ctxt)

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

(** [start_game ctxt rollup refuter defender] initialises the game or
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
let start_game ctxt rollup ~player:refuter ~opponent:defender =
  let open Lwt_tzresult_syntax in
  let stakers = Sc_rollup_game_repr.Index.make refuter defender in
  let* ctxt, game_exists = Store.Game.mem (ctxt, rollup) stakers in
  let* () = fail_when game_exists Sc_rollup_game_already_started in
  let* ctxt, opp_1 = Store.Opponent.find (ctxt, rollup) refuter in
  let* ctxt, opp_2 = Store.Opponent.find (ctxt, rollup) defender in
  let* () =
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
    Commitment_storage.get_commitment_unsafe ctxt rollup child_info.predecessor
  in
  let* ctxt, inbox = Store.Inbox.get ctxt rollup in
  let* ctxt, kind = Store.PVM_kind.get ctxt rollup in
  let default_number_of_sections =
    Constants_storage.sc_rollup_number_of_sections_in_dissection ctxt
  in

  let game =
    Sc_rollup_game_repr.initial
      (Sc_rollup_inbox_repr.take_snapshot inbox)
      ~pvm_name:(Sc_rollups.Kind.name_of kind)
      ~parent:parent_info
      ~child:child_info
      ~refuter
      ~defender
      ~default_number_of_sections
  in
  let* ctxt, _ = Store.Game.init (ctxt, rollup) stakers game in
  let* ctxt, _ =
    Store.Game_timeout.init (ctxt, rollup) stakers (initial_timeout ctxt)
  in
  let* ctxt, _ = Store.Opponent.init (ctxt, rollup) refuter defender in
  let* ctxt, _ = Store.Opponent.init (ctxt, rollup) defender refuter in
  return ctxt

let game_move ctxt rollup ~player ~opponent refutation =
  let open Lwt_tzresult_syntax in
  let stakers = Sc_rollup_game_repr.Index.make player opponent in
  let* game, ctxt = get_game ctxt rollup stakers in
  let* () =
    fail_unless
      (Sc_rollup_repr.Staker.equal
         player
         (Sc_rollup_game_repr.Index.staker stakers game.turn))
      Sc_rollup_wrong_turn
  in
  let*! move_result = Sc_rollup_game_repr.play ~stakers game refutation in
  match move_result with
  | Either.Left game_result -> return (Some game_result, ctxt)
  | Either.Right new_game ->
      let* ctxt, _ = Store.Game.update (ctxt, rollup) stakers new_game in
      let* ctxt = update_timeout ctxt rollup game stakers in
      return (None, ctxt)

let get_timeout ctxt rollup stakers =
  let open Lwt_tzresult_syntax in
  let* ctxt, timeout_opt =
    Storage.Sc_rollup.Game_timeout.find (ctxt, rollup) stakers
  in
  match timeout_opt with
  | Some timeout -> return (timeout, ctxt)
  | None -> fail Sc_rollup_no_game

let timeout ctxt rollup stakers =
  let open Lwt_tzresult_syntax in
  let level = (Raw_context.current_level ctxt).level in
  let* game, ctxt = get_game ctxt rollup stakers in
  let* ctxt, timeout = Store.Game_timeout.get (ctxt, rollup) stakers in
  let* () =
    let block_left_before_timeout =
      match game.turn with Alice -> timeout.alice | Bob -> timeout.bob
    in
    let level_of_timeout =
      Raw_level_repr.add timeout.last_turn_level block_left_before_timeout
    in
    fail_unless
      Raw_level_repr.(level > level_of_timeout)
      (let blocks_left = Raw_level_repr.(diff level_of_timeout level) in
       let staker =
         match game.turn with Alice -> stakers.alice | Bob -> stakers.bob
       in
       Sc_rollup_timeout_level_not_reached (blocks_left, staker))
  in
  let game_result =
    match game.game_state with
    | Dissecting _ ->
        (* Timeout during the dissecting results in a loss. *)
        let loser = Sc_rollup_game_repr.Index.staker stakers game.turn in
        Sc_rollup_game_repr.(Loser {loser; reason = Timeout})
    | Final_move {agreed_start_chunk = _; refuted_stop_chunk = _} ->
        (* Timeout-ed because the opponent played an invalid move and
           the current player is not playing. Both are invalid moves. *)
        Sc_rollup_game_repr.Draw
  in
  return (game_result, ctxt)

let reward ctxt winner =
  let open Lwt_tzresult_syntax in
  let winner_contract = Contract_repr.Implicit winner in
  let stake = Constants_storage.sc_rollup_stake_amount ctxt in
  let*? reward = Tez_repr.(stake /? 2L) in
  Token.transfer
    ctxt
    `Sc_rollup_refutation_rewards
    (`Contract winner_contract)
    reward

let apply_game_result ctxt rollup (stakers : Sc_rollup_game_repr.Index.t)
    (game_result : Sc_rollup_game_repr.game_result) =
  let open Lwt_tzresult_syntax in
  let status = Sc_rollup_game_repr.Ended game_result in
  let* ctxt, balances_updates =
    match game_result with
    | Loser {loser; reason = _} ->
        let losing_staker = loser in
        let winning_staker =
          let Sc_rollup_game_repr.Index.{alice; bob} = stakers in
          if Signature.Public_key_hash.(alice = loser) then bob else alice
        in
        let* ctxt, balance_updates_winner = reward ctxt winning_staker in
        let* ctxt, _, _ = Store.Game.remove (ctxt, rollup) stakers in
        let* ctxt, balance_updates_loser =
          Stake_storage.remove_staker ctxt rollup losing_staker
        in
        let balances_updates = balance_updates_loser @ balance_updates_winner in
        return (ctxt, balances_updates)
    | Draw -> return (ctxt, [])
  in
  let* ctxt, _, _ = Store.Game_timeout.remove (ctxt, rollup) stakers in
  let* ctxt, _, _ = Store.Opponent.remove (ctxt, rollup) stakers.alice in
  let* ctxt, _, _ = Store.Opponent.remove (ctxt, rollup) stakers.bob in
  return (status, ctxt, balances_updates)

module Internal_for_tests = struct
  let get_conflict_point = get_conflict_point
end

type conflict = {
  other : Sc_rollup_repr.Staker.t;
  their_commitment : Sc_rollup_commitment_repr.t;
  our_commitment : Sc_rollup_commitment_repr.t;
  parent_commitment : Sc_rollup_commitment_repr.Hash.t;
}

let conflict_encoding =
  Data_encoding.(
    conv
      (fun {other; their_commitment; our_commitment; parent_commitment} ->
        (other, their_commitment, our_commitment, parent_commitment))
      (fun (other, their_commitment, our_commitment, parent_commitment) ->
        {other; their_commitment; our_commitment; parent_commitment})
      (obj4
         (req "other" Sc_rollup_repr.Staker.encoding)
         (req "their_commitment" Sc_rollup_commitment_repr.encoding)
         (req "our_commitment" Sc_rollup_commitment_repr.encoding)
         (req "parent_commitment" Sc_rollup_commitment_repr.Hash.encoding)))

let conflicting_stakers_uncarbonated ctxt rollup staker =
  let open Lwt_tzresult_syntax in
  let make_conflict ctxt rollup other (our_point, their_point) =
    let our_hash = our_point.hash and their_hash = their_point.hash in
    let get = Sc_rollup_commitment_storage.get_commitment_unsafe ctxt rollup in
    let* our_commitment, _ = get our_hash in
    let* their_commitment, _ = get their_hash in
    let parent_commitment = our_commitment.predecessor in
    return {other; their_commitment; our_commitment; parent_commitment}
  in
  let* _ctxt, stakers = Store.stakers ctxt rollup in
  List.fold_left_es
    (fun conflicts (other_staker, _) ->
      let*! res = get_conflict_point ctxt rollup staker other_staker in
      match res with
      | Ok (conflict_point, _) ->
          let* conflict =
            make_conflict ctxt rollup other_staker conflict_point
          in
          return (conflict :: conflicts)
      | Error _ -> return conflicts)
    []
    stakers
