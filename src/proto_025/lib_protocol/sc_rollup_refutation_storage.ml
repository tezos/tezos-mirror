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
  let open Lwt_result_syntax in
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

let get_ongoing_games_for_staker ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, entries = Store.Game.list_key_values ((ctxt, rollup), staker) in
  let* ctxt, games =
    List.fold_left_es
      (fun (ctxt, games) (opponent, game_index) ->
        let* ctxt, answer = Store.Game_info.find (ctxt, rollup) game_index in
        match answer with
        | None ->
            (* A hash in [Store.Game] is always present in [Store.Game_info]. *)
            assert false
        | Some game ->
            let games =
              (game, Sc_rollup_game_repr.Index.make staker opponent) :: games
            in
            return (ctxt, games))
      (ctxt, [])
      entries
  in
  return (games, ctxt)

(** [commitments_are_conflicting ctxt rollup hash1_opt hash2_opt]
    returns a conflict description iff [hash1_opt] and [hash2_opt] are
    two different commitments with the same predecessor. *)
let commitments_are_conflicting ctxt rollup hash1_opt hash2_opt =
  let open Lwt_result_syntax in
  match (hash1_opt, hash2_opt) with
  | Some hash1, Some hash2 when Commitment_hash.(hash1 <> hash2) ->
      let* commitment1, ctxt =
        Commitment_storage.get_commitment_unsafe ctxt rollup hash1
      in
      let* commitment2, ctxt =
        Commitment_storage.get_commitment_unsafe ctxt rollup hash2
      in
      if Commitment_hash.(commitment1.predecessor = commitment2.predecessor)
      then
        let conflict_point =
          ( {hash = hash1; commitment = commitment1},
            {hash = hash2; commitment = commitment2} )
        in
        return (ctxt, Some conflict_point)
      else return (ctxt, None)
  | _ -> return (ctxt, None)

(** [look_for_conflict ctxt rollup staker1_index staker2_index from_level
    upto_level delta] looks for the first conflict of [staker1_index]
    and [staker2_index].

    It starts at [from_level] which the last cemented inbox level on the
    [rollup], and climbs the staking's storage through a recursive
    function.

    Two important notes:
    {ol
      {li The code can do at most (max_lookahead / commitment_period) recursive
          calls, which can be a lot;}
      {li Therefore, this code must be called only via a RPC, used by the
          rollup-node. The {!check_conflict_point} used by the protocol is
          on the other hand, very cheap.}
    }

    FIXME: https://gitlab.com/tezos/tezos/-/issues/4477
    As it should be used only via an RPC (and by the rollup-node), we should
    move this function (and other related functions) outside the protocol.
*)
let look_for_conflict ctxt rollup staker1_index staker2_index from_level
    upto_level delta =
  let open Lwt_result_syntax in
  let rec go ctxt from_level =
    if Raw_level_repr.(from_level >= upto_level) then
      tzfail Sc_rollup_no_conflict
    else
      let* ctxt, commitments =
        Sc_rollup_stake_storage.commitments_of_inbox_level
          ctxt
          rollup
          from_level
      in
      let* ctxt, hash1_opt =
        Sc_rollup_stake_storage.find_commitment_of_staker_in_commitments
          ctxt
          rollup
          staker1_index
          commitments
      in
      let* ctxt, hash2_opt =
        Sc_rollup_stake_storage.find_commitment_of_staker_in_commitments
          ctxt
          rollup
          staker2_index
          commitments
      in
      let* ctxt, conflict_point_opt =
        commitments_are_conflicting ctxt rollup hash1_opt hash2_opt
      in
      match conflict_point_opt with
      | Some conflict_point -> return (conflict_point, ctxt)
      | None ->
          let from_level = Raw_level_repr.add from_level delta in
          go ctxt from_level
  in
  go ctxt from_level

(** [get_conflict_point ctxt rollup staker1 staker2] starts from the
    LCC's successor and look for the first conflict between [staker1] and
    [staker2], if any. *)
let get_conflict_point ctxt rollup staker1 staker2 =
  let open Lwt_result_syntax in
  let* ctxt, staker1_index =
    Sc_rollup_staker_index_storage.get_staker_index_unsafe ctxt rollup staker1
  in
  let* ctxt, staker2_index =
    Sc_rollup_staker_index_storage.get_staker_index_unsafe ctxt rollup staker2
  in
  let* _lcc, lcc_inbox_level, ctxt =
    Commitment_storage.last_cemented_commitment_hash_with_level ctxt rollup
  in
  let current_level = (Raw_context.current_level ctxt).level in
  let commitment_period =
    Constants_storage.sc_rollup_commitment_period_in_blocks ctxt
  in
  look_for_conflict
    ctxt
    rollup
    staker1_index
    staker2_index
    (Raw_level_repr.add lcc_inbox_level commitment_period)
    current_level
    commitment_period

let find_game ctxt rollup stakers =
  let open Lwt_result_syntax in
  let open Sc_rollup_game_repr.Index in
  let* ctxt, game_index =
    Store.Game.find ((ctxt, rollup), stakers.alice) stakers.bob
  in
  match game_index with
  | None -> return (ctxt, None)
  | Some game_hash -> Store.Game_info.find (ctxt, rollup) game_hash

let get_game ctxt rollup stakers =
  let open Lwt_result_syntax in
  let* ctxt, game = find_game ctxt rollup stakers in
  match game with
  | None -> tzfail Sc_rollup_no_game
  | Some game -> return (game, ctxt)

let create_game ctxt rollup stakers game =
  let open Lwt_result_syntax in
  let open Sc_rollup_game_repr.Index in
  let* ctxt, _ = Store.Game_info.init (ctxt, rollup) stakers game in
  let* ctxt, _ =
    Store.Game.init ((ctxt, rollup), stakers.alice) stakers.bob stakers
  in
  let* ctxt, _ =
    Store.Game.init ((ctxt, rollup), stakers.bob) stakers.alice stakers
  in
  return ctxt

let update_game ctxt rollup stakers new_game =
  let open Lwt_result_syntax in
  let* ctxt, _storage_diff =
    Store.Game_info.update (ctxt, rollup) stakers new_game
  in
  return ctxt

let remove_game ctxt rollup stakers =
  let open Lwt_result_syntax in
  let open Sc_rollup_game_repr.Index in
  let* ctxt, _storage_diff, _was_here =
    Store.Game.remove ((ctxt, rollup), stakers.alice) stakers.bob
  in
  let* ctxt, _storage_diff, _was_here =
    Store.Game.remove ((ctxt, rollup), stakers.bob) stakers.alice
  in
  let* ctxt, _storage_diff, _was_here =
    Store.Game_info.remove (ctxt, rollup) stakers
  in
  return ctxt

let check_conflict_point ctxt rollup ~refuter ~refuter_commitment_hash ~defender
    ~defender_commitment_hash =
  let open Lwt_result_syntax in
  let fail_unless_staker_is_staked_on_commitment ctxt staker commitment_hash =
    let* ctxt, is_staked =
      Sc_rollup_stake_storage.is_staked_on ctxt rollup staker commitment_hash
    in
    let* () =
      fail_unless
        is_staked
        (Sc_rollup_wrong_staker_for_conflict_commitment (staker, commitment_hash))
    in
    return ctxt
  in
  let* ctxt =
    fail_unless_staker_is_staked_on_commitment
      ctxt
      refuter
      refuter_commitment_hash
  in
  let* ctxt =
    fail_unless_staker_is_staked_on_commitment
      ctxt
      defender
      defender_commitment_hash
  in
  let* refuter_commitment, ctxt =
    Commitment_storage.get_commitment_unsafe ctxt rollup refuter_commitment_hash
  in
  let* defender_commitment, ctxt =
    Commitment_storage.get_commitment_unsafe
      ctxt
      rollup
      defender_commitment_hash
  in
  let* () =
    fail_unless
      Commitment_hash.(refuter_commitment_hash <> defender_commitment_hash)
      Sc_rollup_errors.Sc_rollup_no_conflict
  in
  let* () =
    fail_unless
      Commitment_hash.(
        refuter_commitment.predecessor = defender_commitment.predecessor)
      (Sc_rollup_errors.Sc_rollup_not_valid_commitments_conflict
         (refuter_commitment_hash, refuter, defender_commitment_hash, defender))
  in
  return (defender_commitment, ctxt)

let check_staker_availability ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, is_staker =
    Sc_rollup_staker_index_storage.is_staker ctxt rollup staker
  in
  let* () = fail_unless is_staker Sc_rollup_not_staked in
  let* ctxt, entries = Store.Game.list_key_values ((ctxt, rollup), staker) in
  let* () =
    fail_when
      Compare.List_length_with.(
        entries >= Constants_storage.sc_rollup_max_number_of_parallel_games ctxt)
      (Sc_rollup_max_number_of_parallel_games_reached staker)
  in
  return ctxt

(** [start_game ctxt rollup ~player:(player, player_commitment_hash)
    ~opponent:(opponent, opponent_commitment_hash)] initialises the game or if
    it already exists fails with [Sc_rollup_game_already_started].

    The game is created with [player] as the first player to
    move. The initial state of the game will be obtained from the
    commitment pair belonging to [opponent] at the conflict point. See
    [Sc_rollup_game_repr.initial] for documentation on how a pair of
    commitments is turned into an initial game state.

    This also deals with the other bits of data in the storage around
    the game. Notice that a staker can participate in multiple games in
    parallel. However, there is at most one game between two given stakers
    since a staker can publish at most one commitment per inbox level.

    It also initialises the timeout level to the current level plus
    [timeout_period_in_blocks] to mark the block level at which it becomes
    possible for anyone to end the game by timeout.

    May fail with:

   {ul
    {li [Sc_rollup_does_not_exist] if [rollup] does not exist}
    {li [Sc_rollup_no_conflict] if [player] is staked on an
     ancestor of the commitment staked on by [opponent], or vice versa}
    {li [Sc_rollup_not_staked] if one of the [player] or [opponent] is
    not actually staked}
    {li [Sc_rollup_staker_in_game] if one of the [player] or [opponent]
     is already playing a game}
    {li [Sc_rollup_not_first_conflict_between_stakers] if the provided
    commitments are not the first commitments in conflict between
    [player] and [opponent].}
   }
*)
let start_game ctxt rollup ~player:(player, player_commitment_hash)
    ~opponent:(opponent, opponent_commitment_hash) =
  let open Lwt_result_syntax in
  (* When the game is started by a given [player], this player is
     called the [refuter] and its opponent is the [defender]. *)
  let refuter = player
  and refuter_commitment_hash = player_commitment_hash
  and defender = opponent
  and defender_commitment_hash = opponent_commitment_hash in
  let stakers = Sc_rollup_game_repr.Index.make refuter defender in
  let* ctxt, game_exists = Store.Game_info.mem (ctxt, rollup) stakers in
  let* () = fail_when game_exists Sc_rollup_game_already_started in
  let* ctxt = check_staker_availability ctxt rollup stakers.alice in
  let* ctxt = check_staker_availability ctxt rollup stakers.bob in
  let* defender_commitment, ctxt =
    check_conflict_point
      ctxt
      rollup
      ~refuter
      ~defender
      ~refuter_commitment_hash
      ~defender_commitment_hash
  in
  let* parent_commitment, ctxt =
    Commitment_storage.get_commitment_unsafe
      ctxt
      rollup
      defender_commitment.predecessor
  in
  let* inbox, ctxt = Sc_rollup_inbox_storage.get_inbox ctxt in
  let default_number_of_sections =
    Constants_storage.sc_rollup_number_of_sections_in_dissection ctxt
  in
  let* slots_history_snapshot =
    Dal_slot_storage.get_slot_headers_history ctxt
  in
  let current_level = (Raw_context.current_level ctxt).level in
  let game =
    Sc_rollup_game_repr.initial
      ~start_level:current_level
      (Sc_rollup_inbox_repr.take_snapshot inbox)
      slots_history_snapshot
      ~refuter
      ~defender
      ~default_number_of_sections
      ~parent_commitment
      ~defender_commitment
  in
  let* ctxt = create_game ctxt rollup stakers game in
  let* ctxt, _ =
    Store.Game_timeout.init (ctxt, rollup) stakers (initial_timeout ctxt)
  in
  return ctxt

let check_stakes ctxt rollup (stakers : Sc_rollup_game_repr.Index.t) =
  let open Lwt_result_syntax in
  let open Sc_rollup_game_repr in
  let* ctxt, alice_stake =
    Sc_rollup_staker_index_storage.is_staker ctxt rollup stakers.alice
  in
  let* ctxt, bob_stake =
    Sc_rollup_staker_index_storage.is_staker ctxt rollup stakers.bob
  in
  let game_over loser = Loser {loser; reason = Conflict_resolved} in
  match (alice_stake, bob_stake) with
  | true, true -> return (None, ctxt)
  | false, true -> return (Some (game_over stakers.alice), ctxt)
  | true, false -> return (Some (game_over stakers.bob), ctxt)
  | false, false -> return (Some Draw, ctxt)

let game_move ctxt rollup ~player ~opponent ~step ~choice =
  let open Lwt_result_syntax in
  let stakers = Sc_rollup_game_repr.Index.make player opponent in
  let* game, ctxt = get_game ctxt rollup stakers in
  let* ctxt, kind = Store.PVM_kind.get ctxt rollup in
  let* () =
    fail_unless
      (Sc_rollup_repr.Staker.equal
         player
         (Sc_rollup_game_repr.Index.staker stakers game.turn))
      Sc_rollup_wrong_turn
  in
  let* ctxt, metadata = Sc_rollup_storage.get_metadata ctxt rollup in
  let constants = Constants_storage.parametric ctxt in
  let dal = constants.dal in
  let dal_activation_level =
    if dal.feature_enable then
      Some constants.sc_rollup.reveal_activation_level.dal_parameters
    else None
  in
  let dal_attested_slots_validity_lag =
    constants.sc_rollup.reveal_activation_level.dal_attested_slots_validity_lag
  in
  let* check_result, ctxt = check_stakes ctxt rollup stakers in
  match check_result with
  | Some game_result -> return (Some game_result, ctxt)
  | None -> (
      let play_cost = Sc_rollup_game_repr.cost_play ~step ~choice in
      let*? ctxt = Raw_context.consume_gas ctxt play_cost in

      let* move_result =
        Sc_rollup_game_repr.play
          kind
          ~dal_activation_level
          ~find_dal_parameters:(Dal_storage.parameters ctxt)
          ~stakers
          metadata
          game
          ~step
          ~choice
          ~is_reveal_enabled:
            (Sc_rollup_PVM_sig.is_reveal_enabled_predicate
               (Constants_storage.sc_rollup_reveal_activation_level ctxt))
          ~dal_attested_slots_validity_lag
      in
      match move_result with
      | Either.Left game_result -> return (Some game_result, ctxt)
      | Either.Right new_game ->
          let* ctxt = update_game ctxt rollup stakers new_game in
          let* ctxt = update_timeout ctxt rollup game stakers in
          return (None, ctxt))

let get_timeout ctxt rollup stakers =
  let open Lwt_result_syntax in
  let* ctxt, timeout_opt =
    Storage.Sc_rollup.Game_timeout.find (ctxt, rollup) stakers
  in
  match timeout_opt with
  | Some timeout -> return (timeout, ctxt)
  | None -> tzfail Sc_rollup_no_game

let timeout ctxt rollup stakers =
  let open Lwt_result_syntax in
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
  let open Lwt_result_syntax in
  let winner_contract = Contract_repr.Implicit winner in
  let stake = Constants_storage.sc_rollup_stake_amount ctxt in
  let*? reward = Tez_repr.(stake /? 2L) in
  Token.transfer
    ctxt
    `Sc_rollup_refutation_rewards
    (`Contract winner_contract)
    reward

let remove_if_staker_is_still_there ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, is_staker =
    Sc_rollup_staker_index_storage.is_staker ctxt rollup staker
  in
  if is_staker then Stake_storage.remove_staker ctxt rollup staker
  else return (ctxt, [])

let apply_game_result ctxt rollup (stakers : Sc_rollup_game_repr.Index.t)
    (game_result : Sc_rollup_game_repr.game_result) =
  let open Lwt_result_syntax in
  let status = Sc_rollup_game_repr.Ended game_result in
  let* ctxt, balances_updates =
    match game_result with
    | Loser {loser; reason = _} ->
        let losing_staker = loser in
        let winning_staker =
          let Sc_rollup_game_repr.Index.{alice; bob} = stakers in
          if Signature.Public_key_hash.(alice = loser) then bob else alice
        in
        let* ctxt = remove_game ctxt rollup stakers in
        let* ctxt, balance_updates_loser =
          remove_if_staker_is_still_there ctxt rollup losing_staker
        in
        let* ctxt, balance_updates_winner =
          (* The winner is rewarded only if he defeated himself the loser.
             Another way to check this is to reward if the game result's reason
             is not a forfeit.
          *)
          match balance_updates_loser with
          | [] -> return (ctxt, [])
          | _ -> reward ctxt winning_staker
        in
        let balances_updates = balance_updates_loser @ balance_updates_winner in
        return (ctxt, balances_updates)
    | Draw ->
        let* ctxt = remove_game ctxt rollup stakers in
        let* ctxt, balances_updates_alice =
          remove_if_staker_is_still_there ctxt rollup stakers.alice
        in
        let* ctxt, balances_updates_bob =
          remove_if_staker_is_still_there ctxt rollup stakers.bob
        in
        return (ctxt, balances_updates_alice @ balances_updates_bob)
  in
  let* ctxt, _storage_diff, _was_here =
    Store.Game_timeout.remove (ctxt, rollup) stakers
  in
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
  let open Lwt_result_syntax in
  let make_conflict ctxt rollup other (our_point, their_point) =
    let our_hash = our_point.hash and their_hash = their_point.hash in
    let get = Sc_rollup_commitment_storage.get_commitment_unsafe ctxt rollup in
    let* our_commitment, _ctxt = get our_hash in
    let* their_commitment, _ctxt = get their_hash in
    let parent_commitment = our_commitment.predecessor in
    return {other; their_commitment; our_commitment; parent_commitment}
  in
  let*! stakers =
    Sc_rollup_stake_storage.stakers_pkhs_uncarbonated ctxt ~rollup
  in
  List.fold_left_es
    (fun conflicts other_staker ->
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

let migrate_clean_refutation_games ctxt =
  let open Lwt_result_syntax in
  let remove_unstaked_games rollup =
    List.fold_left_es
      (fun ctxt (Sc_rollup_game_repr.Index.{alice; bob} as stakers) ->
        let* ctxt, alice_active =
          Sc_rollup_staker_index_storage.is_staker ctxt rollup alice
        in
        let* ctxt, bob_active =
          Sc_rollup_staker_index_storage.is_staker ctxt rollup bob
        in
        if (not alice_active) && not bob_active then
          remove_game ctxt rollup stakers
        else return ctxt)
  in
  let* rollups = Sc_rollup_storage.list_unaccounted ctxt in
  List.fold_left_es
    (fun ctxt rollup ->
      let*! players =
        Storage.Sc_rollup.Game_info.keys_unaccounted (ctxt, rollup)
      in
      remove_unstaked_games rollup ctxt players)
    ctxt
    rollups
