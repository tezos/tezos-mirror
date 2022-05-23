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

open Sc_rollup_errors
module Store = Storage.Sc_rollup
module Commitment = Sc_rollup_repr.Commitment
module Commitment_storage = Sc_rollup_commitment_storage
module Commitment_hash = Sc_rollup_repr.Commitment_hash
module Stake_storage = Sc_rollup_stake_storage

let originate ctxt ~kind ~boot_sector =
  Raw_context.increment_origination_nonce ctxt >>?= fun (ctxt, nonce) ->
  let level = Raw_context.current_level ctxt in
  Sc_rollup_repr.Address.from_nonce nonce >>?= fun address ->
  Store.PVM_kind.add ctxt address kind >>= fun ctxt ->
  Store.Initial_level.add ctxt address (Level_storage.current ctxt).level
  >>= fun ctxt ->
  Store.Boot_sector.add ctxt address boot_sector >>= fun ctxt ->
  let inbox = Sc_rollup_inbox_repr.empty address level.level in
  Store.Inbox.init ctxt address inbox >>=? fun (ctxt, size_diff) ->
  Store.Last_cemented_commitment.init ctxt address Commitment_hash.zero
  >>=? fun (ctxt, lcc_size_diff) ->
  Store.Staker_count.init ctxt address 0l >>=? fun (ctxt, stakers_size_diff) ->
  let addresses_size = 2 * Sc_rollup_repr.Address.size in
  let stored_kind_size = 2 (* because tag_size of kind encoding is 16bits. *) in
  let boot_sector_size =
    Data_encoding.Binary.length Data_encoding.string boot_sector
  in
  let origination_size = Constants_storage.sc_rollup_origination_size ctxt in
  let size =
    Z.of_int
      (origination_size + stored_kind_size + boot_sector_size + addresses_size
     + size_diff + lcc_size_diff + stakers_size_diff)
  in
  return (address, size, ctxt)

let kind ctxt address = Store.PVM_kind.find ctxt address

type conflict_point = Commitment_hash.t * Commitment_hash.t

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
      return ((commit1, commit2), ctxt)
    else
      (* Different predecessors means they run in parallel. *)
      (traverse_in_parallel [@ocaml.tailcall])
        ctxt
        commit1_info.predecessor
        commit2_info.predecessor
  in
  if Commitment_hash.(commit1 = commit2) then
    (* This case will most dominantly happen when either commit is part of the other's history.
       It occurs when the commit that is farther ahead gets dereferenced to its predecessor often
       enough to land at the other commit. *)
    fail Sc_rollup_no_conflict
  else traverse_in_parallel ctxt commit1 commit2

let list ctxt = Store.PVM_kind.keys ctxt >|= Result.return

let initial_level ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* level = Store.Initial_level.find ctxt rollup in
  match level with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some level -> return level

let get_boot_sector ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* boot_sector = Storage.Sc_rollup.Boot_sector.find ctxt rollup in
  match boot_sector with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some boot_sector -> return boot_sector

(** TODO: #2902 replace with protocol constant and consider good value. *)
let timeout_period_in_blocks = 500

let timeout_level ctxt =
  let level = Raw_context.current_level ctxt in
  Raw_level_repr.add level.level timeout_period_in_blocks

let get_or_init_game ctxt rollup ~refuter ~defender =
  let open Lwt_tzresult_syntax in
  let stakers = Sc_rollup_game_repr.Index.normalize (refuter, defender) in
  let* ctxt, game = Store.Game.find (ctxt, rollup) stakers in
  match game with
  | Some g -> return (g, ctxt)
  | None ->
      let* ctxt, opp_1 = Store.Opponent.find (ctxt, rollup) refuter in
      let* ctxt, opp_2 = Store.Opponent.find (ctxt, rollup) defender in
      let* _ =
        match (opp_1, opp_2) with
        | None, None -> return ()
        | _ -> fail Sc_rollup_staker_in_game
      in
      let* (_, child), ctxt = get_conflict_point ctxt rollup refuter defender in
      let* child, ctxt =
        Commitment_storage.get_commitment_unsafe ctxt rollup child
      in
      let* parent, ctxt =
        Commitment_storage.get_commitment_unsafe ctxt rollup child.predecessor
      in
      let* ctxt, inbox = Store.Inbox.get ctxt rollup in
      let game =
        Sc_rollup_game_repr.initial inbox ~parent ~child ~refuter ~defender
      in
      let* ctxt, _ = Store.Game.init (ctxt, rollup) stakers game in
      let* ctxt, _ =
        Store.Game_timeout.init (ctxt, rollup) stakers (timeout_level ctxt)
      in
      let* ctxt, _ = Store.Opponent.init (ctxt, rollup) refuter defender in
      let* ctxt, _ = Store.Opponent.init (ctxt, rollup) defender refuter in
      return (game, ctxt)

(* TODO: #2926 this requires carbonation *)
let update_game ctxt rollup ~player ~opponent refutation =
  let open Lwt_tzresult_syntax in
  let alice, bob = Sc_rollup_game_repr.Index.normalize (player, opponent) in
  let* game, ctxt =
    get_or_init_game ctxt rollup ~refuter:player ~defender:opponent
  in
  let* _ =
    let turn = match game.turn with Alice -> alice | Bob -> bob in
    if Sc_rollup_repr.Staker.equal turn player then return ()
    else fail Sc_rollup_wrong_turn
  in
  match Sc_rollup_game_repr.play game refutation with
  | Either.Left outcome -> return (Some outcome, ctxt)
  | Either.Right new_game ->
      let* ctxt, _ = Store.Game.update (ctxt, rollup) (alice, bob) new_game in
      let* ctxt, _ =
        Store.Game_timeout.update
          (ctxt, rollup)
          (alice, bob)
          (timeout_level ctxt)
      in
      return (None, ctxt)

(* TODO: #2926 this requires carbonation *)
let timeout ctxt rollup stakers =
  let open Lwt_tzresult_syntax in
  let level = (Raw_context.current_level ctxt).level in
  let alice, bob = Sc_rollup_game_repr.Index.normalize stakers in
  let* ctxt, game = Store.Game.find (ctxt, rollup) (alice, bob) in
  match game with
  | None -> fail Sc_rollup_no_game
  | Some game ->
      let* ctxt, timeout_level =
        Store.Game_timeout.get (ctxt, rollup) (alice, bob)
      in
      if Raw_level_repr.(level > timeout_level) then
        return (Sc_rollup_game_repr.{loser = game.turn; reason = Timeout}, ctxt)
      else fail Sc_rollup_timeout_level_not_reached

(* TODO: #2926 this requires carbonation *)
let apply_outcome ctxt rollup stakers (outcome : Sc_rollup_game_repr.outcome) =
  let open Lwt_tzresult_syntax in
  let alice, bob = Sc_rollup_game_repr.Index.normalize stakers in
  let losing_staker = Sc_rollup_game_repr.Index.staker stakers outcome.loser in
  let* ctxt = Stake_storage.remove_staker ctxt rollup losing_staker in
  let* ctxt, _, _ = Store.Game.remove (ctxt, rollup) (alice, bob) in
  let* ctxt, _, _ = Store.Game_timeout.remove (ctxt, rollup) (alice, bob) in
  let* ctxt, _, _ = Store.Opponent.remove (ctxt, rollup) alice in
  let* ctxt, _, _ = Store.Opponent.remove (ctxt, rollup) bob in
  return (Sc_rollup_game_repr.Ended (outcome.reason, losing_staker), ctxt)

module Outbox = struct
  let level_index ctxt level =
    let max_active_levels =
      Constants_storage.sc_rollup_max_active_outbox_levels ctxt
    in
    Int32.rem (Raw_level_repr.to_int32 level) max_active_levels

  let record_applied_message ctxt rollup level ~message_index =
    let open Lwt_tzresult_syntax in
    (* Check that the 0 <= message index < maximum number of outbox messages per
       level. *)
    let*? () =
      let max_outbox_messages_per_level =
        Constants_storage.sc_rollup_max_outbox_messages_per_level ctxt
      in
      error_unless
        Compare.Int.(
          0 <= message_index && message_index < max_outbox_messages_per_level)
        Sc_rollup_invalid_outbox_message_index
    in
    let level_index = level_index ctxt level in
    let* ctxt, level_and_bitset_opt =
      Store.Applied_outbox_messages.find (ctxt, rollup) level_index
    in
    let*? bitset, ctxt =
      let open Tzresult_syntax in
      let* bitset, ctxt =
        match level_and_bitset_opt with
        | Some (existing_level, bitset)
          when Raw_level_repr.(existing_level = level) ->
            (* The level at the index is the same as requested. Fail if the
               message has been applied already. *)
            let* already_applied = Bitset.mem bitset message_index in
            let* () =
              error_when
                already_applied
                Sc_rollup_outbox_message_already_applied
            in
            return (bitset, ctxt)
        | Some (existing_level, _bitset)
          when Raw_level_repr.(level < existing_level) ->
            fail Sc_rollup_outbox_level_expired
        | Some _ | None ->
            (* The old level is outdated or there is no previous bitset at
               this index. *)
            return (Bitset.empty, ctxt)
      in
      let* bitset = Bitset.add bitset message_index in
      return (bitset, ctxt)
    in
    let+ ctxt, size_diff, _is_new =
      Store.Applied_outbox_messages.add
        (ctxt, rollup)
        level_index
        (level, bitset)
    in
    (Z.of_int size_diff, ctxt)
end
