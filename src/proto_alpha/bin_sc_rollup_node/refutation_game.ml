(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module implements the refutation game logic of the rollup
   node.

   When a new L1 block arises, the rollup node asks the L1 node for
   the current game it is part of, if any.

   If a game is running and it is the rollup operator turn, the rollup
   node injects the next move of the winning strategy.

   If a game is running and it is not the rollup operator turn, the
   rollup node asks the L1 node whether the timeout is reached to play
   the timeout argument if possible.

   Otherwise, if no game is running, the rollup node asks the L1 node
   whether there is a conflict with one of its disputable commitments. If
   there is such a conflict with a commitment C', then the rollup node
   starts a game to refute C' by starting a game with one of its staker.

*)
open Protocol

open Alpha_context

module type S = sig
  module PVM : Pvm.S

  val process : Layer1.head -> Node_context.t -> unit tzresult Lwt.t
end

module Make (Interpreter : Interpreter.S) :
  S with module PVM = Interpreter.PVM = struct
  module PVM = Interpreter.PVM
  open Sc_rollup.Game

  let node_role ~self Sc_rollup.Game.Index.{alice; bob} =
    if Sc_rollup.Staker.equal alice self then Alice
    else if Sc_rollup.Staker.equal bob self then Bob
    else (* By validity of [ongoing_game] RPC. *)
      assert false

  type role = Our_turn of {opponent : public_key_hash} | Their_turn

  let turn ~self game players =
    let Sc_rollup.Game.Index.{alice; bob} = players in
    match (node_role ~self players, game.turn) with
    | Alice, Alice -> Our_turn {opponent = bob}
    | Bob, Bob -> Our_turn {opponent = alice}
    | Alice, Bob -> Their_turn
    | Bob, Alice -> Their_turn

  (** [inject_next_move node_ctxt source ~refutation ~opponent] submits an L1
      operation (signed by [source]) to issue the next move in the refutation
      game. *)
  let inject_next_move (node_ctxt : Node_context.t) source ~refutation ~opponent
      =
    let refute_operation =
      Sc_rollup_refute {rollup = node_ctxt.rollup_address; refutation; opponent}
    in
    Injector.add_pending_operation ~source refute_operation

  let generate_proof node_ctxt game start_state =
    let open Lwt_result_syntax in
    let*! snapshot_hash =
      State.hash_of_level
        node_ctxt.Node_context.store
        (Int32.pred Raw_level.(to_int32 game.start_level))
    in
    let* snapshot_inbox = Inbox.inbox_of_hash node_ctxt snapshot_hash in
    let* snapshot_history = Inbox.history_of_hash node_ctxt snapshot_hash in
    let* snapshot_ctxt =
      Node_context.checkout_context node_ctxt snapshot_hash
    in
    let snapshot_ctxt_index = Context.index snapshot_ctxt in
    let*! snapshot_messages_tree = Context.MessageTrees.find snapshot_ctxt in
    let* snapshot_history, snapshot =
      Context.Inbox.form_history_proof
        snapshot_ctxt_index
        snapshot_history
        snapshot_inbox
        snapshot_messages_tree
      >|= Environment.wrap_tzresult
    in
    let module P = struct
      include PVM

      let context = snapshot_ctxt_index

      let state = start_state

      let reveal hash =
        Reveals.get ~data_dir:node_ctxt.data_dir ~pvm_name:PVM.name ~hash

      module Inbox_with_history = struct
        include Context.Inbox

        let history = snapshot_history

        let inbox = snapshot
      end
    end in
    let metadata = Interpreter.metadata node_ctxt in
    let* proof =
      trace
        (Sc_rollup_node_errors.Cannot_produce_proof
           (snapshot_inbox, snapshot_history, game.inbox_level))
      @@ (Sc_rollup.Proof.produce ~metadata (module P) game.inbox_level
         >|= Environment.wrap_tzresult)
    in
    let*! res =
      Sc_rollup.Proof.valid
        ~metadata
        snapshot
        game.inbox_level
        ~pvm_name:game.pvm_name
        proof
      >|= Environment.wrap_tzresult
    in
    if Result.is_ok res then return proof else assert false

  let new_dissection ~default_number_of_sections node_ctxt last_level ok
      our_view =
    let state_hash_from_tick tick =
      let open Lwt_result_syntax in
      let* r = Interpreter.state_of_tick node_ctxt tick last_level in
      return (Option.map snd r)
    in
    let start_hash, start_tick = ok in
    let start_chunk = {state_hash = Some start_hash; tick = start_tick} in
    let start_hash, start_tick = our_view in
    let our_stop_chunk = {state_hash = start_hash; tick = start_tick} in
    Game_helpers.new_dissection
      ~start_chunk
      ~our_stop_chunk
      ~default_number_of_sections
      ~state_hash_from_tick

  (** [generate_from_dissection ~default_number_of_sections node_ctxt game
      dissection] traverses the current [dissection] and returns a move which
      performs a new dissection of the execution trace or provides a refutation
      proof to serve as the next move of the [game]. *)
  let generate_next_dissection ~default_number_of_sections node_ctxt game
      dissection =
    let open Lwt_result_syntax in
    let rec traverse ok = function
      | [] ->
          (* The game invariant states that the dissection from the
             opponent must contain a tick we disagree with. If the
             retrieved game does not respect this, we cannot trust the
             Tezos node we are connected to and prefer to stop here. *)
          tzfail
            Sc_rollup_node_errors
            .Unreliable_tezos_node_returning_inconsistent_game
      | {state_hash = their_hash; tick} :: dissection -> (
          let open Lwt_result_syntax in
          let* our =
            Interpreter.state_of_tick node_ctxt tick game.inbox_level
          in
          match (their_hash, our) with
          | None, None ->
              (* This case is absurd since: [None] can only occur at the
                 end and the two players disagree about the end. *)
              assert false
          | Some _, None | None, Some _ ->
              return (ok, (Option.map snd our, tick))
          | Some their_hash, Some (_, our_hash) ->
              if Sc_rollup.State_hash.equal our_hash their_hash then
                traverse (their_hash, tick) dissection
              else return (ok, (Some our_hash, tick)))
    in
    match dissection with
    | {state_hash = Some hash; tick} :: dissection ->
        let* ok, ko = traverse (hash, tick) dissection in
        let choice = snd ok in
        let* dissection =
          new_dissection
            ~default_number_of_sections
            node_ctxt
            game.inbox_level
            ok
            ko
        in
        let chosen_section_len = Sc_rollup.Tick.distance (snd ko) choice in
        return (choice, chosen_section_len, dissection)
    | [] | {state_hash = None; _} :: _ ->
        (*
             By wellformedness of dissection.
             A dissection always starts with a tick of the form [(Some hash, tick)].
             A dissection always contains strictly more than one element.
          *)
        tzfail
          Sc_rollup_node_errors
          .Unreliable_tezos_node_returning_inconsistent_game

  let next_move node_ctxt game =
    let open Lwt_result_syntax in
    let final_move start_tick =
      let* start_state =
        Interpreter.state_of_tick node_ctxt start_tick game.inbox_level
      in
      match start_state with
      | None ->
          tzfail
            Sc_rollup_node_errors
            .Unreliable_tezos_node_returning_inconsistent_game
      | Some (start_state, _start_hash) ->
          let* proof = generate_proof node_ctxt game start_state in
          let choice = start_tick in
          return {choice; step = Proof proof}
    in

    match game.game_state with
    | Dissecting {dissection; default_number_of_sections} ->
        let* choice, chosen_section_len, dissection =
          generate_next_dissection
            ~default_number_of_sections
            node_ctxt
            game
            dissection
        in
        if Z.(equal chosen_section_len one) then final_move choice
        else return {choice; step = Dissection dissection}
    | Final_move {agreed_start_chunk; refuted_stop_chunk = _} ->
        let choice = agreed_start_chunk.tick in
        final_move choice

  let play_next_move node_ctxt game self opponent =
    let open Lwt_result_syntax in
    let* refutation = next_move node_ctxt game in
    inject_next_move node_ctxt self ~refutation:(Some refutation) ~opponent

  let play_timeout (node_ctxt : Node_context.t) self stakers =
    let timeout_operation =
      Sc_rollup_timeout {rollup = node_ctxt.rollup_address; stakers}
    in
    let source =
      Node_context.get_operator node_ctxt Timeout |> Option.value ~default:self
      (* We fallback on the [Refute] operator if none is provided for [Timeout] *)
    in
    Injector.add_pending_operation ~source timeout_operation

  let timeout_reached ~self head_block node_ctxt players =
    let open Lwt_result_syntax in
    let Node_context.{rollup_address; cctxt; _} = node_ctxt in
    let* game_result =
      Plugin.RPC.Sc_rollup.timeout_reached
        cctxt
        (cctxt#chain, head_block)
        rollup_address
        players
        ()
    in
    let open Sc_rollup.Game in
    match game_result with
    | Some (Loser {loser; _}) ->
        let is_it_me = Signature.Public_key_hash.(self = loser) in
        return (not is_it_me)
    | _ -> return_false

  let play head_block node_ctxt self game staker1 staker2 =
    let open Lwt_result_syntax in
    let players = (staker1, staker2) in
    let index = Sc_rollup.Game.Index.make staker1 staker2 in
    match turn ~self game index with
    | Our_turn {opponent} -> play_next_move node_ctxt game self opponent
    | Their_turn ->
        let* timeout_reached =
          timeout_reached ~self head_block node_ctxt players
        in
        unless timeout_reached @@ fun () -> play_timeout node_ctxt self index

  let ongoing_game head_block node_ctxt self =
    let Node_context.{rollup_address; cctxt; _} = node_ctxt in
    Plugin.RPC.Sc_rollup.ongoing_refutation_game
      cctxt
      (cctxt#chain, head_block)
      rollup_address
      self
      ()

  let play_opening_move node_ctxt self conflict =
    let open Lwt_syntax in
    let open Sc_rollup.Refutation_storage in
    let* () = Refutation_game_event.conflict_detected conflict in
    inject_next_move node_ctxt self ~refutation:None ~opponent:conflict.other

  let start_game_if_conflict head_block node_ctxt self =
    let open Lwt_result_syntax in
    let Node_context.{rollup_address; cctxt; _} = node_ctxt in
    let* conflicts =
      Plugin.RPC.Sc_rollup.conflicts
        cctxt
        (cctxt#chain, head_block)
        rollup_address
        self
        ()
    in
    let*! res =
      Option.iter_es (play_opening_move node_ctxt self) (List.hd conflicts)
    in
    match res with
    | Ok r -> return r
    | Error
        [
          Environment.Ecoproto_error
            Sc_rollup_errors.Sc_rollup_game_already_started;
        ] ->
        (* The game may already be starting in the meantime. So we
           ignore this error. *)
        return_unit
    | Error errs -> Lwt.return (Error errs)

  let process Layer1.{hash; _} node_ctxt =
    let head_block = `Hash (hash, 0) in
    let open Lwt_result_syntax in
    let refute_signer = Node_context.get_operator node_ctxt Refute in
    match refute_signer with
    | None ->
        (* Not injecting refutations, don't play refutation games *)
        return_unit
    | Some self -> (
        let* res = ongoing_game head_block node_ctxt self in
        match res with
        | Some (game, staker1, staker2) ->
            play head_block node_ctxt self game staker1 staker2
        | None -> start_game_if_conflict head_block node_ctxt self)
end
