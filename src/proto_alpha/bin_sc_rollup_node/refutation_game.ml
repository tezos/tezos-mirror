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

  val process :
    Layer1.head -> Node_context.t -> PVM.context -> unit tzresult Lwt.t
end

module Make (PVM : Pvm.S) : S with module PVM = PVM = struct
  module PVM = PVM
  module Interpreter = Interpreter.Make (PVM)
  open Sc_rollup.Game

  let node_role node_ctxt Sc_rollup.Game.Index.{alice; bob} =
    let self = node_ctxt.Node_context.operator in
    if Sc_rollup.Staker.equal alice self then Alice
    else if Sc_rollup.Staker.equal bob self then Bob
    else (* By validity of [ongoing_game] RPC. *)
      assert false

  type role = Our_turn of {opponent : public_key_hash} | Their_turn

  let turn node_ctxt game players =
    let Sc_rollup.Game.Index.{alice; bob} = players in
    match (node_role node_ctxt players, game.turn) with
    | Alice, Alice -> Our_turn {opponent = bob}
    | Bob, Bob -> Our_turn {opponent = alice}
    | Alice, Bob -> Their_turn
    | Bob, Alice -> Their_turn

  (** [inject_next_move node_ctxt move] submits an L1 operation to
      issue the next move in the refutation game. [node_ctxt] provides
      the connection to the Tezos node. *)
  let inject_next_move node_ctxt ~refutation ~opponent =
    let open Node_context in
    let open Lwt_result_syntax in
    let* source, src_pk, src_sk = Node_context.get_operator_keys node_ctxt in
    let {rollup_address; cctxt; _} = node_ctxt in
    let* _, _, Manager_operation_result {operation_result; _} =
      Client_proto_context.sc_rollup_refute
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~refutation
        ~opponent
        ~source
        ~rollup:rollup_address
        ~src_pk
        ~src_sk
        ~fee_parameter:Configuration.default_fee_parameter
        ()
    in
    let open Apply_results in
    let*! () =
      match operation_result with
      | Applied (Sc_rollup_refute_result _) ->
          Refutation_game_event.refutation_published opponent refutation
      | Failed (Sc_rollup_refute_manager_kind, _errors) ->
          Refutation_game_event.refutation_failed opponent refutation
      | Backtracked (Sc_rollup_refute_result _, _errors) ->
          Refutation_game_event.refutation_backtracked opponent refutation
      | Skipped Sc_rollup_refute_manager_kind ->
          Refutation_game_event.refutation_skipped opponent refutation
    in
    return_unit

  let generate_proof node_ctxt store game start_state =
    let open Lwt_result_syntax in
    let*! hash = Layer1.hash_of_level store (Raw_level.to_int32 game.level) in
    let* history = Inbox.history_of_hash node_ctxt store hash in
    let* inbox = Inbox.inbox_of_hash node_ctxt store hash in
    let*! messages_tree = Inbox.find_message_tree store hash in
    let*! history, history_proof =
      Store.Inbox.form_history_proof store history inbox messages_tree
    in
    let module P = struct
      include PVM

      let context = store

      let state = start_state

      module Inbox_with_history = struct
        include Store.Inbox

        let history = history

        let inbox = history_proof
      end
    end in
    let* r =
      trace
        (Sc_rollup_node_errors.Cannot_produce_proof (inbox, history, game.level))
      @@ (Sc_rollup.Proof.produce (module P) game.level
         >|= Environment.wrap_tzresult)
    in
    let+ check =
      Sc_rollup.Proof.valid history_proof game.level ~pvm_name:game.pvm_name r
      >|= Environment.wrap_tzresult
    in
    assert check ;
    r

  let new_dissection node_ctxt store last_level ok our_view =
    let open Lwt_result_syntax in
    let start_hash, start_tick = ok in
    let our_state, stop_tick = our_view in
    let Node_context.{protocol_constants; _} = node_ctxt in
    let max_number_of_sections =
      Z.of_int
        protocol_constants.parametric.sc_rollup.number_of_sections_in_dissection
    in
    let trace_length = Z.succ (Sc_rollup.Tick.distance stop_tick start_tick) in
    let number_of_sections = Z.min max_number_of_sections trace_length in
    let rem = Z.(rem trace_length number_of_sections) in
    let first_section_length, section_length =
      if Z.Compare.(trace_length < max_number_of_sections) then
        (* In this case, every section is of length one. *)
        Z.(one, one)
      else
        let section_length =
          Z.(max one (div trace_length number_of_sections))
        in
        if Z.Compare.(section_length = Z.one) && not Z.Compare.(rem = Z.zero)
        then
          (* If we put [section_length] in this situation, we will most likely
             have a very long last section. *)
          (rem, section_length)
        else (section_length, section_length)
    in
    (* [k] is the number of sections in [rev_dissection]. *)
    let rec make rev_dissection k tick =
      if Z.(equal k (pred number_of_sections)) then
        return
        @@ List.rev
             ({state_hash = our_state; tick = stop_tick} :: rev_dissection)
      else
        let* r = Interpreter.state_of_tick node_ctxt store tick last_level in
        let state_hash = Option.map snd r in
        let next_tick = Sc_rollup.Tick.jump tick section_length in
        make ({state_hash; tick} :: rev_dissection) (Z.succ k) next_tick
    in
    make
      [{state_hash = Some start_hash; tick = start_tick}]
      Z.one
      (Sc_rollup.Tick.jump start_tick first_section_length)

  (** [generate_from_dissection node_ctxt store game]
      traverses the current [game.dissection] and returns a move which
      performs a new dissection of the execution trace or provides a
      refutation proof to serve as the next move of the [game]. *)
  let generate_next_dissection node_ctxt store game =
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
            Interpreter.state_of_tick node_ctxt store tick game.level
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
    match game.dissection with
    | {state_hash = Some hash; tick} :: dissection ->
        let* ok, ko = traverse (hash, tick) dissection in
        let choice = snd ok in
        let* dissection = new_dissection node_ctxt store game.level ok ko in
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

  let next_move node_ctxt store game =
    let open Lwt_result_syntax in
    let final_move start_tick =
      let* start_state =
        Interpreter.state_of_tick node_ctxt store start_tick game.level
      in
      match start_state with
      | None ->
          tzfail
            Sc_rollup_node_errors
            .Unreliable_tezos_node_returning_inconsistent_game
      | Some (start_state, _start_hash) ->
          let* proof = generate_proof node_ctxt store game start_state in
          let choice = start_tick in
          return {choice; step = Proof proof}
    in
    let* choice, chosen_section_len, dissection =
      generate_next_dissection node_ctxt store game
    in
    if Z.(equal chosen_section_len one) then final_move choice
    else return {choice; step = Dissection dissection}

  let try_ f =
    let open Lwt_result_syntax in
    let*! _res = f () in
    return_unit

  let play_next_move node_ctxt store game opponent =
    let open Lwt_result_syntax in
    let* refutation = next_move node_ctxt store game in
    (* FIXME: #3008

       We currently do not remember that we already have
       injected a refutation move but it is not included yet.
       Hence, we ignore errors here temporarily, waiting for
       the injector to enter the scene.
    *)
    try_ @@ fun () ->
    inject_next_move node_ctxt ~refutation:(Some refutation) ~opponent

  let play_timeout node_ctxt players =
    let Sc_rollup.Game.Index.{alice; bob} = players in
    let open Node_context in
    let open Lwt_result_syntax in
    let* source, src_pk, src_sk = Node_context.get_operator_keys node_ctxt in
    let {rollup_address; cctxt; _} = node_ctxt in
    let* _, _, Manager_operation_result {operation_result; _} =
      Client_proto_context.sc_rollup_timeout
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~source
        ~alice
        ~bob
        ~rollup:rollup_address
        ~src_pk
        ~src_sk
        ~fee_parameter:Configuration.default_fee_parameter
        ()
    in
    let open Apply_results in
    let*! () =
      match operation_result with
      | Applied (Sc_rollup_timeout_result _) ->
          Refutation_game_event.timeout_published players
      | Failed (Sc_rollup_timeout_manager_kind, _errors) ->
          Refutation_game_event.timeout_failed players
      | Backtracked (Sc_rollup_timeout_result _, _errors) ->
          Refutation_game_event.timeout_backtracked players
      | Skipped Sc_rollup_timeout_manager_kind ->
          Refutation_game_event.timeout_skipped players
    in
    return_unit

  let timeout_reached head_block node_ctxt players =
    let open Lwt_result_syntax in
    let Node_context.{rollup_address; cctxt; _} = node_ctxt in
    let* res =
      Plugin.RPC.Sc_rollup.timeout_reached
        cctxt
        (cctxt#chain, head_block)
        rollup_address
        players
        ()
    in
    let open Sc_rollup.Game in
    let index = Index.make (fst players) (snd players) in
    let node_player = node_role node_ctxt index in
    match res with
    | Some player when not (player_equal node_player player) -> return_true
    | None -> return_false
    | Some _myself -> return_false

  let play head_block node_ctxt store game staker1 staker2 =
    let open Lwt_result_syntax in
    let players = (staker1, staker2) in
    let index = Sc_rollup.Game.Index.make staker1 staker2 in
    match turn node_ctxt game index with
    | Our_turn {opponent} -> play_next_move node_ctxt store game opponent
    | Their_turn ->
        let* timeout_reached = timeout_reached head_block node_ctxt players in
        unless timeout_reached @@ fun () ->
        try_ @@ fun () -> play_timeout node_ctxt index

  let ongoing_game head_block node_ctxt =
    let Node_context.{rollup_address; cctxt; operator; _} = node_ctxt in
    Plugin.RPC.Sc_rollup.ongoing_refutation_game
      cctxt
      (cctxt#chain, head_block)
      rollup_address
      operator
      ()

  let play_opening_move node_ctxt conflict =
    let open Lwt_syntax in
    let open Sc_rollup.Refutation_storage in
    let* () = Refutation_game_event.conflict_detected conflict in
    inject_next_move node_ctxt ~refutation:None ~opponent:conflict.other

  let start_game_if_conflict head_block node_ctxt =
    let open Lwt_result_syntax in
    let Node_context.{rollup_address; cctxt; operator; _} = node_ctxt in
    let* conflicts =
      Plugin.RPC.Sc_rollup.conflicts
        cctxt
        (cctxt#chain, head_block)
        rollup_address
        operator
        ()
    in
    let*! res =
      Option.iter_es (play_opening_move node_ctxt) (List.hd conflicts)
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

  let process (Layer1.Head {hash; _}) node_ctxt store =
    let head_block = `Hash (hash, 0) in
    let open Lwt_result_syntax in
    let* res = ongoing_game head_block node_ctxt in
    match res with
    | Some (game, staker1, staker2) ->
        play head_block node_ctxt store game staker1 staker2
    | None -> start_game_if_conflict head_block node_ctxt
end
