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

open Game

let node_role ~self {alice; bob} =
  if Signature.Public_key_hash.equal alice self then Alice
  else if Signature.Public_key_hash.equal bob self then Bob
  else (* By validity of [ongoing_game] RPC. *)
    assert false

type role = Our_turn of {opponent : Signature.public_key_hash} | Their_turn

let turn ~self game ({alice; bob} as players) =
  match (node_role ~self players, game.turn) with
  | Alice, Alice -> Our_turn {opponent = bob}
  | Bob, Bob -> Our_turn {opponent = alice}
  | Alice, Bob -> Their_turn
  | Bob, Alice -> Their_turn

(** [inject_next_move node_ctxt ~refutation ~opponent ~commitment
      ~opponent_commitment] submits an L1 operation to
      issue the next move in the refutation game. *)
let inject_next_move node_ctxt ~refutation ~opponent =
  let open Lwt_result_syntax in
  let refute_operation =
    L1_operation.Refute
      {
        rollup = node_ctxt.Node_context.config.sc_rollup_address;
        refutation;
        opponent;
      }
  in
  let* _hash =
    Injector.check_and_add_pending_operation
      node_ctxt.config.mode
      refute_operation
  in
  return_unit

type pvm_intermediate_state =
  | Hash of State_hash.t
  | Evaluated of {
      head : (Fuel.Accounted.t, Context.pvmstate) Pvm_plugin_sig.eval_state;
          (** The working head which we are currently updating to go forwards
              during dissection traversal. *)
      snapshot :
        ( Fuel.Accounted.t,
          Context.PVMState.immutable_value )
        Pvm_plugin_sig.eval_state;
          (** A fixed view of the most recent agreement point. We must make a
              copy of the current pvm state computed during traversal because we
              use the previous state we agree with as a starting point of the
              following dissection. *)
    }

let new_dissection (module Plugin : Protocol_plugin_sig.S) ~opponent
    ~default_number_of_sections ~commitment_period_tick_offset node_ctxt
    state_cache last_level ok our_view =
  let open Lwt_result_syntax in
  let start_hash, start_tick, start_state =
    match ok with
    | Hash hash, tick -> (hash, tick, None)
    | Evaluated state, tick ->
        (* Only use immutable copy because mutable version has been modified *)
        ( state.snapshot.info.state_hash,
          tick,
          Some (Pvm_plugin_sig.to_mut_eval_state state.snapshot) )
  in
  let start_chunk = Game.{state_hash = Some start_hash; tick = start_tick} in
  let our_state, our_tick = our_view in
  let our_state_hash =
    Option.map (fun s -> s.Pvm_plugin_sig.info.state_hash) our_state
  in
  let our_stop_chunk = Game.{state_hash = our_state_hash; tick = our_tick} in
  let* dissection =
    Plugin.Refutation_game_helpers.make_dissection
      (module Plugin)
      node_ctxt
      state_cache
      ~start_state
      ~start_chunk
      ~our_stop_chunk
      ~default_number_of_sections
      ~commitment_period_tick_offset
      ~last_level
  in
  let*! () =
    Refutation_game_event.computed_dissection
      ~opponent
      ~start_tick
      ~end_tick:our_tick
      dissection
  in
  return dissection

(** [generate_from_dissection ~default_number_of_sections ~tick_offset node_ctxt
    state_cache game dissection] traverses the current [dissection] and returns
    a move which performs a new dissection of the execution trace or provides a
    refutation proof to serve as the next move of the [game]. [tick_offset] is
    the initial global tick (since genesis) of the PVM at the start of the
    commitment period. *)
let generate_next_dissection (module Plugin : Protocol_plugin_sig.S)
    ~default_number_of_sections node_ctxt state_cache ~opponent
    ~commitment_period_tick_offset (game : Octez_smart_rollup.Game.t)
    (dissection : Octez_smart_rollup.Game.dissection_chunk list) =
  let open Lwt_result_syntax in
  let rec traverse ok = function
    | [] ->
        (* The game invariant states that the dissection from the
           opponent must contain a tick we disagree with. If the
           retrieved game does not respect this, we cannot trust the
           Tezos node we are connected to and prefer to stop here. *)
        tzfail
          Rollup_node_errors.Unreliable_tezos_node_returning_inconsistent_game
    | Octez_smart_rollup.Game.{state_hash = their_hash; tick} :: dissection -> (
        let start_state =
          match ok with
          | Hash _, _ -> None
          | Evaluated ok_state, _ -> Some ok_state.head
        in
        let* our =
          Interpreter.state_of_tick
            (module Plugin)
            node_ctxt
            state_cache
            ?start_state
            ~tick:(Z.add tick commitment_period_tick_offset)
            game.inbox_level
        in
        match (their_hash, our) with
        | None, None ->
            (* This case is absurd since: [None] can only occur at the
               end and the two players disagree about the end. *)
            assert false
        | Some _, None | None, Some _ -> return (ok, (our, tick))
        | Some their_hash, Some our_state ->
            if
              Octez_smart_rollup.State_hash.equal
                our_state.info.state_hash
                their_hash
            then
              let ok =
                Evaluated
                  {
                    head = our_state;
                    snapshot = Pvm_plugin_sig.to_imm_eval_state our_state;
                  }
              in
              traverse (ok, tick) dissection
            else return (ok, (our, tick)))
  in
  match dissection with
  | {state_hash = Some hash; tick} :: dissection ->
      let* ok, ko = traverse (Hash hash, tick) dissection in
      let* dissection =
        new_dissection
          (module Plugin)
          ~opponent
          ~default_number_of_sections
          ~commitment_period_tick_offset
          node_ctxt
          state_cache
          game.inbox_level
          ok
          ko
      in
      let _, choice = ok in
      let _, ko_tick = ko in
      let chosen_section_len = Z.abs (Z.sub choice ko_tick) in
      return (choice, chosen_section_len, dissection)
  | [] | {state_hash = None; _} :: _ ->
      (*
             By wellformedness of dissection.
             A dissection always starts with a tick of the form [(Some hash, tick)].
             A dissection always contains strictly more than one element.
          *)
      tzfail
        Rollup_node_errors.Unreliable_tezos_node_returning_inconsistent_game

let next_move (module Plugin : Protocol_plugin_sig.S) node_ctxt state_cache
    ~opponent ~commitment_period_tick_offset (game : Octez_smart_rollup.Game.t)
    =
  let open Lwt_result_syntax in
  let final_move start_tick =
    let* start_state =
      Interpreter.state_of_tick
        (module Plugin)
        node_ctxt
        state_cache
        ~tick:(Z.add start_tick commitment_period_tick_offset)
        game.inbox_level
    in
    match start_state with
    | None ->
        tzfail
          Rollup_node_errors.Unreliable_tezos_node_returning_inconsistent_game
    | Some {state = start_state; _} ->
        let* proof =
          Plugin.Refutation_game_helpers.generate_proof
            node_ctxt
            game
            start_state
        in
        let choice = start_tick in
        Metrics.wrap (fun () ->
            let opponent = Signature.Public_key_hash.to_b58check opponent in
            Metrics.Refutation.clear_state_refutation_game
              [opponent; Int32.to_string game.start_level]) ;
        return (Octez_smart_rollup.Game.Move {choice; step = Proof proof})
  in

  match game.game_state with
  | Dissecting {dissection; default_number_of_sections} ->
      let* choice, chosen_section_len, dissection =
        generate_next_dissection
          (module Plugin)
          ~default_number_of_sections
          node_ctxt
          state_cache
          ~opponent
          ~commitment_period_tick_offset
          game
          dissection
      in
      if Z.(equal chosen_section_len one) then final_move choice
      else
        return
          (Octez_smart_rollup.Game.Move {choice; step = Dissection dissection})
  | Final_move {agreed_start_chunk; refuted_stop_chunk = _} ->
      let choice = agreed_start_chunk.tick in
      final_move choice

let play_next_move plugin node_ctxt state_cache ~commitment_period_tick_offset
    game opponent =
  let open Lwt_result_syntax in
  let* refutation =
    next_move
      plugin
      node_ctxt
      state_cache
      ~opponent
      ~commitment_period_tick_offset
      game
  in
  inject_next_move node_ctxt ~refutation ~opponent

let play_timeout (node_ctxt : _ Node_context.t) stakers =
  let open Lwt_result_syntax in
  let timeout_operation =
    L1_operation.Timeout {rollup = node_ctxt.config.sc_rollup_address; stakers}
  in
  let* _hash =
    Injector.check_and_add_pending_operation
      node_ctxt.config.mode
      timeout_operation
  in
  return_unit

let pick_timeout ~role timeout =
  match role with Alice -> timeout.alice_timeout | Bob -> timeout.bob_timeout

let metric_helper ~node_ctxt ~self ~game ~opponent
    ~(plugin : Protocol_plugins.proto_plugin) =
  let open Lwt_result_syntax in
  let module Plugin = (val plugin) in
  let* timeout_opt =
    Plugin.Refutation_game_helpers.timeout node_ctxt ~self ~opponent
  in
  Lwt.return_ok (Option.map (pick_timeout ~role:game.turn) timeout_opt)

let register_turn_metric ~node_ctxt ~self ~game ~opponent ~plugin turn =
  Metrics.wrap_lwt @@ fun () ->
  let open Lwt_result_syntax in
  Lwt.return_ok
  @@ dont_wait
       (fun () ->
         let* timeout_option =
           metric_helper ~node_ctxt ~self ~game ~opponent ~plugin
         in
         (match timeout_option with
         | Some timeout_player ->
             let opponent = Signature.Public_key_hash.to_b58check opponent in
             Metrics.Refutation.set_state_refutation_game
               ~labels:[opponent; Int32.to_string game.start_level]
               turn ;
             Metrics.Refutation.set_block_timeout
               ~labels:[opponent; Int32.to_string game.start_level]
               timeout_player
         | None -> ()) ;
         return ())
       (fun trace ->
         Event.metrics_error (Format.asprintf "%a" pp_print_trace trace))
       (fun exn -> Event.metrics_error (Printexc.to_string exn))

let play node_ctxt state_cache ~self ~commitment_period_tick_offset game
    opponent =
  let open Lwt_result_syntax in
  let index = make_index self opponent in
  let* plugin = Protocol_plugins.last_proto_plugin node_ctxt in
  match turn ~self game index with
  | Our_turn {opponent} ->
      let module Plugin = (val plugin) in
      let* () =
        register_turn_metric ~node_ctxt ~self ~game ~opponent ~plugin OurTurn
      in
      play_next_move
        plugin
        node_ctxt
        state_cache
        ~commitment_period_tick_offset
        game
        opponent
  | Their_turn ->
      let module Plugin = (val plugin) in
      let* () =
        register_turn_metric ~node_ctxt ~self ~game ~opponent ~plugin TheirTurn
      in
      let* timeout_reached =
        Plugin.Refutation_game_helpers.timeout_reached node_ctxt ~self ~opponent
      in
      when_ timeout_reached @@ fun () ->
      Metrics.wrap (fun () ->
          let opponent = Signature.Public_key_hash.to_b58check opponent in
          Metrics.Refutation.(
            set_state_refutation_game
              ~labels:[opponent; Int32.to_string game.start_level]
              Timeout)) ;
      let*! () = Refutation_game_event.timeout_detected opponent in
      play_timeout node_ctxt index

let play_opening_move node_ctxt (conflict : Octez_smart_rollup.Game.conflict) =
  let open Lwt_syntax in
  let* () = Refutation_game_event.conflict_detected conflict in
  let player_commitment_hash =
    Octez_smart_rollup.Commitment.hash conflict.our_commitment
  in
  let opponent_commitment_hash =
    Octez_smart_rollup.Commitment.hash conflict.their_commitment
  in
  let refutation =
    Octez_smart_rollup.Game.Start
      {player_commitment_hash; opponent_commitment_hash}
  in
  inject_next_move node_ctxt ~refutation ~opponent:conflict.other
