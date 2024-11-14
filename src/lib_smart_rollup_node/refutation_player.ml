(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Refutation_player_types
open Refutation_game

module Types = struct
  type state = {
    node_ctxt : Node_context.rw;
    state_cache : Pvm_plugin_sig.state_cache;
    self : Signature.public_key_hash;
    opponent : Signature.public_key_hash;
    conflict : Game.conflict;
    commitment_period_tick_offset : Z.t;
    mutable last_move_cache :
      (Octez_smart_rollup.Game.game_state * int32) option;
  }

  type parameters = {
    node_ctxt : Node_context.rw;
    self : Signature.public_key_hash;
    conflict : Octez_smart_rollup.Game.conflict;
  }
end

module Name = struct
  let base = Refutation_game_event.Player.section @ ["worker"]

  include Signature.Public_key_hash
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

let table = Worker.create_table Queue

let on_play game
    Types.
      {node_ctxt; state_cache; self; opponent; commitment_period_tick_offset; _}
    =
  play node_ctxt state_cache ~self ~commitment_period_tick_offset game opponent

let on_play_opening conflict (Types.{node_ctxt; _} : Types.state) =
  play_opening_move node_ctxt conflict

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Play game -> protect @@ fun () -> on_play game state
    | Request.Play_opening conflict ->
        protect @@ fun () -> on_play_opening conflict state

  type launch_error = error trace

  let on_launch _w _name Types.{node_ctxt; self; conflict} =
    let open Lwt_result_syntax in
    let* commitment_period_tick_offset =
      Node_context.tick_offset_of_commitment_period
        node_ctxt
        conflict.our_commitment
    in
    let state_cache = Pvm_plugin_sig.make_state_cache 64 in
    return
      Types.
        {
          node_ctxt;
          state_cache;
          self;
          opponent = conflict.other;
          conflict;
          commitment_period_tick_offset;
          last_move_cache = None;
        }

  let on_error (type a b) _w st (r : (a, b) Request.t) (errs : b) :
      unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    let request_view = Request.view r in
    let emit_and_return_errors errs =
      let*! () =
        Refutation_game_event.Player.request_failed request_view st errs
      in
      return_unit
    in
    match r with
    | Request.Play _ -> emit_and_return_errors errs
    | Request.Play_opening _ -> emit_and_return_errors errs

  let on_completion _w r _ st =
    Refutation_game_event.Player.request_completed (Request.view r) st

  let on_no_request _ = Lwt.return_unit

  let on_close w =
    let open Lwt_syntax in
    let state = Worker.state w in
    let* () = Refutation_game_event.Player.stopped state.opponent in
    return_unit
end

let init node_ctxt ~self ~conflict =
  let open Lwt_result_syntax in
  let*! () =
    Refutation_game_event.Player.started
      conflict.Game.other
      conflict.Game.our_commitment
  in
  let worker_promise, worker_waker = Lwt.task () in
  let* worker =
    trace Rollup_node_errors.Refutation_player_failed_to_start
    @@ Worker.launch
         table
         conflict.other
         {node_ctxt; self; conflict}
         (module Handlers)
  in
  let () = Lwt.wakeup worker_waker worker in
  let worker =
    let open Result_syntax in
    match Lwt.state worker_promise with
    | Lwt.Return worker -> return worker
    | Lwt.Fail _ | Lwt.Sleep ->
        tzfail Rollup_node_errors.Refutation_player_failed_to_start
  in
  Lwt.return worker

(* Play if:
    - There's a new game state to play against or
    - The current level is past the buffer for re-playing in the
      same game state.
*)
let should_move ~level game last_move_cache =
  match last_move_cache with
  | None -> true
  | Some (last_move_game_state, last_move_level) ->
      (not (Game.game_state_equal game.Game.game_state last_move_game_state))
      || Int32.(
           sub level last_move_level
           > of_int Configuration.refutation_player_buffer_levels)

let play w game ~(level : int32) =
  let open Lwt_syntax in
  let state = Worker.state w in
  if should_move ~level game state.last_move_cache then (
    let* pushed = Worker.Queue.push_request w (Request.Play game) in
    if pushed then state.last_move_cache <- Some (game.Game.game_state, level) ;
    return_unit)
  else return_unit

let play_opening w conflict =
  let open Lwt_syntax in
  let* (_pushed : bool) =
    Worker.Queue.push_request w (Request.Play_opening conflict)
  in
  return_unit

let init_and_play node_ctxt ~self ~conflict ~game ~level =
  let open Lwt_result_syntax in
  let* worker = init node_ctxt ~self ~conflict in
  let*! () =
    match game with
    | None -> play_opening worker conflict
    | Some game -> play worker game ~level
  in
  return_unit

let current_games () =
  List.map
    (fun (_name, worker) -> ((Worker.state worker).opponent, worker))
    (Worker.list table)

let shutdown = Worker.shutdown
