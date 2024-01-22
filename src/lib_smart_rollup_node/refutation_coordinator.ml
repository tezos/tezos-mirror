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

open Refutation_coordinator_types
module Player = Refutation_player
module Pkh_map = Signature.Public_key_hash.Map
module Pkh_table = Signature.Public_key_hash.Table

type state = {node_ctxt : Node_context.rw; pending_opponents : unit Pkh_table.t}

let untracked_conflicts opponent_players conflicts =
  List.filter
    (fun conflict -> not @@ Pkh_map.mem conflict.Game.other opponent_players)
    conflicts

(* Transform the list of ongoing games [(Game.t * pkh * pkh) list]
   into a mapping from opponents' pkhs to their corresponding game
   state.
*)
let make_game_map self ongoing_games =
  List.fold_left
    (fun acc (game, alice, bob) ->
      let opponent_pkh =
        if Signature.Public_key_hash.equal self alice then bob else alice
      in
      Pkh_map.add opponent_pkh game acc)
    Pkh_map.empty
    ongoing_games

let on_process Layer1.{level; _} state =
  let node_ctxt = state.node_ctxt in
  let open Lwt_result_syntax in
  let refute_signer = Node_context.get_operator node_ctxt Operating in
  match refute_signer with
  | None ->
      (* Not injecting refutations, don't play refutation games *)
      return_unit
  | Some (Single self) ->
      let Node_context.{config; _} = node_ctxt in
      let* plugin = Protocol_plugins.last_proto_plugin node_ctxt in
      let module Plugin = (val plugin) in
      (* Current conflicts in L1 *)
      let* conflicts =
        Plugin.Refutation_game_helpers.get_conflicts
          state.node_ctxt.cctxt
          config.sc_rollup_address
          self
      in
      (* Map of opponents the node is playing against to the corresponding
         player worker *)
      let opponent_players =
        Pkh_map.of_seq @@ List.to_seq @@ Player.current_games ()
      in
      (* Conflicts for which we need to start new refutation players.
         Some of these might be ongoing. *)
      let new_conflicts = untracked_conflicts opponent_players conflicts in
      (* L1 ongoing games *)
      let* ongoing_games =
        Plugin.Refutation_game_helpers.get_ongoing_games
          state.node_ctxt.cctxt
          config.sc_rollup_address
          self
      in
      (* Map between opponents and their corresponding games *)
      let ongoing_game_map = make_game_map self ongoing_games in
      (* Launch new players for new conflicts, and play one step *)
      let* () =
        List.iter_ep
          (fun conflict ->
            let other = conflict.Octez_smart_rollup.Game.other in
            Pkh_table.replace state.pending_opponents other () ;
            let game = Pkh_map.find_opt other ongoing_game_map in
            Player.init_and_play node_ctxt ~self ~conflict ~game ~level)
          new_conflicts
      in
      let*! () =
        (* Play one step of the refutation game in every remaining player *)
        Pkh_map.iter_p
          (fun opponent worker ->
            match Pkh_map.find opponent ongoing_game_map with
            | Some game ->
                Pkh_table.remove state.pending_opponents opponent ;
                Player.play worker game ~level
            | None ->
                (* Kill finished players: those who don't aren't
                   playing against pending opponents that don't have
                   ongoing games in the L1 *)
                if not @@ Pkh_table.mem state.pending_opponents opponent then
                  Player.shutdown worker
                else Lwt.return_unit)
          opponent_players
      in
      return_unit

module Types = struct
  type nonrec state = state

  type parameters = Node_context.rw
end

module Name = struct
  (* We only have a single coordinator in the node *)
  type t = unit

  let encoding = Data_encoding.unit

  let base =
    (* But we can have multiple instances in the unit tests. This is just to
       avoid conflicts in the events declarations. *)
    Refutation_game_event.Coordinator.section @ ["worker"]

  let pp _ _ = ()

  let equal () () = true
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun w request ->
    let state = Worker.state w in
    match request with
    | Request.Process b -> protect @@ fun () -> on_process b state

  type launch_error = error trace

  let on_launch _w () node_ctxt =
    Lwt_result.return {node_ctxt; pending_opponents = Pkh_table.create 5}

  let on_error (type a b) _w st (r : (a, b) Request.t) (errs : b) :
      unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    let request_view = Request.view r in
    let emit_and_return_errors errs =
      let*! () =
        Refutation_game_event.Coordinator.request_failed request_view st errs
      in
      return_unit
    in
    match r with Request.Process _ -> emit_and_return_errors errs

  let on_completion _w r _ st =
    Refutation_game_event.Coordinator.request_completed (Request.view r) st

  let on_no_request _ = Lwt.return_unit

  let on_close _w = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

let start (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  let*! () = Refutation_game_event.Coordinator.starting () in
  let+ worker = Worker.launch table () node_ctxt (module Handlers) in
  Lwt.wakeup worker_waker worker

let start_in_mode mode =
  let open Configuration in
  match mode with
  | Accuser | Bailout | Operator | Maintenance -> true
  | Observer | Batcher -> false
  | Custom ops -> purpose_matches_mode (Custom ops) Operating

let init (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  match Lwt.state worker_promise with
  | Lwt.Return _ ->
      (* Worker already started, nothing to do. *)
      return_unit
  | Lwt.Fail exn ->
      (* Worker crashed, not recoverable. *)
      fail [Rollup_node_errors.No_refutation_coordinator; Exn exn]
  | Lwt.Sleep ->
      (* Never started, start it. *)
      if start_in_mode node_ctxt.config.mode then start node_ctxt
      else return_unit

(* This is a refutation coordinator for a single scoru *)
let worker =
  let open Result_syntax in
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> return worker
    | Lwt.Fail exn -> fail (Error_monad.error_of_exn exn)
    | Lwt.Sleep -> Error Rollup_node_errors.No_refutation_coordinator)

let process b =
  let open Lwt_result_syntax in
  match Lazy.force worker with
  | Ok w ->
      let*! (_pushed : bool) =
        Worker.Queue.push_request w (Request.Process b)
      in
      return_unit
  | Error Rollup_node_errors.No_refutation_coordinator -> return_unit
  | Error e -> tzfail e

let shutdown () =
  let open Lwt_syntax in
  match Lazy.force worker with
  | Error _ ->
      (* There is no refutation coordinator, nothing to do *)
      return_unit
  | Ok w ->
      (* Shut down all current refutation players *)
      let games = Player.current_games () in
      let* () =
        List.iter_s (fun (_opponent, player) -> Player.shutdown player) games
      in
      Worker.shutdown w
