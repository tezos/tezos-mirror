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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2880
   Add corresponding .mli file. *)

let section = ["smart_rollup_node"; "refutation_game"]

module Simple = struct
  include Internal_event.Simple

  let conflict_detected =
    declare_5
      ~section
      ~name:"smart_rollup_node_conflict_detected"
      ~msg:
        "A conflict has been found with our commitment {our_commitment_hash} \
         at level {level} with staker {other} that hash issued commitment \
         {their_commitment_hash} both based on {parent_commitment_hash}"
      ~level:Notice
      ("our_commitment_hash", Octez_smart_rollup.Commitment.Hash.encoding)
      ("level", Data_encoding.int32)
      ("other", Signature.Public_key_hash.encoding)
      ("their_commitment_hash", Octez_smart_rollup.Commitment.Hash.encoding)
      ("parent_commitment_hash", Octez_smart_rollup.Commitment.Hash.encoding)

  let potential_conflict_detected =
    declare_4
      ~section
      ~name:"smart_rollup_node_potential_conflict_detected"
      ~msg:
        "A potential conflict has been found with our commitment \
         {our_commitment_hash} at level {level} with staker {other} that hash \
         issued commitment {their_commitment_hash}"
      ~level:Notice
      ("our_commitment_hash", Commitment.Hash.encoding)
      ("level", Data_encoding.int32)
      ("other", Signature.Public_key_hash.encoding)
      ("their_commitment_hash", Commitment.Hash.encoding)

  let timeout_detected =
    declare_1
      ~section
      ~name:"smart_rollup_node_timeout_detected"
      ~msg:"The rollup node has detected that opponent {other} can be timed out"
      ~level:Notice
      ("other", Signature.Public_key_hash.encoding)

  let computed_dissection =
    declare_4
      ~section
      ~name:"smart_rollup_node_computed_dissection"
      ~msg:
        "Computed dissection against {opponent} between ticks {start_tick} and \
         {end_tick}: {dissection}"
      ~level:Notice
      ("opponent", Signature.Public_key_hash.encoding)
      ("start_tick", Data_encoding.z)
      ("end_tick", Data_encoding.z)
      ( "dissection",
        Data_encoding.list Octez_smart_rollup.Game.dissection_chunk_encoding )
      ~pp4:(fun ppf d ->
        Format.fprintf
          ppf
          "%a"
          Data_encoding.Json.pp
          Data_encoding.Json.(
            construct
              (Data_encoding.list
                 Octez_smart_rollup.Game.dissection_chunk_encoding)
              d))

  let no_committed_context =
    declare_2
      ~section
      ~name:"smart_rollup_node_refutation_game_no_committed_context"
      ~msg:
        "No committed context for {block_hash} at level {level}, walking back \
         to find nearest committed ancestor"
      ~level:Error
      ~pp1:Block_hash.pp_short
      ("block_hash", Block_hash.encoding)
      ("level", Data_encoding.int32)

  let replaying_blocks =
    declare_3
      ~section
      ~name:"smart_rollup_node_refutation_game_replaying_blocks"
      ~msg:
        "Replaying {count} blocks from level {from_level} to reconstruct \
         context for {block_hash}"
      ~level:Warning
      ("count", Data_encoding.int31)
      ("from_level", Data_encoding.int32)
      ~pp3:Block_hash.pp_short
      ("block_hash", Block_hash.encoding)

  let traversing_dissection =
    declare_4
      ~section
      ~name:"smart_rollup_node_refutation_game_traversing_dissection"
      ~msg:
        "Traversing opponent dissection against {opponent}: \
         {dissection_length} chunks from tick {start_tick} to {end_tick}"
      ~level:Debug
      ("opponent", Signature.Public_key_hash.encoding)
      ~pp1:Signature.Public_key_hash.pp
      ("dissection_length", Data_encoding.int31)
      ("start_tick", Data_encoding.z)
      ("end_tick", Data_encoding.z)

  let dissection_agree =
    declare_2
      ~section
      ~name:"smart_rollup_node_refutation_game_dissection_agree"
      ~msg:"Dissection: agree at tick {tick} (hash: {state_hash})"
      ~level:Debug
      ("tick", Data_encoding.z)
      ("state_hash", State_hash.encoding)

  let dissection_disagree =
    declare_3
      ~section
      ~name:"smart_rollup_node_refutation_game_dissection_disagree"
      ~msg:
        "Dissection: disagree at tick {tick} (theirs: {their_hash}, ours: \
         {our_hash})"
      ~level:Debug
      ("tick", Data_encoding.z)
      ("their_hash", State_hash.encoding)
      ("our_hash", State_hash.encoding)

  let computing_dissection =
    declare_3
      ~section
      ~name:"smart_rollup_node_refutation_game_computing_dissection"
      ~msg:
        "Computing new dissection against {opponent} from tick {start_tick} to \
         {end_tick}"
      ~level:Debug
      ("opponent", Signature.Public_key_hash.encoding)
      ~pp1:Signature.Public_key_hash.pp
      ("start_tick", Data_encoding.z)
      ("end_tick", Data_encoding.z)

  let playing_next_move =
    declare_3
      ~section
      ~name:"smart_rollup_node_refutation_game_playing_next_move"
      ~msg:"Playing next move against {opponent} ({game_state}, ticks: {ticks})"
      ~level:Info
      ("opponent", Signature.Public_key_hash.encoding)
      ~pp1:Signature.Public_key_hash.pp
      ("game_state", Data_encoding.string)
      ~pp2:Format.pp_print_string
      ("ticks", Data_encoding.list Data_encoding.z)
      ~pp3:(fun ppf ticks ->
        Format.fprintf
          ppf
          "@[<hov 1>[%a]@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             Z.pp_print)
          ticks)

  module Worker
      (ARG : sig
        val section : string list
      end)
      (Request : Worker_intf.REQUEST) =
  struct
    include ARG

    let request_failed =
      declare_3
        ~section
        ~name:"request_failed"
        ~msg:"Request {view} failed ({worker_status}): {errors}"
        ~level:Notice
        ("view", Request.encoding)
        ~pp1:Request.pp
        ("worker_status", Worker_types.request_status_encoding)
        ~pp2:Worker_types.pp_status
        ("errors", Error_monad.trace_encoding)
        ~pp3:Error_monad.pp_print_trace

    let request_completed =
      declare_2
        ~section
        ~name:"request_completed"
        ~msg:"{view} {worker_status}"
        ~level:Debug
        ("view", Request.encoding)
        ("worker_status", Worker_types.request_status_encoding)
        ~pp1:Request.pp
        ~pp2:Worker_types.pp_status
  end

  module Player = struct
    include
      Worker
        (struct
          let section = section @ ["player"]
        end)
        (Refutation_player_types.Request)

    let started =
      declare_2
        ~section
        ~name:"player_started"
        ~msg:
          "Refutation player started to play against {opponent}, defenfing \
           commitment {commitment}"
        ~level:Notice
        ("opponent", Signature.Public_key_hash.encoding)
        ~pp1:Signature.Public_key_hash.pp
        ("commitment", Octez_smart_rollup.Commitment.encoding)
        ~pp2:Octez_smart_rollup.Commitment.pp

    let stopped =
      declare_1
        ~section
        ~name:"player_stopped"
        ~msg:"Refutation player for opponent {opponent} has been stopped"
        ~level:Notice
        ("opponent", Signature.Public_key_hash.encoding)
        ~pp1:Signature.Public_key_hash.pp
  end

  module Coordinator = struct
    include
      Worker
        (struct
          let section = section @ ["coordinator"]
        end)
        (Refutation_coordinator_types.Request)

    let starting =
      declare_0
        ~section
        ~name:"coordinator_starting"
        ~msg:"Starting refutation coordinator for the smart rollup node"
        ~level:Notice
        ()
  end
end

let conflict_detected (conflict : Octez_smart_rollup.Game.conflict) =
  let our_commitment_hash =
    Octez_smart_rollup.Commitment.hash conflict.our_commitment
  in
  let their_commitment_hash =
    Octez_smart_rollup.Commitment.hash conflict.their_commitment
  in
  let parent_commitment_hash = conflict.parent_commitment in
  let other = conflict.other in
  let level = conflict.our_commitment.inbox_level in
  Simple.(
    emit
      conflict_detected
      ( our_commitment_hash,
        level,
        other,
        their_commitment_hash,
        parent_commitment_hash ))

let potential_conflict_detected ~our_commitment_hash ~their_commitment_hash
    ~other ~level =
  Simple.(
    emit
      potential_conflict_detected
      (our_commitment_hash, level, other, their_commitment_hash))

let timeout_detected other = Simple.(emit timeout_detected other)

let computed_dissection ~opponent ~start_tick ~end_tick dissection =
  Simple.(emit computed_dissection (opponent, start_tick, end_tick, dissection))

let no_committed_context ~block_hash ~level =
  Simple.(emit no_committed_context) (block_hash, level)

let replaying_blocks ~count ~from_level ~block_hash =
  Simple.(emit replaying_blocks) (count, from_level, block_hash)

let traversing_dissection ~opponent ~dissection_length ~start_tick ~end_tick =
  Simple.(
    emit
      traversing_dissection
      (opponent, dissection_length, start_tick, end_tick))

let dissection_agree ~tick ~state_hash =
  Simple.(emit dissection_agree (tick, state_hash))

let dissection_disagree ~tick ~their_hash ~our_hash =
  Simple.(emit dissection_disagree (tick, their_hash, our_hash))

let computing_dissection ~opponent ~start_tick ~end_tick =
  Simple.(emit computing_dissection (opponent, start_tick, end_tick))

let playing_next_move ~opponent (game : Octez_smart_rollup.Game.t) =
  let game_state, ticks =
    match game.game_state with
    | Dissecting {dissection; _} ->
        ( "dissecting",
          List.map (fun (c : Game.dissection_chunk) -> c.tick) dissection )
    | Final_move {agreed_start_chunk; refuted_stop_chunk} ->
        ("final_move", [agreed_start_chunk.tick; refuted_stop_chunk.tick])
  in
  Simple.(emit playing_next_move (opponent, game_state, ticks))

module Player = struct
  let section = Simple.Player.section

  let request_failed view worker_status errors =
    Simple.(emit Player.request_failed (view, worker_status, errors))

  let request_completed view worker_status =
    Simple.(emit Player.request_completed (view, worker_status))

  let started opponent commitment =
    Simple.(emit Player.started (opponent, commitment))

  let stopped opponent = Simple.(emit Player.stopped opponent)
end

module Coordinator = struct
  let section = Simple.Coordinator.section

  let request_failed view worker_status errors =
    Simple.(emit Coordinator.request_failed (view, worker_status, errors))

  let request_completed view worker_status =
    Simple.(emit Coordinator.request_completed (view, worker_status))

  let starting = Simple.(emit Coordinator.starting)
end
