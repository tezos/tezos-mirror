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

open Protocol.Alpha_context

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2880
   Add corresponding .mli file. *)

module Simple = struct
  include Internal_event.Simple

  let section = ["sc_rollup_node"; "refutation_game"]

  let timeout =
    declare_1
      ~section
      ~name:"sc_rollup_node_timeout"
      ~msg:
        "The rollup node has been slashed because of a timeout issued by \
         {address}"
      ~level:Notice
      ("address", Signature.Public_key_hash.encoding)

  let invalid_move =
    declare_0
      ~section
      ~name:"sc_rollup_node_invalid_move"
      ~msg:
        "The rollup node is about to make an invalid move in the refutation \
         game! It is stopped to avoid being slashed. The problem should be \
         reported immediately or the rollup node should be upgraded to have a \
         chance to be back before the timeout is reached."
      ~level:Notice
      ()

  let conflict_detected =
    declare_5
      ~name:"sc_rollup_node_conflict_detected"
      ~msg:
        "A conflict has been found with our commitment {our_commitment_hash} \
         at level {level} with staker {other} that hash issued commitment \
         {their_commitment_hash} both based on {parent_commitment_hash}."
      ~level:Notice
      ("our_commitment_hash", Sc_rollup.Commitment.Hash.encoding)
      ("level", Raw_level.encoding)
      ("other", Sc_rollup.Staker.encoding)
      ("their_commitment_hash", Sc_rollup.Commitment.Hash.encoding)
      ("parent_commitment_hash", Sc_rollup.Commitment.Hash.encoding)

  let refutation_event state =
    declare_2
      ~section
      ~name:("sc_rollup_node_refutation_" ^ state)
      ~msg:
        ("Refutation was " ^ state
       ^ " - opponent: {opponent}, refutation: {refutation}")
      ~level:Notice
      ("opponent", Sc_rollup.Staker.encoding)
      ("refutation", Data_encoding.option Sc_rollup.Game.refutation_encoding)

  let refutation_published = refutation_event "published"

  let refutation_failed = refutation_event "failed"

  let refutation_backtracked = refutation_event "backtracked"

  let refutation_skipped = refutation_event "skipped"

  let timeout_event state =
    declare_1
      ~section
      ~name:("sc_rollup_node_timeout_" ^ state)
      ~msg:("Timeout was " ^ state ^ " - players: {players}")
      ~level:Notice
      ("players", Sc_rollup.Game.Index.encoding)

  let timeout_published = timeout_event "published"

  let timeout_failed = timeout_event "failed"

  let timeout_backtracked = timeout_event "backtracked"

  let timeout_skipped = timeout_event "skipped"
end

let timeout address = Simple.(emit timeout address)

let invalid_move () = Simple.(emit invalid_move ())

let refutation_published opponent refutation =
  Simple.(emit refutation_published (opponent, refutation))

let refutation_failed opponent refutation =
  Simple.(emit refutation_failed (opponent, refutation))

let refutation_backtracked opponent refutation =
  Simple.(emit refutation_backtracked (opponent, refutation))

let refutation_skipped opponent refutation =
  Simple.(emit refutation_skipped (opponent, refutation))

let timeout_published players = Simple.(emit timeout_published players)

let timeout_failed players = Simple.(emit timeout_failed players)

let timeout_backtracked players = Simple.(emit timeout_backtracked players)

let timeout_skipped players = Simple.(emit timeout_skipped players)

let conflict_detected (conflict : Sc_rollup.Refutation_storage.conflict) =
  let our_commitment_hash = Sc_rollup.Commitment.hash conflict.our_commitment in
  let their_commitment_hash =
    Sc_rollup.Commitment.hash conflict.their_commitment
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
