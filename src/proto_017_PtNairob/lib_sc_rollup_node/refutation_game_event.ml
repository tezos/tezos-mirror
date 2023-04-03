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

  let section = [Protocol.name; "sc_rollup_node"; "refutation_game"]

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

  let potential_conflict_detected =
    declare_4
      ~name:"sc_rollup_node_potential_conflict_detected"
      ~msg:
        "A potential conflict has been found with our commitment \
         {our_commitment_hash} at level {level} with staker {other} that hash \
         issued commitment {their_commitment_hash}."
      ~level:Notice
      ("our_commitment_hash", Sc_rollup.Commitment.Hash.encoding)
      ("level", Raw_level.encoding)
      ("other", Sc_rollup.Staker.encoding)
      ("their_commitment_hash", Sc_rollup.Commitment.Hash.encoding)

  let timeout_detected =
    declare_1
      ~section
      ~name:"sc_rollup_node_timeout_detected"
      ~msg:
        "The rollup node has detected that opponent {other} can be timed out."
      ~level:Notice
      ("other", Sc_rollup.Staker.encoding)
end

let timeout address = Simple.(emit timeout address)

let invalid_move () = Simple.(emit invalid_move ())

let conflict_detected (conflict : Sc_rollup.Refutation_storage.conflict) =
  let our_commitment_hash =
    Sc_rollup.Commitment.hash_uncarbonated conflict.our_commitment
  in
  let their_commitment_hash =
    Sc_rollup.Commitment.hash_uncarbonated conflict.their_commitment
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
