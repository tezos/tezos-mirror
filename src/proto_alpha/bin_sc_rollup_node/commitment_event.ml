(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2880 
   Add corresponding .mli file. *)

open Protocol
open Alpha_context

module Simple = struct
  include Internal_event.Simple

  let section = ["sc_rollup_node"; "commitment"]

  let starting =
    declare_0
      ~section
      ~name:"sc_rollup_commitment_publisher_starting"
      ~msg:"Starting commitment publisher for the smart contract rollup node"
      ~level:Notice
      ()

  let stopping =
    declare_0
      ~section
      ~name:"sc_rollup_node_commitment_publisher_stopping"
      ~msg:"Stopping commitment publisher for the smart contract rollup node"
      ~level:Notice
      ()

  let commitment_stored =
    declare_5
      ~section
      ~name:"sc_rollup_node_commitment_stored"
      ~msg:
        "Commitment was stored - predecessor: {predecessor}, inbox_level: \
         {inbox_level}, compressed_state: {compressed_state}, \
         number_of_messages: {number_of_messages}, number_of_ticks: \
         {number_of_ticks}"
      ~level:Notice
      ("predecessor", Sc_rollup.Commitment_hash.encoding)
      ("inbox_level", Raw_level.encoding)
      ("compressed_state", Sc_rollup.State_hash.encoding)
      ("number_of_messages", Sc_rollup.Number_of_messages.encoding)
      ("number_of_ticks", Sc_rollup.Number_of_ticks.encoding)

  let commitment_published =
    declare_5
      ~section
      ~name:"sc_rollup_node_commitment_published"
      ~msg:
        "Commitment was published - predecessor: {predecessor}, inbox_level: \
         {inbox_level}, compressed_state: {compressed_state}, \
         number_of_messages: {number_of_messages}, number_of_ticks: \
         {number_of_ticks}"
      ~level:Notice
      ("predecessor", Sc_rollup.Commitment_hash.encoding)
      ("inbox_level", Raw_level.encoding)
      ("compressed_state", Sc_rollup.State_hash.encoding)
      ("number_of_messages", Sc_rollup.Number_of_messages.encoding)
      ("number_of_ticks", Sc_rollup.Number_of_ticks.encoding)

  let commitment_skipped =
    declare_5
      ~section
      ~name:"sc_rollup_node_commitment_skipped"
      ~msg:
        "Publishing commitment was skipped - predecessor: {predecessor}, \
         inbox_level: {inbox_level}, compressed_state: {compressed_state}, \
         number_of_messages: {number_of_messages}, number_of_ticks: \
         {number_of_ticks}"
      ~level:Notice
      ("predecessor", Sc_rollup.Commitment_hash.encoding)
      ("inbox_level", Raw_level.encoding)
      ("compressed_state", Sc_rollup.State_hash.encoding)
      ("number_of_messages", Sc_rollup.Number_of_messages.encoding)
      ("number_of_ticks", Sc_rollup.Number_of_ticks.encoding)

  let commitment_backtracked =
    declare_5
      ~section
      ~name:"sc_rollup_node_commitment_backtracked"
      ~msg:
        "Publishing commitment was backtracked - predecessor: {predecessor}, \
         inbox_level: {inbox_level}, compressed_state: {compressed_state}, \
         number_of_messages: {number_of_messages}, number_of_ticks: \
         {number_of_ticks}"
      ~level:Notice
      ("predecessor", Sc_rollup.Commitment_hash.encoding)
      ("inbox_level", Raw_level.encoding)
      ("compressed_state", Sc_rollup.State_hash.encoding)
      ("number_of_messages", Sc_rollup.Number_of_messages.encoding)
      ("number_of_ticks", Sc_rollup.Number_of_ticks.encoding)

  let commitment_failed =
    declare_5
      ~section
      ~name:"sc_rollup_node_commitment_failed"
      ~msg:
        "Publishing commitment has failed - predecessor: {predecessor}, \
         inbox_level: {inbox_level}, compressed_state: {compressed_state}, \
         number_of_messages: {number_of_messages}, number_of_ticks: \
         {number_of_ticks}"
      ~level:Notice
      ("predecessor", Sc_rollup.Commitment_hash.encoding)
      ("inbox_level", Raw_level.encoding)
      ("compressed_state", Sc_rollup.State_hash.encoding)
      ("number_of_messages", Sc_rollup.Number_of_messages.encoding)
      ("number_of_ticks", Sc_rollup.Number_of_ticks.encoding)

  let compute_commitment =
    declare_2
      ~section
      ~name:"sc_rollup_node_commitment_process_head"
      ~msg:
        "Computing and storing new commitment for head {head} at level {level}"
      ~level:Notice
      ("head", Block_hash.encoding)
      ("level", Raw_level.encoding)
end

let starting = Simple.(emit starting)

let stopping = Simple.(emit stopping)

open Sc_rollup.Commitment

let commitment_stored
    {
      predecessor;
      inbox_level;
      compressed_state;
      number_of_messages;
      number_of_ticks;
    } =
  Simple.(
    emit
      commitment_stored
      ( predecessor,
        inbox_level,
        compressed_state,
        number_of_messages,
        number_of_ticks ))

let commitment_published
    {
      predecessor;
      inbox_level;
      compressed_state;
      number_of_messages;
      number_of_ticks;
    } =
  Simple.(
    emit
      commitment_published
      ( predecessor,
        inbox_level,
        compressed_state,
        number_of_messages,
        number_of_ticks ))

let commitment_skipped
    {
      predecessor;
      inbox_level;
      compressed_state;
      number_of_messages;
      number_of_ticks;
    } =
  Simple.(
    emit
      commitment_skipped
      ( predecessor,
        inbox_level,
        compressed_state,
        number_of_messages,
        number_of_ticks ))

let commitment_backtracked
    {
      predecessor;
      inbox_level;
      compressed_state;
      number_of_messages;
      number_of_ticks;
    } =
  Simple.(
    emit
      commitment_backtracked
      ( predecessor,
        inbox_level,
        compressed_state,
        number_of_messages,
        number_of_ticks ))

let commitment_failed
    {
      predecessor;
      inbox_level;
      compressed_state;
      number_of_messages;
      number_of_ticks;
    } =
  Simple.(
    emit
      commitment_failed
      ( predecessor,
        inbox_level,
        compressed_state,
        number_of_messages,
        number_of_ticks ))

let compute_commitment head level =
  Simple.(emit compute_commitment (head, level))
