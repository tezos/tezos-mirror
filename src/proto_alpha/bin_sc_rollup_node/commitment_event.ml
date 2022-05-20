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

  let event_name ?op_result op_type =
    let op_type_string =
      match op_type with
      | `Cement -> "cement"
      | `Publish -> "publish"
      | `Store -> "stored"
    in
    let op_result_string_opt =
      op_result
      |> Option.map (function
             | `Injected -> "injected"
             | `Backtracked -> "backtracked"
             | `Skipped -> "skipped"
             | `Failed -> "failed")
    in
    match op_result_string_opt with
    | None -> "sc_rollup_node_commitment_" ^ op_type_string
    | Some op_result_string ->
        "sc_rollup_" ^ op_type_string ^ "_commitment_" ^ op_result_string

  let event_msg_prefix ?op_result op_type =
    let op_type_string =
      match op_type with
      | `Cement -> "Cementing"
      | `Publish -> "Publishing"
      | `Store -> "was stored"
    in
    let op_result_string_opt =
      op_result
      |> Option.map (function
             | `Injected -> "was injected"
             | `Backtracked -> "was backtracked"
             | `Skipped -> "was skipped"
             | `Failed -> "has failed")
    in
    match op_result_string_opt with
    | None -> "Commitment " ^ op_type_string
    | Some op_result_string ->
        op_type_string ^ " Commitment " ^ op_result_string

  let commitment_event ?op_result op_type =
    let name = event_name ?op_result op_type in
    let msg =
      event_msg_prefix ?op_result op_type
      ^ " - predecessor: {predecessor}, inbox_level: {inbox_level}, \
         compressed_state: {compressed_state}, number_of_messages: \
         {number_of_messages}, number_of_ticks: {number_of_ticks}"
    in
    declare_5
      ~section
      ~name
      ~msg
      ~level:Notice
      ("predecessor", Sc_rollup.Commitment_hash.encoding)
      ("inbox_level", Raw_level.encoding)
      ("compressed_state", Sc_rollup.State_hash.encoding)
      ("number_of_messages", Sc_rollup.Number_of_messages.encoding)
      ("number_of_ticks", Sc_rollup.Number_of_ticks.encoding)

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

  let commitment_will_not_be_published =
    declare_6
      ~section
      ~name:"sc_rollup_node_commitment_will_not_be_published"
      ~msg:
        "Commitment will not be published: its inbox level is less or equal \
         than the last cemented commitment level {lcc_level} - predecessor: \
         {predecessor}, inbox_level: {inbox_level}, compressed_state: \
         {compressed_state}, number_of_messages: {number_of_messages}, \
         number_of_ticks: {number_of_ticks}"
      ~level:Notice
      ("lcc_level", Raw_level.encoding)
      ("predecessor", Sc_rollup.Commitment_hash.encoding)
      ("inbox_level", Raw_level.encoding)
      ("compressed_state", Sc_rollup.State_hash.encoding)
      ("number_of_messages", Sc_rollup.Number_of_messages.encoding)
      ("number_of_ticks", Sc_rollup.Number_of_ticks.encoding)

  let last_cemented_commitment_updated =
    declare_2
      ~section
      ~name:"sc_rollup_node_lcc_updated"
      ~msg:
        "Last cemented commitment was updated to hash {hash} at inbox level \
         {level}"
      ~level:Notice
      ("hash", Sc_rollup.Commitment_hash.encoding)
      ("level", Raw_level.encoding)

  let compute_commitment =
    declare_2
      ~section
      ~name:"sc_rollup_node_commitment_process_head"
      ~msg:
        "Computing and storing new commitment for head {head} at level {level}"
      ~level:Notice
      ("head", Block_hash.encoding)
      ("level", Raw_level.encoding)

  let commitment_parent_is_not_lcc =
    declare_3
      ~section
      ~name:"sc_rollup_commitment_parent_is_not_lcc"
      ~msg:
        "Trying to publish a commitment at inbox level {level} whose parent is \
         the last cemented commitment, but the commitment's predecessor hash \
         {predecessor_hash} differs from the last cemented commitment hash \
         {lcc_hash}. This is a critical error, and the rollup node will be \
         terminated."
      ~level:Fatal
      ("level", Raw_level.encoding)
      ("predecessor_hash", Sc_rollup.Commitment_hash.encoding)
      ("lcc_hash", Sc_rollup.Commitment_hash.encoding)

  let commitment_stored = commitment_event `Store

  let publish_commitment_injected =
    commitment_event `Publish ~op_result:`Injected

  let publish_commitment_backtracked =
    commitment_event `Publish ~op_result:`Backtracked

  let publish_commitment_skipped = commitment_event `Publish ~op_result:`Skipped

  let publish_commitment_failed = commitment_event `Publish ~op_result:`Failed

  let cement_commitment_injected = commitment_event `Cement ~op_result:`Injected

  let cement_commitment_backtracked =
    commitment_event `Cement ~op_result:`Backtracked

  let cement_commitment_skipped = commitment_event `Cement ~op_result:`Skipped

  let cement_commitment_failed = commitment_event `Cement ~op_result:`Failed
end

let starting = Simple.(emit starting)

let stopping = Simple.(emit stopping)

open Sc_rollup.Commitment

let emit_commitment_event f
    {
      predecessor;
      inbox_level;
      compressed_state;
      number_of_messages;
      number_of_ticks;
    } =
  Simple.(
    emit
      f
      ( predecessor,
        inbox_level,
        compressed_state,
        number_of_messages,
        number_of_ticks ))

let commitment_will_not_be_published lcc_level
    {
      predecessor;
      inbox_level;
      compressed_state;
      number_of_messages;
      number_of_ticks;
    } =
  Simple.(
    emit
      commitment_will_not_be_published
      ( lcc_level,
        predecessor,
        inbox_level,
        compressed_state,
        number_of_messages,
        number_of_ticks ))

let commitment_stored = emit_commitment_event Simple.commitment_stored

let publish_commitment_injected =
  emit_commitment_event Simple.publish_commitment_injected

let publish_commitment_skipped =
  emit_commitment_event Simple.publish_commitment_skipped

let publish_commitment_backtracked =
  emit_commitment_event Simple.publish_commitment_backtracked

let publish_commitment_failed =
  emit_commitment_event Simple.publish_commitment_failed

let cement_commitment_injected =
  emit_commitment_event Simple.cement_commitment_injected

let cement_commitment_skipped =
  emit_commitment_event Simple.cement_commitment_skipped

let cement_commitment_backtracked =
  emit_commitment_event Simple.cement_commitment_backtracked

let cement_commitment_failed =
  emit_commitment_event Simple.cement_commitment_failed

let last_cemented_commitment_updated head level =
  Simple.(emit last_cemented_commitment_updated (head, level))

let compute_commitment head level =
  Simple.(emit compute_commitment (head, level))

let commitment_parent_is_not_lcc level predecessor_hash lcc_hash =
  Simple.(emit commitment_parent_is_not_lcc (level, predecessor_hash, lcc_hash))
