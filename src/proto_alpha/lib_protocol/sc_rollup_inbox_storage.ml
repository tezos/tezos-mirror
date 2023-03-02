(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Store = Storage.Sc_rollup

let get_inbox ctxt =
  let open Lwt_result_syntax in
  let* inbox = Store.Inbox.get ctxt in
  return (inbox, ctxt)

let add_messages ctxt messages =
  let open Lwt_result_syntax in
  let open Raw_context in
  let current_messages = Sc_rollup_in_memory_inbox.current_messages ctxt in
  let*? ctxt =
    List.fold_left_e
      (fun ctxt (message : Sc_rollup_inbox_message_repr.serialized) ->
        let msg_len = String.length (message :> string) in
        (* The average cost of adding a message with a
           [current_index] from [0] to [1_000_000] is reached after [100_000]
           messages.
           If we use the real index, the simulations of [Sc_rollup_add_messages]
           are always performed on an empty skip list.
        *)
        let cost =
          Sc_rollup_costs.cost_add_message
            ~current_index:Z.(of_int 100_000)
            ~msg_len
        in
        Raw_context.consume_gas ctxt cost)
      ctxt
      messages
  in
  (*
      Notice that the protocol is forgetful: it throws away the inbox
      history. On the contrary, the history is stored by the rollup
      node to produce inclusion proofs when needed.
  *)
  let*? current_messages =
    Sc_rollup_inbox_repr.add_messages_no_history messages current_messages
  in
  let ctxt =
    Sc_rollup_in_memory_inbox.set_current_messages ctxt current_messages
  in
  return ctxt

let serialize_external_messages ctxt external_messages =
  let open Sc_rollup_inbox_message_repr in
  List.fold_left_map_e
    (fun ctxt message ->
      let open Result_syntax in
      (* Pay gas for serializing an external message. *)
      let* ctxt =
        let bytes_len = String.length message in
        Raw_context.consume_gas
          ctxt
          (Sc_rollup_costs.cost_serialize_external_inbox_message ~bytes_len)
      in
      let* serialized_message = serialize @@ External message in
      return (ctxt, serialized_message))
    ctxt
    external_messages

let serialize_internal_message ctxt internal_message =
  let open Result_syntax in
  (* Pay gas for serializing an internal message. *)
  let* ctxt =
    Raw_context.consume_gas
      ctxt
      (Sc_rollup_costs.cost_serialize_internal_inbox_message internal_message)
  in
  let* message =
    Sc_rollup_inbox_message_repr.(serialize @@ Internal internal_message)
  in
  return (message, ctxt)

let add_external_messages ctxt external_messages =
  let open Lwt_result_syntax in
  let*? ctxt, messages = serialize_external_messages ctxt external_messages in
  add_messages ctxt messages

let add_internal_message ctxt internal_message =
  let open Lwt_result_syntax in
  let*? message, ctxt = serialize_internal_message ctxt internal_message in
  add_messages ctxt [message]

let add_deposit ctxt ~payload ~sender ~source ~destination =
  let internal_message : Sc_rollup_inbox_message_repr.internal_inbox_message =
    Transfer {destination; payload; sender; source}
  in
  add_internal_message ctxt internal_message

let finalize_inbox_level ctxt =
  let open Lwt_result_syntax in
  let* inbox, ctxt = get_inbox ctxt in
  let witness = Raw_context.Sc_rollup_in_memory_inbox.current_messages ctxt in
  let inbox =
    Sc_rollup_inbox_repr.finalize_inbox_level_no_history inbox witness
  in
  Store.Inbox.update ctxt inbox

let add_protocol_migration ctxt =
  let witness = Raw_context.Sc_rollup_in_memory_inbox.current_messages ctxt in
  let witness =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.add_payload_no_history
      witness
      Raw_context.protocol_migration_serialized_message
  in
  Raw_context.Sc_rollup_in_memory_inbox.set_current_messages ctxt witness

let add_info_per_level ~predecessor ctxt =
  let predecessor_timestamp = Raw_context.predecessor_timestamp ctxt in
  let witness = Raw_context.Sc_rollup_in_memory_inbox.current_messages ctxt in
  let witness =
    Sc_rollup_inbox_repr.add_info_per_level_no_history
      ~predecessor_timestamp
      ~predecessor
      witness
  in
  Raw_context.Sc_rollup_in_memory_inbox.set_current_messages ctxt witness

let init_inbox ~predecessor ctxt =
  let ({level; _} : Level_repr.t) = Raw_context.current_level ctxt in
  let predecessor_timestamp = Raw_context.predecessor_timestamp ctxt in
  let inbox =
    Sc_rollup_inbox_repr.genesis
      ~protocol_migration_message:
        Raw_context.protocol_migration_serialized_message
      ~predecessor_timestamp
      ~predecessor
      level
  in
  Store.Inbox.init ctxt inbox
