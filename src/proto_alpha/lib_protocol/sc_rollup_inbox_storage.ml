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

open Sc_rollup_errors
module Store = Storage.Sc_rollup

let get_inbox ctxt =
  let open Lwt_result_syntax in
  let* inbox = Store.Inbox.get ctxt in
  return (inbox, ctxt)

let _assert_inbox_nb_messages_in_commitment_period ctxt inbox extra_messages =
  let nb_messages_in_commitment_period =
    Int64.add
      (Sc_rollup_inbox_repr.number_of_messages_during_commitment_period inbox)
      (Int64.of_int extra_messages)
  in
  let limit =
    Constants_storage.sc_rollup_max_number_of_messages_per_commitment_period
      ctxt
    |> Int64.of_int
  in
  fail_when
    Compare.Int64.(nb_messages_in_commitment_period > limit)
    Sc_rollup_max_number_of_messages_reached_for_commitment_period

let add_messages ctxt messages =
  let open Lwt_result_syntax in
  let open Raw_context in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3978
     In the current setup, we can stop the chain when hitting the
     limit. *)
  (* let* () = *)
  (*   assert_inbox_nb_messages_in_commitment_period ctxt inbox num_messages *)
  (* in *)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3292

     The carbonation needs to be activated again with the new internal inbox's
     design, i.e. the skip list.
  *)
  let current_messages = Sc_rollup_in_memory_inbox.current_messages ctxt in
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
  let*? inbox =
    Sc_rollup_inbox_repr.finalize_inbox_level_no_history inbox witness
  in
  let* ctxt = Store.Inbox.update ctxt inbox in
  return ctxt

let add_info_per_level ~timestamp ~predecessor ctxt =
  let open Lwt_result_syntax in
  let witness = Raw_context.Sc_rollup_in_memory_inbox.current_messages ctxt in
  let*? witness =
    Sc_rollup_inbox_repr.add_info_per_level_no_history
      ~timestamp
      ~predecessor
      witness
  in
  let ctxt =
    Raw_context.Sc_rollup_in_memory_inbox.set_current_messages ctxt witness
  in
  return ctxt

let init_inbox ~timestamp ~predecessor ctxt =
  let open Lwt_result_syntax in
  let ({level; _} : Level_repr.t) = Raw_context.current_level ctxt in
  let*? inbox = Sc_rollup_inbox_repr.genesis ~timestamp ~predecessor level in
  let* ctxt = Store.Inbox.init ctxt inbox in
  return ctxt

module Internal_for_tests = struct
  let add_start_of_level ctxt =
    add_internal_message ctxt Sc_rollup_inbox_message_repr.Start_of_level

  let add_end_of_level ctxt =
    add_internal_message ctxt Sc_rollup_inbox_message_repr.End_of_level

  let add_info_per_level ctxt timestamp predecessor =
    add_internal_message
      ctxt
      (Sc_rollup_inbox_message_repr.Info_per_level {timestamp; predecessor})
end
