(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

module Lwt_result_option_syntax = struct
  let ( let** ) a f =
    let open Lwt_result_syntax in
    let* a in
    match a with None -> return_none | Some a -> f a

  let ( let*!* ) a f =
    let open Lwt_result_syntax in
    let*! a in
    Option.map f a |> Option.value ~default:return_none

  let ( let*:* ) a f =
    let open Lwt_result_syntax in
    Option.map f a |> Option.value ~default:return_none

  let fail = Lwt_result_syntax.return_none
end

let executable_whitelist_update_message (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  let open Lwt_result_option_syntax in
  let lcc = Reference.get node_ctxt.lcc in
  (* if rollup is private this will return None and no further
     computation will be done. *)
  let*:* Node_context.{last_outbox_level_searched; last_whitelist_update} =
    Reference.get node_ctxt.private_info
  in
  let current_protocol = Reference.get node_ctxt.current_protocol in
  let commitment_period =
    Int32.of_int
      current_protocol.constants.sc_rollup.commitment_period_in_blocks
  in
  let max_cemented_commitment =
    current_protocol.constants.sc_rollup
      .max_number_of_stored_cemented_commitments
  in
  let rec fold_over_outbox_level_in_commmitment ~cemented_commitment_hash
      ~bottom_outbox_level ~outbox_level state =
    if outbox_level < bottom_outbox_level then fail
    else
      let* (module Plugin) =
        Protocol_plugins.proto_plugin_for_level node_ctxt outbox_level
      in
      let*! message_index =
        Plugin.Pvm.find_whitelist_update_output_index
          node_ctxt
          state
          ~outbox_level
      in
      match message_index with
      | None ->
          (* No whitelist update found for the outbox level, continue
             folding with predecessor outbox level.*)
          fold_over_outbox_level_in_commmitment
            ~cemented_commitment_hash
            ~bottom_outbox_level
            ~outbox_level:(Int32.pred outbox_level)
            state
      | Some message_index
        when outbox_level = last_whitelist_update.outbox_level
             && message_index <= last_whitelist_update.message_index ->
          (* the found (outbox_level, index) is older than the last
             whitelist update. *)
          return_none
      | Some message_index ->
          let*! () =
            Commitment_event.publish_execute_whitelist_update
              cemented_commitment_hash
              outbox_level
              message_index
          in
          let* proof =
            Plugin.Pvm.produce_serialized_output_proof
              node_ctxt
              state
              ~outbox_level
              ~message_index
          in
          return_some (cemented_commitment_hash, proof)
  in
  let rec fold_over_commitment i (cemented_commitment_hash : Commitment.Hash.t)
      =
    if i <= 0 then fail
    else
      let** commitment =
        Node_context.find_commitment node_ctxt cemented_commitment_hash
      in
      let inbox_level = commitment.inbox_level in
      if inbox_level < last_whitelist_update.outbox_level then
        (* last whitelist update is more recent.*)
        fail
      else if inbox_level < last_outbox_level_searched then
        (* level already explored in a previous run. *)
        fail
      else
        let* block_hash = Node_context.hash_of_level node_ctxt inbox_level in
        let* ctxt = Node_context.checkout_context node_ctxt block_hash in
        let*!* state = Context.PVMState.find ctxt in
        let bottom_outbox_level =
          Int32.(
            max
              last_whitelist_update.outbox_level
              (sub inbox_level commitment_period))
        in
        let* found_message =
          fold_over_outbox_level_in_commmitment
            ~cemented_commitment_hash
            ~bottom_outbox_level
            ~outbox_level:inbox_level
            state
        in
        match found_message with
        | Some commitment_and_proof -> return_some commitment_and_proof
        | None -> fold_over_commitment (i - 1) commitment.predecessor
  in
  fold_over_commitment max_cemented_commitment lcc.commitment

let execute_whitelist_update_message_aux (node_ctxt : _ Node_context.t)
    (cemented_commitment, output_proof) =
  let open Lwt_result_syntax in
  let outbox_message =
    L1_operation.Execute_outbox_message
      {
        rollup = node_ctxt.config.sc_rollup_address;
        cemented_commitment;
        output_proof;
      }
  in
  let* _hash = Injector.add_pending_operation outbox_message in
  return_unit

let publish_execute_whitelist_update_message (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  let operator = Node_context.get_operator node_ctxt Executing_outbox in
  unless (operator = None) @@ fun () ->
  (* Configured to execute whitelist update commitments *)
  let* cemented_commitment_and_proof =
    executable_whitelist_update_message node_ctxt
  in
  let () =
    Reference.map
      (Option.map (fun private_info ->
           let ({level; _} : Node_context.lcc) = Reference.get node_ctxt.lcc in
           Node_context.{private_info with last_outbox_level_searched = level}))
      node_ctxt.private_info
  in
  match cemented_commitment_and_proof with
  | Some cemented_commitment_and_proof ->
      execute_whitelist_update_message_aux
        node_ctxt
        cemented_commitment_and_proof
  | None -> return_unit

let outbox_message_destination_match_filter
    (dest_filter : Configuration.outbox_destination_filter) dest =
  match dest_filter with
  | Any_destination -> true
  | Destination_among allowed_dests ->
      List.mem ~equal:String.equal dest allowed_dests

let outbox_message_entrypoint_match_filter
    (entrypoint_filter : Configuration.outbox_entrypoint_filter) entrypoint =
  match entrypoint_filter with
  | Any_entrypoint -> true
  | Entrypoint_among allowed_entrypoints ->
      List.mem ~equal:String.equal entrypoint allowed_entrypoints

let outbox_message_transaction_match_filter destination_filter entrypoint_filter
    (transaction : Outbox_message.transaction_summary) =
  outbox_message_destination_match_filter
    destination_filter
    transaction.destination
  && outbox_message_entrypoint_match_filter
       entrypoint_filter
       transaction.entrypoint

let outbox_message_match_filter (message : Outbox_message.summary)
    (filter : Configuration.outbox_message_filter) =
  match (filter, message) with
  | ( Transaction {destination = Any_destination; entrypoint = Any_entrypoint},
      Transaction_batch _ ) ->
      true
  | Transaction {destination; entrypoint}, Transaction_batch txs ->
      List.for_all
        (outbox_message_transaction_match_filter destination entrypoint)
        txs
  | _ -> false

let publish_executable_messages (node_ctxt : _ Node_context.rw_context) =
  let open Lwt_result_syntax in
  let operator = Node_context.get_operator node_ctxt Executing_outbox in
  unless (operator = None) @@ fun () ->
  (* Configured to execute outbox messages *)
  let lcc = Reference.get node_ctxt.lcc in
  let* lcc_block_hash = Node_context.hash_of_level node_ctxt lcc.level in
  let* ctxt = Node_context.checkout_context node_ctxt lcc_block_hash in
  let*! state = Context.PVMState.find ctxt in
  match state with
  | None -> failwith "No PVM state at LCC to execute outbox messages"
  | Some state ->
      let* executable_messages =
        Node_context.get_executable_pending_outbox_messages node_ctxt
      in
      let*? (module Plugin) =
        Protocol_plugins.proto_plugin_for_protocol
          (Reference.get node_ctxt.current_protocol).hash
      in
      let*! () =
        let open Lwt_syntax in
        List.iter_s
          (fun (outbox_level, message_indexes) ->
            let* outbox =
              Plugin.Pvm.get_outbox_messages node_ctxt state ~outbox_level
            in
            List.iter_s
              (fun message_index ->
                let message =
                  List.assoc ~equal:Int.equal message_index outbox
                in
                match message with
                | None -> assert false
                | Some (Whitelist_update _) ->
                    (* Don't execute whitelist updates as this is done by
                       {!publish_execute_whitelist_update_message}. *)
                    return_unit
                | Some message
                  when not
                         (List.exists
                            (outbox_message_match_filter message)
                            node_ctxt.config.execute_outbox_messages_filter) ->
                    (* Message does not match filter of config: ignore. *)
                    return_unit
                | Some message -> (
                    let*! () =
                      Commitment_event.execute_outbox_message
                        ~outbox_level
                        ~message_index
                        message
                    in
                    let* res =
                      let open Lwt_result_syntax in
                      let* output_proof =
                        Plugin.Pvm.produce_serialized_output_proof
                          node_ctxt
                          state
                          ~outbox_level
                          ~message_index
                      in
                      let outbox_message =
                        L1_operation.Execute_outbox_message
                          {
                            rollup = node_ctxt.config.sc_rollup_address;
                            cemented_commitment = lcc.commitment;
                            output_proof;
                          }
                      in
                      let* _hash =
                        Injector.add_pending_operation outbox_message
                      in
                      return_unit
                    in
                    match res with
                    | Error trace ->
                        let*! () =
                          Commitment_event.outbox_message_execution_failed
                            ~outbox_level
                            ~message_index
                            message
                            trace
                        in
                        return_unit
                    | Ok () -> return_unit))
              message_indexes)
          executable_messages
      in
      return_unit
