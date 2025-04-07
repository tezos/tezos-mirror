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

(* module Constants will be shadowed by Alpha_context.Constansts
   once we open Alpha_context, hence we we alias it to Rollup_node_constants
*)
open Protocol
open Alpha_context

let lift promise = Lwt.map Environment.wrap_tzresult promise

let get_messages Node_context.{l1_ctxt; _} head =
  let open Lwt_result_syntax in
  let* block = Layer1_helpers.fetch_tezos_block l1_ctxt head in
  let apply (type kind) accu ~source:_ (operation : kind manager_operation)
      _result =
    let open Result_syntax in
    let+ accu in
    match operation with
    | Sc_rollup_add_messages {messages} ->
        let messages =
          List.map
            (fun message -> Sc_rollup.Inbox_message.External message)
            messages
        in
        List.rev_append messages accu
    | _ -> accu
  in
  let apply_internal (type kind) accu ~source
      (operation : kind Apply_internal_results.internal_operation)
      (result :
        kind Apply_internal_results.successful_internal_operation_result) =
    let open Result_syntax in
    let* accu in
    match (operation, result) with
    | ( {
          operation = Transaction {destination = Sc_rollup rollup; parameters; _};
          sender = Contract (Originated sender);
          _;
        },
        ITransaction_result (Transaction_to_sc_rollup_result _) ) ->
        let+ payload =
          Environment.wrap_tzresult @@ Script_repr.force_decode parameters
        in
        let message =
          Sc_rollup.Inbox_message.Transfer
            {destination = rollup; payload; sender; source}
        in
        Sc_rollup.Inbox_message.Internal message :: accu
    | _ -> return accu
  in
  let*? rev_messages =
    Layer1_services.(
      process_applied_manager_operations
        (Ok [])
        block.operations
        {apply; apply_internal})
  in
  let*? messages =
    Environment.wrap_tzresult
    @@ List.rev_map_e
         (fun msg ->
           let open Result_syntax in
           let+ msg = Sc_rollup.Inbox_message.serialize msg in
           Sc_rollup.Inbox_message.unsafe_to_string msg)
         rev_messages
  in
  return messages

let same_as_layer_1 node_ctxt head_hash inbox =
  let open Lwt_result_syntax in
  let head_block = `Hash (head_hash, 0) in
  let Node_context.{cctxt; _} = node_ctxt in
  let cctxt = new Protocol_client_context.wrap_full cctxt in
  let* layer1_inbox =
    Plugin.RPC.Sc_rollup.inbox cctxt (cctxt#chain, head_block)
  in
  let layer1_inbox = Sc_rollup_proto_types.Inbox.to_octez layer1_inbox in
  fail_unless
    (Octez_smart_rollup.Inbox.equal layer1_inbox inbox)
    (Sc_rollup_node_errors.Inconsistent_inbox {layer1_inbox; inbox})

let add_messages ~is_first_block ~predecessor_timestamp ~predecessor inbox
    messages =
  let open Lwt_result_syntax in
  let no_history = Sc_rollup.Inbox.History.empty ~capacity:0L in
  lift
  @@ let*? ( messages_history,
             _no_history,
             inbox,
             witness,
             messages_with_protocol_internal_messages ) =
       Sc_rollup.Inbox.add_all_messages
         ~first_block:is_first_block
         ~predecessor_timestamp
         ~predecessor
         no_history
         inbox
         messages
     in
     let witness_hash =
       Sc_rollup.Inbox_merkelized_payload_hashes.hash witness
     in
     return
       ( messages_history,
         witness_hash,
         inbox,
         messages_with_protocol_internal_messages )

let process_messages (node_ctxt : _ Node_context.t) ~is_first_block
    ~(predecessor : Layer1.header) messages =
  let open Lwt_result_syntax in
  let* inbox =
    Node_context.inbox_of_head node_ctxt (Layer1.head_of_header predecessor)
  in
  let predecessor_timestamp = predecessor.header.timestamp in
  let inbox = Sc_rollup_proto_types.Inbox.of_octez inbox in
  let*? messages =
    Environment.wrap_tzresult
    @@ List.map_e
         (fun msg ->
           Sc_rollup.Inbox_message.(deserialize @@ unsafe_of_string msg))
         messages
  in
  let* ( _messages_history,
         witness_hash,
         inbox,
         messages_with_protocol_internal_messages ) =
    add_messages
      ~is_first_block
      ~predecessor_timestamp
      ~predecessor:predecessor.hash
      inbox
      messages
  in
  let*? messages_with_protocol_internal_messages =
    Environment.wrap_tzresult
    @@ List.map_e
         (fun msg ->
           let open Result_syntax in
           let+ msg = Sc_rollup.Inbox_message.serialize msg in
           Sc_rollup.Inbox_message.unsafe_to_string msg)
         messages_with_protocol_internal_messages
  in
  let* () =
    Node_context.save_messages
      node_ctxt
      witness_hash
      ~level:(Int32.succ predecessor.level)
      messages_with_protocol_internal_messages
  in
  let inbox = Sc_rollup_proto_types.Inbox.to_octez inbox in
  let* inbox_hash = Node_context.save_inbox node_ctxt inbox in
  let witness_hash =
    Sc_rollup_proto_types.Merkelized_payload_hashes_hash.to_octez witness_hash
  in
  return
    (inbox_hash, inbox, witness_hash, messages_with_protocol_internal_messages)

let process_head (node_ctxt : _ Node_context.t) ~(predecessor : Layer1.header)
    (head : Layer1.header) =
  let open Lwt_result_syntax in
  let first_inbox_level = node_ctxt.genesis_info.level |> Int32.succ in
  if head.level >= first_inbox_level then
    (* We compute the inbox of this block using the inbox of its
       predecessor. That way, the computation of inboxes is robust to chain
       reorganization. *)
    let* collected_messages = get_messages node_ctxt head.hash in
    let*! () =
      Inbox_event.get_messages
        head.hash
        head.level
        (List.length collected_messages)
    in
    let* head_proto = Node_context.protocol_of_level node_ctxt head.level in
    let is_first_block = head_proto.first_level_of_protocol in
    process_messages node_ctxt ~is_first_block ~predecessor collected_messages
  else
    let* inbox =
      Layer1_helpers.genesis_inbox
        node_ctxt.cctxt
        ~genesis_level:node_ctxt.genesis_info.level
    in
    let Octez_smart_rollup.Inbox.{hash = witness; _} =
      Octez_smart_rollup.Inbox.Skip_list.content inbox.old_levels_messages
    in
    let* () =
      Node_context.save_messages node_ctxt witness ~level:head.level []
    in
    let* inbox_hash = Node_context.save_inbox node_ctxt inbox in
    return (inbox_hash, inbox, witness, [])

let payloads_history_of_messages ~is_first_block ~predecessor
    ~predecessor_timestamp messages =
  let open Result_syntax in
  let dummy_inbox =
    (* The inbox is not necessary to compute the payloads *)
    Sc_rollup.Inbox.genesis ~predecessor_timestamp ~predecessor Raw_level.root
  in
  let* messages =
    Environment.wrap_tzresult
    @@ List.map_e
         (fun msg ->
           Sc_rollup.Inbox_message.(deserialize @@ unsafe_of_string msg))
         messages
  in
  let+ ( payloads_history,
         _history,
         _inbox,
         _witness,
         _messages_with_protocol_internal_messages ) =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4918 Inject
       [Protocol_migration (Proto_017)] when migrating to proto_alpha
       (N after next snapshot). *)
    Environment.wrap_tzresult
    @@ Sc_rollup.Inbox.add_all_messages
         ~first_block:is_first_block
         ~predecessor_timestamp
         ~predecessor
         (Sc_rollup.Inbox.History.empty ~capacity:0L)
         dummy_inbox
         messages
  in
  payloads_history

let payloads_history_of_all_messages messages =
  let open Result_syntax in
  let payloads_history =
    let capacity = List.length messages |> Int64.of_int in
    Sc_rollup.Inbox_merkelized_payload_hashes.History.empty ~capacity
  in
  match List.map Sc_rollup.Inbox_message.unsafe_of_string messages with
  | [] -> assert false
  | first :: messages ->
      Environment.wrap_tzresult
      @@ let* payloads_history, witness =
           Sc_rollup.Inbox_merkelized_payload_hashes.genesis
             payloads_history
             first
         in
         let* payloads_history, _witness =
           List.fold_left_e
             (fun (payloads_history, witness) ->
               Sc_rollup.Inbox_merkelized_payload_hashes.add_payload
                 payloads_history
                 witness)
             (payloads_history, witness)
             messages
         in
         return payloads_history

let serialize_external_message msg =
  Environment.wrap_tzresult
  @@
  let open Result_syntax in
  let open Sc_rollup.Inbox_message in
  let+ msg = serialize @@ External msg in
  unsafe_to_string msg

let init ~predecessor_timestamp ~predecessor ~level =
  Sc_rollup.Inbox.genesis
    ~predecessor_timestamp
    ~predecessor
    (Raw_level.of_int32_exn level)
  |> Sc_rollup_proto_types.Inbox.to_octez

module Internal_for_tests = struct
  let process_messages = process_messages
end
