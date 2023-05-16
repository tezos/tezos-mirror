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
  let* block = Layer1.fetch_tezos_block l1_ctxt head in
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
          source = Contract (Originated sender);
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
  return (List.rev rev_messages)

let same_as_layer_1 node_ctxt head_hash inbox =
  let open Lwt_result_syntax in
  let head_block = `Hash (head_hash, 0) in
  let Node_context.{cctxt; _} = node_ctxt in
  let* layer1_inbox =
    Plugin.RPC.Sc_rollup.inbox cctxt (cctxt#chain, head_block)
  in
  fail_unless
    (Sc_rollup.Inbox.equal layer1_inbox inbox)
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
    ~(predecessor : Layer1.header) (head : Layer1.header) messages =
  let open Lwt_result_syntax in
  let level = head.level in
  let* inbox =
    Node_context.inbox_of_head node_ctxt (Layer1.head_of_header predecessor)
  in
  let predecessor_timestamp = predecessor.header.timestamp in
  let inbox_metrics = Metrics.Inbox.metrics in
  Prometheus.Gauge.set inbox_metrics.head_inbox_level @@ Int32.to_float level ;
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
  Metrics.Inbox.Stats.set
    messages_with_protocol_internal_messages
    ~is_internal:(function
      | Sc_rollup.Inbox_message.Internal _ -> true
      | External _ -> false) ;
  let* () =
    Node_context.save_messages
      node_ctxt
      witness_hash
      ~block_hash:head.hash
      messages
  in
  let* inbox_hash = Node_context.save_inbox node_ctxt inbox in
  return
    (inbox_hash, inbox, witness_hash, messages_with_protocol_internal_messages)

let process_head (node_ctxt : _ Node_context.t) ~(predecessor : Layer1.header)
    (head : Layer1.header) =
  let open Lwt_result_syntax in
  let first_inbox_level =
    Raw_level.to_int32 node_ctxt.genesis_info.level |> Int32.succ
  in
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
    let* grandparent =
      Node_context.get_predecessor_header node_ctxt predecessor
    in
    let is_first_block =
      (* head is the first block of the protocol if its predecessor is a
         migration block. *)
      grandparent.header.proto_level <> predecessor.header.proto_level
    in
    process_messages
      node_ctxt
      ~is_first_block
      ~predecessor
      head
      collected_messages
  else
    let* inbox = Node_context.genesis_inbox node_ctxt in
    return
      ( Sc_rollup.Inbox.hash inbox,
        inbox,
        Sc_rollup.Inbox.current_witness inbox,
        [] )

let start () = Inbox_event.starting ()

let payloads_history_of_messages ~is_first_block ~predecessor
    ~predecessor_timestamp messages =
  let open Result_syntax in
  let dummy_inbox =
    (* The inbox is not necessary to compute the payloads *)
    Sc_rollup.Inbox.genesis ~predecessor_timestamp ~predecessor Raw_level.root
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

module Internal_for_tests = struct
  let process_messages = process_messages
end
