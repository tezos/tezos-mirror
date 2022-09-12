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

let lift = Lwt.map Environment.wrap_tzresult

let head_processing_failure e =
  Format.eprintf
    "Error during head processing: @[%a@]"
    Error_monad.(TzTrace.pp_print_top pp)
    e ;
  Lwt_exit.exit_and_raise 1

module State = struct
  let add_messages = Store.Messages.add

  let add_inbox = Store.Inboxes.add

  let add_history = Store.Histories.add

  (** [inbox_of_hash node_ctxt store block_hash] returns the latest
      inbox at the given [block_hash]. This function always returns
      [Some inbox] for all levels after the rollup genesis even when
      no messages has been issued at this specific [block_hash]. In
      this case, the inbox is the same as the one found in the level
      when the latest message has been inserted. *)
  let inbox_of_hash node_ctxt block_hash =
    let open Lwt_result_syntax in
    let open Node_context in
    let*! possible_inbox = Store.Inboxes.find node_ctxt.store block_hash in
    match possible_inbox with
    | None ->
        (* We won't find inboxes for blocks before the rollup origination level.
           Fortunately this case will only ever be called once when dealing with
           the rollup origination block. After that we would always find an
           inbox. *)
        let*! block_level = Layer1.level_of_hash node_ctxt.store block_hash in
        let block_level = Raw_level.of_int32_exn block_level in
        if Raw_level.(block_level <= node_ctxt.genesis_info.level) then
          let*! inbox =
            Context.Inbox.empty
              node_ctxt.context
              node_ctxt.rollup_address
              node_ctxt.genesis_info.level
          in
          return inbox
        else
          failwith
            "The inbox for block hash %a (level = %a) is missing."
            Block_hash.pp
            block_hash
            Raw_level.pp
            block_level
    | Some inbox -> return inbox

  let history_of_hash node_ctxt block_hash =
    let open Lwt_result_syntax in
    let open Node_context in
    let*! res = Store.Histories.find node_ctxt.store block_hash in
    match res with
    | Some history -> return history
    | None ->
        (* We won't find inboxes for blocks before the rollup origination level.
           Fortunately this case will only ever be called once when dealing with
           the rollup origination block. After that we would always find an
           inbox. *)
        let*! block_level = Layer1.level_of_hash node_ctxt.store block_hash in
        let block_level = Raw_level.of_int32_exn block_level in
        if Raw_level.(block_level <= node_ctxt.genesis_info.level) then
          return @@ Sc_rollup.Inbox.History.empty ~capacity:60000L
        else
          failwith
            "The inbox history for hash %a is missing."
            Block_hash.pp
            block_hash
end

let get_messages Node_context.{l1_ctxt; rollup_address; _} head =
  let open Lwt_result_syntax in
  let* block = Layer1.fetch_tezos_block l1_ctxt head in
  let apply (type kind) accu ~source:_ (operation : kind manager_operation)
      _result =
    let open Result_syntax in
    let+ accu = accu in
    match operation with
    | Sc_rollup_add_messages {rollup; messages}
      when Sc_rollup.Address.(rollup = rollup_address) ->
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
    let* accu = accu in
    match (operation, result) with
    | ( {
          operation = Transaction {destination = Sc_rollup rollup; parameters; _};
          source = Originated sender;
          _;
        },
        ITransaction_result (Transaction_to_sc_rollup_result _) )
      when Sc_rollup.Address.(rollup = rollup_address) ->
        let+ payload =
          Environment.wrap_tzresult @@ Script_repr.force_decode parameters
        in
        let message = Sc_rollup.Inbox_message.{payload; sender; source} in
        Sc_rollup.Inbox_message.Internal message :: accu
    | _ -> return accu
  in
  let*? messages =
    Layer1_services.(
      process_applied_manager_operations
        (Ok [])
        block.operations
        {apply; apply_internal})
  in
  return (List.rev messages)

let same_inbox_as_layer_1 node_ctxt head_hash inbox =
  let open Lwt_result_syntax in
  let head_block = `Hash (head_hash, 0) in
  let Node_context.{cctxt; rollup_address; _} = node_ctxt in
  let* layer1_inbox =
    Plugin.RPC.Sc_rollup.inbox cctxt (cctxt#chain, head_block) rollup_address
  in
  fail_unless
    (Sc_rollup.Inbox.equal layer1_inbox inbox)
    (Sc_rollup_node_errors.Inconsistent_inbox {layer1_inbox; inbox})

let process_head node_ctxt Layer1.(Head {level; hash = head_hash} as head) =
  let open Lwt_result_syntax in
  let*! res = get_messages node_ctxt head_hash in
  match res with
  | Error e -> head_processing_failure e
  | Ok messages ->
      let*! () =
        Inbox_event.get_messages head_hash level (List.length messages)
      in
      let*! () = State.add_messages node_ctxt.store head_hash messages in
      (*

          We compute the inbox of this block using the inbox of its
          predecessor. That way, the computation of inboxes is robust
          to chain reorganization.

      *)
      let*! predecessor = Layer1.predecessor node_ctxt.store head in
      let* inbox = State.inbox_of_hash node_ctxt predecessor in
      let* history = State.history_of_hash node_ctxt predecessor in
      let* ctxt =
        if level <= Raw_level.to_int32 node_ctxt.Node_context.genesis_info.level
        then
          (* This is before we have interpreted the boot sector, so we start
             with an empty context in genesis *)
          return (Context.empty node_ctxt.context)
        else Node_context.checkout_context node_ctxt predecessor
      in
      let*! messages_tree = Context.MessageTrees.find ctxt in
      let* history, inbox, ctxt =
        lift
        @@ let*? level = Raw_level.of_int32 level in
           let*? messages =
             List.map_e Sc_rollup.Inbox_message.serialize messages
           in
           if messages = [] then return (history, inbox, ctxt)
           else
             let commitment_period =
               node_ctxt.protocol_constants.parametric.sc_rollup
                 .commitment_period_in_blocks |> Int32.of_int
             in
             let inbox =
               Sc_rollup.Inbox.refresh_commitment_period
                 ~commitment_period
                 ~level
                 inbox
             in
             let* messages_tree, history, inbox =
               Context.Inbox.add_messages
                 node_ctxt.context
                 history
                 inbox
                 level
                 messages
                 messages_tree
             in

             let*! ctxt = Context.MessageTrees.set ctxt messages_tree in
             return (history, inbox, ctxt)
      in
      let* () = same_inbox_as_layer_1 node_ctxt head_hash inbox in
      let*! () = State.add_inbox node_ctxt.store head_hash inbox in
      let*! () = State.add_history node_ctxt.store head_hash history in
      return ctxt

let inbox_of_hash = State.inbox_of_hash

let history_of_hash = State.history_of_hash

let start () = Inbox_event.starting ()
