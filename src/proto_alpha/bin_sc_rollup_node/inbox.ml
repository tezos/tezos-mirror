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

  let inbox_of_hash node_ctxt store block_hash =
    let open Lwt_result_syntax in
    let open Node_context in
    let*! possible_inbox = Store.Inboxes.find store block_hash in
    match possible_inbox with
    | None ->
        (* We won't find inboxes for blocks before the rollup origination level.
           Fortunately this case will only ever be called once when dealing with
           the rollup origination block. After that we would always find an
           inbox. *)
        let*! block_level = Layer1.level_of_hash store block_hash in
        let block_level = Raw_level.of_int32_exn block_level in
        if Raw_level.(block_level <= node_ctxt.genesis_info.level) then
          let*! inbox =
            Store.Inbox.empty
              store
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

  let history_of_hash node_ctxt store block_hash =
    let open Lwt_result_syntax in
    let open Node_context in
    let*! res = Store.Histories.find store block_hash in
    match res with
    | Some history -> return history
    | None ->
        (* We won't find inboxes for blocks before the rollup origination level.
           Fortunately this case will only ever be called once when dealing with
           the rollup origination block. After that we would always find an
           inbox. *)
        let*! block_level = Layer1.level_of_hash store block_hash in
        let block_level = Raw_level.of_int32_exn block_level in
        if Raw_level.(block_level <= node_ctxt.genesis_info.level) then
          return @@ Store.Inbox.history_at_genesis ~bound:(Int64.of_int 60000)
        else
          failwith
            "The inbox history for hash %a is missing."
            Block_hash.pp
            block_hash

  let find_message_tree = Store.MessageTrees.find

  let set_message_tree = Store.MessageTrees.set
end

let find_message_tree = State.find_message_tree

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
            (fun message -> Store.Inbox.Message.External message)
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
        let message = Store.Inbox.Message.{payload; sender; source} in
        Store.Inbox.Message.Internal message :: accu
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

let process_head node_ctxt store Layer1.(Head {level; hash = head_hash} as head)
    =
  let open Lwt_result_syntax in
  let*! res = get_messages node_ctxt head_hash in
  match res with
  | Error e -> head_processing_failure e
  | Ok messages ->
      let*! () =
        Inbox_event.get_messages head_hash level (List.length messages)
      in
      let*! () = State.add_messages store head_hash messages in
      (*

          We compute the inbox of this block using the inbox of its
          predecessor. That way, the computation of inboxes is robust
          to chain reorganization.

      *)
      let*! predecessor = Layer1.predecessor store head in
      let* inbox = State.inbox_of_hash node_ctxt store predecessor in
      let* history = State.history_of_hash node_ctxt store predecessor in
      lift
      @@ let*! messages_tree = State.find_message_tree store predecessor in
         let*? level = Raw_level.of_int32 level in
         let*? messages = List.map_e Store.Inbox.Message.serialize messages in
         let* history, inbox =
           if messages = [] then return (history, inbox)
           else
             let* messages_tree, history, inbox =
               Store.Inbox.add_messages
                 store
                 history
                 inbox
                 level
                 messages
                 messages_tree
             in
             let*! () = State.set_message_tree store head_hash messages_tree in
             return (history, inbox)
         in
         let*! () = State.add_inbox store head_hash inbox in
         let*! () = State.add_history store head_hash history in
         return_unit

let inbox_of_hash = State.inbox_of_hash

let history_of_hash = State.history_of_hash

let start () = Inbox_event.starting ()
