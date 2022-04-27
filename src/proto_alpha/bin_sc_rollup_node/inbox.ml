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
module Block_services = Block_services.Make (Protocol) (Protocol)

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
    let open Lwt_syntax in
    let open Node_context in
    let+ possible_inbox = Store.Inboxes.find store block_hash in
    match possible_inbox with
    | None ->
        (* We won't find inboxes for blocks before the rollup origination level.
           Fortunately this case will only ever be called once when dealing with
           the rollup origination block. After that we would always find an
           inbox. *)
        Sc_rollup.Inbox.empty node_ctxt.rollup_address node_ctxt.initial_level
    | Some inbox -> inbox

  let history_of_hash store block_hash =
    Store.Histories.find_with_default store block_hash ~on_default:(fun () ->
        Store.Inbox.history_at_genesis ~bound:(Int64.of_int 60000))

  let get_message_tree = Store.MessageTrees.get

  let set_message_tree = Store.MessageTrees.set
end

let get_messages cctxt head rollup =
  let open Lwt_result_syntax in
  let open Block_services in
  let+ operations =
    Operations.operations cctxt ~chain:`Main ~block:(`Level (snd head)) ()
  in
  let is_add_message = function
    | Contents
        (Manager_operation
          {operation = Sc_rollup_add_messages {rollup = rollup'; messages}; _})
      when Sc_rollup.Address.(rollup' = rollup) ->
        messages
    | _ -> []
  in
  let process_contents {protocol_data = Operation_data {contents; _}; _} =
    let operations = Operation.to_list (Contents_list contents) in
    List.concat_map is_add_message operations
  in
  let process_operations operations =
    List.concat_map process_contents operations
  in
  List.concat_map process_operations operations

let process_head Node_context.({cctxt; rollup_address; _} as node_ctxt) store
    Layer1.(Head {level; hash = head_hash} as head) =
  let open Lwt_result_syntax in
  let*! res = get_messages cctxt (head_hash, level) rollup_address in
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
      let*! inbox = State.inbox_of_hash node_ctxt store predecessor in
      lift
      @@ let*! history = State.history_of_hash store predecessor in
         let*! messages_tree = State.get_message_tree store predecessor in
         let*? level = Raw_level.of_int32 level in
         let* (messages_tree, history, inbox) =
           Store.Inbox.add_messages history inbox level messages messages_tree
         in
         let*! () = State.set_message_tree store head_hash messages_tree in
         let*! () = State.add_inbox store head_hash inbox in
         let*! () = State.add_history store head_hash history in
         return_unit

let inbox_of_hash = State.inbox_of_hash

let start () = Inbox_event.starting ()
