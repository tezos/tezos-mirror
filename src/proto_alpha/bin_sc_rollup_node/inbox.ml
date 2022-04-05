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
module Rollup_node_constants = Constants
open Protocol
open Alpha_context
module Block_services = Block_services.Make (Protocol) (Protocol)

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

  let inbox_exists = Store.Inboxes.mem

  let inbox_of_hash = Store.Inboxes.get

  let history_of_hash = Store.Histories.get

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

let process_head cctxt store Layer1.(Head {level; hash = head_hash} as head) =
  let rollup = Rollup_node_constants.get_sc_rollup_address () in
  let open Lwt_result_syntax in
  get_messages cctxt (head_hash, level) rollup >>= function
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
      let*! inbox = State.inbox_of_hash store predecessor in
      let*! history = State.history_of_hash store predecessor in
      let*! messages_tree = State.get_message_tree store predecessor in
      let*? level = Raw_level.of_int32 level in
      let* (messages_tree, history, inbox) =
        Store.Inbox.add_messages history inbox level messages messages_tree
      in
      let*! () = State.set_message_tree store head_hash messages_tree in
      let*! () = State.add_inbox store head_hash inbox in
      let*! () = State.add_history store head_hash history in
      return_unit

let update cctxt store chain_event =
  let open Lwt_result_syntax in
  let open Layer1 in
  Lwt.map Environment.wrap_tzresult
  @@
  match chain_event with
  | SameBranch {new_head; intermediate_heads} ->
      let* () = List.iter_es (process_head cctxt store) intermediate_heads in
      process_head cctxt store new_head
  | Rollback {new_head = _} ->
      (*

          Since [process_head] is robust to chain reorganizations, we do
          need a specific treatment of [Rollback] events.

      *)
      return_unit

let inbox_of_hash = State.inbox_of_hash

let start store sc_rollup_address =
  let open Lwt_syntax in
  Inbox_event.starting () >>= fun () ->
  State.inbox_exists store Layer1.genesis_hash >>= function
  | false ->
      let* () =
        State.add_inbox
          store
          Layer1.genesis_hash
          (Sc_rollup.Inbox.empty sc_rollup_address Raw_level.root)
      in
      State.add_history
        store
        Layer1.genesis_hash
        (Store.Inbox.history_at_genesis ~bound:(Int64.of_int 60000))
  | true -> Lwt.return ()
