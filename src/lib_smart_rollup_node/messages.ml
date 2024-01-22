(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let add_all_messages (node_ctxt : _ Node_context.t) ~messages ~pred_hash =
  let open Lwt_result_syntax in
  let* pred_header = Node_context.header_of_hash node_ctxt pred_hash in
  let* grand_parent_header =
    Node_context.header_of_hash node_ctxt pred_header.header.predecessor
  in
  let is_first_block =
    pred_header.header.proto_level <> grand_parent_header.header.proto_level
  in
  let inbox_level = Int32.succ pred_header.level in
  let* plugin = Protocol_plugins.proto_plugin_for_level node_ctxt inbox_level in
  let module Plugin = (val plugin) in
  return
  @@ Plugin.Pvm.start_of_level_serialized
     ::
     (if is_first_block then
      Option.to_list Plugin.Pvm.protocol_migration_serialized
     else [])
  @ Plugin.Pvm.info_per_level_serialized
      ~predecessor:pred_header.hash
      ~predecessor_timestamp:pred_header.header.timestamp
    :: messages
  @ [Plugin.Pvm.end_of_level_serialized]

(** Returns [true] if the first messages of the parameter list is an encoded
    [Start_of_level] message.  *)
let has_sol = function
  | "\x00\x01" :: _ ->
      (* 00 is for Internal_message and 01 is for Start_of_level *)
      true
  | _ -> false

let find node_ctxt messages_hash =
  let open Lwt_result_syntax in
  let* msg = Node_context.unsafe_find_stored_messages node_ctxt messages_hash in
  match msg with
  | None -> return_none
  | Some (messages, pred_hash) ->
      if has_sol messages then return_some messages
      else
        (* The messages do not contain the internal protocol messages, we add
           them back. NOTE: this requires to potentially make L1 rpc calls. *)
        let* messages = add_all_messages node_ctxt ~messages ~pred_hash in
        let* () =
          Node_context.save_messages
            node_ctxt
            messages_hash
            ~predecessor:pred_hash
            messages
        in
        return_some messages

let get node_ctxt messages_hash =
  let open Lwt_result_syntax in
  let* res = find node_ctxt messages_hash in
  match res with
  | None ->
      failwith
        "Could not retrieve messages with payloads merkelized hash %a"
        Merkelized_payload_hashes_hash.pp
        messages_hash
  | Some res -> return res
