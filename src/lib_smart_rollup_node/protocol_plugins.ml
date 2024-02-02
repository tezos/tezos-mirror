(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

type error += Unsupported_protocol of Protocol_hash.t

let () =
  register_error_kind
    ~id:"smart_rollup.node.unsupported_protocol"
    ~title:"Protocol not supported by rollup node"
    ~description:"Protocol not supported by rollup node."
    ~pp:(fun ppf proto ->
      Format.fprintf
        ppf
        "Protocol %a is not supported by the rollup node."
        Protocol_hash.pp
        proto)
    `Permanent
    Data_encoding.(obj1 (req "protocol" Protocol_hash.encoding))
    (function Unsupported_protocol p -> Some p | _ -> None)
    (fun p -> Unsupported_protocol p)

type proto_plugin = (module Protocol_plugin_sig.S)

let proto_plugins : proto_plugin Protocol_hash.Table.t =
  Protocol_hash.Table.create 7

let last_registered = ref None

let register (plugin : proto_plugin) =
  let module Plugin = (val plugin) in
  if Protocol_hash.Table.mem proto_plugins Plugin.protocol then
    Format.kasprintf
      invalid_arg
      "The rollup node protocol plugin for protocol %a is already registered. \
       Did you register it manually multiple times?"
      Protocol_hash.pp
      Plugin.protocol ;
  last_registered := Some Plugin.protocol ;
  Protocol_hash.Table.add proto_plugins Plugin.protocol plugin

let registered_protocols () =
  Protocol_hash.Table.to_seq_keys proto_plugins |> List.of_seq

let last_registered () =
  match !last_registered with
  | None -> Stdlib.failwith "No protocol plugins registered"
  | Some p -> p

let proto_plugin_for_protocol protocol =
  Protocol_hash.Table.find proto_plugins protocol
  |> Option.to_result ~none:[Unsupported_protocol protocol]

let proto_plugin_for_level node_ctxt level =
  let open Lwt_result_syntax in
  let* {protocol; _} = Node_context.protocol_of_level node_ctxt level in
  let*? plugin = proto_plugin_for_protocol protocol in
  return plugin

let proto_plugin_for_level_with_store node_store level =
  let open Lwt_result_syntax in
  let* {protocol; _} =
    Node_context.protocol_of_level_with_store node_store level
  in
  let*? plugin = proto_plugin_for_protocol protocol in
  return plugin

let proto_plugin_for_block node_ctxt block_hash =
  let open Lwt_result_syntax in
  let* level = Node_context.level_of_hash node_ctxt block_hash in
  proto_plugin_for_level node_ctxt level

let last_proto_plugin node_ctxt =
  let open Lwt_result_syntax in
  let* protocol = Node_context.last_seen_protocol node_ctxt in
  match protocol with
  | None -> failwith "No known last protocol, cannot get plugin"
  | Some protocol ->
      let*? plugin = proto_plugin_for_protocol protocol in
      return plugin

module Constants_cache =
  Aches_lwt.Lache.Make_result
    (Aches.Rache.Transfer (Aches.Rache.LRU) (Protocol_hash))

let constants_cache =
  let cache_size = 3 in
  Constants_cache.create cache_size

let get_constants_of_protocol ?level (node_ctxt : _ Node_context.t)
    protocol_hash =
  let open Lwt_result_syntax in
  if Protocol_hash.(protocol_hash = node_ctxt.current_protocol.hash) then
    return node_ctxt.current_protocol.constants
  else
    let retrieve protocol_hash =
      let*? plugin = proto_plugin_for_protocol protocol_hash in
      let module Plugin = (val plugin) in
      let* level =
        match level with
        | None ->
            let+ (First_known l | Activation_level l) =
              Node_context.protocol_activation_level node_ctxt protocol_hash
            in
            l
        | Some l -> return l
      in
      Plugin.Layer1_helpers.retrieve_constants
        ~block:(`Level level)
        node_ctxt.cctxt
    in
    Constants_cache.bind_or_put
      constants_cache
      protocol_hash
      retrieve
      Lwt.return

let get_constants_of_level node_ctxt level =
  let open Lwt_result_syntax in
  let* {protocol; _} = Node_context.protocol_of_level node_ctxt level in
  get_constants_of_protocol ~level node_ctxt protocol

let get_constants_of_block_hash node_ctxt block_hash =
  let open Lwt_result_syntax in
  let* level = Node_context.level_of_hash node_ctxt block_hash in
  get_constants_of_level node_ctxt level
