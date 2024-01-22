(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
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

module Types = Tezos_dal_node_services.Types

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5583

   Version this type to ease future migrations. *)
module P2p_message_V1 = struct
  type px_peer = {point : P2p_point.Id.t; peer : P2p_peer.Id.t}

  type p2p_message =
    | Graft of {topic : Types.Topic.t}
    | Prune of {
        topic : Types.Topic.t;
        px : px_peer Seq.t;
        backoff : Types.Span.t;
      }
    | IHave of {topic : Types.Topic.t; message_ids : Types.Message_id.t list}
    | IWant of {message_ids : Types.Message_id.t list}
    | Subscribe of {topic : Types.Topic.t}
    | Unsubscribe of {topic : Types.Topic.t}
    | Message_with_header of {
        message : Types.Message.t;
        topic : Types.Topic.t;
        message_id : Types.Message_id.t;
      }

  let px_peer_encoding =
    let open Data_encoding in
    conv
      (fun {point; peer} -> (point, peer))
      (fun (point, peer) -> {point; peer})
      (obj2
         (req "point" P2p_point.Id.encoding)
         (req "peer" P2p_peer.Id.encoding))

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5564

     DAL/GS: bound the lists/seqs in exchanged p2p messages. *)
  let encoding =
    let open Data_encoding in
    let case ?max_length ~tag ~title encoding unwrap wrap =
      P2p_params.Encoding {tag; title; encoding; wrap; unwrap; max_length}
    in
    [
      case
        ~tag:0x10
        ~title:"Graft"
        (obj2
           (req "kind" (constant "graft"))
           (req "topic" Types.Topic.encoding))
        (function Graft {topic} -> Some ((), topic) | _ -> None)
        (fun ((), topic) -> Graft {topic});
      case
        ~tag:0x11
        ~title:"Prune"
        (obj4
           (req "kind" (constant "prune"))
           (req "topic" Types.Topic.encoding)
           (req "px" (list px_peer_encoding))
           (req "backoff" Types.Span.encoding))
        (function
          | Prune {topic; px; backoff} ->
              Some ((), topic, List.of_seq px, backoff)
          | _ -> None)
        (fun ((), topic, px, backoff) ->
          Prune {topic; px = List.to_seq px; backoff});
      case
        ~tag:0x12
        ~title:"IHave"
        (obj3
           (req "kind" (constant "ihave"))
           (req "topic" Types.Topic.encoding)
           (req "message_ids" (list Types.Message_id.encoding)))
        (function
          | IHave {topic; message_ids} -> Some ((), topic, message_ids)
          | _ -> None)
        (fun ((), topic, message_ids) -> IHave {topic; message_ids});
      case
        ~tag:0x13
        ~title:"IWant"
        (obj2
           (req "kind" (constant "iwant"))
           (req "message_ids" (list Types.Message_id.encoding)))
        (function IWant {message_ids} -> Some ((), message_ids) | _ -> None)
        (fun ((), message_ids) -> IWant {message_ids});
      case
        ~tag:0x14
        ~title:"Subscribe"
        (obj2
           (req "kind" (constant "subscribe"))
           (req "topic" Types.Topic.encoding))
        (function Subscribe {topic} -> Some ((), topic) | _ -> None)
        (fun ((), topic) -> Subscribe {topic});
      case
        ~tag:0x15
        ~title:"Unsubscribe"
        (obj2
           (req "kind" (constant "unsubscribe"))
           (req "topic" Types.Topic.encoding))
        (function Unsubscribe {topic} -> Some ((), topic) | _ -> None)
        (fun ((), topic) -> Unsubscribe {topic});
      case
        ~tag:0x16
        ~title:"Message_with_header"
        (obj4
           (req "kind" (constant "message_with_header"))
           (req "message" Types.Message.encoding)
           (req "topic" Types.Topic.encoding)
           (req "message_id" Types.Message_id.encoding))
        (function
          | Message_with_header {message; topic; message_id} ->
              Some ((), message, topic, message_id)
          | _ -> None)
        (fun ((), message, topic, message_id) ->
          Message_with_header {message; topic; message_id});
    ]

  let distributed_db_version = Distributed_db_version.zero

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5638

     Decide how to safely choose the node db version. *)
  let distributed_db_versions = [distributed_db_version]

  let message_config ~network_name : p2p_message P2p_params.message_config =
    let chain_name = Distributed_db_version.Name.of_string network_name in
    {encoding; chain_name; distributed_db_versions}
end

let version ~network_name =
  Network_version.
    {
      chain_name = Distributed_db_version.Name.of_string network_name;
      distributed_db_version = P2p_message_V1.distributed_db_version;
      p2p_version = P2p_version.one;
    }

(* Exposed interface *)

include P2p_message_V1
