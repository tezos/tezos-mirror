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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5583

   Version this type to ease future migrations. *)
module P2p_message_V1 = struct
  type p2p_message =
    | Graft of {topic : Gs_interface.topic}
    | Prune of {
        topic : Gs_interface.topic;
        px : P2p_point.Id.t Seq.t;
        backoff : Gs_interface.Span.t;
      }
    | IHave of {
        topic : Gs_interface.topic;
        message_ids : Gs_interface.message_id list;
      }
    | IWant of {message_ids : Gs_interface.message_id list}
    | Subscribe of {topic : Gs_interface.topic}
    | Unsubscribe of {topic : Gs_interface.topic}
    | Message_with_header of {
        message : Gs_interface.message;
        topic : Gs_interface.topic;
        message_id : Gs_interface.message_id;
      }

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
           (req "topic" Gs_interface.topic_encoding))
        (function Graft {topic} -> Some ((), topic) | _ -> None)
        (fun ((), topic) -> Graft {topic});
      case
        ~tag:0x11
        ~title:"Prune"
        (obj4
           (req "kind" (constant "prune"))
           (req "topic" Gs_interface.topic_encoding)
           (req "px" (list P2p_point.Id.encoding))
           (req "backoff" Gs_interface.span_encoding))
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
           (req "topic" Gs_interface.topic_encoding)
           (req "message_ids" (list Gs_interface.message_id_encoding)))
        (function
          | IHave {topic; message_ids} -> Some ((), topic, message_ids)
          | _ -> None)
        (fun ((), topic, message_ids) -> IHave {topic; message_ids});
      case
        ~tag:0x13
        ~title:"IWant"
        (obj2
           (req "kind" (constant "iwant"))
           (req "message_ids" (list Gs_interface.message_id_encoding)))
        (function IWant {message_ids} -> Some ((), message_ids) | _ -> None)
        (fun ((), message_ids) -> IWant {message_ids});
      case
        ~tag:0x14
        ~title:"Subscribe"
        (obj2
           (req "kind" (constant "subscribe"))
           (req "topic" Gs_interface.topic_encoding))
        (function Subscribe {topic} -> Some ((), topic) | _ -> None)
        (fun ((), topic) -> Subscribe {topic});
      case
        ~tag:0x15
        ~title:"Unsubscribe"
        (obj2
           (req "kind" (constant "unsubscribe"))
           (req "topic" Gs_interface.topic_encoding))
        (function Unsubscribe {topic} -> Some ((), topic) | _ -> None)
        (fun ((), topic) -> Unsubscribe {topic});
      case
        ~tag:0x16
        ~title:"Message_with_header"
        (obj4
           (req "kind" (constant "message_with_header"))
           (req "message" Gs_interface.message_encoding)
           (req "topic" Gs_interface.topic_encoding)
           (req "message_id" Gs_interface.message_id_encoding))
        (function
          | Message_with_header {message; topic; message_id} ->
              Some ((), message, topic, message_id)
          | _ -> None)
        (fun ((), message, topic, message_id) ->
          Message_with_header {message; topic; message_id});
    ]

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5638

     Decide how to safely choose the node db version. *)
  let distributed_db_versions = [Distributed_db_version.zero]

  let message_config ~network_name : p2p_message P2p_params.message_config =
    let chain_name = Distributed_db_version.Name.of_string network_name in
    {encoding; chain_name; distributed_db_versions}
end

(* Exposed interface *)

include P2p_message_V1

type peer_metadata = unit

let peer_meta_config : peer_metadata P2p_params.peer_meta_config =
  let empty () = () in
  let encoding = Data_encoding.unit in
  let score (_ : peer_metadata) = 1.0 in
  {peer_meta_encoding = encoding; peer_meta_initial = empty; score}

type connection_metadata = unit

let conn_meta_config : connection_metadata P2p_params.conn_meta_config =
  {
    conn_meta_encoding = Data_encoding.unit;
    private_node = (fun () -> false);
    conn_meta_value = (fun () -> ());
  }
