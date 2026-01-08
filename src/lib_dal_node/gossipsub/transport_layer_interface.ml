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

module P2p_message_V1 = struct
  type px_peer = Types.Peer.t

  type p2p_message = Gs_interface.Worker_instance.p2p_message

  let px_peer_encoding =
    let open Data_encoding in
    conv
      (fun Types.Peer.{maybe_reachable_point; peer_id} ->
        (maybe_reachable_point, peer_id))
      (fun (maybe_reachable_point, peer_id) -> {maybe_reachable_point; peer_id})
      (obj2
         (req "maybe_reachable_point" P2p_point.Id.encoding)
         (req "peer" P2p_peer.Id.encoding))

  (* This ping payload is to be considered as a dummy one using is the
     pkh zero address. *)
  let ping_topic =
    Types.Topic.{slot_index = 0; pkh = Signature.Public_key_hash.zero}

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5564

     DAL/GS: bound the lists/seqs in exchanged p2p messages. *)
  let p2p_message_app_encoding =
    let open Data_encoding in
    let open Gs_interface.Worker_instance in
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
      (* In the following encoding we introduce a special case such that the [Ping] is
         encoded as a [Subscribe] using a dummy payload [ping_topic] and the p2p messages
         encoding remain compatible.

         FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/7768

         Define a [Ping] own message encoding when bumping the p2p message version.
      *)
      case
        ~tag:0x14
        ~title:"Subscribe"
        (obj2
           (req "kind" (constant "subscribe"))
           (req "topic" Types.Topic.encoding))
        (function
          | Subscribe {topic} -> Some ((), topic)
          | Ping -> Some ((), ping_topic)
          | _ -> None)
        (fun ((), topic) ->
          if Types.Topic.(topic = ping_topic) then Ping else Subscribe {topic});
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

  let p2p_message_encoding =
    let open Data_encoding in
    List.map
      (fun (P2p_params.Encoding
              {tag; title; encoding; wrap; unwrap; max_length = _})
         -> case (Tag tag) ~title encoding unwrap wrap)
      p2p_message_app_encoding
    |> union
    |> def
         "dal_p2p_message"
         ~title:"dal-p2p-message"
         ~description:"The encoding of P2P messages used for the DAL"

  let _ = Data_encoding.(Registration.register p2p_message_encoding)

  let pp_list pp_elt =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") pp_elt

  let pp_p2p_message fmt =
    let open Gs_interface.Worker_instance in
    function
    | Graft {topic} -> Format.fprintf fmt "Graft{topic=%a}" Types.Topic.pp topic
    | Prune {topic; px; backoff} ->
        Format.fprintf
          fmt
          "Prune{topic=%a, px=%a, backoff=%a}"
          Types.Topic.pp
          topic
          (pp_list Types.Peer.pp)
          (List.of_seq px)
          Types.Span.pp
          backoff
    | IHave {topic; message_ids} ->
        Format.fprintf
          fmt
          "IHave{topic=%a, message_ids=%a}"
          Types.Topic.pp
          topic
          (pp_list Types.Message_id.pp)
          message_ids
    | IWant {message_ids} ->
        Format.fprintf
          fmt
          "IWant{message_ids=%a}"
          (pp_list Types.Message_id.pp)
          message_ids
    | Subscribe {topic} ->
        Format.fprintf fmt "Subscribe{topic=%a}" Types.Topic.pp topic
    | Unsubscribe {topic} ->
        Format.fprintf fmt "Unsubscribe{topic=%a}" Types.Topic.pp topic
    | Message_with_header {message = _; message_id; topic = _} ->
        Format.fprintf
          fmt
          "FullMessage{message_id=%a}"
          Types.Message_id.pp
          message_id
    | Ping -> Format.fprintf fmt "Ping"

  let distributed_db_version = Distributed_db_version.zero

  let distributed_db_versions = [distributed_db_version]

  let message_config ~network_name : p2p_message P2p_params.message_config =
    {
      encoding = p2p_message_app_encoding;
      chain_name = network_name;
      distributed_db_versions;
    }
end

let version ~network_name =
  Network_version.
    {
      chain_name = network_name;
      distributed_db_version = P2p_message_V1.distributed_db_version;
      p2p_version = P2p_version.one;
    }

(* Exposed interface *)

include P2p_message_V1
