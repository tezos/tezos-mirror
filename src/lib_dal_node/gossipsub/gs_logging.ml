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

open Gs_interface.Worker_instance

let pp_sep = Format.pp_print_space

module Events = struct
  include Internal_event.Simple
  open Data_encoding

  let section = ["dal"; "gs"]

  let heartbeat =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"heartbeat"
      ~msg:"Process Heartbeat"
      ~level:Debug
      ()

  let batch_treatment =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"batch_treatment"
      ~msg:"Validation of a message batch"
      ~level:Debug
      ()

  let check_unknown_messages =
    declare_0
      ~section
      ~prefix_name_with_section:true
      ~name:"check_unknown_messages"
      ~msg:"Process unknown messages"
      ~level:Debug
      ()

  let publish_message =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"publish_message"
      ~msg:"Process Publish_message id {message_id} with topic {topic}"
      ~level:Debug
      ~pp1:GS.Topic.pp
      ~pp2:GS.Message_id.pp
      ("topic", Types.Topic.encoding)
      ("message_id", Types.Message_id.encoding)

  let join =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"join"
      ~msg:"Process Join {topic}"
      ~level:Info
      ~pp1:GS.Topic.pp
      ("topic", Types.Topic.encoding)

  let leave =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"leave"
      ~msg:"Process Leave {topic}"
      ~level:Info
      ~pp1:GS.Topic.pp
      ("topic", Types.Topic.encoding)

  let new_connection =
    declare_4
      ~section
      ~prefix_name_with_section:true
      ~name:"new_connection"
      ~msg:
        "Process New_connection from/to {peer} (direct={direct}, \
         trusted={trusted}, bootstrap={bootstrap})"
      ~level:Notice
      ~pp1:Types.Peer.pp
      ("peer", Types.Peer.encoding)
      ("direct", bool)
      ("trusted", bool)
      ("bootstrap", bool)

  let disconnection =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"disconnection"
      ~msg:"Process Disconnection of {peer}"
      ~level:Notice
      ~pp1:Types.Peer.pp
      ("peer", Types.Peer.encoding)

  let message_with_header =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"message_with_header"
      ~msg:
        "Process Message_with_header from {peer} with id {message_id} and \
         topic {topic}"
      ~level:Debug
      ~pp1:P2p_peer.Id.pp
      ~pp2:GS.Topic.pp
      ~pp3:GS.Message_id.pp
      ("peer", P2p_peer.Id.encoding)
      ("topic", Types.Topic.encoding)
      ("message_id", Types.Message_id.encoding)

  let subscribe =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"subscribe"
      ~msg:"Process Subscribe {peer} to {topic}"
      ~level:Info
      ~pp1:P2p_peer.Id.pp
      ~pp2:GS.Topic.pp
      ("peer", P2p_peer.Id.encoding)
      ("topic", Types.Topic.encoding)

  let ping =
    declare_1
      ~section
      ~prefix_name_with_section:true
      ~name:"ping"
      ~msg:"Process Ping from {peer}"
      ~level:Debug
      ~pp1:P2p_peer.Id.pp
      ("peer", P2p_peer.Id.encoding)

  let unsubscribe =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"unsubscribe"
      ~msg:"Process Unsubscribe {peer} from {topic}"
      ~level:Info
      ~pp1:P2p_peer.Id.pp
      ~pp2:GS.Topic.pp
      ("peer", P2p_peer.Id.encoding)
      ("topic", Types.Topic.encoding)

  let graft =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"graft"
      ~msg:"Process Graft {peer} for {topic}"
      ~level:Debug
      ~pp1:P2p_peer.Id.pp
      ~pp2:GS.Topic.pp
      ("peer", P2p_peer.Id.encoding)
      ("topic", Types.Topic.encoding)

  let prune =
    declare_4
      ~section
      ~prefix_name_with_section:true
      ~name:"prune"
      ~msg:"Process Prune {peer} for {topic} with backoff {backoff} and px {px}"
      ~level:Debug
      ~pp1:P2p_peer.Id.pp
      ~pp2:GS.Topic.pp
      ~pp3:Types.Span.pp
      ~pp4:(Format.pp_print_list ~pp_sep P2p_peer.Id.pp)
      ("peer", P2p_peer.Id.encoding)
      ("topic", Types.Topic.encoding)
      ("backoff", Types.Span.encoding)
      ("px", list P2p_peer.Id.encoding)

  let ihave =
    declare_3
      ~section
      ~prefix_name_with_section:true
      ~name:"ihave"
      ~msg:
        "Process IHave from {peer} for {topic} with message_ids {message_ids}"
      ~level:Debug
      ~pp1:P2p_peer.Id.pp
      ~pp2:GS.Topic.pp
      ~pp3:(Format.pp_print_list ~pp_sep GS.Message_id.pp)
      ("peer", P2p_peer.Id.encoding)
      ("topic", Types.Topic.encoding)
      ("message_ids", list Types.Message_id.encoding)

  let iwant =
    declare_2
      ~section
      ~prefix_name_with_section:true
      ~name:"iwant"
      ~msg:"Process IWant from {peer} with message_ids {message_ids}"
      ~level:Debug
      ~pp1:P2p_peer.Id.pp
      ~pp2:(Format.pp_print_list ~pp_sep GS.Message_id.pp)
      ("peer", P2p_peer.Id.encoding)
      ("message_ids", list Types.Message_id.encoding)
end

let event ~verbose =
  let open Events in
  function
  | Check_unknown_messages -> emit check_unknown_messages ()
  | Heartbeat -> emit heartbeat ()
  | App_input event -> (
      match event with
      | Publish_message {message = _; message_id; topic} ->
          emit publish_message (topic, message_id)
      | Join topic -> emit join topic
      | Leave topic -> emit leave topic)
  | P2P_input event -> (
      match event with
      | New_connection {peer; direct; trusted; bootstrap} ->
          emit new_connection (peer, direct, trusted, bootstrap)
      | Disconnection {peer} -> emit disconnection peer
      | In_message {from_peer; p2p_message} -> (
          match p2p_message with
          | Ping -> emit ping from_peer.peer_id
          | Message_with_header {message = _; topic; message_id} ->
              emit message_with_header (from_peer.peer_id, topic, message_id)
          | Subscribe {topic} -> emit subscribe (from_peer.peer_id, topic)
          | Unsubscribe {topic} -> emit unsubscribe (from_peer.peer_id, topic)
          | Graft {topic} ->
              if not verbose then Lwt.return_unit
              else emit graft (from_peer.peer_id, topic)
          | Prune {topic; px; backoff} ->
              if not verbose then Lwt.return_unit
              else
                emit
                  prune
                  ( from_peer.peer_id,
                    topic,
                    backoff,
                    List.of_seq px
                    |> List.map (fun Types.Peer.{peer_id; _} -> peer_id) )
          | IHave {topic; message_ids} ->
              emit ihave (from_peer.peer_id, topic, message_ids)
          | IWant {message_ids} -> emit iwant (from_peer.peer_id, message_ids)))
  | Process_batch _ -> emit batch_treatment ()
