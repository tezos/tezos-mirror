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

module Events = struct
  include Internal_event.Simple
  open Data_encoding
  open Gs_interface

  let section = ["gossipsub"; "worker"; "event"]

  let prefix =
    let prefix = String.concat "_" section in
    fun s -> prefix ^ "-" ^ s

  let heartbeat =
    declare_0
      ~section
      ~name:(prefix "heartbeat")
      ~msg:"Heartbeat"
      ~level:Info
      ()

  let publish_message =
    declare_3
      ~section
      ~name:(prefix "publish_message")
      ~msg:"Processing publish_message"
      ~level:Info
      ("topic", topic_encoding)
      ("message", message_encoding)
      ("message_id", message_id_encoding)

  let join =
    declare_1
      ~section
      ~name:(prefix "join")
      ~msg:"Processing join"
      ~level:Info
      ("topic", topic_encoding)

  let leave =
    declare_1
      ~section
      ~name:(prefix "leave")
      ~msg:"Processing leave"
      ~level:Info
      ("topic", topic_encoding)

  let new_connection =
    declare_3
      ~section
      ~name:(prefix "new_connection")
      ~msg:"new_connection"
      ~level:Info
      ("peer", P2p_peer.Id.encoding)
      ("direct", bool)
      ("outbound", bool)

  let disconnection =
    declare_1
      ~section
      ~name:(prefix "disconnection")
      ~msg:"Disconnection"
      ~level:Info
      ("peer", P2p_peer.Id.encoding)

  let message_with_header =
    declare_4
      ~section
      ~name:(prefix "message_with_header")
      ~msg:"Processing Message_with_header"
      ~level:Info
      ("peer", P2p_peer.Id.encoding)
      ("topic", topic_encoding)
      ("message", message_encoding)
      ("message_id", message_id_encoding)

  let subscribe =
    declare_2
      ~section
      ~name:(prefix "subscribe")
      ~msg:"Processing subscribe"
      ~level:Info
      ("peer", P2p_peer.Id.encoding)
      ("topic", topic_encoding)

  let unsubscribe =
    declare_2
      ~section
      ~name:(prefix "unsubscribe")
      ~msg:"Processing unsubscribe"
      ~level:Info
      ("peer", P2p_peer.Id.encoding)
      ("topic", topic_encoding)

  let graft =
    declare_2
      ~section
      ~name:(prefix "graft")
      ~msg:"Processing graft"
      ~level:Info
      ("peer", P2p_peer.Id.encoding)
      ("topic", topic_encoding)

  let prune =
    declare_4
      ~section
      ~name:(prefix "prune")
      ~msg:"Processing prune"
      ~level:Info
      ("peer", P2p_peer.Id.encoding)
      ("topic", topic_encoding)
      ("backoff", span_encoding)
      ("px", list P2p_peer.Id.encoding)

  let ihave =
    declare_3
      ~section
      ~name:(prefix "ihave")
      ~msg:"Processing IHave"
      ~level:Info
      ("peer", P2p_peer.Id.encoding)
      ("topic", topic_encoding)
      ("message_ids", list message_id_encoding)

  let iwant =
    declare_2
      ~section
      ~name:(prefix "iwant")
      ~msg:"Processing IWant"
      ~level:Info
      ("peer", P2p_peer.Id.encoding)
      ("message_ids", list message_id_encoding)
end

let event =
  let open Events in
  let open Gs_interface.Worker_instance in
  function
  | Heartbeat -> emit heartbeat ()
  | App_input event -> (
      match event with
      | Publish_message {message; message_id; topic} ->
          emit publish_message (topic, message, message_id)
      | Join topic -> emit join topic
      | Leave topic -> emit leave topic)
  | P2P_input event -> (
      match event with
      | New_connection {peer; direct; outbound} ->
          emit new_connection (peer, direct, outbound)
      | Disconnection {peer} -> emit disconnection peer
      | In_message {from_peer; p2p_message} -> (
          match p2p_message with
          | Message_with_header {message; topic; message_id} ->
              emit message_with_header (from_peer, topic, message, message_id)
          | Subscribe {topic} -> emit subscribe (from_peer, topic)
          | Unsubscribe {topic} -> emit unsubscribe (from_peer, topic)
          | Graft {topic} -> emit graft (from_peer, topic)
          | Prune {topic; px; backoff} ->
              emit prune (from_peer, topic, backoff, List.of_seq px)
          | IHave {topic; message_ids} ->
              emit ihave (from_peer, topic, message_ids)
          | IWant {message_ids} -> emit iwant (from_peer, message_ids)))
