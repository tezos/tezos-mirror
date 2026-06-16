(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2018-2025 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

module Worker = Gs_interface.Worker_instance
include Internal_event.Simple

let section = ["dal"; "gs"]

let no_connection_for_peer =
  declare_1
    ~section
    ~prefix_name_with_section:true
    ~name:"no_connection_for_peer"
    ~msg:"No running connection found for peer {peer}"
    ~level:Notice
    ~pp1:P2p_peer.Id.pp
    ("peer", P2p_peer.Id.encoding)

let message_notified_to_app =
  declare_1
    ~section
    ~prefix_name_with_section:true
    ~name:"message_notified_to_app"
    ~msg:"Successfully notified message id {message_id} to the application"
    ~level:Debug
    ~pp1:Worker.GS.Message_id.pp
    ("message_id", Types.Message_id.encoding)

let app_message_callback_failed =
  declare_2
    ~section
    ~prefix_name_with_section:true
    ~name:"app_message_callback_failed"
    ~msg:"Callback failed for message id {message_id}. Failure is {failure}"
    ~level:Warning
    ~pp1:Worker.GS.Message_id.pp
    ~pp2:pp_print_trace
    ("message_id", Types.Message_id.encoding)
    ("failure", trace_encoding)

let send_p2p_message_failed =
  declare_2
    ~section
    ~prefix_name_with_section:true
    ~name:"p2p_send_failed"
    ~msg:"Sending P2P message to {peer} failed with error {failure}"
    ~level:Warning
    ~pp1:P2p_peer.Id.pp
    ~pp2:pp_print_trace
    ("peer", P2p_peer.Id.encoding)
    ("failure", trace_encoding)

let send_p2p_message =
  declare_2
    ~section
    ~prefix_name_with_section:true
    ~name:"p2p_send"
    ~msg:"Sending to {peer} P2P message {message}"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp
    ~pp2:Transport_layer_interface.pp_p2p_message
    ("peer", P2p_peer.Id.encoding)
    ("message", Transport_layer_interface.p2p_message_encoding)
