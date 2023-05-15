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

(** This module defines the data structures used to instantiate the Octez P2P
    library. More exactly, it exposes:

    - the types {!p2p_message}, {!peer_metadata} and {!connection_metadata},
    used to instantiate type variables of {!P2p.t};

    - the values {!peer_metadata_cfg} of type {!P2p_params.peer_meta_config},
    {!conn_metadata_cfg} of type {!P2p_params.conn_meta_config} and
    {!message_cfg} of type {!P2p_params.message_config} that are used when
    calling {!P2p.create}.
*)

(** Without piggybacking, {!p2p_message} is almost identical to
    {!Gs_interface.p2p_message}, except that for the [Prune] case,
    {!P2p_peer.Id.t} elements in [px] are replaced by their {!P2p_point.Id.t}
    counterpart. *)
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

(** {!peer_metadata} is not used. So, its value is [unit]. *)
type peer_metadata = unit

(** {!connection_metadata} is not used currently. So, its value is [unit]. *)
type connection_metadata = unit

(** A P2P message config is parameterized by the network's name. *)
val message_config :
  network_name:string -> p2p_message P2p_params.message_config

val peer_meta_config : peer_metadata P2p_params.peer_meta_config

val conn_meta_config : connection_metadata P2p_params.conn_meta_config
