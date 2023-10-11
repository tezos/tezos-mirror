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

(** This module defines the data structures used to instantiate the Octez P2P
    library. More exactly, it exposes:

    - the types {!p2p_message}, {!peer_metadata} and {!connection_metadata},
    used to instantiate type variables of {!P2p.t};

    - the values {!peer_metadata_cfg} of type {!P2p_params.peer_meta_config},
    {!conn_metadata_cfg} of type {!P2p_params.conn_meta_config} and
    {!message_cfg} of type {!P2p_params.message_config} that are used when
    calling {!P2p.create}.
*)

(** Peers exchanged via PX. [point] represents the (address, port) pair of the
    exchanged peer, while [peer] represents the cryptographic identity of the
    peer. *)
type px_peer = {point : P2p_point.Id.t; peer : P2p_peer.Id.t}

(** Without piggybacking, {!p2p_message} is almost identical to
    {!Gs_interface.p2p_message}, except that for the [Prune] case,
    {!P2p_peer.Id.t} elements in [px] are augmented by their {!P2p_point.Id.t}
    counterpart. *)
type p2p_message =
  | Graft of {topic : Types.Topic.t}
  | Prune of {
      topic : Types.Topic.t;
      px : px_peer Seq.t;
      backoff : Gs_interface.Span.t;
    }
  | IHave of {topic : Types.Topic.t; message_ids : Gs_interface.message_id list}
  | IWant of {message_ids : Gs_interface.message_id list}
  | Subscribe of {topic : Types.Topic.t}
  | Unsubscribe of {topic : Types.Topic.t}
  | Message_with_header of {
      message : Gs_interface.message;
      topic : Types.Topic.t;
      message_id : Gs_interface.message_id;
    }

(** {!peer_metadata} is not used. So, its value is [unit]. *)
type peer_metadata = unit

(** {!connection_metadata} type. Currently, peers are using them to exchange
    their public net addresses and ports on which they can be reached. The
    {!advertised_net_port} is not mandatory, as it is already sent via the first
    P2P message after a connection is authenticated. But, we decide to duplicate
    the information here for consistency. The [is_bootstrap_peer] indicates
    whether the remote peer has a bootstrap profile or not. *)
type connection_metadata = {
  advertised_net_addr : P2p_addr.t option;
  advertised_net_port : int option;
  is_bootstrap_peer : bool;
}

(** A P2P message config is parameterized by the network's name. *)
val message_config :
  network_name:string -> p2p_message P2p_params.message_config

val peer_meta_config : peer_metadata P2p_params.peer_meta_config

val conn_meta_config :
  connection_metadata -> connection_metadata P2p_params.conn_meta_config
