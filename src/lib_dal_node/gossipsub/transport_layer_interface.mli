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
type px_peer = Types.Peer.t

val p2p_message_encoding :
  Gs_interface.Worker_instance.p2p_message Data_encoding.t

val pp_p2p_message :
  Format.formatter -> Gs_interface.Worker_instance.p2p_message -> unit

(** A P2P message config is parameterized by the network's name. *)
val message_config :
  network_name:Distributed_db_version.Name.t ->
  Gs_interface.Worker_instance.p2p_message P2p_params.message_config

val version : network_name:Distributed_db_version.Name.t -> Network_version.t
