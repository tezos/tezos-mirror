(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Declares logging events for [p2p-node] *)

open Data_encoding
include Internal_event.Simple

let alternative_color = Internal_event.Green

let section_root = ["p2p-node"]

let section = section_root

let bootstrapping =
  declare_0
    ~section
    ~name:"bootstrapping"
    ~msg:"P2P node bootstrapping"
    ~level:Notice
    ()

let maintenance_started =
  declare_0
    ~section
    ~name:"maintenance_started"
    ~msg:"P2P node maintainance started"
    ~level:Notice
    ()

let shutdown =
  declare_0 ~section ~name:"shutdown" ~msg:"P2P node shutdown" ~level:Notice ()

let connection_incoming =
  declare_1
    ~section
    ~alternative_color
    ~name:"connection_incoming"
    ~msg:"Incoming connection from address {address}"
    ~level:Notice
    ("address", P2p_point.Id.encoding)

let connection_established =
  declare_2
    ~section
    ~alternative_color
    ~name:"connection_established"
    ~msg:"Connection established with peer {peer} at address {address}"
    ~level:Notice
    ("peer", P2p_peer.Id.encoding)
    ("address", string)

let disconnected =
  declare_1
    ~section
    ~alternative_color
    ~name:"disconnected"
    ~msg:"Disconnected from peer {peer}"
    ~level:Notice
    ("peer", P2p_peer.Id.encoding)
