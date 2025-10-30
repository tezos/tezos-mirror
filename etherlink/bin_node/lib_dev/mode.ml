(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Available modes. *)
type t =
  | Sequencer
  | Observer
  | Proxy
  | Rpc of {evm_node_endpoint : Uri.t; websocket : Websocket_client.t option}
