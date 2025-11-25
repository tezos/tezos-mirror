(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type local_node_info = {
  evm_node_endpoint : Uri.t;
  evm_node_private_endpoint : Uri.t;
  websocket : Websocket_client.t option;
}

(** Available modes. *)
type 'f t =
  | Sequencer : 'a Services_backend_sig.tx_container -> 'a t
  | Observer : 'a Services_backend_sig.tx_container -> 'a t
  | Proxy : 'a Services_backend_sig.tx_container -> 'a t
  | Rpc : local_node_info -> 'a t
