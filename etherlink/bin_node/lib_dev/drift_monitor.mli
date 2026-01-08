(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [run] is a never-ending promise that will regularly check the difference
    between the local head of the node and the head of the upstream EVM node
    used to fetch blueprint from.

    It sets {!last_observed_drift} accordingly. *)
val run :
  evm_node_endpoint:Uri.t ->
  timeout:float ->
  (unit -> Ethereum_types.quantity Lwt.t) ->
  'a tzresult Lwt.t

(** The difference between the remote upstream head and the local head of the
    node, as last observed by {!monitor_drift}. *)
val last_observed_drift : unit -> Z.t
