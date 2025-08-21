(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* [register_tezlink_services ~l2_chain_id (module Backend)]
   creates a directory where all the Tezlink services are registered. *)
val register_tezlink_services :
  l2_chain_id:L2_types.chain_id ->
  add_operation:
    (Tezos_types.Operation.t -> bytes -> Ethereum_types.hash tzresult Lwt.t) ->
  (module Tezlink_backend_sig.S) ->
  unit Tezos_rpc.Directory.t
