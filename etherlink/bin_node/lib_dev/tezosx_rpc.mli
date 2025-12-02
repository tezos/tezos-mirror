(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

val add_rpc_directory :
  (module Services_backend_sig.S) ->
  l2_chain_id:L2_types.chain_id ->
  unit Tezos_rpc.Directory.t ->
  Tezosx.runtime ->
  unit Tezos_rpc.Directory.t
