(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* THIS IS THE ENTRYPOINT *)
val register_tezlink_services :
  l2_chain_id:L2_types.chain_id ->
  (module Tezlink_backend_sig.S) ->
  unit Tezos_rpc.Directory.t
