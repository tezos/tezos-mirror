(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [is_tx_valid backend_rpc tx_raw] validates the transaction
    [tx_raw].  If the transaction is not yet supported by the node
    validation, we fallback to [backend_rpc.is_tx_valid]. *)
val is_tx_valid :
  (module Services_backend_sig.S) ->
  string ->
  Simulation.validation_result Simulation.simulation_result tzresult Lwt.t
