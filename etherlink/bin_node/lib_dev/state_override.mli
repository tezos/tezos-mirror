(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val update_accounts :
  Ethereum_types.state_override option ->
  Evm_state.t ->
  Evm_state.t tzresult Lwt.t
