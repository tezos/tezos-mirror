(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Add Opentelemetry tracing to an RPC context. *)
class with_telemetry :
  service_name:string ->
  #Tezos_rpc.Context.generic ->
  Tezos_rpc.Context.generic
