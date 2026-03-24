(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

val build_rpc_directory :
  profiling_config:Tezos_profiler.Profiler.profiling_config ->
  unit Tezos_rpc.Directory.t
