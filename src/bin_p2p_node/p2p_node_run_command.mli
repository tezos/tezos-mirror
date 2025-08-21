(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

val run :
  listen_addr:string ->
  ?peers:string trace ->
  ?discovery_addr:string ->
  rpc_addr:string ->
  ping_interval:float ->
  unit ->
  unit tzresult Lwt.t
