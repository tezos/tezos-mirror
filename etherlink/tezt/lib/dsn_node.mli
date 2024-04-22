(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val bundler :
  ?runner:Runner.t ->
  ?rpc_addr:string ->
  ?rpc_port:int ->
  endpoint:string ->
  unit ->
  t Lwt.t

val endpoint : t -> string
