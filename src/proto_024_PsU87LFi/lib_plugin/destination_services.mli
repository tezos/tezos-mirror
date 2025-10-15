(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Environment
open Environment.Error_monad
open Protocol.Alpha_context

module S : sig
  val index :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context * Destination.t,
      unit,
      unit,
      Z.t option )
    RPC_service.t
end

val index :
  'a #RPC_context.simple ->
  'a ->
  Destination.t ->
  Z.t option shell_tzresult Lwt.t

val register : unit -> unit
