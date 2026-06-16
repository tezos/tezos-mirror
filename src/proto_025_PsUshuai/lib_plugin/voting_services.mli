(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides RPC services that return voting-related information. *)

open Protocol
open Environment
open Error_monad
open Alpha_context

val ballots : 'a #RPC_context.simple -> 'a -> Vote.ballots shell_tzresult Lwt.t

val ballot_list :
  'a #RPC_context.simple ->
  'a ->
  (Signature.Public_key_hash.t * Vote.ballot) list shell_tzresult Lwt.t

val current_period :
  'a #RPC_context.simple -> 'a -> Voting_period.info shell_tzresult Lwt.t

val successor_period :
  'a #RPC_context.simple -> 'a -> Voting_period.info shell_tzresult Lwt.t

val current_quorum :
  'a #RPC_context.simple -> 'a -> Int32.t shell_tzresult Lwt.t

val listings :
  'a #RPC_context.simple ->
  'a ->
  (Signature.Public_key_hash.t * int64) list shell_tzresult Lwt.t

val proposals :
  'a #RPC_context.simple ->
  'a ->
  Int64.t Protocol_hash.Map.t shell_tzresult Lwt.t

val current_proposal :
  'a #RPC_context.simple -> 'a -> Protocol_hash.t option shell_tzresult Lwt.t

val register : unit -> unit

val total_voting_power :
  'a #RPC_context.simple -> 'a -> Int64.t shell_tzresult Lwt.t

val delegate_proposal_count :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  int shell_tzresult Lwt.t

module S : sig
  val path : (Updater.rpc_context, Updater.rpc_context) RPC_path.t

  val ballots :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      Vote.ballots )
    RPC_service.t

  val ballot_list :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      (public_key_hash * Vote.ballot) list )
    RPC_service.t

  val current_period :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      Voting_period.info )
    RPC_service.t

  val successor_period :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      Voting_period.info )
    RPC_service.t

  val current_quorum :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      int32 )
    RPC_service.t

  val listings :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      (public_key_hash * int64) list )
    RPC_service.t

  val proposals :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      int64 Protocol_hash.Map.t )
    RPC_service.t

  val current_proposal :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      Protocol_hash.t option )
    RPC_service.t

  val total_voting_power :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context,
      unit,
      unit,
      int64 )
    RPC_service.t

  val delegate_proposal_count :
    ( [`GET],
      Updater.rpc_context,
      Updater.rpc_context * public_key_hash,
      unit,
      unit,
      int )
    RPC_service.t
end
