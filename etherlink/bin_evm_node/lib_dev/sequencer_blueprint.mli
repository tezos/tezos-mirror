(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [create ~secret_key ~timestamp ~smart_rollup_address ~number ~transactions]
    creates a sequencer blueprint at [timestamp] with a given [number]
    containing [transactions], signed with [secret_key].  Returns
    valid list of external messages inputs to put in the inbox.
*)
val create :
  secret_key:Signature.secret_key ->
  timestamp:Time.Protocol.t ->
  smart_rollup_address:string ->
  number:Ethereum_types.quantity ->
  transactions:string list ->
  Blueprint_types.t
