(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [create ~timestamp ~smart_rollup_address ~number ~transactions]
    creates a sequencer blueprint at [timestamp] with a given [number]
    containing [transactions].  Returns valid payload of external
    messages inputs to put in the inbox.
*)
val create :
  timestamp:Time.Protocol.t ->
  smart_rollup_address:string ->
  number:Ethereum_types.quantity ->
  transactions:string list ->
  [> `External of string] list
