(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [store ctxt blueprint number] saves the [blueprint] of height [number] in
    the store. *)
val store :
  Sequencer_context.t ->
  Blueprint_types.t ->
  Ethereum_types.quantity ->
  unit tzresult Lwt.t

(** [find ctxt number] tries to fetch the blueprint of height [number] in the
    store, and returns [None] if it does not exist. *)
val find :
  Sequencer_context.t ->
  Ethereum_types.quantity ->
  Blueprint_types.t option Lwt.t
