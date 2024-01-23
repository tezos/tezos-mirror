(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** This module is about shard amplification, a feature allowing DAL
    nodes which receive enough shards of a given slot to contribute to
    the DAL reliability by reconstructing the slot, recomputing all the
    shards, and republishing the missing shards on the DAL network. *)

(** [amplify shard_store commitment node_ctxt] is called each time a
    new shard is received by an observer node, after being added to
    the shard store [shard_store]. The argument [commitment] is the
    commitment of the received shard. *)
val amplify :
  Store.Shards.t ->
  Cryptobox.Commitment.t ->
  Node_context.t ->
  unit tzresult Lwt.t
