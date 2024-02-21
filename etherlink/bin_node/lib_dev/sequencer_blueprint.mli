(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Type representing the messages describing a blueprint.

    The sequencer has to produce two different versions of every blueprint.
    The first one is the version published to L1. In this one, only the hashes
    of delayed transactions are added, as the kernel already has the full
    transactions in the delayed inbox.
    The second one is the one the sequencer executes locally. In this case,
    instead of replicating the state of the delayed inbox, the full delayed
    transactions are added in the blueprint, respecting the same order as
    their corresponding hashes.
*)
type t = {
  to_publish : Blueprint_types.payload;
  to_execute : Blueprint_types.payload;
}

(** [create ~secret_key ~timestamp ~smart_rollup_address ~number
    ~parent_hash ~delayed_transactions ~transactions]
    creates a sequencer blueprint at [timestamp] with a given [number]
    containing [transactions], signed with [secret_key].  Returns
    valid list of external messages inputs to put in the inbox.
*)
val create :
  cctxt:#Client_context.wallet ->
  sequencer_key:Client_keys.sk_uri ->
  timestamp:Time.Protocol.t ->
  smart_rollup_address:string ->
  number:Ethereum_types.quantity ->
  parent_hash:Ethereum_types.block_hash ->
  delayed_transactions:Ethereum_types.Delayed_transaction.t list ->
  transactions:string list ->
  t tzresult Lwt.t
