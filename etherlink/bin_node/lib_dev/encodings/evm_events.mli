(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module Delayed_transaction : sig
  type kind = EthereumTransaction | Deposit | Fa_deposit | TezosOperation

  type t = {kind : kind; hash : hash; raw : string}

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val of_rlp_content :
    ?transaction_tag:string ->
    ?fa_deposit_tag:string ->
    ?operation_tag:string ->
    hash ->
    Rlp.item ->
    t option

  val to_rlp : t -> Rlp.item
end

module Upgrade : sig
  type t = {hash : hash; timestamp : Time.Protocol.t}

  val of_bytes : bytes -> t option

  val to_bytes : t -> bytes

  val encoding : t Data_encoding.t
end

module Sequencer_upgrade : sig
  type t = {
    sequencer : Signature.public_key;
    pool_address : address;
    timestamp : Time.Protocol.t;
  }

  val of_bytes : bytes -> t option

  val to_bytes : t -> bytes

  val encoding : t Data_encoding.t
end

module Blueprint_applied : sig
  type t = {number : quantity; hash : block_hash}
end

module Flushed_blueprint : sig
  type t = {
    transactions : Delayed_transaction.t list;
    timestamp : Time.Protocol.t;
    level : quantity;
  }
end

type t =
  | Upgrade_event of Upgrade.t
  | Sequencer_upgrade_event of Sequencer_upgrade.t
  | Blueprint_applied of Blueprint_applied.t
  | New_delayed_transaction of Delayed_transaction.t
  | Flush_delayed_inbox of Flushed_blueprint.t

val pp : Format.formatter -> t -> unit

val encoding : t Data_encoding.t

val of_bytes : bytes -> t option Lwt.t

val of_parts :
  delayed_transactions:Delayed_transaction.t list ->
  kernel_upgrade:Upgrade.t option ->
  sequencer_upgrade:Sequencer_upgrade.t option ->
  t list
