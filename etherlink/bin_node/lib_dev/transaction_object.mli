(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A transaction object value to be returned to users through the RPC server.

    The EVM kernel always stores {i legacy} transaction objects, even for
    transactions using a more recent format (like EIP-1559). *)
type t

val encoding : t Data_encoding.t

(** [from_store_transaction_object obj] does not attempt to reconstruct [obj]
    to be compliant with its original format, but instead returned the stored
    data as if it was legacy. *)
val from_store_transaction_object :
  Ethereum_types.legacy_transaction_object -> t

(** [block_from_legacy block] folds over the transactions of [block], assuming
    they are indeed legacy (see {!from_store_transaction_object}). *)
val block_from_legacy :
  Ethereum_types.legacy_transaction_object Ethereum_types.block ->
  t Ethereum_types.block

val hash : t -> Ethereum_types.hash

(** [reconstruct blueprint_payload obj] reconstructs the full transaction object
    from the raw transaction of [obj] stored in [blueprint_payload].

    Fails if [blueprint_payload] is inconsistent (does not contain the raw
    transaction, is corrupted, etc.). *)
val reconstruct :
  Blueprint_types.payload ->
  Ethereum_types.legacy_transaction_object ->
  t tzresult

(** [reconstruct_block blueprint_payload block] folds over the transactions of
    [block] to reconstruct them (see {!reconstruct}). *)
val reconstruct_block :
  Blueprint_types.payload ->
  Ethereum_types.legacy_transaction_object Ethereum_types.block ->
  t Ethereum_types.block tzresult
