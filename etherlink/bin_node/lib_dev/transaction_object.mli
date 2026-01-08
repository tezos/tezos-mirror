(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

(** A transaction object value to be returned to users through the RPC server.

    The EVM kernel always stores {i legacy} transaction objects, even for
    transactions using a more recent format (like EIP-1559). *)
type t

val encoding : t Data_encoding.t

(** A single access list entry as introduced by
    {{:https://eips.ethereum.org/EIPS/eip-2930}EIP-2930}.
    Each entry specifies an [address] and a list of [storage_keys]
    that the transaction plans to access. *)
type access = {address : address; storage_keys : hex list}

(** A single authorization item as introduced by
    {{:https://eips.ethereum.org/EIPS/eip-7702}EIP-7702}.
    Each item authorizes an address to act on behalf of a signer and includes
    the signature components [y_parity], [r], and [s]. *)
type authorization_item = {
  chain_id : quantity;
  address : address;
  nonce : quantity;
  y_parity : quantity;
  r : quantity;
  s : quantity;
}
(** {2 Transaction metadata} *)

(** [hash t] returns the hash of the transaction [t]. *)
val hash : t -> hash

(** [block_number t] returns the number of the block containing the transaction [t], if known. *)
val block_number : t -> quantity option

(** [block_hash t] returns the hash of the block containing the transaction [t], if known. *)
val block_hash : t -> block_hash option

(** [transaction_index t] returns the index of the transaction [t] within its block, if known. *)
val transaction_index : t -> quantity option

(** [chain_id t] returns the chain ID associated with the transaction [t], if present.
    This function may fail when applied to legacy transactions whose
    format does not include or allow reconstruction of the chain ID.
    In such cases, the information cannot be retrieved from [t]. *)
val chain_id : t -> quantity option tzresult

(** {2 Participants and addresses} *)

(** [sender t] returns the sender address of the transaction [t]. *)
val sender : t -> address

(** [to_ t] returns the recipient address of the transaction [t], if present.
    It is [None] for contract creation transactions. *)
val to_ : t -> address option

(** {2 Execution and gas} *)

(** [nonce t] returns the nonce of the transaction [t], i.e. the number of transactions
    previously sent from the sender. *)
val nonce : t -> quantity

(** [gas t] returns the gas limit specified by the transaction [t]. *)
val gas : t -> quantity

(** [gas_price t] returns the max fee per gas or its equivalent.
    For legacy and EIP-2930 transactions this is the actual gas price.
    For EIP-1559 and EIP-7702 transactions this is [max_fee_per_gas],
    which is an upper bound rather than the effective gas price. *)
val max_fee_per_gas : t -> quantity

(** [value t] returns the amount of Ether transferred in the transaction [t]. *)
val value : t -> quantity

(** {2 Payload and advanced features} *)

(** [input t] returns the calldata (data payload) of the transaction [t]. *)
val input : t -> hex

(** [access_list t] returns the access list attached to the transaction [t], if present.
    It is available for EIP-2930, EIP-1559, and EIP-7702 transactions. *)
val access_list : t -> access list

(** [authorization_list t] returns the authorization list attached to the transaction [t],
    if present. It is available for EIP-7702 transactions. *)
val authorization_list : t -> authorization_item list

(** {2 Decoding and helpers} *)

(** [decode raw_txn] decodes a raw RLP-encoded transaction string [raw_txn]
    into a transaction object [t]. The transaction type (Legacy, EIP-2930,
    EIP-1559, or EIP-7702) is inferred from the first byte of [raw_txn]. *)
val decode : string -> t tzresult

(** [is_eip7702 t] returns [true] if the transaction [t] is an
    {{:https://eips.ethereum.org/EIPS/eip-7702}EIP-7702} transaction,
    and [false] otherwise. *)
val is_eip7702 : t -> bool

(** [authorization_signer item] recovers the signer address from an
    {{:https://eips.ethereum.org/EIPS/eip-7702}EIP-7702} [authorization_item].
    This is done by hashing the authorization message according to the EIP-7702
    specification and recovering the public key from the signature components
    contained in [item]. *)
val authorization_signer : authorization_item -> (address, string) result

(** Internal representation used to satisfy [txpool_content] requests.
    A [txqueue_content] groups transactions known into two categories,
    organized by sender address and nonce:
    - [pending]: transactions ready for execution
    - [queued]: transactions valid but not yet executable

    This type is not used internally by the tx queue logic itself.
    It only exists to provide response data to [txpool_content] queries.
*)
type txqueue_content = {
  pending : t NonceMap.t AddressMap.t;
  queued : t NonceMap.t AddressMap.t;
}

(** Encoding for {!txqueue_content}. Used by [txpool_content] RPC responses. *)
val txqueue_content_encoding : txqueue_content Data_encoding.t

(** {2 Reconstruction} *)

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

(** [rereconstruct blueprint_payload obj] can be used to retry to reconstruct
    [obj] using [blueprint_payload], exactly as {!reconstruct} would, in case
    [obj] was created with {!from_store_transaction_object}.

    - [rereconstruct blueprint_payload (reconstruct blueprint_payload obj)]
      is a no-op.
    - [rereconstruct blueprint_payload (from_store_transaction_object obj)]
      is equivalent to [reconstruct blueprint_payload obj] *)
val rereconstruct : Blueprint_types.payload -> t -> t tzresult

(** [rereconstruct_block] can be used to retry to reconstruct a block, exactly
    as {!reconstruct_block} would, in case [block] was created with
    {!block_from_legacy}.

    See {!rereconstruct}. *)
val rereconstruct_block :
  Blueprint_types.payload ->
  t Ethereum_types.block ->
  t Ethereum_types.block tzresult
