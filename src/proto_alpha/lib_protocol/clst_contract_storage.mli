(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Abstraction to handle high level interactions of the CLST enshrined contract.

    This module is responsible for maintaining the contract's storage, and
    handling the token transfers between the different containers via {!Token}
    and {!Clst_storage} (via the {!Alpha_context} abstraction).
*)

open Alpha_context
open Script_native_types
open Script_typed_ir

(** Type of the storage of the CLST contract.
    It contains the ledger and the total number of CLST tokens. *)
type t

(** [from_clst_storage s] creates a high-level storage representation from
    the contract internal storage representation. *)
val from_clst_storage : CLST_types.storage -> t

(** [to_clst_storage s] creates a contract internal storage representation from
    the high-level storage representation. *)
val to_clst_storage : t -> CLST_types.storage

(** [token_id] is the token identifier of CLST, being 0 by default for FA2
    single asset contracts. *)
val token_id : CLST_types.nat

(** [ticket_token] returns a ticket-token of CLST with the [(token_id,
    (None : bytes option))] contents. *)
val ticket_token :
  clst_hash:Contract_hash.t -> Ticket_token.ex_token tzresult Lwt.t

(** [get_storage ctxt] returns the storage retrieved and parsed from the
    context. It doesn't fail if the storage didn't exist in the context, i.e.
    the CLST contract has not been originated. *)
val get_storage : context -> (t option * context) tzresult Lwt.t

(** [get_balance_from_storage ctxt storage address] returns the balance from the
    given address, extracted from the CLST contract's storage. Returns `zero_n`
    if no balance has been found, following FA2.1 specification. *)
val get_balance_from_storage :
  context -> t -> address -> (CLST_types.nat * context) tzresult Lwt.t

(** [get_balance context contract] retrieves the balance of a given contract on
    the CLST contract. This is a combination of `get_storage` and
    `get_balance_from_storage`. *)
val get_balance :
  context -> Contract.t -> (CLST_types.nat * context) tzresult Lwt.t

(** [set_balance_from_storage ctxt storage address amount] updates the balance of the
    given address to [amount]. *)
val set_balance_from_storage :
  context -> t -> address -> CLST_types.nat -> (t * context) tzresult Lwt.t

(** [get_total_supply context] returns the total supply of CLST tokens
    in the CLST contract. *)
val get_total_supply : context -> (CLST_types.nat * context) tzresult Lwt.t

(** [get_total_supply_from_storage storage] returns the total supply
    of CLST tokens in the CLST contract. *)
val get_total_supply_from_storage : t -> CLST_types.nat

(** [increment_total_supply storage amount] increments the total supply by [amount]. *)
val increment_total_supply : t -> CLST_types.nat -> t

(** [decrement_total_supply storage amount] decrements the total supply by [amount]. *)
val decrement_total_supply : t -> CLST_types.nat -> t tzresult

(** [deposit_to_clst_deposits context ~clst_contract_hash amount] deposits
    [amount] tez from [clst_contract_hash] balance to the CLST deposits
    container *)
val deposit_to_clst_deposits :
  context ->
  clst_contract_hash:Contract_hash.t ->
  Tez.t ->
  (context * Receipt.balance_updates) tzresult Lwt.t

(** [redeem_from_clst_deposits context ~staker amount] redeems
    [amount] tez from the CLST deposits container to [staker]'s redeemed frozen
    deposits. *)
val redeem_from_clst_deposits :
  context ->
  staker:Contract.t ->
  Tez.t ->
  (context * Receipt.balance_updates) tzresult Lwt.t

(** [finalize context ~clst_cotnract_hash ~staker] finalizes all finalizable redemption
    requests from [staker], moving redeemed frozen deposits to
    [clst_contract_hash] spendable balance. *)
val finalize :
  context ->
  clst_contract_hash:Contract_hash.t ->
  staker:Contract.t ->
  (context * Receipt.balance_updates * Tez.t) tzresult Lwt.t

(** Type of spender allowance for owner's tokens:
    - [Infinite] if a spender is an operator (infinite allowance)
    - [Finite n] if a spender can spend [n] tokens (finite allowance). *)
type allowance = Infinite | Finite of CLST_types.nat

(** [get_account_operator_allowance context storage owner spender] get
    [spender] allowance on [owner]'s tokens:

    - [None] if [spender] has no permission
    - [Some Infinite] if [spender] has an infinite allowance
    - [Some (Finite n)] if [spender] can spend [n] tokens. *)
val get_account_operator_allowance :
  context ->
  t ->
  owner:address ->
  spender:address ->
  (allowance option * context) tzresult Lwt.t

(** [set_account_operator_allowance context storage owner spender new_allowance]
    set [spender] allowance on [owner]'s tokens:

    - [None] removes the permission
    - [Some Infinite] sets an infinite allowance
    - [Some (Finite n)] sets a finite allowance of [n] tokens. *)
val set_account_operator_allowance :
  context ->
  t ->
  owner:address ->
  spender:address ->
  allowance option ->
  (t * context) tzresult Lwt.t

(** [get_token_info context storage token_id] returns the token
    metadata associated with [token_id], or [None] if [token_id] does
    not exist. *)
val get_token_info :
  context ->
  t ->
  token_id:CLST_types.nat ->
  ((CLST_types.nat * CLST_types.token_info) option * context) tzresult Lwt.t

(** [is_delegate_registered ctxt delegate] returns [true] if
    [delegate] has active or pending parameters. *)
val is_delegate_registered : context -> public_key_hash -> bool tzresult Lwt.t

(** [register_delegate context ~delegate
    ~edge_of_clst_staking_over_baking_millionth
    ~ratio_of_clst_staking_over_direct_staking_billionth] registers new
    [delegate's] parameters, to be activated after
    {!Constants_storage.consensus_rights_delay} cycles. *)
val register_delegate :
  context ->
  delegate:public_key_hash ->
  edge_of_clst_staking_over_baking_millionth:CLST_types.nat ->
  ratio_of_clst_staking_over_direct_staking_billionth:CLST_types.nat ->
  (context, error trace) result Lwt.t
