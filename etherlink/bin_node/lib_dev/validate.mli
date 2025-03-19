(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Review note: might have to rename this, I was not very inspired kekw *)
type validation_mode =
  | Stateless
      (** Minimal validation, includes chain id, nonce, and sender checks *)
  | With_state  (** State related validation, includes fees and gas checks *)
  | Full  (** Combination of both *)

(** [is_tx_valid backend_rpc tx_raw] validates the transaction
    [tx_raw] and returns the next allowed nonce for the sender of the
    transaction alongside the transaction object. *)
val is_tx_valid :
  ?max_number_of_chunks:int ->
  (module Services_backend_sig.S) ->
  mode:validation_mode ->
  string ->
  ( Ethereum_types.quantity * Ethereum_types.legacy_transaction_object,
    string )
  result
  tzresult
  Lwt.t

type validation_config = {
  base_fee_per_gas : Ethereum_types.quantity;
  maximum_gas_limit : Ethereum_types.quantity;
  da_fee_per_byte : Ethereum_types.quantity;
  next_nonce :
    Ethereum_types.address -> Ethereum_types.quantity option tzresult Lwt.t;
  balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t;
}

type validation_state = {
  config : validation_config;
  addr_balance : Z.t String.Map.t;
  addr_nonce : Z.t String.Map.t;
}

val validate_balance_gas_nonce_with_validation_state :
  validation_state ->
  caller:Ethereum_types.address ->
  Transaction.transaction ->
  (validation_state, string) result tzresult Lwt.t
