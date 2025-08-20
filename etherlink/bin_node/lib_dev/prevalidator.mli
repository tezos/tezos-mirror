(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error += Gas_limit_too_low of string | Prague_not_enabled

type mode =
  | Minimal
      (** Minimal validation, includes:
            - chain id is valid
            - nonce is superior to current one
            - sender is an EOA
            - transaction data is not too large
            - gas limit is valid with minimum gas price  *)
  | Full
      (** Full validation, is minimal validation plus:
            - balance is enough
            - max_fee_per_gas is inferior to current gas price *)

type prevalidation_result = {
  next_nonce : Ethereum_types.quantity;
      (** The next nonce expected for the given caller, which is lesser than or
          equal to the nonce of the prevalidated transaction. *)
  transaction_object : Ethereum_types.legacy_transaction_object;
      (** Decoded informations about the prevalidated transaction. *)
}

(** [start ~chain_family mode backend] will attempt to start the worker.

    @param chain_family is not [EVM], this operation is a no-op.
    @param backend is being used to fetch the most recent state of the chain,
    and to read key parameters from it.
    @param max_number_of_chunks can be optionally provided to limit the size of
    transactions accepted by the prevalidator. This is typically done by the
    sequencer mode of the node based on its configuration. *)
val start :
  ?max_number_of_chunks:int ->
  chain_family:'a L2_types.chain_family ->
  mode ->
  (module Services_backend_sig.S with type Reader.state = 'state) ->
  unit tzresult Lwt.t

(** [prevalidate_raw_transaction raw_txn] sends a prevalidation request to the
    worker, and waits for the result.

    If the worker failed to start, a new initialization attempt will be
    performed. *)
val prevalidate_raw_transaction :
  string -> (prevalidation_result, string) result tzresult Lwt.t

(** [refresh_state ()] sends a refresh request to the worker, to be processed
    in the background.

    If the worker failed to start, a new initialization attempt will be
    performed. *)
val refresh_state : unit -> unit tzresult Lwt.t

type validation_config = {
  minimum_base_fee_per_gas : Ethereum_types.quantity;
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
