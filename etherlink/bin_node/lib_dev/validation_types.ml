(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Configuration of a validation. Depends on the context in which the
    validation is done, e.g. contains functions to read the balance and counter
    of a contract in that context (i.e. the last block). *)
type michelson_validation_config = {
  get_balance : Tezos_types.Contract.t -> Z.t tzresult Lwt.t;
  get_counter : Tezos_types.Contract.t -> Z.t tzresult Lwt.t;
}

type evm_validation_config = {
  minimum_base_fee_per_gas : Ethereum_types.quantity;
  base_fee_per_gas : Ethereum_types.quantity;
  maximum_gas_limit : Ethereum_types.quantity;
  da_fee_per_byte : Ethereum_types.quantity;
  next_nonce :
    Ethereum_types.address -> Ethereum_types.quantity option tzresult Lwt.t;
  balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t;
}

(** Current state of a blueprint validation. Maintains a cache of balances and
    counter, created from the context (i.e. the last block) and updated with
    the operations/transactions already included in the blueprint. *)
type validation_state = {
  michelson_config : michelson_validation_config;
  evm_config : evm_validation_config;
  addr_balance : Z.t String.Map.t;
  addr_nonce : Z.t String.Map.t;
  gas : Z.t;
  current_size : int;
}

let dummy_michelson_config =
  let open Lwt_result_syntax in
  {
    get_balance = (fun _ -> return Z.zero);
    get_counter = (fun _ -> return Z.zero);
  }

let dummy_evm_config =
  let open Lwt_result_syntax in
  {
    minimum_base_fee_per_gas = Qty Z.zero;
    base_fee_per_gas = Qty Z.zero;
    maximum_gas_limit = Qty Z.zero;
    da_fee_per_byte = Qty Z.zero;
    next_nonce = (fun _addr -> return None);
    balance = (fun _addr -> return (Ethereum_types.Qty Z.zero));
  }

let empty_validation_state ~evm_config ~michelson_config =
  {
    evm_config;
    michelson_config;
    addr_balance = String.Map.empty;
    addr_nonce = String.Map.empty;
    gas = Z.zero;
    current_size = 0;
  }
