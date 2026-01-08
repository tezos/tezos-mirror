(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {mutable nonce : Z.t; mutable balance : Z.t; signer : Signer.t}

let timeout = 10.

let from_signer ~evm_node_endpoint signer =
  let open Lwt_result_syntax in
  let* (Qty nonce) =
    Batch.call
      (module Rpc_encodings.Get_transaction_count)
      ~keep_alive:true
      ~timeout
      ~evm_node_endpoint
      (Signer.to_address signer, Block_parameter Latest)
  in
  let+ (Qty balance) =
    Batch.call
      (module Rpc_encodings.Get_balance)
      ~keep_alive:true
      ~timeout
      ~evm_node_endpoint
      (Signer.to_address signer, Block_parameter Latest)
  in
  {nonce; balance; signer}

let increment_nonce account = account.nonce <- Z.succ account.nonce

let credit account x = account.balance <- Z.add account.balance x

let debit account x = account.balance <- Z.sub account.balance x

let address_et {signer; _} = Signer.to_address signer

let address account =
  let (Address (Hex address)) = address_et account in
  Efunc_core.Types.a address

let fresh () =
  let signer = Signer.fresh () in
  {nonce = Z.zero; balance = Z.zero; signer}

let pp_pkh fmt account =
  Format.pp_print_string
    fmt
    (address_et account |> Ethereum_types.Address.to_string)
