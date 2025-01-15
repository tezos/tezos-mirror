(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let ctxt = Efunc_core.Eth.Crypto.context ()

module Secret_key = struct
  open Libsecp256k1.External

  type t = Key.secret Key.t

  let fresh () =
    let random = Mirage_crypto_rng.generate 32 |> Bigstring.of_string in
    Key.read_sk_exn ctxt random

  let from_hex_string v =
    let open Result_syntax in
    try
      match (String.sub v 0 2, String.sub v 2 (String.length v - 2)) with
      | "0x", value ->
          let res =
            Libsecp256k1.External.Key.read_sk_exn ctxt
            @@ Hex.(to_bigstring_exn (`Hex value))
          in
          return res
      | _, _ -> raise (Invalid_argument "secret_key_from_hex_value")
    with _ ->
      fail [error_of_fmt "%s is not a valid hexa-encoded secret key" v]
end

module Public_key = struct
  open Libsecp256k1.External

  type t = Key.public Key.t

  let from_sk (sk : Secret_key.t) = Key.neuterize_exn ctxt sk

  let to_address pk =
    let addr = (Efunc_core.Crypto.to_address pk :> string) in
    Ethereum_types.Address (Ethereum_types.hex_of_string addr)
end

type t = {
  mutable nonce : Z.t;
  mutable balance : Z.t;
  public_key : Public_key.t;
  secret_key : Secret_key.t;
}

let from_secret_key ~evm_node_endpoint secret_key =
  let open Lwt_result_syntax in
  let public_key = Public_key.from_sk secret_key in
  let* (Qty nonce) =
    Batch.call
      (module Rpc_encodings.Get_transaction_count)
      ~keep_alive:true
      ~evm_node_endpoint
      (Public_key.to_address public_key, Block_parameter Latest)
  in
  let+ (Qty balance) =
    Batch.call
      (module Rpc_encodings.Get_balance)
      ~keep_alive:true
      ~evm_node_endpoint
      (Public_key.to_address public_key, Block_parameter Latest)
  in
  {nonce; balance; public_key; secret_key}

let increment_nonce account = account.nonce <- Z.succ account.nonce

let credit account x = account.balance <- Z.add account.balance x

let debit account x = account.balance <- Z.sub account.balance x

let address {public_key; _} = Efunc_core.Crypto.to_address public_key

let address_et account =
  Ethereum_types.Address (Hex (address account :> string))

let fresh () =
  let secret_key = Secret_key.fresh () in
  let public_key = Public_key.from_sk secret_key in
  {nonce = Z.zero; balance = Z.zero; secret_key; public_key}

let pp_pkh fmt account =
  Format.pp_print_string
    fmt
    (address_et account |> Ethereum_types.Address.to_string)
