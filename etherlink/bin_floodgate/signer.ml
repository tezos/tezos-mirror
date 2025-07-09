(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Libsecp256k1.External

let ctxt = Efunc_core.Eth.Crypto.context ()

type secret_key = Key.secret Key.t

module Secret_key = struct
  type t = secret_key

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
  type t = Key.public Key.t

  let from_sk (sk : Secret_key.t) = Key.neuterize_exn ctxt sk

  let to_address pk =
    let addr = (Efunc_core.Crypto.to_address pk :> string) in
    Ethereum_types.Address (Ethereum_types.hex_of_string addr)
end

type t = Wallet of {public_key : Public_key.t; secret_key : secret_key}

let from_secret_key secret_key =
  let public_key = Public_key.from_sk secret_key in
  Wallet {public_key; secret_key}

let to_address = function
  | Wallet {public_key; _} -> Public_key.to_address public_key

let fresh () =
  let secret_key = Secret_key.fresh () in
  from_secret_key secret_key

let secret_key_from_hex (`Hex hex) = Secret_key.from_hex_string hex

let sign signer payload =
  match signer with
  | Wallet {secret_key; _} -> Efunc_core.Crypto.sign secret_key payload
