(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Libsecp256k1.External
open Efunc_core

let ctxt = Eth.Crypto.context ()

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
    let addr = (Crypto.to_address pk :> string) in
    Ethereum_types.Address (Ethereum_types.hex_of_string addr)
end

let unwrap_et_address Ethereum_types.(Address (Hex addr)) = Types.a addr

type t =
  | Wallet of {public_key : Public_key.t; secret_key : secret_key}
  | Gcp_kms of Gcp_kms.t

let from_secret_key secret_key =
  let public_key = Public_key.from_sk secret_key in
  Wallet {public_key; secret_key}

let from_gcp_key gcp_kms gcp_key =
  let open Lwt_result_syntax in
  let* kms = Gcp_kms.from_gcp_key gcp_kms gcp_key in
  let*? _ =
    (* We just check that the key is of the correct signature algorithm *)
    Option.to_result ~none:[error_of_fmt "Not an Ethereum-compatible key"]
    @@ Gcp_kms.ethereum_address_opt kms
  in
  return (Gcp_kms kms)

let to_address = function
  | Wallet {public_key; _} -> Public_key.to_address public_key
  | Gcp_kms kms ->
      WithExceptions.Option.get ~loc:__LOC__ (Gcp_kms.ethereum_address_opt kms)

let fresh () =
  let secret_key = Secret_key.fresh () in
  from_secret_key secret_key

let secret_key_from_hex (`Hex hex) = Secret_key.from_hex_string hex

let try_v r s v hash (addr : Types.address) =
  let s0 = Types.{r; s; v} in
  let addr' = Crypto.(recover s0 (Bigstring.of_string hash) |> to_address) in
  String.equal (addr' :> string) (addr :> string)

let sign signer payload =
  let open Lwt_result_syntax in
  match signer with
  | Wallet {secret_key; _} -> return (Crypto.sign secret_key payload)
  | Gcp_kms kms -> (
      let* signature =
        Gcp_kms.sign kms Keccak256 (Bytes.unsafe_of_string payload)
      in
      let hash = Digestif.KECCAK_256.(to_raw_string @@ digest_string payload) in
      match signature with
      | Secp256k1 s ->
          let addr = unwrap_et_address @@ to_address signer in
          let s_bytes = Signature.Secp256k1.to_bytes s in
          let r, s =
            Bytes.
              ( sub s_bytes 0 32 |> Hex.of_bytes |> Hex.show |> Types.b,
                sub s_bytes 32 32 |> Hex.of_bytes |> Hex.show |> Types.b )
          in
          (* We need to recompute v now, as GCP does not give it to us *)
          if try_v r s 0 hash addr then return Types.{r; s; v = 0}
          else if try_v r s 1 hash addr then return Types.{r; s; v = 1}
          else failwith "Could not recover v from GCP’s signature"
      | _ -> assert false)
