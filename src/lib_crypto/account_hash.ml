(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad
include Signature.Public_key_hash

type error += Invalid_notation of string (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"account_hash_invalid_notation"
    ~title:"Account hash invalid notation"
    ~description:"Account hash invalid notation"
    ~pp:(fun ppf s -> Format.fprintf ppf "Account hash invalid notation: %s" s)
    (obj1 (req "hash" string))
    (function Invalid_notation s -> Some s | _ -> None)
    (fun s -> Invalid_notation s)

let of_b58data : Base58.data -> Signature.public_key_hash option = function
  | Ed25519.Public_key_hash.Data h -> Some (Signature.Ed25519 h)
  | Secp256k1.Public_key_hash.Data h -> Some (Signature.Secp256k1 h)
  | P256.Public_key_hash.Data h -> Some (Signature.P256 h)
  | Bls.Public_key_hash.Data h -> Some (Signature.Bls h)
  | Mldsa44.Public_key_hash.Data h -> Some (Signature.Mldsa44 h)
  | _ -> None

let of_b58check s =
  let open Error_monad.Result_syntax in
  match of_b58check_opt s with
  | Some c -> return c
  | None -> tzfail (Invalid_notation s)

let is_tz5 = function
  | (Signature.Mldsa44 _ : Signature.Public_key_hash.t) -> true
  | _ -> false

module Make_hex (H : sig
  type t

  val to_bytes : t -> bytes

  val of_bytes_opt : bytes -> t option
end) =
struct
  let path_length = 1

  let to_path t l =
    let (`Hex key) = Hex.of_bytes (H.to_bytes t) in
    key :: l

  let of_path = function
    | [path] -> Option.bind (Hex.to_bytes (`Hex path)) H.of_bytes_opt
    | _ -> None
end

module Path = struct
  module Path_Ed25519 = Make_hex (Ed25519.Public_key_hash)
  module Path_Secp256k1 = Make_hex (Secp256k1.Public_key_hash)
  module Path_P256 = Make_hex (P256.Public_key_hash)
  module Path_Bls = Make_hex (Bls.Public_key_hash)
  module Path_Mldsa44 = Make_hex (Mldsa44.Public_key_hash)

  let to_path (key : t) l =
    match key with
    | Ed25519 h -> "ed25519" :: Path_Ed25519.to_path h l
    | Secp256k1 h -> "secp256k1" :: Path_Secp256k1.to_path h l
    | P256 h -> "p256" :: Path_P256.to_path h l
    | Bls h -> "bls" :: Path_Bls.to_path h l
    | Mldsa44 h -> "mldsa44" :: Path_Mldsa44.to_path h l

  let of_path : string list -> t option = function
    | "ed25519" :: rest -> (
        match Path_Ed25519.of_path rest with
        | Some pkh -> Some (Ed25519 pkh)
        | None -> None)
    | "secp256k1" :: rest -> (
        match Path_Secp256k1.of_path rest with
        | Some pkh -> Some (Secp256k1 pkh)
        | None -> None)
    | "p256" :: rest -> (
        match Path_P256.of_path rest with
        | Some pkh -> Some (P256 pkh)
        | None -> None)
    | "bls" :: rest -> (
        match Path_Bls.of_path rest with
        | Some pkh -> Some (Bls pkh)
        | None -> None)
    | "mldsa44" :: rest -> (
        match Path_Mldsa44.of_path rest with
        | Some pkh -> Some (Mldsa44 pkh)
        | None -> None)
    | _ -> None

  let path_length =
    let l1 = Path_Ed25519.path_length
    and l2 = Path_Secp256k1.path_length
    and l3 = Path_P256.path_length
    and l4 = Path_Bls.path_length
    and l5 = Path_Mldsa44.path_length in
    assert (Compare.Int.(l1 = l2 && l2 = l3 && l3 = l4 && l4 = l5)) ;
    l1 + 1
end

module Forbidden = struct
  let of_pkh x = x

  let to_pkh x = x
end
