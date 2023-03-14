(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type CONV = sig
  module V_from : S.COMMON_SIGNATURE

  module V_to : S.COMMON_SIGNATURE

  val public_key_hash : V_from.Public_key_hash.t -> V_to.Public_key_hash.t

  val public_key : V_from.Public_key.t -> V_to.Public_key.t

  val secret_key : V_from.Secret_key.t -> V_to.Secret_key.t

  val signature : V_from.t -> V_to.t
end

module type CONV_OPT = sig
  module V_from : S.COMMON_SIGNATURE

  module V_to : S.COMMON_SIGNATURE

  val public_key_hash :
    V_from.Public_key_hash.t -> V_to.Public_key_hash.t option

  val public_key : V_from.Public_key.t -> V_to.Public_key.t option

  val secret_key : V_from.Secret_key.t -> V_to.Secret_key.t option

  val signature : V_from.t -> V_to.t option
end

module V_latest = Signature_v1

module V0 = struct
  include Signature_v0

  module Of_V_latest :
    CONV_OPT with module V_from := V_latest and module V_to := Signature_v0 =
  struct
    let public_key_hash : V_latest.Public_key_hash.t -> Public_key_hash.t option
        = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls _ -> None

    let public_key : V_latest.Public_key.t -> Public_key.t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls _ -> None

    let secret_key : V_latest.Secret_key.t -> Secret_key.t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls _ -> None

    let signature : V_latest.t -> t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Unknown k -> Some (Unknown k)
      | V_latest.Bls _ -> None
  end
end

module V1 = struct
  include Signature_v1

  module Of_V_latest :
    CONV_OPT with module V_from := V_latest and module V_to := Signature_v1 =
  struct
    let public_key_hash = Option.some

    let public_key = Option.some

    let secret_key = Option.some

    let signature = Option.some
  end
end

include V_latest
module Of_V_latest = V1.Of_V_latest

module Of_V1 : CONV with module V_from := V1 and module V_to := V1 = struct
  let public_key_hash = Fun.id

  let public_key = Fun.id

  let secret_key = Fun.id

  let signature = Fun.id
end
