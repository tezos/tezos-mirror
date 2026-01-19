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

type version = Version_0 | Version_1 | Version_2 | Version_3

let version_encoding =
  let open Data_encoding in
  conv_with_guard
    (function
      | Version_0 -> 0 | Version_1 -> 1 | Version_2 -> 2 | Version_3 -> 3)
    (function
      | 0 -> Ok Version_0
      | 1 -> Ok Version_1
      | 2 -> Ok Version_2
      | 3 -> Ok Version_3
      | _ -> Error "Invalid signature version")
    int8

let version_arg =
  let open Tezos_rpc.Arg in
  make
    ~descr:"Supported signature version are version '0','1' and '2'(default)"
    ~name:"version"
    ~destruct:(function
      | "0" -> Ok Version_0
      | "1" -> Ok Version_1
      | "2" -> Ok Version_2
      | "3" -> Ok Version_3
      | _ -> Error "Invalid signature version")
    ~construct:(function
      | Version_0 -> "0"
      | Version_1 -> "1"
      | Version_2 -> "2"
      | Version_3 -> "3")
    ()

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

  val get_public_key :
    V_from.Public_key.t -> V_to.Public_key.t Error_monad.tzresult

  val get_public_key_exn : V_from.Public_key.t -> V_to.Public_key.t

  val get_public_key_hash :
    V_from.Public_key_hash.t -> V_to.Public_key_hash.t Error_monad.tzresult

  val get_public_key_hash_exn :
    V_from.Public_key_hash.t -> V_to.Public_key_hash.t

  val get_secret_key :
    V_from.Secret_key.t -> V_to.Secret_key.t Error_monad.tzresult

  val get_secret_key_exn : V_from.Secret_key.t -> V_to.Secret_key.t

  val get_signature : V_from.t -> V_to.t Error_monad.tzresult

  val get_signature_exn : V_from.t -> V_to.t
end

module V_latest = struct
  let version = Version_3

  include Signature_v3
end

module V0 = struct
  include Signature_v0

  let version = Version_0

  module Of_V_latest :
    CONV_OPT with module V_from := V_latest and module V_to := Signature_v0 =
  struct
    let public_key_hash : V_latest.Public_key_hash.t -> Public_key_hash.t option
        = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls _ -> None
      | V_latest.Mldsa44 _ -> None

    let public_key : V_latest.Public_key.t -> Public_key.t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls _ -> None
      | V_latest.Mldsa44 _ -> None

    let secret_key : V_latest.Secret_key.t -> Secret_key.t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls _ -> None
      | V_latest.Mldsa44 _ -> None

    let signature : V_latest.t -> t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Unknown k -> Some (Unknown k)
      | V_latest.Bls _ -> None
      | V_latest.Mldsa44 _ -> None

    let get_public_key pk =
      match public_key pk with
      | Some pk -> Ok pk
      | None ->
          Error_monad.error_with
            "Conversion of public key from latest signature version to V0 \
             impossible."

    let get_public_key_exn pk =
      match public_key pk with
      | Some pk -> pk
      | None ->
          Stdlib.failwith
            "Conversion of public key hash from latest signature version to V0 \
             impossible."

    let get_public_key_hash pkh =
      match public_key_hash pkh with
      | Some pkh -> Ok pkh
      | None ->
          Error_monad.error_with
            "Conversion of public key hash from latest signature version to V0 \
             impossible."

    let get_public_key_hash_exn pkh =
      match public_key_hash pkh with
      | Some pkh -> pkh
      | None ->
          Stdlib.failwith
            "Conversion of public key hash from latest signature version to V0 \
             impossible."

    let get_secret_key s =
      match secret_key s with
      | Some s -> Ok s
      | None ->
          Error_monad.error_with
            "Conversion of secret key from latest signature version to V0 \
             impossible."

    let get_secret_key_exn s =
      match secret_key s with
      | Some s -> s
      | None ->
          Stdlib.failwith
            "Conversion of secret key from latest signature version to V0 \
             impossible."

    let get_signature s =
      match signature s with
      | Some s -> Ok s
      | None ->
          Error_monad.error_with
            "Conversion of signature from latest signature version to V0 \
             impossible."

    let get_signature_exn s =
      match signature s with
      | Some s -> s
      | None ->
          Stdlib.failwith
            "Conversion of signature from latest signature version to V0 \
             impossible."
  end
end

module V1 = struct
  include Signature_v1

  let version = Version_1

  module Of_V_latest :
    CONV_OPT with module V_from := V_latest and module V_to := Signature_v1 =
  struct
    let public_key_hash : V_latest.Public_key_hash.t -> Public_key_hash.t option
        = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls k -> Some (Bls k)
      | V_latest.Mldsa44 _ -> None

    let public_key : V_latest.Public_key.t -> Public_key.t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls k -> Some (Bls k)
      | V_latest.Mldsa44 _ -> None

    let secret_key : V_latest.Secret_key.t -> Secret_key.t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls k -> Some (Bls k)
      | V_latest.Mldsa44 _ -> None

    let signature : V_latest.t -> t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Unknown k -> Some (Unknown k)
      | V_latest.Bls k -> Some (Bls k)
      | V_latest.Mldsa44 _ -> None

    let get_public_key pk =
      match public_key pk with
      | Some pk -> Ok pk
      | None ->
          Error_monad.error_with
            "Conversion of public key from latest signature version to V1 \
             impossible."

    let get_public_key_exn pk =
      match public_key pk with
      | Some pk -> pk
      | None ->
          Stdlib.failwith
            "Conversion of public key hash from latest signature version to V1 \
             impossible."

    let get_public_key_hash pkh =
      match public_key_hash pkh with
      | Some pkh -> Ok pkh
      | None ->
          Error_monad.error_with
            "Conversion of public key hash from latest signature version to V1 \
             impossible."

    let get_public_key_hash_exn pkh =
      match public_key_hash pkh with
      | Some pkh -> pkh
      | None ->
          Stdlib.failwith
            "Conversion of public key hash from latest signature version to V1 \
             impossible."

    let get_secret_key s =
      match secret_key s with
      | Some s -> Ok s
      | None ->
          Error_monad.error_with
            "Conversion of secret key from latest signature version to V1 \
             impossible."

    let get_secret_key_exn s =
      match secret_key s with
      | Some s -> s
      | None ->
          Stdlib.failwith
            "Conversion of secret key from latest signature version to V1 \
             impossible."

    let get_signature s =
      match signature s with
      | Some s -> Ok s
      | None ->
          Error_monad.error_with
            "Conversion of signature from latest signature version to V1 \
             impossible."

    let get_signature_exn s =
      match signature s with
      | Some s -> s
      | None ->
          Stdlib.failwith
            "Conversion of signature from latest signature version to V1 \
             impossible."
  end
end

module V2 = struct
  include Signature_v2

  let version = Version_2

  module Of_V_latest :
    CONV_OPT with module V_from := V_latest and module V_to := Signature_v2 =
  struct
    let public_key_hash : V_latest.Public_key_hash.t -> Public_key_hash.t option
        = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls k -> Some (Bls k)
      | V_latest.Mldsa44 _ -> None

    let public_key : V_latest.Public_key.t -> Public_key.t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls k -> Some (Bls k)
      | V_latest.Mldsa44 _ -> None

    let secret_key : V_latest.Secret_key.t -> Secret_key.t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Bls k -> Some (Bls k)
      | V_latest.Mldsa44 _ -> None

    let signature : V_latest.t -> t option = function
      | V_latest.Ed25519 k -> Some (Ed25519 k)
      | V_latest.Secp256k1 k -> Some (Secp256k1 k)
      | V_latest.P256 k -> Some (P256 k)
      | V_latest.Unknown k -> Some (Unknown k)
      | V_latest.Bls k -> Some (Bls k)
      | V_latest.Mldsa44 _ -> None

    let get_public_key pk =
      match public_key pk with
      | Some pk -> Ok pk
      | None ->
          Error_monad.error_with
            "Conversion of public key from latest signature version to V2 \
             impossible."

    let get_public_key_exn pk =
      match public_key pk with
      | Some pk -> pk
      | None ->
          Stdlib.failwith
            "Conversion of public key hash from latest signature version to V2 \
             impossible."

    let get_public_key_hash pkh =
      match public_key_hash pkh with
      | Some pkh -> Ok pkh
      | None ->
          Error_monad.error_with
            "Conversion of public key hash from latest signature version to V2 \
             impossible."

    let get_public_key_hash_exn pkh =
      match public_key_hash pkh with
      | Some pkh -> pkh
      | None ->
          Stdlib.failwith
            "Conversion of public key hash from latest signature version to V2 \
             impossible."

    let get_secret_key s =
      match secret_key s with
      | Some s -> Ok s
      | None ->
          Error_monad.error_with
            "Conversion of secret key from latest signature version to V2 \
             impossible."

    let get_secret_key_exn s =
      match secret_key s with
      | Some s -> s
      | None ->
          Stdlib.failwith
            "Conversion of secret key from latest signature version to V2 \
             impossible."

    let get_signature s =
      match signature s with
      | Some s -> Ok s
      | None ->
          Error_monad.error_with
            "Conversion of signature from latest signature version to V2 \
             impossible."

    let get_signature_exn s =
      match signature s with
      | Some s -> s
      | None ->
          Stdlib.failwith
            "Conversion of signature from latest signature version to V2 \
             impossible."
  end
end

module V3 = struct
  include Signature_v3

  let version = Version_3

  module Of_V_latest :
    CONV_OPT with module V_from := V_latest and module V_to := Signature_v3 =
  struct
    let public_key_hash = Option.some

    let public_key = Option.some

    let secret_key = Option.some

    let signature = Option.some

    let get_public_key pk =
      match public_key pk with
      | Some pk -> Ok pk
      | None ->
          Error_monad.error_with
            "Conversion of public key from latest signature version to V3 \
             impossible."

    let get_public_key_exn pk =
      match public_key pk with
      | Some pk -> pk
      | None ->
          Stdlib.failwith
            "Conversion of public key hash from latest signature version to V3 \
             impossible."

    let get_public_key_hash pkh =
      match public_key_hash pkh with
      | Some pkh -> Ok pkh
      | None ->
          Error_monad.error_with
            "Conversion of public key hash from latest signature version to V3 \
             impossible."

    let get_public_key_hash_exn pkh =
      match public_key_hash pkh with
      | Some pkh -> pkh
      | None ->
          Stdlib.failwith
            "Conversion of public key hash from latest signature version to V3 \
             impossible."

    let get_secret_key s =
      match secret_key s with
      | Some s -> Ok s
      | None ->
          Error_monad.error_with
            "Conversion of secret key from latest signature version to V3 \
             impossible."

    let get_secret_key_exn s =
      match secret_key s with
      | Some s -> s
      | None ->
          Stdlib.failwith
            "Conversion of secret key from latest signature version to V3 \
             impossible."

    let get_signature s =
      match signature s with
      | Some s -> Ok s
      | None ->
          Error_monad.error_with
            "Conversion of signature from latest signature version to V3 \
             impossible."

    let get_signature_exn s =
      match signature s with
      | Some s -> s
      | None ->
          Stdlib.failwith
            "Conversion of signature from latest signature version to V3 \
             impossible."
  end
end

include V_latest
module Of_V_latest = V3.Of_V_latest

module Of_V3 : CONV with module V_from := V3 and module V_to := V_latest =
struct
  let public_key_hash = Fun.id

  let public_key = Fun.id

  let secret_key = Fun.id

  let signature = Fun.id
end
