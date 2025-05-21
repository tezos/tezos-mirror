(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let signature_descr =
  "Must be provided if the signer requires authentication. In this case, it \
   must be the signature of the public key hash and message concatenated, by \
   one of the keys authorized by the signer."

let query =
  let open Tezos_rpc.Query in
  query (fun signature -> signature)
  |+ opt_field
       ~descr:signature_descr
       "authentication"
       Tezos_crypto.Signature.rpc_arg
       (fun signature -> signature)
  |> seal

let sign_query =
  let open Tezos_rpc.Query in
  query (fun signature version -> (signature, version))
  |+ opt_field
       ~descr:signature_descr
       "authentication"
       Tezos_crypto.Signature.rpc_arg
       fst
  |+ opt_field
       ~descr:
         "Define the signature version that need to be used to sign the data"
       "version"
       Tezos_crypto.Signature.version_arg
       snd
  |> seal

let sign =
  Tezos_rpc.Service.post_service
    ~description:"Sign a piece of data with a given remote key"
    ~query:sign_query
    ~input:Data_encoding.bytes
    ~output:
      Data_encoding.(obj1 (req "signature" Tezos_crypto.Signature.encoding))
    Tezos_rpc.Path.(
      root / "keys" /: Tezos_crypto.Signature.Public_key_hash.rpc_arg)

let deterministic_nonce =
  Tezos_rpc.Service.post_service
    ~description:
      "Obtain some random data generated deterministically from some piece of \
       data with a given remote key"
    ~query
    ~input:Data_encoding.bytes
    ~output:Data_encoding.(obj1 (req "deterministic_nonce" bytes))
    Tezos_rpc.Path.(
      root / "keys" /: Tezos_crypto.Signature.Public_key_hash.rpc_arg)

let deterministic_nonce_hash =
  Tezos_rpc.Service.post_service
    ~description:
      "Obtain the hash of some random data generated deterministically from \
       some piece of data with a given remote key"
    ~query
    ~input:Data_encoding.bytes
    ~output:Data_encoding.(obj1 (req "deterministic_nonce_hash" bytes))
    Tezos_rpc.Path.(
      root / "keys" /: Tezos_crypto.Signature.Public_key_hash.rpc_arg)

let supports_deterministic_nonces =
  Tezos_rpc.Service.get_service
    ~description:
      "Obtain whether the signing service supports the deterministic nonces \
       functionality"
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.(obj1 (req "supports_deterministic_nonces" bool))
    Tezos_rpc.Path.(
      root / "keys" /: Tezos_crypto.Signature.Public_key_hash.rpc_arg)

let bls_prove_possession =
  Tezos_rpc.Service.get_service
    ~description:"Obtain a proof of possession for a given remote key"
    ~query:Tezos_rpc.Query.empty
    ~output:
      Data_encoding.(
        obj1 (req "bls_prove_possession" Tezos_crypto.Signature.Bls.encoding))
    Tezos_rpc.Path.(
      root / "bls_prove_possession"
      /: Tezos_crypto.Signature.Public_key_hash.rpc_arg)

let public_key =
  Tezos_rpc.Service.get_service
    ~description:"Retrieve the public key of a given remote key"
    ~query:Tezos_rpc.Query.empty
    ~output:
      Data_encoding.(
        obj1 (req "public_key" Tezos_crypto.Signature.Public_key.encoding))
    Tezos_rpc.Path.(
      root / "keys" /: Tezos_crypto.Signature.Public_key_hash.rpc_arg)

let authorized_keys =
  Tezos_rpc.Service.get_service
    ~description:
      "Retrieve the public keys that can be used to authenticate signing \
       commands.\n\
       If the empty object is returned, the signer has been set to accept \
       unsigned commands."
    ~query:Tezos_rpc.Query.empty
    ~output:
      Data_encoding.(
        obj1
          (opt
             "authorized_keys"
             (list Tezos_crypto.Signature.Public_key_hash.encoding)))
    Tezos_rpc.Path.(root / "authorized_keys")

let known_keys =
  Tezos_rpc.Service.get_service
    ~description:"Retrieve the hashes of the public keys known to the signer."
    ~query:Tezos_rpc.Query.empty
    ~output:
      Data_encoding.(
        obj1
          (req
             "known_keys"
             (list Tezos_crypto.Signature.Public_key_hash.encoding)))
    Tezos_rpc.Path.(root / "known_keys")
