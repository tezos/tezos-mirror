(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

type t = {
  root_hash : Dac_plugin.raw_hash;
  signature : Tezos_crypto.Aggregate_signature.t;
  signer_pkh : Tezos_crypto.Aggregate_signature.public_key_hash;
}

let encoding =
  Data_encoding.(
    conv
      (fun {root_hash; signature; signer_pkh} ->
        (root_hash, signature, signer_pkh))
      (fun (root_hash, signature, signer_pkh) ->
        {root_hash; signature; signer_pkh})
      (obj3
         (req "root_hash" Dac_plugin.raw_hash_encoding)
         (req "signature" Tezos_crypto.Aggregate_signature.encoding)
         (req
            "signer_pkh"
            Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)))

(** [ensure_unknown_aggregate_sig_variant sig_rep] ensures that the underlying
    [Aggregate_signature.t] always corresponds to [Unknown] variant. 
    
    Motivation: 
    [Aggregate_signature.t] has two variants: [Bls12_381 of Bls.t] and
    [Unknown of Bytes.t]. Their binary repr is exactly the same. In json though,
    the encoding adds a prefix. For [Bls12_381] variant it adds the "BLsig"
    prefix and for [Unknown] variant it adds the "asig" prefix.
    In practice this means that when signature is computed by Committee member,
    it will have "Blsig" json prefix. When read by Coordinator from the store,
    it will be read as [Unknown] variant and later served with "asig" prefix in
    JSON. This helper is used to guarantee consistent "asig" prefix. *)
let ensure_unknown_sig_variant signature =
  let open Tezos_crypto.Aggregate_signature in
  match signature with
  | Unknown _ as unknown -> unknown
  | Bls12_381 _ as bls -> Unknown (to_bytes bls)

let make root_hash signature signer_pkh =
  {root_hash; signature = ensure_unknown_sig_variant signature; signer_pkh}

let get_root_hash {root_hash; _} = root_hash

let get_signature {signature; _} = signature

let get_signer_pkh {signer_pkh; _} = signer_pkh
