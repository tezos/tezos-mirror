(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

(** Representation of committee member signature. *)
type t = {
  root_hash : Dac_plugin.hash;
  signature : Tezos_crypto.Aggregate_signature.t;
  signer_pkh : Tezos_crypto.Aggregate_signature.public_key_hash;
}

(** [encoding dac_plugin] returns the [t Data_encoding.t] given a [dac_plugin]. *)
let encoding ((module P) : Dac_plugin.t) =
  Data_encoding.(
    conv
      (fun {root_hash; signature; signer_pkh} ->
        (root_hash, signature, signer_pkh))
      (fun (root_hash, signature, signer_pkh) ->
        {root_hash; signature; signer_pkh})
      (obj3
         (req "root_hash" P.encoding)
         (req "signature" Tezos_crypto.Aggregate_signature.encoding)
         (req
            "signer_pkh"
            Tezos_crypto.Aggregate_signature.Public_key_hash.encoding)))
