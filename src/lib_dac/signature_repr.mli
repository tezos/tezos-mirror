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

(** [t] is a representation of committee member signature. *)
type t

(** [encoding dac_plugin] returns the [t Data_encoding.t] given a [dac_plugin]. *)
val encoding : t Data_encoding.t

(** [make root_hash signature pkh] creates a [t]. In addition, it ensures that
    the underlying [Aggregate_signature.t] always corresponds to [Unknown]
    variant.
    
    Motivation for the [Unknown] variant:
    [Unknown] variant uses "asig" JSON prefix, whereas [Bls12_381] variant uses
    "BLsig" JSON prefix. To ensure consistent signature prefix, when data is
    exchanged accross the DAC network we use [Unknown] variant only. *)
val make :
  Dac_plugin.raw_hash ->
  Tezos_crypto.Aggregate_signature.signature ->
  Tezos_crypto.Aggregate_signature.public_key_hash ->
  t

(** [get_root_hash signature] returns a root hash of a given signature
    representation. *)
val get_root_hash : t -> Dac_plugin.raw_hash

(** [get_signature signature] returns an aggregated signature of a given
    signature representation. *)
val get_signature : t -> Tezos_crypto.Aggregate_signature.signature

(** [get_signer_pkh signature] returns a signer public key hash of a given
    signature representation. *)
val get_signer_pkh : t -> Tezos_crypto.Aggregate_signature.public_key_hash
