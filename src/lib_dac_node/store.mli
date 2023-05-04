(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech  <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold  <contact@marigold.dev>                       *)
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

(** [Irmin_store] representation for Dac node. *)
module Irmin_store : sig
  include Store_sigs.Store

  include Store_sigs.BACKEND
end

(** [Signature_store] is a nested map where [primary_key]s are root_hashes and
    [secondary_key]s are dac member public key hashes. Root hashes are strings
    instead of [Dac_hash.t] because we want to avoid runtime functorization of
    the module. 
  *)
module Signature_store :
  Store_sigs.Nested_map
    with type 'a store = 'a Irmin_store.t
     and type primary_key = Dac_plugin.hash
     and type secondary_key = Tezos_crypto.Aggregate_signature.public_key_hash
     and type value = Tezos_crypto.Aggregate_signature.signature

type certificate_store_value = {
  aggregate_signature : Tezos_crypto.Aggregate_signature.signature;
  witnesses : Z.t;
}

(** Key-value store for Dac certificates where keys are hexified [Dac_hash.t]
    and values are [Certificate_repr.V0.t]. *)
module Certificate_store :
  Store_sigs.Map
    with type 'a store = 'a Irmin_store.t
     and type key = Dac_plugin.hash
     and type value = certificate_store_value
