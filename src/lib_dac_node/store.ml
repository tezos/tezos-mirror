(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech  <contact@trili.tech>                       *)
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

module Irmin_store = struct
  module IStore = Irmin_store.Make (struct
    let name = "Tezos DAC (Data Availability Committee) Node"
  end)

  include IStore
  include Store_utils.Make (IStore)
end

module Signature_store =
  Irmin_store.Make_nested_map
    (struct
      let path = ["coordinator"; "signature_store"]
    end)
    (struct
      type key = Dac_plugin.hash

      let to_path_representation key = Hex.show @@ Dac_plugin.hash_to_hex key
    end)
    (struct
      type key = Tezos_crypto.Aggregate_signature.public_key_hash

      let name = "Committee member public key hash"

      let compare = Tezos_crypto.Aggregate_signature.Public_key_hash.compare

      let encoding = Tezos_crypto.Aggregate_signature.Public_key_hash.encoding
    end)
    (struct
      type value = Tezos_crypto.Aggregate_signature.signature

      let name = "Committee member signature of root hash"

      let encoding = Tezos_crypto.Aggregate_signature.encoding
    end)

module Certificate_store =
  Irmin_store.Make_updatable_map
    (struct
      let path = ["coordinator"; "certificate_store"]
    end)
    (struct
      type key = Dac_plugin.raw_hash

      let to_path_representation key =
        Hex.show @@ Dac_plugin.raw_hash_to_hex key
    end)
    (struct
      type value = Certificate_repr.t

      let name = "Data availability certificate for root hash"

      let encoding = Certificate_repr.encoding
    end)
