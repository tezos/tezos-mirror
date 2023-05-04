(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/5073
   Update Certificate repr to handle a dynamic dac.
*)
module V0 = struct
  (** Current version of the [Certificate]
      must be equal to the version of the module, 0 in this case. *)
  let version = 0

  (** Representation of a Data Availibility Committee Certificate.
     Type is private to make sure correct [version] is used.
     Use [make] function to create a [Certificate_repr.V0.t] *)
  type t = {
    version : int;
    root_hash : Dac_plugin.raw_hash;
    aggregate_signature : Tezos_crypto.Aggregate_signature.signature;
    witnesses : Z.t;
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4853
           Use BitSet for witnesses field in external message
        *)
  }

  let make root_hash aggregate_signature witnesses =
    {version; root_hash; aggregate_signature; witnesses}

  let encoding =
    let obj_enc =
      Data_encoding.(
        obj4
          (req "version" Data_encoding.uint8)
          (req "root_hash" Dac_plugin.raw_hash_encoding)
          (req "aggregate_signature" Tezos_crypto.Aggregate_signature.encoding)
          (req "witnesses" z))
    in
    Data_encoding.(
      conv
        (fun {version; root_hash; aggregate_signature; witnesses} ->
          (version, root_hash, aggregate_signature, witnesses))
        (fun (version, root_hash, aggregate_signature, witnesses) ->
          {version; root_hash; aggregate_signature; witnesses})
        obj_enc)

  let all_committee_members_have_signed committee_members {witnesses; _} =
    let length = List.length committee_members in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4562
       The following is equivalent to Bitset.fill length. The Bitset module
       should be used once it is moved from the protocol to the environment. *)
    let expected_witnesses = Z.(pred (shift_left one length)) in
    (* Equivalent to Bitset.diff expected_witnesses witnesses. *)
    let missing_witnesses = Z.logand expected_witnesses (Z.lognot witnesses) in
    Z.(equal missing_witnesses zero)
end
