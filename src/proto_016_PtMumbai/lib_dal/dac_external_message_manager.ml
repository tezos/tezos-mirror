(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

open Protocol
open Environment.Error_monad

type error += Could_not_serialize_rollup_external_message of string

let () =
  register_error_kind
    `Permanent
    ~id:"dac_could_not_serialize_rollup_external_message"
    ~title:"Could not serialize rollup external message"
    ~description:
      "Serialization of rollup external message containing Dac root page hash \
       failed"
    ~pp:(fun ppf b58_hash ->
      Format.fprintf
        ppf
        "Serialization of rollup external message containing Dac root page \
         hash %sfailed"
        b58_hash)
    Data_encoding.(obj1 (req "hash" string))
    (function
      | Could_not_serialize_rollup_external_message b58_hash -> Some b58_hash
      | _ -> None)
    (fun b58_hash -> Could_not_serialize_rollup_external_message b58_hash)

module type REVEAL_HASH = module type of Sc_rollup_reveal_hash

module Make
    (Hashing_scheme : REVEAL_HASH) (Encoding_metadata : sig
      val tag : int

      val title : string
    end) =
struct
  type dac_message =
    | Dac_message of {
        root_hash : Hashing_scheme.t;
        signature : Tezos_crypto.Aggregate_signature.t;
        witnesses : Bitset.t;
      }

  let untagged_encoding =
    Data_encoding.(
      conv
        (function
          | Dac_message {root_hash; signature; witnesses} ->
              (root_hash, signature, witnesses))
        (fun (root_hash, signature, witnesses) ->
          Dac_message {root_hash; signature; witnesses})
        (obj3
           (req "root_hash" Hashing_scheme.encoding)
           (req "signature" Tezos_crypto.Aggregate_signature.encoding)
           (req "witnesses" Bitset.encoding)))

  let dac_message_encoding =
    Data_encoding.(
      union
        ~tag_size:`Uint8
        [
          case
            ~title:("dac_message_" ^ Encoding_metadata.title)
            (Tag Encoding_metadata.tag)
            untagged_encoding
            (fun msg -> Some msg)
            (fun msg -> msg);
        ])

  let make root_hash signature witnesses =
    let message = Dac_message {root_hash; signature; witnesses} in
    let res = Data_encoding.Binary.to_bytes dac_message_encoding message in
    match res with
    | Ok bytes -> Ok bytes
    | Error _ ->
        error
        @@ Could_not_serialize_rollup_external_message
             (Hashing_scheme.to_b58check root_hash)

  let of_bytes encoded_message =
    Data_encoding.Binary.of_bytes_opt dac_message_encoding encoded_message
end

module Reveal_hash =
  Make
    (Sc_rollup_reveal_hash)
    (struct
      let tag = 42

      let title = "reveal_hash_v0"
    end)
