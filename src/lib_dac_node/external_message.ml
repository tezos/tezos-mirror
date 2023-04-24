(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech  <contact@trili.tech>                  *)
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

type t = {
  root_hash : Dac_plugin.raw_hash;
  signature : Tezos_crypto.Aggregate_signature.signature;
  witnesses : Z.t;
      (** TODO: https://gitlab.com/tezos/tezos/-/issues/4853
      Switch to BitSet.t which is ideal but it is not exposed outside 
      the Protocol. *)
}

module Make (Encoding_metadata : sig
  val tag : int

  val title : string
end) =
struct
  let untagged_encoding dac_hash_encoding =
    Data_encoding.(
      conv
        (function
          | {root_hash; signature; witnesses} ->
              (root_hash, signature, witnesses))
        (fun (root_hash, signature, witnesses) ->
          {root_hash; signature; witnesses})
        (obj3
           (req "root_hash" dac_hash_encoding)
           (req "signature" Tezos_crypto.Aggregate_signature.encoding)
           (req "witnesses" z)))

  let dac_message_encoding encoding =
    Data_encoding.(
      union
        ~tag_size:`Uint8
        [
          case
            ~title:("dac_message_" ^ Encoding_metadata.title)
            (Tag Encoding_metadata.tag)
            (untagged_encoding encoding)
            (fun msg -> Some msg)
            (fun msg -> msg);
        ])

  let make ((module P) : Dac_plugin.t) root_hash signature witnesses =
    let open Lwt_result_syntax in
    let message = {root_hash; signature; witnesses} in
    let res =
      Data_encoding.Binary.to_bytes
        (dac_message_encoding Dac_plugin.raw_hash_encoding)
        message
    in
    match res with
    | Ok bytes -> return bytes
    | Error _ ->
        tzfail
        @@ Could_not_serialize_rollup_external_message
             (Dac_plugin.raw_hash_to_hex root_hash)

  let of_bytes dac_hash_encoding encoded_message =
    Data_encoding.Binary.of_bytes_opt
      (dac_message_encoding dac_hash_encoding)
      encoded_message
end

module Default = Make (struct
  (** TODO: https://gitlab.com/tezos/tezos/-/issues/4854
      Use a more ideal tag. *)
  let tag = 42

  let title = "reveal_hash_v0"
end)
