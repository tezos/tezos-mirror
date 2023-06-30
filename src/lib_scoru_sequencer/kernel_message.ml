(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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
open Alpha_context

module Framed (Payload : sig
  type t

  val encoding : t Data_encoding.t

  val equal : t -> t -> bool
end) =
struct
  type t = {rollup_address : Sc_rollup_repr.Address.t; payload : Payload.t}

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Framed message of version 0"
          (obj3
             (req "version" @@ constant "0")
             (req "destination" Sc_rollup_repr.Address.encoding)
             (req "payload" Payload.encoding))
          (fun {rollup_address; payload} -> Some ((), rollup_address, payload))
          (fun ((), rollup_address, payload) -> {rollup_address; payload});
      ]

  let equal x y =
    Sc_rollup_repr.Address.equal x.rollup_address y.rollup_address
    && Payload.equal x.payload y.payload
end

module Signed (Payload : sig
  type t

  val encoding : t Data_encoding.t

  val equal : t -> t -> bool
end) =
struct
  type t = {unsigned_payload : Payload.t; signature : Signature.V0.t}

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {unsigned_payload; signature} -> (unsigned_payload, signature))
      (fun (unsigned_payload, signature) -> {unsigned_payload; signature})
    @@ obj2
         (req "unsigned_payload" Payload.encoding)
         (req "signature" Signature.V0.encoding)

  let equal (x : t) (y : t) =
    Signature.V0.equal x.signature y.signature
    && Payload.equal x.unsigned_payload y.unsigned_payload
end

type msg_body =
  | Sequence of {
      nonce : int32;
      (* Fix: it should be uint32 *)
      delayed_messages_prefix : int32;
      (* Fix: it should be uint32 *)
      delayed_messages_suffix : int32;
      (* Fix: it should be uint32 *)
      l2_messages : string list;
    }

module Framed_message = Framed (struct
  type t = msg_body

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Sequence message"
          (obj5
             (req "msg_type" @@ constant "sequence")
             (req "nonce" int32)
             (req "delayed_messages_prefix" int32)
             (req "delayed_messages_suffix" int32)
             (req "l2_messages" @@ list string))
          (function
            | Sequence
                {
                  nonce;
                  delayed_messages_prefix;
                  delayed_messages_suffix;
                  l2_messages;
                } ->
                Some
                  ( (),
                    nonce,
                    delayed_messages_prefix,
                    delayed_messages_suffix,
                    l2_messages ))
          (fun ( (),
                 nonce,
                 delayed_messages_prefix,
                 delayed_messages_suffix,
                 l2_messages ) ->
            Sequence
              {
                nonce;
                delayed_messages_prefix;
                delayed_messages_suffix;
                l2_messages;
              });
      ]

  let equal x y =
    match (x, y) with
    | Sequence x, Sequence y ->
        x.nonce = y.nonce
        && x.delayed_messages_prefix = y.delayed_messages_prefix
        && x.delayed_messages_suffix = y.delayed_messages_suffix
        && List.equal String.equal x.l2_messages y.l2_messages
end)

include Signed (Framed_message)

(* Signature consisting of zeroes *)
let dummy_signature =
  Signature.V0.of_bytes_exn @@ Bytes.make Signature.V0.size @@ Char.chr 0

let encode_sequence_message rollup_address ~prefix ~suffix
    (l2_messages : Sc_rollup.Inbox_message.serialized list) : string =
  (* Fix: actually sign a message *)
  let unsigned_payload =
    Framed_message.
      {
        rollup_address;
        payload =
          Sequence
            {
              nonce = 0l;
              delayed_messages_prefix = prefix;
              delayed_messages_suffix = suffix;
              l2_messages =
                List.map Sc_rollup.Inbox_message.unsafe_to_string l2_messages;
            };
      }
  in
  let signed_framed_sequence =
    {unsigned_payload; signature = dummy_signature}
  in
  Data_encoding.Binary.to_string_exn encoding signed_framed_sequence

let single_l2_message_overhead =
  let dummy_address =
    Sc_rollup.Address.of_b58check_exn "sr1EzLeJYWrvch2Mhvrk1nUVYrnjGQ8A4qdb"
  in
  String.length
  @@ encode_sequence_message
       dummy_address
       ~prefix:1000l
       ~suffix:1000l
       [Sc_rollup.Inbox_message.unsafe_of_string ""]
