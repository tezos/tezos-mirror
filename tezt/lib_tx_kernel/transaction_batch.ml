(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

open Tezos_protocol_alpha.Protocol
open Tezos_crypto
open Types

type t = verifiable_operation list

let empty = []

let add_transfer ~counter ~signer_secret_key ?signer ~destination ~ticketer
    ~ticket_content ~amount t =
  let signer =
    Option.value
      ~default:
        (Tz1
           (Signature.Ed25519.Public_key.hash
           @@ Signature.Ed25519.Secret_key.to_public_key signer_secret_key))
      signer
  in
  let ticket_hash =
    ticket_hash
      ~ticketer:
        (Contract_repr.Originated (Contract_hash.of_b58check_exn ticketer))
      ~content:ticket_content
  in
  let operation =
    {
      signer;
      counter = Int64.of_int counter;
      contents =
        Transfer {destination; ticket_hash; amount = Int64.of_int amount};
    }
  in
  let operation_bytes =
    Data_encoding.Binary.to_bytes_exn operation_encoding operation
  in
  let signature = Signature.Ed25519.sign signer_secret_key operation_bytes in
  let verifiable_operation = {operation; signature} in
  (* We prepend the operation below, then reverse the whole list in [make_encoded_batch]
     to preserve the ordering. *)
  verifiable_operation :: t

let add_withdraw ~counter ~signer_secret_key ?signer ~destination ~entrypoint
    ~ticketer ~ticket_content ~amount t =
  let signer =
    Option.value
      ~default:
        (Tz1
           (Signature.Ed25519.Public_key.hash
           @@ Signature.Ed25519.Secret_key.to_public_key signer_secret_key))
      signer
  in
  let ticket =
    {
      ticketer =
        Contract_repr.Originated (Contract_hash.of_b58check_exn ticketer);
      content = ticket_content;
      amount;
    }
  in
  let operation =
    {
      signer;
      counter = Int64.of_int counter;
      contents = Withdraw {destination; ticket; entrypoint};
    }
  in
  let operation_bytes =
    Data_encoding.Binary.to_bytes_exn operation_encoding operation
  in
  let signature = Signature.Ed25519.sign signer_secret_key operation_bytes in
  let verifiable_operation = {operation; signature} in
  (* We prepend the operation below, then reverse the whole list in [make_encoded_batch]
     to preserve the ordering. *)
  verifiable_operation :: t

let make_encoded_batch ?(wrap_with = `None) t =
  let open Data_encoding in
  (* We construct [encoded_batch] directly without relying on the data-encoding library.
     This choice is made because the data-encoding library requries prefixing variable
     length elements in a list with their size, whereas the Rust Nom parser used in the
     tx-kernel does not impose such requirements on list elements. *)
  let encoded_verifiable_operations =
    List.rev_map
      (Data_encoding.Binary.to_string_exn verifiable_operation_encoding)
      t
  in
  let encoded_batch_body = String.concat "" encoded_verifiable_operations in
  let encoded_batch = Binary.to_string_exn string encoded_batch_body in
  match wrap_with with
  | `None -> encoded_batch
  | `External_message_frame sc_rollup_addr ->
      let batch_tag = "\001" in
      let contents = batch_tag ^ encoded_batch in
      let external_message = Targetted {address = sc_rollup_addr; contents} in
      let encoded_external_message =
        Binary.to_string_exn external_message_frame_encoding external_message
      in
      let len = String.length encoded_external_message in
      if len <= 4095 then
        (* [4095] is the maximum external message size minus the 1 byte
           used for the external message tag *)
        encoded_external_message
      else
        raise
        @@ Invalid_argument
             (sf
                "The size of the encoded external message is %d which exceeds \
                 the limit of 4095."
                len)
