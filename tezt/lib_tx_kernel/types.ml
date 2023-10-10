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

type operation_transfer = {
  destination : Signature.Ed25519.Public_key_hash.t;
  ticket_hash : string;
  amount : int64;
}

type ticket = {ticketer : Contract_repr.t; content : string; amount : int}

type operation_withdraw = {
  destination : Contract_hash.t;
  ticket : ticket;
  entrypoint : string;
}

type operation_transfer_compressed = {
  destination : Contract_hash.t;
  ticket : int;
  amount : int;
}

type operation_content =
  | Withdraw of operation_withdraw
  | Transfer of operation_transfer
  | CTransfer of operation_transfer_compressed

type signer =
  | Public_key of Signature.Ed25519.Public_key.t
  | Tz1 of Signature.Ed25519.Public_key_hash.t

type operation = {
  signer : signer;
  counter : int64;
  contents : operation_content;
}

type verifiable_operation = {
  operation : operation;
  signature : Signature.Ed25519.t;
}

type targetted = {address : Hashed.Smart_rollup_address.t; contents : string}

type external_message_frame = Targetted of targetted

let operation_transfer_encoding =
  let open Data_encoding in
  (* We require a slightly hacky int <-> little-endian conversion
     since the rust implementation uses little-endian but
     data-encoding does not support little-endian ints.

     TODO: https://gitlab.com/tezos/tezos/-/issues/6384
     Get rid of this awkward conversion by using big endian in
     the rust implementation. *)
  let rev_bytes b =
    b |> Bytes.to_seq |> List.of_seq |> List.rev |> List.to_seq |> Bytes.of_seq
  in
  let le_int64_of_int i =
    let be_int64 = Binary.to_bytes_exn int64 i in
    rev_bytes be_int64
  in
  let int_of_le_int64 b =
    let le_int64 = rev_bytes b in
    let b = rev_bytes le_int64 in
    Binary.of_bytes_exn int64 b
  in
  conv
    (fun {destination; ticket_hash; amount} ->
      (destination, ticket_hash, le_int64_of_int amount))
    (fun (destination, ticket_hash, amount) ->
      {destination; ticket_hash; amount = int_of_le_int64 amount})
    (obj3
       (req "destination" Signature.Ed25519.Public_key_hash.encoding)
       (req "ticket_hash" Fixed.(string 32))
       (req "amount" Fixed.(bytes 8)))

let ticket_encoding =
  let open Data_encoding in
  let open Tezos_micheline in
  let open Micheline in
  let open Michelson_v1_primitives in
  let node_encoding =
    Micheline_encoding.erased_encoding
      ~variant:"test"
      0
      Michelson_v1_primitives.prim_encoding
  in
  conv_with_guard
    (fun {ticketer; content; amount} ->
      Prim
        ( 0,
          D_Pair,
          [
            Bytes (0, Binary.to_bytes_exn Contract_repr.encoding ticketer);
            Prim (0, D_Pair, [String (0, content); Int (0, Z.of_int amount)], []);
          ],
          [] ))
    (function
      | Prim
          ( 0,
            D_Pair,
            [
              Bytes (0, ticketer);
              Prim (0, D_Pair, [String (0, content); Int (0, amount)], []);
            ],
            [] ) ->
          Ok
            {
              ticketer = Binary.of_bytes_exn Contract_repr.encoding ticketer;
              content;
              amount = Z.to_int amount;
            }
      | _ -> Error "Unexpected micheline expression.")
    node_encoding

let operation_withdraw_encoding =
  let open Data_encoding in
  conv
    (fun {destination; ticket; entrypoint} -> (destination, ticket, entrypoint))
    (fun (destination, ticket, entrypoint) -> {destination; ticket; entrypoint})
    (obj3
       (req "destination" Contract_hash.encoding)
       (req "ticket" ticket_encoding)
       (req "entrypoint" string))

let operation_transfer_compressed_encoding =
  let open Data_encoding in
  conv
    (fun {destination; ticket; amount} ->
      (destination, Int64.of_int ticket, Int64.of_int amount))
    (fun (destination, ticket, amount) ->
      {destination; ticket = Int64.to_int ticket; amount = Int64.to_int amount})
    (obj3
       (req "destination" Contract_hash.encoding)
       (req "ticket" int64)
       (req "amount" int64))

let operation_content_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"withdraw"
        (Tag 0)
        operation_withdraw_encoding
        (function Withdraw w -> Some w | _ -> None)
        (fun w -> Withdraw w);
      case
        ~title:"transfer"
        (Tag 1)
        operation_transfer_encoding
        (function Transfer t -> Some t | _ -> None)
        (fun t -> Transfer t);
      case
        ~title:"ctransfer"
        (Tag 2)
        operation_transfer_compressed_encoding
        (function CTransfer t -> Some t | _ -> None)
        (fun t -> CTransfer t);
    ]

let signer_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"public_key"
        (Tag 0)
        Signature.Ed25519.Public_key.encoding
        (function Public_key pk -> Some pk | _ -> None)
        (fun pk -> Public_key pk);
      case
        ~title:"tz1"
        (Tag 1)
        Signature.Ed25519.Public_key_hash.encoding
        (function Tz1 pkh -> Some pkh | _ -> None)
        (fun pkh -> Tz1 pkh);
    ]

let operation_encoding =
  let open Data_encoding in
  conv
    (fun {signer; counter; contents} -> (signer, counter, contents))
    (fun (signer, counter, contents) -> {signer; counter; contents})
    (obj3
       (req "signer" signer_encoding)
       (req "counter" int64)
       (req "contents" operation_content_encoding))

let verifiable_operation_encoding =
  let open Data_encoding in
  conv
    (fun {operation; signature} -> (operation, signature))
    (fun (operation, signature) -> {operation; signature})
    (obj2
       (req "operation" operation_encoding)
       (req "signature" Signature.Ed25519.encoding))

let targetted_encoding =
  let open Data_encoding in
  conv
    (fun {address; contents} -> (address, contents))
    (fun (address, contents) -> {address; contents})
    (obj2
       (req "address" Hashed.Smart_rollup_address.encoding)
       (req "contents" Data_encoding.Variable.string))

let external_message_frame_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"targetted"
        (Tag 0)
        targetted_encoding
        (function Targetted t -> Some t)
        (fun t -> Targetted t);
    ]

let ticket_hash ~ticketer ~content =
  let open Data_encoding in
  let open Tezos_micheline in
  let open Micheline in
  let node_encoding =
    Micheline_encoding.erased_encoding
      ~variant:"test"
      0
      Michelson_v1_primitives.prim_encoding
  in
  let michelson_contract_encoding =
    conv_with_guard
      (fun contract ->
        Bytes (0, Binary.to_bytes_exn Contract_repr.encoding contract))
      (function
        | Bytes (0, contract) -> (
            match Binary.of_bytes Contract_repr.encoding contract with
            | Ok contract -> Ok contract
            | Error e -> Error (Format.asprintf "%a" Binary.pp_read_error e))
        | _ -> Error "Unexpected micheline expression.")
      node_encoding
  in
  let michelson_string_encoding =
    conv_with_guard
      (fun string -> String (0, string))
      (function
        | String (0, string) -> Ok string
        | _ -> Error "Unexpected micheline expression.")
      node_encoding
  in
  let ticketer_content_encoding =
    obj2
      (req "ticketer" michelson_contract_encoding)
      (req "contents" michelson_string_encoding)
  in
  [Binary.to_bytes_exn ticketer_content_encoding (ticketer, content)]
  |> Blake2B.hash_bytes |> Blake2B.to_string
