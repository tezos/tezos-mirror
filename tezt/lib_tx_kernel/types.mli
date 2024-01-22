(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** Types and their encodings of transactions.
    Ported from the Rust implementation of the tx-kernel. *)

open Tezos_protocol_alpha.Protocol
open Tezos_crypto

(** Corresponds to
    inbox::external::external::v1::OperationTransfer *)
type operation_transfer = {
  destination : Signature.Ed25519.Public_key_hash.t;
  ticket_hash : string;
  amount : int64;
}

(** Corresponds to
    kernel_sdk::encoding::src::michelson::ticket::TicketRepr *)
type ticket = {ticketer : Contract_repr.t; content : string; amount : int}

(** Corresponds to
    inbox::external::external::v1::OperationWithdraw *)
type operation_withdraw = {
  destination : Contract_hash.t;
  ticket : ticket;
  entrypoint : string;
}

(** Corresponds to
    inbox::external::external::v1::OperationTransferCompressed *)
type operation_transfer_compressed = {
  destination : Contract_hash.t;
  ticket : int;
  amount : int;
}

(** Corresponds to
    inbox::external::external::v1::OperationContent *)
type operation_content =
  | Withdraw of operation_withdraw
  | Transfer of operation_transfer
  | CTransfer of operation_transfer_compressed

(** Corresponds to
    inbox::external::v1::external::Signer *)
type signer =
  | Public_key of Signature.Ed25519.Public_key.t
  | Tz1 of Signature.Ed25519.Public_key_hash.t

(** Corresponds to
    inbox::external::verifiable::Operation *)
type operation = {
  signer : signer;
  counter : int64;
  contents : operation_content;
}

(** Corresponds to
    inbox::external::verifiable::VerifiableOperation *)
type verifiable_operation = {
  operation : operation;
  signature : Signature.Ed25519.t;
}

(** Corresponds to
    kernel_sdk::encoding::src::inbox::ExternalMessageFrame::Targetted *)
type targetted = {address : Hashed.Smart_rollup_address.t; contents : string}

(** Corresponds to
    kernel_sdk::encoding::src::inbox::ExternalMessageFrame *)
type external_message_frame = Targetted of targetted

(** Corresponds to
    kernel_sdk::encoding::src::michelson::ticket::Ticket::hash *)
val ticket_hash : ticketer:Contract_repr.t -> content:string -> string

(** Data encoding for [operation_transfer]. *)
val operation_transfer_encoding : operation_transfer Data_encoding.t

(** Data encoding for [ticket]. *)
val ticket_encoding : ticket Data_encoding.t

(** Data encoding for [operation_withdraw]. *)
val operation_withdraw_encoding : operation_withdraw Data_encoding.t

(** Data encoding for [operation_transfer_compressed]. *)
val operation_transfer_compressed_encoding :
  operation_transfer_compressed Data_encoding.t

(** Data encoding for [operation_content]. *)
val operation_content_encoding : operation_content Data_encoding.t

(** Data encoding for [signer]. *)
val signer_encoding : signer Data_encoding.t

(** Data encoding for [operation].  *)
val operation_encoding : operation Data_encoding.t

(** Data encoding for [verifiable_operation]. *)
val verifiable_operation_encoding : verifiable_operation Data_encoding.t

(** Data encoding for [targetted_encoding]. *)
val targetted_encoding : targetted Data_encoding.t

(** Data encoding for [external_message_frame_encoding]. *)
val external_message_frame_encoding : external_message_frame Data_encoding.t
