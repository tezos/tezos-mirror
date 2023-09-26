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

(** Module for constructing an encoded batch of transactions.
    The transactions can be sent to DAC/DAL as a payload or
    can be wrapped in an external message frame and sent to
    the L1 shared inbox.

    The un-wrapped version corresponds to:
    inbox::external::external::v1::ParsedBatch

    The version wrapped by the external message frame corresponds to:
    kernel_sdk::encoding::src::inbox::ExternalMessageFrame
*)

open Tezos_protocol_alpha.Protocol
open Tezos_crypto

(** A batch of transactions. *)
type t

(** An empty batch. *)
val empty : t

(** [add_transfer ~counter ~signer_secret_key ?signer ~destination ~ticketer
    ~ticket_content ~amount t] adds a ticket transfer operation to the batch.

    @param counter The transaction counter that should increase by one with each
    transaction executed by the signer's account.

    @param signer_secret_key The secret key of the sender. Used for signing the operation.

    @param signer Allows to manually set the signer of the operation,
    e.g. to craft an operation with a wrong signature, or use the signer's public key.
    If omitted, the signer will be the public key hash derived from the signer_secret_key.

    @param destination The L2 account in which we send the ticket to.

    @param ticketer The ticketer of the ticket that is being transferred.

    @param ticket_content The ticket content of the ticket that is being transferred.

    @param amount The number of tickets to transfer. *)
val add_transfer :
  counter:int ->
  signer_secret_key:Signature.Ed25519.Secret_key.t ->
  ?signer:Types.signer ->
  destination:Signature.Ed25519.Public_key_hash.t ->
  ticketer:string ->
  ticket_content:string ->
  amount:int ->
  t ->
  t

(** [add_withdraw ~counter ~signer_secret_key ?signer ~destination ~entrypoint ~ticketer
    ~ticket_content ~amount t] adds a ticket withdraw operation to the batch.

    @param counter The transaction counter that should increase by one with each
    transaction executed by the signer's account.

    @param signer_secret_key The secret key of the withdrawer. Used for signing the operation.

    @param signer Allows to manually set the signer of the operation,
    e.g. to craft an operation with a wrong signature, or use the signer's public key.
    If omitted, the signer will be the public key hash derived from the signer_secret_key.

    @param destination The L1 contract in which we send the ticket to.

    @param entrypoint The L1 contract's entrypoint in which we send the ticket to.

    @param ticketer The ticketer of the ticket that is being withdrawn.

    @param ticket_content The ticket content of the ticket that is being withdrawn.

    @param amount The number of tickets to withdraw. *)
val add_withdraw :
  counter:int ->
  signer_secret_key:Signature.Ed25519.Secret_key.t ->
  ?signer:Types.signer ->
  destination:Contract_hash.t ->
  entrypoint:string ->
  ticketer:string ->
  ticket_content:string ->
  amount:int ->
  t ->
  t

(** [make_encoded_batch ?wrap_with t] constructs the encoded transaction batch.
    When [wrap_with = `External_message_frame sc_rollup_addr], it wraps the encoded
    batch with an external message frame so it can be sent to the L1 inbox.
    When [wrap_with = `None], it does not wrap the encoded batch. This is useful when
    we want to transmit the batch via methods outside of the inbox, like DAC or DAL.

    Note that the transactions in the encoded batch will appear in the order they were added.
    E.g. If you add operations [A], [B], then [C], the batch appears as [[A; B; C]].

    Raises [Invalid_argument] if you pass [wrap_with = `External_message_frame sc_rollup_addr]
    and the resulting encoded batch exceeds the external message limit. *)
val make_encoded_batch :
  ?wrap_with:[`External_message_frame of Hashed.Smart_rollup_address.t | `None] ->
  t ->
  string
