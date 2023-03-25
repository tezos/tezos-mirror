(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** This module provides a typed API for the Rollup Management Protocol that
    defines the communication protocol for exchanging messages between Layer 1
    and Layer 2 for smart-contract rollups.

    The API exposes functions for constructing inbox messages. These are
    messages produced by the Layer 1 protocol and added to a smart-contract
    rollups inbox.

    The Layer 2 node is responsible for decoding and interpreting the messages.

    A type {!outbox_message} representing messages from Layer 2 to Layer 1
    is also provided. An {!outbox_message} consists of a set of transactions
    to L1 accounts.
  *)

open Alpha_context

type error += (* Permanent *) Sc_rollup_invalid_destination

(** A type representing a Layer 2 to Layer 1 transaction. *)
type transaction = private
  | Transaction : {
      destination : Contract_hash.t;
      entrypoint : Entrypoint.t;
      parameters_ty : ('a, _) Script_typed_ir.ty;
      parameters : 'a;
      unparsed_parameters : Script.expr;
    }
      -> transaction

(** A type representing a batch of Layer 2 to Layer 1 transactions. *)
type atomic_transaction_batch = private {transactions : transaction list}

(** A typed representation of {!Sc_rollup.Outbox.Message.t}. *)
type outbox_message = private
  | Atomic_transaction_batch of atomic_transaction_batch

(** [make_internal_transfer ctxt ty ~payload ~sender ~source ~destination]
    constructs a smart rollup's [inbox message] (an L1 to L2 message)
    with the given [payload], [sender], and [source] targeting [destination]. *)
val make_internal_transfer :
  context ->
  ('a, _) Script_typed_ir.ty ->
  payload:'a ->
  sender:Contract_hash.t ->
  source:public_key_hash ->
  destination:Sc_rollup.Address.t ->
  (Sc_rollup.Inbox_message.t * context) tzresult Lwt.t

(** [outbox_message_of_outbox_message_repr ctxt msg] returns a typed version of
    of the given outbox message [msg].

    Fails with an [Sc_rollup_invalid_destination] error in case the parameters
    don't match the type of the entrypoint and destination. *)
val outbox_message_of_outbox_message_repr :
  context ->
  Sc_rollup.Outbox.Message.t ->
  (outbox_message * context) tzresult Lwt.t

(** Function for constructing and encoding {!inbox_message} and
    {!outbox_message} values. Since Layer 1 only ever consumes {!outbox_message}
    values and produces {!inbox_message} values, these functions are used for
    testing only. *)
module Internal_for_tests : sig
  (** [make_transaction ctxt ty ~parameters ~destination ~entrypoint] creates a
      Layer 1 to Layer 2 transaction. *)
  val make_transaction :
    context ->
    ('a, _) Script_typed_ir.ty ->
    parameters:'a ->
    destination:Contract_hash.t ->
    entrypoint:Entrypoint.t ->
    (transaction * context) tzresult Lwt.t

  (** [make_atomic_batch ts] creates an atomic batch with the given
      transactions [ts]. *)
  val make_atomic_batch : transaction list -> outbox_message

  (** [serialize_output_message_untyped msg] encodes the outbox message
      [msg] in binary format using the untyped outbox message
      representation. *)
  val serialize_outbox_message_untyped :
    outbox_message -> Sc_rollup.Outbox.Message.serialized tzresult

  (** [serialize_output_message_typed msg] encodes the outbox
      message [msg] in binary format using the typed outbox message
      representation. *)
  val serialize_outbox_message_typed :
    outbox_message -> Sc_rollup.Outbox.Message.serialized tzresult

  (** [deserialize_inbox_message bs] decodes an inbox message from the given data
      [bs]. *)
  val deserialize_inbox_message :
    Sc_rollup.Inbox_message.serialized -> Sc_rollup.Inbox_message.t tzresult
end
