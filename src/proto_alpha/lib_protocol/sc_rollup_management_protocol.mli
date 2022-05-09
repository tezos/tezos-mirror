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

(** The Rollup Management Protocol defines the communication protocol for
    exchanging messages between Layer 1 and Layer 2 for a SCORU.

    This module exposes a type {!inbox_message}. Inbox messages produced by the
    Layer 1 protocol are encoded using the {!bytes_of_inbox} function, before
    added to a SCORU's inbox.

    An {!inbox_message} consists of:
     - [payload] the parameters passed to the SCORU.
     - [sender] the Layer 1 contract caller.
     - [source] the public key hash used for originating the transaction.

    The Layer 2 node is responsible for decoding and interpreting the messages.

    Another type {!outbox_message} representing messages from Layer 2 to Layer 1
    is also provided. An {!outbox_message} consists of a set of transactions
    to L1 accounts.
  *)

open Alpha_context

(** A type representing messages from Layer 1 to Layer 2. *)
type inbox_message

(** A type representing a Layer 2 to Layer 1 transaction. *)
type transaction = private
  | Transaction : {
      destination : Destination.t;
      entrypoint : Entrypoint.t;
      parameters_ty : ('a, _) Script_typed_ir.ty;
      parameters : 'a;
      unparsed_parameters_ty : Script.expr;
      unparsed_parameters : Script.expr;
    }
      -> transaction

(** A type representing a batch of Layer 2 to Layer 1 transactions. *)
type atomic_transaction_batch = private {transactions : transaction list}

(** A type representing messages from Layer 2 to Layer 1. *)
type outbox_message = private
  | Atomic_transaction_batch of atomic_transaction_batch

(** [make_inbox_message ctxt ty ~payload ~sender ~source] constructs a SCORU
    [inbox message] (an L1 to L2 message) with the given [payload], [sender],
    and [source]. *)
val make_inbox_message :
  context ->
  ('a, _) Script_typed_ir.ty ->
  payload:'a ->
  sender:Contract.t ->
  source:public_key_hash ->
  (inbox_message * context) tzresult Lwt.t

(** [bytes_of_inbox_message msg] encodes the inbox message [msg] in binary
    format. *)
val bytes_of_inbox_message : inbox_message -> bytes tzresult

(** [outbox_message_of_bytes ctxt bs] decodes an outbox message value from the
    given bytes [bs]. The function involves parsing Micheline expressions to
    typed values. *)
val outbox_message_of_bytes :
  context -> bytes -> (outbox_message * context) tzresult Lwt.t

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
    destination:Destination.t ->
    entrypoint:Entrypoint.t ->
    (transaction * context) tzresult Lwt.t

  (** [make_atomic_batch ts] creates an atomic batch with the given
      transactions [ts]. *)
  val make_atomic_batch : transaction list -> outbox_message

  (** [bytes_of_output_message msg] encodes the outbox message [msg] in binary
      format. *)
  val bytes_of_outbox_message : outbox_message -> bytes tzresult

  (** [inbox_message_of_bytes bs] decodes an inbox message from the given bytes
      [bs]. *)
  val inbox_message_of_bytes : bytes -> inbox_message tzresult
end
