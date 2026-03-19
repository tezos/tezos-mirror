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

(** This module defines a data type {!t} that represents messages from Layer 2
    to Layer 1.

    They are part of the [Rollup Management Protocol] that defines the
    communication protocol for exchanging messages between Layer 1 and Layer 2
    for smart-contract rollups.

    An outbox-message consists of a sequence of transactions to L1
    smart-contract accounts. All transactions contained in a message are
    intended to be executed as a batch.
  *)

(** A transaction from L2 to L1. *)
type transaction = {
  unparsed_parameters : Script_repr.expr;  (** The payload. *)
  destination : Contract_hash.t;  (** The recipient contract. *)
  entrypoint : Entrypoint_repr.t;  (** Entrypoint of the destination. *)
}

(** A transaction from L2 to L1, with typed payload. *)
type typed_transaction = {
  unparsed_parameters : Script_repr.expr;  (** The payload. *)
  unparsed_ty : Script_repr.expr;  (** The type of the payload. *)
  destination : Contract_hash.t;  (** The recipient contract. *)
  entrypoint : Entrypoint_repr.t;  (** Entrypoint of the destination. *)
}

(** A type representing messages from Layer 2 to Layer 1. *)
type t =
  | Atomic_transaction_batch of {transactions : transaction list}
  | Atomic_transaction_batch_typed of {transactions : typed_transaction list}
  | Whitelist_update of Sc_rollup_whitelist_repr.t option

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

type serialized = private string

(** [deserialize ctxt bs] decodes an outbox message value from the
    given data [bs]. The function involves parsing Micheline expressions to
    typed values. *)
val deserialize : serialized -> t tzresult

(** [serialize msg] serializes the given outbox message [msg]. *)
val serialize : t -> serialized tzresult

(** [unsafe_of_string s] builds a serialized value out of a string.
    You must understand the invariants of [serialized] to do so. *)
val unsafe_of_string : string -> serialized

(** [unsafe_to_string s] builds a string out of a serialized value.
    You must understand the invariants of [serialized] to manipulate
    the resulting string. *)
val unsafe_to_string : serialized -> string
