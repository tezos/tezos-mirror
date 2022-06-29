(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(** Communication from the layer-1 (Tezos) to the layer-2 (a
    transaction rollup) happens thanks to messages, crafted in the
    layer-1 to be interpreted in the layer-2.

    Messages are constructed and gathered in the layer-1, in
    inboxes (see {!Tx_rollup_repr_storage.append_message}). *)

(** Smart contract on the layer-1 can deposit tickets into a
    transaction rollup, for the benefit of a {!Tx_rollup_l2_address.t}.
    The [sender] is an implicit account where the deposit is returned in form of
    a withdrawal, should the application of the deposit fail.
 *)
type deposit = {
  sender : Signature.Public_key_hash.t;
  destination : Tx_rollup_l2_address.Indexable.value;
  ticket_hash : Ticket_hash_repr.t;
  amount : Tx_rollup_l2_qty.t;
}

(** A [message] is a piece of data originated from the layer-1 to be
    interpreted by the layer-2.

    Transaction rollups feature two kind of messages:

    {ul {li An array of bytes that supposedly contains a valid
            sequence of layer-2 operations; their interpretation and
            validation is deferred to the layer-2..}
        {li A deposit order for a L1 ticket.}} *)
type t = Batch of string | Deposit of deposit

(** [size msg] returns the number of bytes that are allocated in an
    inbox by [msg]. *)
val size : t -> int

val deposit_encoding : deposit Data_encoding.t

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit
