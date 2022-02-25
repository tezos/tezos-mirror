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

(** The type of hashes used to identify an inbox based on its
    contents. For an inbox containing the messages hashes [h1; h2;
    .. ; hn], its hash is computed with [H(.. H(H(empty + h1) + h2)
    .. + hn)] *)
type hash

val compare_hash : hash -> hash -> int

val equal_hash : hash -> hash -> bool

val pp_hash : Format.formatter -> hash -> unit

val hash_of_bytes_exn : bytes -> hash

val hash_of_bytes_opt : bytes -> hash option

val hash_of_b58check_exn : string -> hash

val hash_of_b58check_opt : string -> hash option

val hash_encoding : hash Data_encoding.t

val hash_to_bytes : hash -> bytes

val hash_to_b58check : hash -> string

(** [extend_hash inbox_hash hash] computes [H(inbox_hash + hash)],
    which is used to identify an inbox identified by [inbox_hash]
    after appending a new message identified by [hash]. *)
val extend_hash : hash -> Tx_rollup_message_repr.hash -> hash

(** [hash_inbox messages] hashes a list of messages, starting
    from an empty inbox and recursively running extend_hash on
    each. *)
val hash_inbox : Tx_rollup_message_repr.t list -> hash

(** [hash_hashed_inbox messages] hashes a list of already-hashed
    messages, starting from an empty inbox and recursively running
    extend_hash on each. *)
val hash_hashed_inbox : Tx_rollup_message_repr.hash list -> hash

(** An inbox gathers, for a given Tezos level, messages crafted by the
    layer-1 for the layer-2 to interpret.

    The structure comprises two fields: (1) [contents] is the list of
    message hashes, and (2) [cumulated_size] is the quantity of bytes
    allocated by the related messages.

    We recall that a transaction rollup can have up to one inbox per
    Tezos level, starting from its origination. See
    {!Storage.Tx_rollup} for more information. *)
type t = {
  contents : Tx_rollup_message_repr.hash list;
  cumulated_size : int;
  hash : hash;
}

val pp : Format.formatter -> t -> unit

val encoding : t Data_encoding.t

(** The metadata for an inbox stores the [cumulated_size] in bytes for
    the inbox, the [inbox_length] ({i i.e.}, the number of messages),
    and the cumulative [hash] of the inbox contents. For newly created
    inboxes, the [hash] is initialized as an array 32 null byte. *)
type metadata = {inbox_length : int32; cumulated_size : int; hash : hash}

val metadata_encoding : metadata Data_encoding.t

val empty_metadata : metadata
