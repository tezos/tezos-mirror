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

open Tx_rollup_l2_context_sig

(** This module introduces the batches of transactions that the
    layer-2 (1) reads from its inboxes (see
    {!Tx_rollup_message_repr.Batch}), and (2) interprets off-chain.

    One of the main concerns of the transaction rollups is to provide
    a high-throughput to its participants. That is, transaction
    rollups are expected to be able to process a significant number of
    operations “per second.”

    Putting aside the computational power required by the rollup node,
    the main limit to the throughput of a transaction rollup is the
    number of operations that can fit in a Tezos block. As such, the
    number of bytes that are necessary to store the batches is of key
    importance.

    To estimate the theoretical maximum throughput of the transaction
    rollups as a feature, we can use the following methodology:

    {ul {li Determine the number of bytes that can be allocated to
            layer-2 batches in a Tezos block, under the hypothesis
            that only layer-2 batch submissions and the
            consensus-related operations are included in said
            block. Ideally, this needs to take into account the
            limitation of the size of a layer-2 batch imposed by the
            layer-1 protocol, and the size of the signature that comes
            with an individual batch.}
        {li Divide this number by the average size of a layer-2
            operation, this gives an estimate of the maximum layer-2
            operations per block.}
        {li Divide again the result by the average time (in seconds)
            between two Tezos blocks; the result is the theoretical
            maximum number of operations per second the transaction
            rollups allow to process.}}

   That is, there is three parameters that decide the throughput of
   transaction rollups, and the average size of an operation is the
   only one under the control of the layer-2 implementation.
   Henceforth, both the definitions of types of this module and the
   implementation of their encodings have been carefully crafted in
   order to allow for compact batches. *)

(** Represents the [signer] of an layer-2 operation. This is either a
    BLS public key or a layer-2 address index, whose metadata in turn
    contains a corresponding BLS public. key *)
type signer =
  | Bls_pk of Bls.Public_key.t  (** A signer identified by a BLS public key. *)
  | L2_addr of Tx_rollup_l2_address.t
      (** A signer identified by a layer-2 address. Each such adress
          is in turn identified with a BLS public key. *)

module Signer_indexable : sig
  type nonrec 'state t = ('state, signer) Indexable.t

  type nonrec index = signer Indexable.index

  type nonrec value = signer Indexable.value

  type either = signer Indexable.either

  val encoding : either Data_encoding.t

  val compare : either -> either -> int

  val pp : Format.formatter -> either -> unit
end

(** {1 Layer-2 Batches Definitions} *)

(** The operations are versioned, to let the possibility to propose
    new features in future iterations of the protocol. *)

module V1 : sig
  type 'status operation_content =
    | Withdraw of {
        destination : Signature.Public_key_hash.t;
        ticket_hash : Alpha_context.Ticket_hash.t;
        qty : Tx_rollup_l2_qty.t;
      }
        (** A [Withdraw] removes [qty] of the tickets represented by
            [ticket_hash] from the operation's signer in layer-2, and
            permits [destination] to retrieve those tickets in layer-1
            through a [Tx_rollup_withdraw] operation. *)
    | Transfer of {
        destination : 'status Tx_rollup_l2_address.Indexable.t;
        ticket_hash : 'status Ticket_indexable.t;
        qty : Tx_rollup_l2_qty.t;
      }
        (** A [Transfer] moves [qty] of the tickets represented by
            [ticket_hash] from the operation's signer in layer-2 to
            [destination] in layer-2. *)

  type ('signer, 'content) operation = {
    signer : 'signer Signer_indexable.t;
    counter : int64;
    contents : 'content operation_content list;
  }

  type ('signer, 'content) transaction = ('signer, 'content) operation list

  type signature = Bls.t

  type ('signer, 'content) t = {
    contents : ('signer, 'content) transaction list;
    aggregated_signature : signature;
  }

  (** [compact ~bits] is a specialized, space-efficient encoding for a
      batch of layer-2 operations, such as the [bits] first bits of
      the first byte of the resulting binary array are used to encode
      small lists of transactions. *)
  val compact :
    bits:int -> (Indexable.unknown, Indexable.unknown) t Data_encoding.Compact.t

  (** A specialized, space-efficient encoding for [transaction].

      The first byte of the resulting binary array is used to encode
      the size of lists of less than 254 elements. For larger lists,
      the tag is [11111111] and the list is prefixed by its size,
      which consumes eight bytes. *)
  val compact_transaction :
    (Indexable.unknown, Indexable.unknown) transaction Data_encoding.Compact.t

  (** A specialized {!compact_transaction} where the signers are indexes only. *)
  val compact_transaction_signer_index :
    (Indexable.index_only, Indexable.unknown) transaction
    Data_encoding.Compact.t

  (** The encoding of reference used to sign a transaction. It is
      derived from {!compact_transaction}. *)
  val transaction_encoding :
    (Indexable.unknown, Indexable.unknown) transaction Data_encoding.t

  (** A specialized, space-efficient encoding for [operation].

      The first byte of the binary output describes precisely the layout
      of the encoded value.

      Considering the tag [ooooccss], [ss] describes the format of
      [signer], [cc] of [counter] and [oooo] of [contents].

      More precisely, for [signer],

      {ul {li [00] means an index fitting on 1 byte.}
          {li [01] means an index fitting on 2 bytes.}
          {li [10] means an index fitting on 4 bytes.}
          {li [11] means a value of type {!Bls.Public_key.t}.}}

      The [counter] field follows a similar logic,

      {ul {li [00] means an index fitting on 1 byte.}
          {li [01] means an index fitting on 2 bytes.}
          {li [10] means an index fitting on 4 bytes.}
          {li [11] means an integer fitting on 8 bytes.}
      }

      Finally, the [contents] field follows this pattern

      {ul {li From [0000] to [1110], the tag encodes the size of the
              list of [operation_content], {i e.g.}, [0010] means that
              there is two elements in [contents].}
          {li [1111] means that [contents] is prefixed by its number
              of elements.}
      } *)
  val compact_operation :
    (Indexable.unknown, Indexable.unknown) operation Data_encoding.Compact.t

  (** A specialized, space-efficient encoding for [operation_content].

      The first byte of the binary output describes precisely the layout
      of the encoded value.

      Considering the tag [0qqttddd], [ddd] describes the format of
      [destination], [tt] of [ticket_hash] and [qq] of [qty]. More
      precisely, for [destination],

      {ul {li [000] means a layer-1 address.}
          {li [100] means an index for a layer-2 address, fitting on 1 byte.}
          {li [101] means an index for a layer-2 address, fitting on 2 bytes.}
          {li [110] means an index for a layer-2 address, fitting on 4 bytes.}
          {li [111] means a value (of type {!Tx_rollup_l2_address.t},
              that is a layer-2 address.}
      }

      The [ticket_hash] is encoded using this logic:

      {ul {li [00] means an index for a ticket hash, fitting on 1 byte.}
          {li [01] means an index for a ticket hash, fitting on 2 bytes.}
          {li [10] means an index for a ticket hash, fitting on 4 bytes.}
          {li [11] means a value (of type {!Ticket_hash.t}.}
      }

      The [qty] field follows a similar logic,

      {ul {li [00] means an integer fitting on 1 byte.}
          {li [01] means an integer fitting on 2 bytes.}
          {li [10] means an integer fitting on 4 bytes.}
          {li [11] means an integer fitting on 8 bytes.}
      }

      If used to read, respectively write, a value where the
      the [destination] is a layer-1 address and the ticket_hash is an
      index, which is not allowed by the layer-2 protocol, then a

       - [Data_encoding.Binary.Read_error (Exception_raised_in_user_function ...)],

      respectively

       - [Data_encoding.Binary.Write_error (Exception_raised_in_user_function ...)]

      exception is raised.
   *)
  val compact_operation_content :
    Indexable.unknown operation_content Data_encoding.Compact.t
end

(** {1 Versioning} *)

(** To pave the road towards being able to update the semantics of the
    transaction rollups without having to interfere with the rejection
    mechanism, we preemptively back the notion of semantics versioning
    into the definition of a layer-2 batch. *)

type ('signer, 'content) t = V1 of ('signer, 'content) V1.t

(** An encoding for [t] that uses a specialized, space-efficient encoding
    for the list of transactions. *)
val encoding : (Indexable.unknown, Indexable.unknown) t Data_encoding.t
