(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

type signature = Bls_signature.signature

let signature_encoding =
  let open Data_encoding in
  conv_with_guard
    (fun signature -> Bls_signature.signature_to_bytes signature)
    (fun bytes ->
      match Bls_signature.signature_of_bytes_opt bytes with
      | Some x -> Ok x
      | None -> Error "Not a valid bls_signature")
    (Fixed.bytes Bls_signature.signature_size_in_bytes)

module Ticket_indexable = Indexable.Make (Alpha_context.Ticket_hash)

(** An integer used to identified a layer-2 address. See
    {!Tx_rollup_l2_address.index}. *)
type address_index = Tx_rollup_l2_address.Indexable.index

(** An integer used to identified a layer-1 ticket deposited in a
    transaction rollup. *)
type ticket_index = Ticket_indexable.index

(** The metadata associated to a layer-2 address.

    The counter is an counter-measure against replay attack. Each
    operation is signed with an integer (its counter). The counter
    is incremented when the operation is applied. This prevents the
    operation to be applied once again, since its integer will not
    be in sync with the counter of the account.  The choice of [int64]
    for the type of the counter theoretically prevents the rollup to
    an integer overflow. However, it can only happen if a single account
    makes more than [1.8446744e+19] operations. If an account sends 1000
    operations per seconds, it would take them more than 5845420
    centuries to achieve that.

    The [public_key] allows to authenticate the owner of the address,
    by verifying BLS signatures. *)
type metadata = {counter : int64; public_key : Bls_signature.pk}

type error +=
  | Balance_too_low
  | Balance_overflow
  | Invalid_quantity
  | Unknown_address_index of address_index
  | Metadata_already_initialized of address_index
  | Too_many_l2_addresses
  | Too_many_l2_tickets
  | Counter_overflow

(** This module type describes the API of the [Tx_rollup] context,
    which is used to implement the semantics of the L2 operations. *)
module type CONTEXT = sig
  (** The state of the [Tx_rollup] context.

      The context provides a type-safe, functional API to interact
      with the state of a transaction rollup.  The functions of this
      module, manipulating and creating values of type [t] are called
      “context operations” afterwards. *)
  type t

  (** The monad used by the context.

      {b Note:} It is likely to be the monad of the underlying
      storage. In the case of the proof verifier, as it is expected to
      be run into the L1, the monad will also be used to perform gas
      accounting. This is why all the functions of this module type
      needs to be inside the monad [m]. *)
  type 'a m

  (** The necessary monadic operators the storage monad is required to
      provide. *)
  module Syntax : sig
    val ( let+ ) : 'a m -> ('a -> 'b) -> 'b m

    val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m

    (** [fail err] shortcuts the current computation by raising an
        error.

        Said error can be handled with the [catch] combinator. *)
    val fail : error -> 'a m

    (** [catch p k h] tries to executes the monadic computation [p].
        If [p] terminates without an error, then its result is passed
        to the continuation [k]. On the contrary, if an error [err] is
        raised, it is passed to the error handler [h]. *)
    val catch : 'a m -> ('a -> 'b m) -> (error -> 'b m) -> 'b m

    (** [return x] is the simplest computation inside the monad [m] which simply
        computes [x] and nothing else. *)
    val return : 'a -> 'a m

    (** [list_fold_left_m f] is a monadic version of [List.fold_left
        f], wherein [f] is not a pure computation, but a computation
        in the monad [m]. *)
    val list_fold_left_m : ('a -> 'b -> 'a m) -> 'a -> 'b list -> 'a m

    (** [fail_unless cond err] raises [err] iff [cond] is [false]. *)
    val fail_unless : bool -> error -> unit m
  end

  (** [bls_aggregate_verify] allows to verify the aggregated signature
      of a batch. *)
  val bls_verify : (Bls_signature.pk * bytes) list -> signature -> bool m

  (** The metadata associated to an address. *)
  module Address_metadata : sig
    (** [get ctxt idx] returns the current metadata associated to the
        address indexed by [idx]. *)
    val get : t -> address_index -> metadata option m

    (** [incr_counter ctxt idx] increments the counter of the
        address indexed by [idx].

        This function can fail with [Counter_overflow] iff the counter
        has reached the {Int64.max_int} limit.

        This function can fail with [Unknown_address_index] if [idx]
        has not been associated with a layer-2 address already. *)
    val incr_counter : t -> address_index -> t m

    (** [init_with_public_key ctxt idx pk] initializes the metadata
        associated to the address indexed by [idx].

        This can fails with [Metadata_already_initialized] if this
        function has already been called with [idx]. *)
    val init_with_public_key : t -> address_index -> Bls_signature.pk -> t m

    (**/**)

    module Internal_for_tests : sig
      val set : t -> address_index -> metadata -> t m
    end
  end

  (** Mapping between {!Tx_rollup_l2_address.address} and {!address_index}.

      Addresses are supposed to be associated to a {!address_index} in
      order to reduce the batches' size submitted from the layer1 to the
      layer2.  Therefore, the first time an address is used in a layer2
      operation, we associate it to a address_index that should be use
      in future layer2 operations.
  *)
  module Address_index : sig
    (** [get ctxt addr] returns the index associated to [addr], if
        any. *)
    val get : t -> Tx_rollup_l2_address.t -> address_index option m

    (** [get_or_associate_index ctxt addr] associates a fresh [address_index]
        to [addr], and returns it. If the [addr] has already been associated to
        an index, it returns it.
        It also returns the information on whether the index was created or
        already existed.

        This function can fail with [Too_many_l2_addresses] iff there
        is no fresh index available. *)
    val get_or_associate_index :
      t ->
      Tx_rollup_l2_address.t ->
      (t * [`Created | `Existed] * address_index) m

    (** [count ctxt] returns the number of addresses that have been
        involved in the transaction rollup. *)
    val count : t -> int32 m

    (**/**)

    module Internal_for_tests : sig
      (** [set ctxt count] sets the [count] in [ctxt]. It is used to test
          the behavior of [Too_many_l2_addresses]. *)
      val set_count : t -> int32 -> t m
    end
  end

  (** Mapping between {!Ticket_hash.t} and {!ticket_index}.

      Ticket hashes are supposed to be associated to a {!ticket_index} in
      order to reduce the batches' size submitted from the layer1 to the
      layer2.  Therefore, the first time a ticket hash is used in a layer2
      operation, we associate it to a ticket_index that should be use
      in future layer2 operations.
  *)
  module Ticket_index : sig
    (** [get ctxt ticket] returns the index associated to [ticket], if
        any. *)
    val get : t -> Alpha_context.Ticket_hash.t -> ticket_index option m

    (** [get_or_associate_index ctxt ticket] associates a fresh [ticket_index]
        to [ticket], and returns it. If the [ticket] has already been associated
        to an index, it returns it.
        It also returns the information on whether the index was created or
        already existed.

        This function can fail with [Too_many_l2_tickets] iff there
        is no fresh index available. *)
    val get_or_associate_index :
      t ->
      Alpha_context.Ticket_hash.t ->
      (t * [`Created | `Existed] * ticket_index) m

    (** [count ctxt] returns the number of tickets that have been
        involved in the transaction rollup. *)
    val count : t -> int32 m

    (**/**)

    module Internal_for_tests : sig
      (** [set_count ctxt count] sets the [count] in [ctxt]. It is used to test
          the behavior of [Too_many_l2_addresses]. *)
      val set_count : t -> int32 -> t m
    end
  end

  (** The ledger of the layer 2 where are registered the amount of a
      given ticket a L2 [account] has in its possession. *)
  module Ticket_ledger : sig
    (** [get ctxt tidx aidx] returns the quantity of tickets ([tidx]) [aidx]
        owns.

        {b Note:} It is the responsibility of the caller to verify that [aidx]
        and [tidx] have been associated to an address and
        a ticket respectively. The function will return zero when the address
        has no such ticket. *)
    val get : t -> ticket_index -> address_index -> Tx_rollup_l2_qty.t m

    (** [credit ctxt tidx aidx qty] updates the ledger to
        increase the number of tickets indexed by [tidx] the address
        [aidx] owns by [qty] units.

        This function can fail with [Balance_overflow] if adding
        [qty] to the current balance of [aidx] causes an integer
        overflow.

        This function can fail with [Invalid_quantity] if [qty]
        is not strictly positive.

        {b Note:} It is the responsibility of the caller to verify that [aidx]
        and [tidx] have been associated to an address and
        a ticket respectively. *)
    val credit : t -> ticket_index -> address_index -> Tx_rollup_l2_qty.t -> t m

    (** [spend ctxt tidx aidx qty] updates the ledger to
        decrease the number of tickets indexed by [tidx] the address
        [aidx] owns by [qty] units.

        This function can fail with [Balance_too_low] if [aidx]
        does not own at least [qty] ticket.

        {b Note:} It is the responsibility of the caller to verify
        that [aidx] and [tidx] have been associated to an address and
        a ticket respectively. *)
    val spend : t -> ticket_index -> address_index -> Tx_rollup_l2_qty.t -> t m
  end
end
