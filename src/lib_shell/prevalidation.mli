(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** A newly received block is validated by replaying locally the block
    creation, applying each operation and its finalization to ensure their
    consistency. This module is stateless and creates and manipulates the
    prevalidation_state. *)

type 'protocol_operation operation = private {
  hash : Operation_hash.t;  (** Hash of an operation. *)
  raw : Operation.t;
      (** Raw representation of an operation (from the point view of the
          shell). *)
  protocol : 'protocol_operation;
      (** Economic protocol specific data of an operation. It is the
          unserialized representation of [raw.protocol_data]. For
          convenience, the type associated to this type may be [unit] if we
          do not have deserialized the operation yet. *)
  count_successful_prechecks : int;
      (** This field provides an under-approximation for the number of times
          the operation has been successfully prechecked. It is an
          under-approximation because if the operation is e.g., parsed more than
          once, or is prechecked in other modes, this flag is not globally
          updated. *)
}

module type T = sig
  (** Similar to the same type in the protocol,
      see {!Tezos_protocol_environment.PROTOCOL.operation} *)
  type protocol_operation

  (** Similar to the same type in the protocol,
      see {!Tezos_protocol_environment.PROTOCOL} *)
  type operation_receipt

  (** Similar to the same type in the protocol,
      see {!Tezos_protocol_environment.PROTOCOL} *)
  type validation_state

  (** The type implemented by {!Tezos_store.Store.chain_store} in
      production, and mocked in tests *)
  type chain_store

  (** The type used internally by this module. Created by {!create} and
      then passed back and possibly updated by {!apply_operation}. *)
  type t

  (** [parse hash op] reads a usual {!Operation.t} and lifts it to the
      type {!protocol_operation} used by this module. This function is in the
      {!tzresult} monad, because it can return the following errors:

      - {!Validation_errors.Oversized_operation} if the size of the operation
        data within [op] is too large (to protect against DoS attacks), and
      - {!Validation_errors.Parse_error} if serialized data cannot be parsed. *)
  val parse :
    Operation_hash.t -> Operation.t -> protocol_operation operation tzresult

  (** [increment_successful_precheck op] increments the field
      [count_successful_prechecks] of the given operation [op]. It is supposed
      to be called after each successful precheck of a given operation [op],
      and nowhere else. Overflow is unlikely to occur in practice, as the
      counter grows very slowly and the number of prechecks is bounded. *)
  val increment_successful_precheck :
    protocol_operation operation -> protocol_operation operation

  (** Creates a new prevalidation context w.r.t. the protocol associated with
      the predecessor block. When [?protocol_data] is passed to this function,
      it will  be used to create the new block *)
  val create :
    chain_store ->
    ?protocol_data:Bytes.t ->
    predecessor:Store.Block.t ->
    live_operations:Operation_hash.Set.t ->
    timestamp:Time.Protocol.t ->
    unit ->
    t tzresult Lwt.t

  (** Values returned by {!create}. They are obtained from the result
      of the protocol [apply_operation] function and the classification of
      errors. *)
  type result =
    | Applied of t * operation_receipt
    | Branch_delayed of tztrace
    | Branch_refused of tztrace
    | Refused of tztrace
    | Outdated of tztrace

  (** [apply_operation t op] calls the protocol [apply_operation] function
      and handles possible errors, hereby yielding a classification *)
  val apply_operation : t -> protocol_operation operation -> result Lwt.t

  (** [validation_state t] returns the subset of [t] corresponding
      to the type {!validation_state} of the protocol. *)
  val validation_state : t -> validation_state

  val pp_result : Format.formatter -> result -> unit

  module Internal_for_tests : sig
    (** Returns operations for which {!apply_operation} returned [Applied _]
        so far. *)
    val to_applied :
      t -> (protocol_operation operation * operation_receipt) list
  end
end

(** How-to obtain an instance of this module's main module type: {!T} *)
module Make : functor (Proto : Tezos_protocol_environment.PROTOCOL) ->
  T
    with type protocol_operation = Proto.operation
     and type operation_receipt = Proto.operation_receipt
     and type validation_state = Proto.validation_state
     and type chain_store = Store.chain_store

(**/**)

module Internal_for_tests : sig
  (** Returns the {!Operation.t} underlying an {!operation} *)
  val to_raw : _ operation -> Operation.t

  (** A constructor for the [operation] datatype. It by-passes the
      checks done by the [parse] function. *)
  val make_operation : Operation.t -> Operation_hash.t -> 'a -> 'a operation

  (** [safe_binary_of_bytes encoding bytes] parses [bytes] using [encoding].
      Any error happening during parsing becomes {!Parse_error}.

      If one day the functor's signature is simplified, tests could use
      [parse_unsafe] directly rather than relying on this function to
      replace [Proto.operation_data_encoding]. *)
  val safe_binary_of_bytes : 'a Data_encoding.t -> bytes -> 'a tzresult

  module type CHAIN_STORE = sig
    (** The [chain_store] type. Implemented by
        {!Tezos_store.Store.chain_store} in production and mocked in
        tests *)
    type chain_store

    (** [context store block] checkouts and returns the context of [block] *)
    val context : chain_store -> Store.Block.t -> Context.t tzresult Lwt.t

    (** [chain_id store] returns the {!Chain_id.t} to which [store]
        corresponds *)
    val chain_id : chain_store -> Chain_id.t
  end

  (** A variant of [Make] above that is parameterized by {!CHAIN_STORE},
      for mocking purposes. *)
  module Make : functor
    (Chain_store : CHAIN_STORE)
    (Proto : Tezos_protocol_environment.PROTOCOL)
    ->
    T
      with type protocol_operation = Proto.operation
       and type operation_receipt = Proto.operation_receipt
       and type validation_state = Proto.validation_state
       and type chain_store = Chain_store.chain_store
end
