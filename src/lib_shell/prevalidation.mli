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

(** Module containing types and functions to handle branch_refused,
 *  branch_delayed, and refused operations *)
module Classification : sig
  type bounded_map = {
    ring : Operation_hash.t Ringo.Ring.t;
    mutable map : (Operation.t * error list) Operation_hash.Map.t;
  }

  type classification =
    [ `Refused of tztrace
    | `Branch_refused of tztrace
    | `Branch_delayed of tztrace
    | `Applied ]

  type t = {
    refused : bounded_map;
    branch_refused : bounded_map;
    branch_delayed : bounded_map;
    mutable applied : (Operation_hash.t * Operation.t) list;
    mutable in_mempool : Operation_hash.Set.t;
  }

  (** [mk_empty ring_size] returns an empty {!t} whose rings hold at
   *  most [ring_size] values. {!Invalid_argument} is raised
   *  if [ring_size] is [0] or less. *)
  val mk_empty : int -> t

  (** [clear classes] resets the state of all fields of [classes],
    * except for [refused] *)
  val clear : t -> unit

  (** [is_in_mempool oph classes] indicates whether [oph] is present
      in field [in_mempool] of [classes]. *)
  val is_in_mempool : Operation_hash.t -> t -> bool

  (** [is_applied oph classes] indicates whether [oph] is present
      in field [applied] of [classes]. *)
  val is_applied : Operation_hash.t -> t -> bool

  (** [remove_applied oph classes] removes operation of hash [oph]
      from fields [applied] and [in_mempool] of [classes]. *)
  val remove_applied : Operation_hash.t -> t -> unit

  (** [remove_not_applied oph classes] removes operation of hash [oph]
      from all fields of [classes] except from [applied]. *)
  val remove_not_applied : Operation_hash.t -> t -> unit

  (** [add ~on_discarded_operation class oph op classes] adds the
     operation [op] with hash [oph] classified as [class] to the
     classifier [classes]. The [on_discarded_operation] callback is
     called for any operation discarded in this process. Currently, an
     operation is discarded if the corresponding class field is full. In
     that case, the new operation is added to the class, and the one
     removed is discarded. An operation is also discarded when it is
     classified as [Refused]. **)
  val add :
    on_discarded_operation:(Operation_hash.t -> unit) ->
    classification ->
    Operation_hash.t ->
    Operation.t ->
    t ->
    unit
end

(** The requester used by [Prevalidator], backed by [Distributed_db]. *)
module Requester :
  Requester.REQUESTER
    with type t = Distributed_db.chain_db
     and type key = Operation_hash.t
     and type value = Operation.t
     and type param = unit

type 'operation_data operation = private {
  hash : Operation_hash.t;  (** Hash of an operation. *)
  raw : Operation.t;
      (** Raw representation of an operation (from the point view of the
     shell). *)
  protocol_data : 'operation_data;
      (** Economic protocol specific data of an operation. It is the
     unserialized representation of [raw.protocol_data]. For
     convenience, the type associated to this type may be [unit] if we
     do not have deserialized the operation yet. *)
}

module type T = sig
  module Proto : Tezos_protocol_environment.PROTOCOL

  type t

  val parse : Operation.t -> Proto.operation_data operation tzresult

  (** [parse_unsafe bytes] parses [bytes] as operation data. Any error happening during parsing becomes {!Parse_error}.

      [unsafe] because there are no length checks, unlike {!parse}. *)
  val parse_unsafe : bytes -> Proto.operation_data tzresult

  (** Creates a new prevalidation context w.r.t. the protocol associate to the
      predecessor block . When ?protocol_data is passed to this function, it will
      be used to create the new block *)
  val create :
    Store.chain_store ->
    ?protocol_data:Bytes.t ->
    predecessor:Store.Block.t ->
    live_blocks:Block_hash.Set.t ->
    live_operations:Operation_hash.Set.t ->
    timestamp:Time.Protocol.t ->
    unit ->
    t tzresult Lwt.t

  type result =
    | Applied of t * Proto.operation_receipt
    | Branch_delayed of error list
    | Branch_refused of error list
    | Refused of error list
    | Outdated

  val apply_operation : t -> Proto.operation_data operation -> result Lwt.t

  type status = {
    applied_operations :
      (Proto.operation_data operation * Proto.operation_receipt) list;
    block_result : Tezos_protocol_environment.validation_result;
    block_metadata : Proto.block_header_metadata;
  }

  val status : t -> status tzresult Lwt.t

  val validation_state : t -> Proto.validation_state

  val pp_result : Format.formatter -> result -> unit
end

module Make (Proto : Tezos_protocol_environment.PROTOCOL) :
  T with module Proto = Proto

(** Pre-apply creates a new block and returns it. *)
val preapply :
  Store.chain_store ->
  user_activated_upgrades:User_activated.upgrades ->
  user_activated_protocol_overrides:User_activated.protocol_overrides ->
  predecessor:Store.Block.t ->
  timestamp:Time.Protocol.t ->
  protocol_data:Bytes.t ->
  Operation.t list list ->
  (Block_header.shell_header * error Preapply_result.t list) tzresult Lwt.t

module Internal_for_tests : sig
  (** [safe_binary_of_bytes encoding bytes] parses [bytes] using [encoding]. Any error happening during parsing becomes {!Parse_error}.

      If one day the functor signature is simplified, tests could use [parse_unsafe] directly rather than relying on this function to
      replace [Proto.operation_data_encoding].

      TODO: https://gitlab.com/tezos/tezos/-/issues/1487
      Move this function to [data_encoding] or [tezos_base] and consider not catching some exceptions
      *)
  val safe_binary_of_bytes : 'a Data_encoding.t -> bytes -> 'a tzresult
end
