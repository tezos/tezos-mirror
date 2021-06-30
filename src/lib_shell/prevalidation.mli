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
  type 'a bounded_map = {
    ring : Operation_hash.t Ringo.Ring.t;
    mutable map : (Operation.t * error list) Operation_hash.Map.t;
  }

  type t = {
    refused : [`Refused] bounded_map;
    branch_refused : [`Branch_refused] bounded_map;
    branch_delayed : [`Branch_delayed] bounded_map;
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
end

(** The requester used by [Prevalidator], backed by [Distributed_db]. *)
module Requester :
  Requester.REQUESTER
    with type t = Distributed_db.chain_db
     and type key = Operation_hash.t
     and type value = Operation.t
     and type param = unit

module type T = sig
  module Proto : Tezos_protocol_environment.PROTOCOL

  type t

  type operation = private {
    hash : Operation_hash.t;
    raw : Operation.t;
    protocol_data : Proto.operation_data;
  }

  val parse : Operation.t -> operation tzresult

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

  val apply_operation : t -> operation -> result Lwt.t

  type status = {
    applied_operations : (operation * Proto.operation_receipt) list;
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
