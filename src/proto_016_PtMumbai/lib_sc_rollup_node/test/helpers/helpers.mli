(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>.                   *)
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

open Octez_smart_rollup
open Protocol
open Alpha_context

(** {1 Helper functions to build and run unit tests for the rollup node} *)

(** {2 Creating Node Contexts} *)

(** [with_node_context ?constants kind ~boot_sector f] creates a node context
    and (with a store, a context, etc.) where protocol [constants] can be
    specified, and runs [f] with this node context. The L2 chain is initialized
    with a genesis block and the PVM with the [boot_sector]. When [f] terminates
    or fails, the created node context is closed properly. Test that need a node
    context need to use this function in order to avoid file descriptor
    leaks. *)
val with_node_context :
  ?constants:Constants.Parametric.t ->
  Sc_rollup.Kind.t ->
  boot_sector:string ->
  ([`Read | `Write] Node_context.t ->
  genesis:Sc_rollup_block.t ->
  'a tzresult Lwt.t) ->
  'a tzresult Lwt.t

(** {2 Building L2 Chains} *)

(** Create and add a genesis block for the L2 chain. The [boot_sector] for the
    rollup/kernel needs to be provided. The newly created L2 block is
    returned. *)
val add_l2_genesis_block :
  [`Read | `Write] Node_context.t ->
  boot_sector:string ->
  Sc_rollup_block.t tzresult Lwt.t

(** [append_l2_block node_ctxt messages] creates and append
    an L2 block containing the [messages] given in argument. The block is added
    on top of the last L2 block in the chain (i.e. the head known by the node),
    and is returned. *)
val append_l2_block :
  [`Read | `Write] Node_context.t ->
  Sc_rollup.Inbox_message.t list ->
  Sc_rollup_block.t tzresult Lwt.t

(** [append_l2_block node_ctxt message_batches] appends as many blocks as there
    are batches in [message_batches]. Each block contain a batch of
    messages. The portion of the chain that was added is returned. *)
val append_l2_blocks :
  [`Read | `Write] Node_context.t ->
  Sc_rollup.Inbox_message.t list list ->
  Sc_rollup_block.t list tzresult Lwt.t

(** [append_dummy_l2_chain node_ctxt ~length] append [length] L2 blocks with an
    arbitrary content to the chain. The portion of the chain that was added is
    returned. This function is useful for quickly building long(er) L2 chains
    for the tests. *)
val append_dummy_l2_chain :
  [`Read | `Write] Node_context.t ->
  length:int ->
  Sc_rollup_block.t list tzresult Lwt.t

(** {2 Assertions} *)

module Assert : sig
  (** Assertions on L2 blocks *)
  module L2_block : Assert.EQUALITIES with type t = Sc_rollup_block.t

  (** Assertions on commitments *)
  module Commitment : Assert.EQUALITIES with type t = Sc_rollup.Commitment.t

  (** Assertions on commitment hashes *)
  module Commitment_hash :
    Assert.EQUALITIES with type t = Sc_rollup.Commitment.Hash.t

  (** Assertions on PVM state hashes *)
  module State_hash : Assert.EQUALITIES with type t = Sc_rollup.State_hash.t
end

(** {2 Building and Running tests} *)

(** Build an alcotest test case that executes with node context initialized with
    a genesis block with the provided [boot_sector]. *)
val alcotest :
  string ->
  Alcotest.speed_level ->
  ?constants:Constants.Parametric.t ->
  Sc_rollup.Kind.t ->
  boot_sector:string ->
  ([`Read | `Write] Node_context.t ->
  genesis:Sc_rollup_block.t ->
  unit tzresult Lwt.t) ->
  unit Alcotest_lwt.test_case
