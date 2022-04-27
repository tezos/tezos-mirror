(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** The rollup node stores and publishes commitments for the PVM 
    every 20 levels.

    Every time a finalized block is processed  by the rollup node, 
    the latter determines whether the last commitment that the node 
    has produced referred to 20 blocks earlier. In this case, it 
    computes and stores a new commitment in a level-indexed map. 

    Stored commitments are signed by the rollup node operator 
    and published on the layer1 chain. To ensure that commitments 
    produced by the rollup node are eventually published, 
    storing and publishing commitments are decoupled. Every time 
    a new head is processed, the node tries to publish the oldest 
    commitment that was not published already.
*)

open Protocol.Alpha_context

module type Mutable_level_store =
  Store.Mutable_value with type value = Raw_level.t

(** [process_head (module PVM) node_ctxt store head] checks whether a new
    commitment needs to be computed and stored, by looking at the level of
    [head] and checking whether it is a multiple of 20 levels away from
    [node_ctxt.initial_level]. It uses the functionalities of [PVM] to
    compute the hash of to be included in the commitment.
*)

val process_head :
  (module Pvm.S) ->
  Node_context.t ->
  Store.t ->
  Layer1.head ->
  unit tzresult Lwt.t

(** [publish_commitment node_ctxt store] publishes the earliest
    commitment stored in [store] that has not been published yet.
    It uses [node_ctxt.cctxt] to make the RPC call to the Layer1 node.
*)

val publish_commitment : Node_context.t -> Store.t -> unit tzresult Lwt.t

(** [start ()] only emits the event that the commitment manager
    for the rollup node has started. *)
val start : unit -> unit Lwt.t

(** [last_commitment (module Last_level_module: Mutable_level_store) store]
    returns the last commitment (if any) stored according to the value of
    level indicated by [module Last_level_module]. Two possible implementations
    for the latter are [Store.Last_published_commitment_level] and
    [Store.Last_stored_commitment_level].
*)

val last_commitment :
  (module Mutable_level_store) -> Store.t -> Sc_rollup.Commitment.t option Lwt.t
