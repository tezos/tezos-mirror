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
  Store_utils.Mutable_value with type value = Raw_level.t

(** [last_commitment_with_hash (module Last_level_module: Mutable_level_store) store]
      returns the last commitment and relative hash
      stored according to the value of level indicated by
      [module Last_level_module]. If no commitment has been stored for the
      level indicated by [module Last_level_module], then None is returned.
      Two possible implementations for [module Last_level_module] are
      [Store.Last_published_commitment_level] and
      [Store.Last_stored_commitment_level].
  *)

val last_commitment_with_hash :
  (module Mutable_level_store) ->
  Store.t ->
  (Sc_rollup.Commitment.t * Sc_rollup.Commitment.Hash.t) option Lwt.t

module Make (PVM : Pvm.S) : Commitment_sig.S with module PVM = PVM
