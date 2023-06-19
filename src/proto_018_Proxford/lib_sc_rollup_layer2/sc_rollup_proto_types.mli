(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

open Protocol.Alpha_context

module Address : sig
  type t = Sc_rollup.Address.t

  val of_octez : Octez_smart_rollup.Address.t -> t

  val to_octez : t -> Octez_smart_rollup.Address.t
end

module State_hash : sig
  type t = Sc_rollup.State_hash.t

  val of_octez : Octez_smart_rollup.State_hash.t -> t

  val to_octez : t -> Octez_smart_rollup.State_hash.t
end

module Merkelized_payload_hashes_hash : sig
  type t = Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t

  val of_octez : Octez_smart_rollup.Merkelized_payload_hashes_hash.t -> t

  val to_octez : t -> Octez_smart_rollup.Merkelized_payload_hashes_hash.t
end

module Commitment_hash : sig
  type t = Sc_rollup.Commitment.Hash.t

  val of_octez : Octez_smart_rollup.Commitment.Hash.t -> t

  val to_octez : t -> Octez_smart_rollup.Commitment.Hash.t
end

module Commitment : sig
  type t = Sc_rollup.Commitment.t

  val of_octez : Octez_smart_rollup.Commitment.t -> t

  val to_octez : t -> Octez_smart_rollup.Commitment.t
end

module Inbox_hash : sig
  type t = Sc_rollup.Inbox.Hash.t

  val of_octez : Octez_smart_rollup.Inbox_hash.t -> t

  val to_octez : t -> Octez_smart_rollup.Inbox_hash.t
end

module Inbox : sig
  type t = Sc_rollup.Inbox.t

  val of_octez : Octez_smart_rollup.Inbox.t -> t

  val to_octez : t -> Octez_smart_rollup.Inbox.t
end

module Game : sig
  type dissection_chunk = Sc_rollup.Game.dissection_chunk

  val dissection_chunk_of_octez :
    Octez_smart_rollup.Game.dissection_chunk -> dissection_chunk

  val dissection_chunk_to_octez :
    dissection_chunk -> Octez_smart_rollup.Game.dissection_chunk

  type step = Sc_rollup.Game.step

  val step_of_octez : Octez_smart_rollup.Game.step -> step

  val step_to_octez : step -> Octez_smart_rollup.Game.step

  type refutation = Sc_rollup.Game.refutation

  val refutation_of_octez : Octez_smart_rollup.Game.refutation -> refutation

  val refutation_to_octez : refutation -> Octez_smart_rollup.Game.refutation

  type index = Sc_rollup.Game.Index.t

  val index_of_octez : Octez_smart_rollup.Game.index -> index

  val index_to_octez : index -> Octez_smart_rollup.Game.index
end

module Kind : sig
  type t = Sc_rollup.Kind.t

  val of_octez : Octez_smart_rollup.Kind.t -> t

  val to_octez : t -> Octez_smart_rollup.Kind.t
end
