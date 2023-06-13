(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module defines functions that emit the events used for the rollup node
    when it is storing and publishing commitments (see {!Commitment}). *)

open Protocol.Alpha_context
open Publisher_worker_types

val starting : unit -> unit Lwt.t

val stopping : unit -> unit Lwt.t

(** Section for commitment events. *)
val section : string list

(** [commitment_stored commitment_hash commitment] emits the event
   that the [commitment] was stored. *)
val commitment_stored :
  Sc_rollup.Commitment.Hash.t -> Sc_rollup.Commitment.t -> unit Lwt.t

(** [commitment_will_not_be_published level commitment] emits the event that
    [commitment] will not be published: its inbox level is less or equal than
    the last cemented commitment [level]. *)
val commitment_will_not_be_published :
  Raw_level.t -> Sc_rollup.Commitment.t -> unit Lwt.t

(** [last_cemented_commitment_updated hash level] emits the event that the last
    cemented commitment was updated to the given [hash] at the given inbox
    [level]. *)
val last_cemented_commitment_updated :
  Sc_rollup.Commitment.Hash.t -> Raw_level.t -> unit Lwt.t

(** [last_published_commitment_updated hash level] emits the event that the last
    published commitment was updated to the given [hash] at the given inbox
    [level]. *)
val last_published_commitment_updated :
  Sc_rollup.Commitment.Hash.t -> Raw_level.t -> unit Lwt.t

(** [commitment_parent_is_not_lcc predecessor_hash last_cemented_commitment_hash]
    emits the event that a commitment at the given inbox [level] is being
    published, whose parent is the last cemented commitment, but the
    commitment's [predecessor_hash] differs from the
    [last_cemented_commitment_hash].
    This is a critical error, the rollup node will be terminated. *)
val commitment_parent_is_not_lcc :
  Raw_level.t ->
  Sc_rollup.Commitment.Hash.t ->
  Sc_rollup.Commitment.Hash.t ->
  unit Lwt.t

(** [compute_commitment level] emits the event that a new commitment is being
    computed and stored for the block at the given [level]. *)
val compute_commitment : int32 -> unit Lwt.t

(** [publish_commitment hash level] emits the event that a new commitment is
    being published. *)
val publish_commitment :
  Sc_rollup.Commitment.Hash.t -> Raw_level.t -> unit Lwt.t

(** Events emmitted by the Publisher worker *)
module Publisher : sig
  (** [request_failed view status errors] emits the event that a worker
      request [view] has failed with [status] after producing [errors]. *)
  val request_failed :
    Request.view ->
    Worker_types.request_status ->
    Error_monad.tztrace ->
    unit Lwt.t

  (** [request_completed view status] emits the event that a worker
      request [view] has been completed with [status]. *)
  val request_completed :
    Request.view -> Worker_types.request_status -> unit Lwt.t
end
