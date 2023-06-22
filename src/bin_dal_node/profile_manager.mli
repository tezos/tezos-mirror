(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

(** This module provides different handlers related to DAL profiles. *)

(** A profile manager context stores profile-specific data used by the daemon.  *)
type t

(** Create an empty profile manager context. *)
val empty : unit -> t

(** Adds a profile to the dal [node_store]. If already present,
    the store does not change. *)
val add_profile :
  Dal_plugin.proto_parameters ->
  Store.node_store ->
  t ->
  Gossipsub.Worker.t ->
  Services.Types.profile ->
  unit tzresult Lwt.t

(** [perform_pending_actions c gs_worker get_committee] performs profile-related
    actions that depend on the current head, more precisely on the availability
    of a [get_committee] function. *)
val on_new_head :
  t -> Gossipsub.Worker.t -> Committee_cache.committee -> unit tzresult Lwt.t

(** [get_profiles node_store] returns the list of profiles that the node tracks *)
val get_profiles :
  Store.node_store ->
  (Services.Types.profile list, Errors.decoding) result Lwt.t

(** See {!Services.get_attestable_slots} *)
val get_attestable_slots :
  shard_indices:int list ->
  Store.node_store ->
  Dal_plugin.proto_parameters ->
  attested_level:int32 ->
  (Services.Types.attestable_slots, [Errors.decoding | Errors.other]) result
  Lwt.t
