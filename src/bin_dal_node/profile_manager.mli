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

(** [is_bootstrap_profile t] returns [true] if the node has a bootstrap profile. *)
val is_bootstrap_profile : t -> bool

(** The empty profile manager context. *)
val empty : t

(** The bootstrap profile. *)
val bootstrap_profile : t

(** [add_operator_profiles t proto_parameters gs_worker operator_profiles]
    registers operator profiles (attester or producer).
    If the current profile is a bootstrap profile, it will return [None] as bootstrap
    profiles are incompatible with operator profiles. *)
val add_operator_profiles :
  t ->
  Dal_plugin.proto_parameters ->
  Gossipsub.Worker.t ->
  Types.operator_profiles ->
  t option

(** Checks that each producer profile only refers to slot indexes strictly
    smaller than [number_of_slots]. This may not be the case when the profile
    context is first built because there is no information about the number of
    slots. Returns an [Invalid_slot_index] error if the check fails. *)
val validate_slot_indexes : t -> number_of_slots:int -> unit tzresult

(** [on_new_head t proto_parameters gs_worker committee] performs profile-related
    actions that depend on the current head, more precisely on the current committee. *)
val on_new_head :
  t ->
  Dal_plugin.proto_parameters ->
  Gossipsub.Worker.t ->
  Committee_cache.committee ->
  unit

(** [get_profiles node_store] returns the list of profiles that the node tracks *)
val get_profiles : t -> Types.profiles

(** See {!Services.get_attestable_slots} *)
val get_attestable_slots :
  shard_indices:int list ->
  Store.node_store ->
  Dal_plugin.proto_parameters ->
  attested_level:int32 ->
  (Types.attestable_slots, [Errors.decoding | Errors.other]) result Lwt.t
