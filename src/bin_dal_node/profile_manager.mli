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

(** The empty profile manager context. *)
val empty : t

(** The bootstrap profile. *)
val bootstrap_profile : t

(** [add_operator_profiles t proto_parameters gs_worker operator_profiles]
    registers operator profiles (attestor or producer).
    If the current profile is a bootstrap profile, it will return [None] as bootstrap
    profiles are incompatible with operator profiles. *)
val add_operator_profiles :
  t ->
  Dal_plugin.proto_parameters ->
  Gossipsub.Worker.t ->
  Services.Types.operator_profiles ->
  t option

(** [on_new_head t proto_parameters gs_worker committee] performs profile-related
    actions that depend on the current head, more precisely on the current committee. *)
val on_new_head :
  t ->
  Dal_plugin.proto_parameters ->
  Gossipsub.Worker.t ->
  Committee_cache.committee ->
  unit

(** [get_profiles node_store] returns the list of profiles that the node tracks *)
val get_profiles : t -> Services.Types.profiles

(** See {!Services.get_attestable_slots} *)
val get_attestable_slots :
  shard_indices:int list ->
  Store.node_store ->
  Dal_plugin.proto_parameters ->
  attested_level:int32 ->
  (Services.Types.attestable_slots, [Errors.decoding | Errors.other]) result
  Lwt.t
