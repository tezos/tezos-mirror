(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** [is_prover_profile profile] returns [true] if producing proofs is part of
    the activity of the provided [profile]. This is the case for observer and
    slot producers but bootstrap and attester profiles never need to produce
    proofs. *)
val is_prover_profile : t -> bool

(** [is_attester_only_profile profile] returns [true] if the node has an
    operator profile, with at least one attester role and no producer nor
    observer roles. *)
val is_attester_only_profile : t -> bool

val encoding : t Data_encoding.t

(** The empty profile manager context. *)
val empty : t

val bootstrap : t

(** [operator op] returns an operator with the profile described by [op] *)
val operator : Operator_profile.t -> t

(** Merge the two sets of profiles. In case of incompatibility (that is, case
   [Bootstrap] vs the other kinds), the profiles from [higher_prio] take
   priority. *)
val merge_profiles : lower_prio:t -> higher_prio:t -> t

(** [add_operator_profiles t proto_parameters gs_worker operator_profiles]
    registers operator profiles (attester or producer).
    If the current profile is a bootstrap profile, it will return [None] as bootstrap
    profiles are incompatible with operator profiles. *)
val add_operator_profiles :
  t ->
  Dal_plugin.proto_parameters ->
  Gossipsub.Worker.t ->
  Operator_profile.t ->
  t option

(** [add_profiles t proto_parameters gs_worker profiles] registers [profiles].
    If the current profiles are incompatible with provided [profiles], it
    returns [None]. *)
val add_profiles :
  t -> Dal_plugin.proto_parameters -> Gossipsub.Worker.t -> t -> t option

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
val get_profiles : t -> Types.profile

(** Returns the number of previous blocks for which the node should keep the
    shards in the storage, depending on the profile of the node (3 months for
    observer & slot producer, twice attestation lag for attester) *)
val get_attested_data_default_store_period :
  t -> Dal_plugin.proto_parameters -> int

(** Returns [true] iff the node should support refutation games. *)
val supports_refutations : t -> bool

(** Load the profile context from disk. The file where the context is loaded
    from is relative to the given [base_dir]. An error is returned in case of an
    IO failure or an ill formatted file. *)
val load_profile_ctxt : base_dir:string -> t tzresult Lwt.t

(** Save the profile context to disk. The file where the context is saved is
    relative to the given [base_dir]. An error is returned in case of an
    IO failure. *)
val save_profile_ctxt : t -> base_dir:string -> unit tzresult Lwt.t
