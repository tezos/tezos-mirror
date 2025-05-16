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

(** Type for a "profile" produced from CLI parsing and/or reading the
    configuration file, which may be refined (for instance by determining
    the slot to be observed for "random observers"). *)
type unresolved_profile = Profile of t | Random_observer | Empty

(** [is_bootstrap_profile t] returns [true] if the node has a bootstrap profile. *)
val is_bootstrap_profile : t -> bool

(** [is_prover_profile profile] returns [true] if producing proofs is part of
    the activity of the provided [profile]. This is the case for observer and
    slot producers but bootstrap and attester profiles never need to produce
    proofs. *)
val is_prover_profile : t -> bool

(** [is_empty profile] returns [true] if it is a [Controller] profile
    which is empty. *)
val is_empty : t -> bool

(** [is_attester_only_profile profile] returns [true] if the node is in
    controller mode, with at least one attester role and no producer nor
    observer roles. *)
val is_attester_only_profile : t -> bool

(** [can_publish_on_slot_index slot_index profile] returns [true] if
    the node has either the producer or the observer profile of the
    given slot index. *)
val can_publish_on_slot_index : Types.slot_index -> t -> bool

val encoding : t Data_encoding.t

val unresolved_encoding : unresolved_profile Data_encoding.t

(** The empty profile manager context. *)
val empty : t

val bootstrap : unresolved_profile

val random_observer : unresolved_profile

(** [controller profiles] returns an unresolved profile matching [profiles] *)
val controller : Controller_profiles.t -> unresolved_profile

(** Merge the two sets of profiles. In case of incompatibility (that is, case
   [Bootstrap] vs the other kinds), the profiles from [higher_prio] take
   priority. *)
val merge_profiles :
  lower_prio:unresolved_profile ->
  higher_prio:unresolved_profile ->
  unresolved_profile

(** [add_and_register_controller_profiles t ~number_of_slots gs_worker
    controller_profile] adds the new [controller_profiles] to [t]. It registers any
    new attester profile within [controller_profiles] with gossipsub, that is, it
    instructs the [gs_worker] to join the corresponding topics.

    If the current profile [t] is a bootstrap profile, it will return [None] as
    bootstrap profiles are incompatible with controller profiles.

    It assumes the current profile is not a random observer. *)
val add_and_register_controller_profiles :
  t ->
  number_of_slots:int ->
  Gossipsub.Worker.t ->
  Controller_profiles.t ->
  t option

(** [register_profile t ~number_of_slots gs_worker] does the following:

    - It registers the attester profiles within [t] with gossipsub, that is, it
    instructs the [gs_worker] to join the corresponding topics.

    - If [t] is the random observer profile, then it randomly selects a slot
    index, and transforms the profile into an observer profile for the selected
    slot index.

    The function returns the updated profile. *)
val register_profile : t -> number_of_slots:int -> Gossipsub.Worker.t -> t

(** Checks that each producer (operator or observer) profile only refers to slot
    indexes strictly smaller than [number_of_slots]. This may not be the case
    when the profile context is first built because there is no information
    about the number of slots. Returns an [Invalid_slot_index] error if the
    check fails. *)
val validate_slot_indexes : t -> number_of_slots:int -> unit tzresult

(** [on_new_head t ~number_of_slots gs_worker committee] performs profile-related
    actions that depend on the current head, more precisely on the current committee. *)
val on_new_head :
  t ->
  number_of_slots:int ->
  Gossipsub.Worker.t ->
  Committee_cache.committee ->
  unit

(** [get_profiles node_store] returns the list of profiles that the node tracks *)
val get_profiles : t -> Types.profile

(** Returns the number of previous blocks for which the node should keep the
    shards in the storage, depending on the profile of the node (3 months for
    operators, twice attestation lag for observers and attesters.) *)
val get_attested_data_default_store_period : t -> Types.proto_parameters -> int

(** Resolves a profile by either returning it unchanged (for bootstrap
    and controller profiles) or generating a new observer profile for
    random observer profile. The random slot is selected within the
    DAL slots range defined by the protocol parameters.

    This function is called when generating an observer profile from a
    random profile before launching a DAL node, as implemented in the
    daemon. *)
val resolve_profile : unresolved_profile -> number_of_slots:int -> t

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

(** [get_storage_period profile_ctxt proto_parameters ~head_level
    ~first_seen_level] computes the number of levels of DAL data the node should
    retain, based on its profile and L1 activity.

    For nodes that support refutations, the returned storage period is the
    minimum between:

    - the default retention period defined by the profile context, and

    - the number of levels the node has been online (i.e. since
    [first_seen_level]), capped to ensure at least two attestation lags worth of
    levels are always retained.

    For nodes that do not support refutations, the returned value is always the
    default period.

    This function is used to decide how far back attested data should be kept in
    the store. *)
val get_storage_period :
  t ->
  Types.proto_parameters ->
  head_level:int32 ->
  first_seen_level:int32 option ->
  int
