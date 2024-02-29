(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Trili tech, Inc. <contact@trili.tech>                  *)
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

(** State of the validation.

    Two parts:

    1. Context.t: what is stored between blocks, this includes an
    Irmin tree typically stored on disk and the cache (stored in
    RAM).

    2. Additional information needed during the validation of a
    block but not persisted across blocks, always stored in
    RAM. The gas counter is here.

    [Alpha_context.t] is actually implemented as [Raw_context.t].
    The difference is that Alpha_context.mli does not expose this
    so functions manipulating an Alpha_context.t are guaranteed
    to only access the context through the storage modules
    exposed in Alpha_context.mli. These modules are in charge of
    maintaining invariants over the structure of the context. *)

(** {1 Errors} *)

type error += Too_many_internal_operations (* `Permanent *)

type missing_key_kind = Get | Set | Del | Copy

(** An internal storage error that should not happen *)
type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * missing_key_kind
  | Existing_key of string list
  | Corrupted_data of string list

type error += Storage_error of storage_error

type error += Failed_to_parse_parameter of bytes

type error += Failed_to_decode_parameter of Data_encoding.json * string

val storage_error : storage_error -> 'a tzresult

(** {1 Abstract Context} *)

(** Abstract view of the context.
    Includes a handle to the functional key-value database
    ({!Context.t}) along with some in-memory values (gas, etc.). *)
type t

type root = t

(** The internal message to be injected into the smart rollups’ shared
    inbox when validating the very first block of this protocol. *)
val protocol_migration_internal_message :
  Sc_rollup_inbox_message_repr.internal_inbox_message

(** Serialized version of {!protocol_migration_internal_message}. *)
val protocol_migration_serialized_message :
  Sc_rollup_inbox_message_repr.serialized

(** Retrieves the state of the database and gives its abstract view.
    It also returns wether this is the first block validated
    with this version of the protocol. *)
val prepare :
  level:Int32.t ->
  predecessor_timestamp:Time.t ->
  timestamp:Time.t ->
  adaptive_issuance_enable:bool ->
  Context.t ->
  t tzresult Lwt.t

type previous_protocol = Genesis of Parameters_repr.t | Oxford_018

val prepare_first_block :
  level:int32 ->
  timestamp:Time.t ->
  Chain_id.t ->
  Context.t ->
  (previous_protocol * Constants_parametric_previous_repr.t option * t) tzresult
  Lwt.t

val activate : t -> Protocol_hash.t -> t Lwt.t

(** Returns the state of the database resulting of operations on its
    abstract view *)
val recover : t -> Context.t

val current_level : t -> Level_repr.t

val predecessor_timestamp : t -> Time.t

val current_timestamp : t -> Time.t

val constants : t -> Constants_parametric_repr.t

val sc_rollup : t -> Constants_parametric_repr.sc_rollup

val zk_rollup : t -> Constants_parametric_repr.zk_rollup

val patch_constants :
  t -> (Constants_parametric_repr.t -> Constants_parametric_repr.t) -> t Lwt.t

val round_durations : t -> Round_repr.Durations.t

(** Retrieve the cycle eras. *)
val cycle_eras : t -> Level_repr.cycle_eras

(** Increment the current block fee stash that will be credited to the payload
    producer's account at finalize_application *)
val credit_collected_fees_only_call_from_token : t -> Tez_repr.t -> t tzresult

(** Decrement the current block fee stash that will be credited to the payload
    producer's account at finalize_application *)
val spend_collected_fees_only_call_from_token : t -> Tez_repr.t -> t tzresult

(** Returns the current block fee stash that will be credited to the payload
    producer's account at finalize_application *)
val get_collected_fees : t -> Tez_repr.t

(** [consume_gas_limit_in_block ctxt gas_limit] checks that
    [gas_limit] is well-formed (i.e. it does not exceed the hard gas
    limit per operation as defined in [ctxt], and it is positive), then
    consumes [gas_limit] in the current block gas level of [ctxt].

    @return [Error Gas_limit_repr.Gas_limit_too_high] if [gas_limit]
    is greater than the allowed limit for operation gas level or
    negative.

    @return [Error Block_quota_exceeded] if not enough gas remains in
    the block. *)
val consume_gas_limit_in_block : t -> 'a Gas_limit_repr.Arith.t -> t tzresult

val set_gas_limit : t -> 'a Gas_limit_repr.Arith.t -> t

val set_gas_unlimited : t -> t

val gas_level : t -> Gas_limit_repr.t

val gas_consumed : since:t -> until:t -> Gas_limit_repr.Arith.fp

val remaining_operation_gas : t -> Gas_limit_repr.Arith.fp

val update_remaining_operation_gas : t -> Gas_limit_repr.Arith.fp -> t

val block_gas_level : t -> Gas_limit_repr.Arith.fp

val update_remaining_block_gas : t -> Gas_limit_repr.Arith.fp -> t

type error += Undefined_operation_nonce (* `Permanent *)

(** [init_origination_nonce ctxt hash] initialise the origination nonce in
    memory from [hash]. See [Origination_nonce.t] for more information. *)
val init_origination_nonce : t -> Operation_hash.t -> t

val get_origination_nonce : t -> Origination_nonce.t tzresult

val increment_origination_nonce : t -> (t * Origination_nonce.t) tzresult

(** [unset_origination_nonce ctxt] unset the origination nonce in memory. To be
    used only when no more origination can be done in that operation. See
    [Origination_nonce.t] for more information. *)
val unset_origination_nonce : t -> t

(** {1 Generic accessors} *)

type key = string list

type value = bytes

type tree

type local_context

module type T =
  Raw_context_intf.T
    with type root := root
     and type key := key
     and type value := value
     and type tree := tree

include T with type t := t and type local_context := local_context

(** Initialize the local nonce used for preventing a script to
    duplicate an internal operation to replay it. *)
val reset_internal_nonce : t -> t

(** Increments the internal operation nonce. *)
val fresh_internal_nonce : t -> (t * int) tzresult

(** Mark an internal operation nonce as taken. *)
val record_internal_nonce : t -> int -> t

(** Check is the internal operation nonce has been taken. *)
val internal_nonce_already_recorded : t -> int -> bool

val fold_map_temporary_lazy_storage_ids :
  t ->
  (Lazy_storage_kind.Temp_ids.t -> Lazy_storage_kind.Temp_ids.t * 'res) ->
  t * 'res

val map_temporary_lazy_storage_ids_s :
  t ->
  (Lazy_storage_kind.Temp_ids.t -> (t * Lazy_storage_kind.Temp_ids.t) Lwt.t) ->
  t Lwt.t

module Cache : sig
  include
    Context.CACHE
      with type t := t
       and type size := int
       and type index := int
       and type identifier := string
       and type key = Context.Cache.key
       and type value = Context.Cache.value

  val sync : t -> bytes -> t Lwt.t
end

(* Hashes of non-consensus operations are stored so that, when
   finalizing the block, we can compute the block's payload hash. *)
val record_non_consensus_operation_hash : t -> Operation_hash.t -> t

val non_consensus_operations : t -> Operation_hash.t list

type consensus_pk = {
  delegate : Signature.Public_key_hash.t;
  consensus_pk : Signature.Public_key.t;
  consensus_pkh : Signature.Public_key_hash.t;
}

val consensus_pk_encoding : consensus_pk Data_encoding.t

(** Record that the dictator already voted in this block. *)
val record_dictator_proposal_seen : t -> t

(** Checks whether the dictator voted in this block. *)
val dictator_proposal_seen : t -> bool

(** [init_sampler_for_cycle ctxt cycle seed state] caches the seeded stake
    sampler (a.k.a. [seed, state]) for [cycle] in memory for quick access.

    @return [Error Sampler_already_set] if the sampler was already
    cached. *)
val init_sampler_for_cycle :
  t -> Cycle_repr.t -> Seed_repr.seed -> consensus_pk Sampler.t -> t tzresult

(** [sampler_for_cycle ~read ctxt cycle] returns the seeded stake
    sampler for [cycle]. The sampler is read in memory if
    [init_sampler_for_cycle] or [sampler_for_cycle] was previously
    called for the same [cycle]. Otherwise, it is read "on-disk" with
    the [read] function and then cached in [ctxt] like
    [init_sampler_for_cycle]. *)
val sampler_for_cycle :
  read:(t -> (Seed_repr.seed * consensus_pk Sampler.t) tzresult Lwt.t) ->
  t ->
  Cycle_repr.t ->
  (t * Seed_repr.seed * consensus_pk Sampler.t) tzresult Lwt.t

(* The stake distribution is stored both in [t] and in the cache. It
   may be sufficient to only store it in the cache. *)
val stake_distribution_for_current_cycle :
  t -> Stake_repr.t Signature.Public_key_hash.Map.t tzresult

(** Like [stake_distribution_for_current_cycle] but returns [None] rather than
    an error. *)
val find_stake_distribution_for_current_cycle :
  t -> Stake_repr.t Signature.Public_key_hash.Map.t option

val init_stake_distribution_for_current_cycle :
  t -> Stake_repr.t Signature.Public_key_hash.Map.t -> t

(** Returns the reward coefficient for the current cycle
    This value is equal to the value in {!Storage.Issuance_coeff} if it exists,
    or equal to [Q.one] otherwise. *)
val reward_coeff_for_current_cycle : t -> Q.t

(** Updates the reward coefficient for the current cycle.
    This update should only be called once per cycle. It is done in
    [Adaptive_issuance_storage] *)
val update_reward_coeff_for_current_cycle : t -> Q.t -> t

(** Returns true if adaptive issuance has launched. *)
val adaptive_issuance_enable : t -> bool

(** Set the feature flag of adaptive issuance. *)
val set_adaptive_issuance_enable : t -> t

module Internal_for_tests : sig
  val add_level : t -> int -> t

  val add_cycles : t -> int -> t
end

module type CONSENSUS = sig
  type t

  type 'value slot_map

  type slot_set

  type slot

  type round

  type consensus_pk

  (** Returns a map where each attester's pkh is associated to the
     list of its attesting slots (in decreasing order) for a given
     level. *)
  val allowed_attestations : t -> (consensus_pk * int) slot_map option

  (** Returns a map where each attester's pkh is associated to the
     list of its attesting slots (in decreasing order) for a given
     level. *)
  val allowed_preattestations : t -> (consensus_pk * int) slot_map option

  (** Returns the set of delegates that are not allowed to bake or
      attest blocks; i.e., delegates which have zero frozen deposit
      due to a previous slashing. *)
  val forbidden_delegates : t -> Signature.Public_key_hash.Set.t

  (** Missing pre-computed map by first slot. This error should not happen. *)
  type error += Slot_map_not_found of {loc : string}

  (** [attestation power ctx] returns the attestation power of the
     current block. *)
  val current_attestation_power : t -> int

  (** Initializes the map of allowed attestations and preattestations, this
      function must be called only once and before applying any consensus
      operation. *)
  val initialize_consensus_operation :
    t ->
    allowed_attestations:(consensus_pk * int) slot_map option ->
    allowed_preattestations:(consensus_pk * int) slot_map option ->
    t

  (** [record_attestation ctx ~initial_slot ~power] records an
     attestation for the current block.

      The attestation should be valid in the sense that
      [Int_map.find_opt initial_slot allowed_attestation ctx = Some
      (pkh, power)].  *)
  val record_attestation : t -> initial_slot:slot -> power:int -> t tzresult

  (** [record_preattestation ctx ~initial_slot ~power round
     payload_hash power] records a preattestation for a proposal at
     [round] with payload [payload_hash].

      The preattestation should be valid in the sense that
     [Int_map.find_opt initial_slot allowed_preattestation ctx = Some
     (pkh, power)].  *)
  val record_preattestation :
    t -> initial_slot:slot -> power:int -> round -> t tzresult

  (** [forbid_delegate ctx delegate] adds [delegate] to the set of
      forbidden delegates, which prevents this delegate from baking or
      attesting. *)
  val forbid_delegate : t -> Signature.Public_key_hash.t -> t

  (** [set_forbidden_delegate ctx delegates] sets [delegates] as the
      current forbidden delegates. *)
  val set_forbidden_delegates : t -> Signature.Public_key_hash.Set.t -> t

  val attestations_seen : t -> slot_set

  (** [get_preattestations_quorum_round ctx] returns [None] if no
     preattestation are included in the current block. Otherwise,
     return [Some r] where [r] is the round of the preattestations
     included in the block. *)
  val get_preattestations_quorum_round : t -> round option

  (** [set_preattestations_quorum_round ctx round] sets the round for
     preattestations included in this block. This function should be
     called only once.

      This function is only used in [Full_construction] mode.  *)
  val set_preattestations_quorum_round : t -> round -> t

  (** [locked_round_evidence ctx] returns the round of the recorded
     preattestations as well as their power. *)
  val locked_round_evidence : t -> (round * int) option

  val set_attestation_branch : t -> Block_hash.t * Block_payload_hash.t -> t

  val attestation_branch : t -> (Block_hash.t * Block_payload_hash.t) option
end

module Consensus :
  CONSENSUS
    with type t := t
     and type slot := Slot_repr.t
     and type 'a slot_map := 'a Slot_repr.Map.t
     and type slot_set := Slot_repr.Set.t
     and type round := Round_repr.t
     and type consensus_pk := consensus_pk

module Sc_rollup_in_memory_inbox : sig
  val current_messages : t -> Sc_rollup_inbox_merkelized_payload_hashes_repr.t

  val set_current_messages :
    t -> Sc_rollup_inbox_merkelized_payload_hashes_repr.t -> t
end

module Dal : sig
  type cryptobox = Dal.t

  val make : t -> (t * cryptobox) tzresult

  val number_of_slots : t -> int

  (** [record_attested_shards ctxt attestation shards] records that the
     list of shards [shards] were attested (declared available by some
     attester). The function assumes that a shard belongs to the
     interval [0; number_of_shards - 1]. Otherwise, for each shard
     outside this interval, it is a no-op. *)
  val record_attested_shards : t -> Dal_attestation_repr.t -> int list -> t

  (** [register_slot_header ctxt slot_header] returns a new context
     where the new candidate [slot] have been taken into
     account. Returns [Some (ctxt,updated)] where [updated=true] if
     the candidate is registered. [Some (ctxt,false)] if another
     candidate was already registered previously. Returns an error if
     the slot is invalid. *)
  val register_slot_header : t -> Dal_slot_repr.Header.t -> t tzresult

  (** [candidates ctxt] returns the current list of slot for which
     there is at least one candidate. *)
  val candidates : t -> Dal_slot_repr.Header.t list

  (** [is_slot_index_attested ctxt slot_index] returns [true] if the
     [slot_index] is declared available by the protocol. [false]
     otherwise. If the [index] is out of the interval
     [0;number_of_slots - 1], returns [false]. *)
  val is_slot_index_attested : t -> Dal_slot_index_repr.t -> bool

  (** [shards_of_attester ctxt ~attester] returns the shard assignment
     of the DAL committee of the current level for [attester]. This
     function never returns an empty list. *)
  val shards_of_attester :
    t -> attester:Signature.Public_key_hash.t -> int list option

  (** The DAL committee is a subset of the Tenderbake committee.  A
     shard from [0; number_of_shards - 1] is associated to a public key
     hash. The committee is a mapping from public key hashes to shards and
     The DAL committee ensures the shards associated to the
     same public key hash are contiguous. The list of shards is
     represented as two natural numbers [(initial, power)] which
     encodes the list of shards:
     [initial; initial + 1; ... ; initial + power - 1]. *)
  type committee = {
    pkh_to_shards :
      (Dal_attestation_repr.shard_index * int) Signature.Public_key_hash.Map.t;
  }

  (** [compute_committee ctxt pkh_from_tenderbake_slot] computes the
     DAL committee using the [pkh_from_tenderbake_slot] function. This
     functions takes into account the fact that the DAL committee and
     the Tenderbake committee may have different sizes. If the DAL
     committee is smaller, then we simply take a projection of the
     Tenderbake committee for the first [n] slots. If the DAL
     committee is larger, shards are computed modulo the Tenderbake
     committee. Slots assignments are reordered for a given a public
     key hash to ensure all the slots (or shards in the context of
     DAL) shards are contiguous (see {!type:committee}). *)
  val compute_committee :
    t ->
    (Slot_repr.t -> (t * Signature.Public_key_hash.t) tzresult Lwt.t) ->
    committee tzresult Lwt.t

  (** [init_committee ctxt committee] returns a context where the
     [committee] is cached. The committee is expected to be the one
     for the current level. *)
  val init_committee : t -> committee -> t
end
