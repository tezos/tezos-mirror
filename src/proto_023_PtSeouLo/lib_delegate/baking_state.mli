(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol
open Alpha_context
open Baking_state_types

val pp_delegate_slot : Format.formatter -> delegate_slot -> unit

module Delegate_slots : sig
  (** Information regarding the slot distribution at some level. *)
  type t

  (** Returns the list of our own delegates that have at least a slot. There are
      no duplicates, the associated slot is the first one. *)
  val own_delegates : t -> delegate_slot list

  (** Returns, among our *own* delegates, the delegate (together with its
      first attesting slot) that owns the given slot, if any (even if the
      given slot is not the delegate's first slot). *)
  val own_slot_owner : t -> slot:Slot.t -> delegate_slot option

  (** Returns, among our *own* delegates, the delegate (together with its
      first attesting slot) that owns the given round, if any. *)
  val own_round_owner :
    t -> committee_size:int -> round:Round.t -> delegate_slot option tzresult

  (** Returns the voting power of the delegate whose first slot is the given
      slot. Returns [None] if the slot is not the first slot of any delegate. *)
  val voting_power : t -> slot:Slot.t -> int option

  (** Finds the first slot greater than or equal to [slot]. Returns the
     corresponding (slot, delegate) pair if found, or [None] if no such slot
     exist. *)
  val find_first_slot_from : t -> slot:Slot.t -> (Slot.t * delegate_slot) option

  (** Returns the slot with the smallest index, along with its associated
      delegate. Returns [None] if the map is empty. *)
  val min_slot : t -> (Slot.t * delegate_slot) option
end

type delegate_slots = Delegate_slots.t

val pp_delegate_slots : Format.formatter -> delegate_slots -> unit

(** [compute_delegate_slots cctxt ?block ~level ~chain delegates] computes the
    delegate slots of the given [delegates] for the [level]
    @param block default to [`Head 0]*)
val compute_delegate_slots :
  Protocol_client_context.full ->
  ?block:Block_services.block ->
  level:int32 ->
  chain:Shell_services.chain ->
  Key.t list ->
  delegate_slots tzresult Lwt.t

(** {2 Consensus operations types functions} *)

type consensus_vote_kind = Attestation | Preattestation

val consensus_vote_kind_encoding : consensus_vote_kind Data_encoding.t

val pp_consensus_vote_kind : Format.formatter -> consensus_vote_kind -> unit

val pp_dal_content : Format.formatter -> dal_content -> unit

val dal_content_encoding : dal_content Data_encoding.t

(** An unsigned consensus vote consists of the consensus vote kind, either an
    attestation or a preattestation, the delegate keys and its protocol and dal
    content. *)
type unsigned_consensus_vote = {
  vote_kind : consensus_vote_kind;
  vote_consensus_content : consensus_content;
  delegate : Delegate.t;
  dal_content : dal_content option;
}

val pp_unsigned_consensus_vote :
  Format.formatter -> unsigned_consensus_vote -> unit

(** Partial encoding that omits secret keys to avoid leaking them in
    event logs; see
    {!Baking_state_types.Key.encoding_for_logging__cannot_decode}.

    Warning: As a consequence, decoding from this encoding will always
    fail. *)
val unsigned_consensus_vote_encoding_for_logging__cannot_decode :
  unsigned_consensus_vote Data_encoding.t

(** A batch content contains information common to all consensus operation in a
    batch of consensus votes. *)
type batch_content = {
  level : Raw_level.t;
  round : Round.t;
  block_payload_hash : Block_payload_hash.t;
}

(** An unsigned consensus batch consists of a batch kind: either preattestations
    or attestations, a batch content and a batch branch. These values should be
    common to each operations in the batch. This type also contains the list of
    unsigned consensus votes. *)
type unsigned_consensus_vote_batch = private {
  batch_kind : consensus_vote_kind;
  batch_content : batch_content;
  batch_branch : Block_hash.t;
  unsigned_consensus_votes : unsigned_consensus_vote list;
}

(** [make_unsigned_consensus_vote_batch kind batch_content ~batch_branch
    delegates_and_slots] maps the [delegates_and_slots] list to create a list of
    {!type-unsigned_consensus_vote} that is then included in an
    {!type-unsigned_consensus_vote_batch} with [kind], [batch_content] and
    [batch_branch]. Note: [dal_content] of each operations is set to [None],
    {!dal_content_map_p} needs to be called to update the [dal_content] field.
*)
val make_unsigned_consensus_vote_batch :
  consensus_vote_kind ->
  batch_content ->
  batch_branch:Block_hash.t ->
  (Delegate.t * Slot.t) list ->
  unsigned_consensus_vote_batch

(** [dal_content_map_p f unsigned_consensus_vote_batch] map each
    [unsigned_consensus_vote] in the batch with [f]. *)
val dal_content_map_p :
  (unsigned_consensus_vote -> dal_content option tzresult Lwt.t) ->
  unsigned_consensus_vote_batch ->
  unsigned_consensus_vote_batch Lwt.t

(** A signed consensus vote consists of an unsigned consensus vote and its
    packed version signed. *)
type signed_consensus_vote = {
  unsigned_consensus_vote : unsigned_consensus_vote;
  signed_operation : packed_operation;
}

val pp_signed_consensus_vote : Format.formatter -> signed_consensus_vote -> unit

(** Partial encoding that omits secret keys to avoid leaking them in
    event logs; see
    {!Baking_state_types.Key.encoding_for_logging__cannot_decode}.

    Warning: As a consequence, decoding from this encoding will always
    fail. *)
val signed_consensus_vote_encoding_for_logging__cannot_decode :
  signed_consensus_vote Data_encoding.t

(** Similar to {!unsigned_consensus_vote_batch} type but the list of the operation
    are signed consensus votes. *)
type signed_consensus_vote_batch = private {
  batch_kind : consensus_vote_kind;
  batch_content : batch_content;
  batch_branch : Block_hash.t;
  signed_consensus_votes : signed_consensus_vote list;
}

(** [make_signed_consensus_vote_batch batch_kind batch_content ~batch_branch
    signed_consensus_votes] creates a [signed_consensus_vote_batch]. This
    function raises an error if the batch is ill-formed:

    - batch consensus vote has a different kind than [batch_kind]
    - batch consensus vote has a different level than the one in [batch_content]
    - batch consensus vote has a different round than the one in [batch_content]
    - batch consensus vote has a different block hash payload than the one in
    [batch_content]
    - kind is [Preattestation] and a batch consensus vote contains a
    dal_content. *)
val make_signed_consensus_vote_batch :
  consensus_vote_kind ->
  batch_content ->
  batch_branch:Block_hash.t ->
  signed_consensus_vote list ->
  signed_consensus_vote_batch tzresult

(** [make_singleton_consensus_vote_batch signed_consensus_vote] similar to
    {!make_signed_consensus_vote_batch} but creates a batch with only one
    operation. *)
val make_singleton_consensus_vote_batch :
  signed_consensus_vote -> signed_consensus_vote_batch

(** {2 Block info types and functions}  *)

val block_info_encoding : block_info Data_encoding.t

val pp_block_info : Format.formatter -> block_info -> unit

(** {2 Proposal type and functions}  *)

val proposal_encoding : proposal Data_encoding.t

val pp_proposal : Format.formatter -> proposal -> unit

(** {2 Elected block type and functions} *)

(** An elected block is a [proposal] where the quorum has been reached. The type
    consists of the proposal and the quorum proof ie. an attestation list where
    the total voting power of the operation is higher than the protocol
    threshold. *)
type elected_block = {
  proposal : proposal;
  attestation_qc : Kind.attestation operation list;
}

val pp_elected_block : Format.formatter -> elected_block -> unit

(** Identify the first block of the protocol, ie. the block that activates the
    current protocol.

    This block should be baked by the baker of the previous protocol (that's why
    this same block is also referred to as the last block of the previous
    protocol). It is always considered final and therefore is not attested. *)
val is_first_block_in_protocol : proposal -> bool

(** {2 Locked_round type and functions} *)

(** A locked round is the last block attested by the baker. *)
type locked_round = {payload_hash : Block_payload_hash.t; round : Round.t}

val locked_round_encoding : locked_round Data_encoding.t

val pp_locked_round : Format.formatter -> locked_round -> unit

(** {2 Attestable_payload type and functions} *)

(** An attestable payload is a proposal on which the prequorum has been
    reached. *)
type attestable_payload = {proposal : proposal; prequorum : prequorum}

val attestable_payload_encoding : attestable_payload Data_encoding.t

val pp_attestable_payload : Format.formatter -> attestable_payload -> unit

(** {2 Level_state type and functions} *)

(** A level state consists of all usefull information regarding the current
    level such as the latest proposal, its status, the delegate slots, etc. *)
type level_state = {
  current_level : int32;
  latest_proposal : proposal;
      (** latest validated block by the node that the baker work on. *)
  is_latest_proposal_applied : bool;
      (** true when the proposal, validated by the block has also been
          applied. *)
  locked_round : locked_round option;
      (** Has value when the baker have attested the current proposal *)
  attestable_payload : attestable_payload option;
      (** Has value when a pqc has been reached on a previous or current
          round. *)
  elected_block : elected_block option;
      (** A quorum has been reached on an applied proposal. *)
  delegate_slots : delegate_slots;
      (** Delegate slots for the baker delegates at the current level *)
  next_level_delegate_slots : delegate_slots;
      (** Delegate slots for the baker delegates at the next level *)
  next_level_latest_forge_request : Round.t option;
      (** Some if a forge request has been sent for the next level on the given
          round *)
  dal_attestable_slots : dal_attestable_slots;
      (** For each (own) delegate having a DAL slot at the current level, store
          a promise to obtain the attestable slots for that level. *)
  next_level_dal_attestable_slots : dal_attestable_slots;
      (** and similarly for the next level *)
}

val pp_level_state : Format.formatter -> level_state -> unit

(** {2 Round_state type and functions} *)

type phase =
  | Idle
  | Awaiting_preattestations
  | Awaiting_attestations
  | Awaiting_application

val phase_encoding : phase Data_encoding.t

val pp_phase : Format.formatter -> phase -> unit

(** A round state consists of information related to the current round phase *)
type round_state = {
  current_round : Round.t;
  current_phase : phase;
  delayed_quorum : Kind.attestation operation list option;
      (** Has value when the Quorum has been reached but the current proposal
          has not yet been applied *)
  early_attestations : signed_consensus_vote list;
      (** Attestations ready for injection before the prequorum has been
          reached *)
  awaiting_unlocking_pqc : bool;
      (** Used to avoid preattesting reproposal if its round is lower than the
          one we are locked on *)
}

val pp_round_state : Format.formatter -> round_state -> unit

(** {2 Forge types and functions} *)

(** A block kind is either a reproposal of a proposal that has reached a
    prequorum, or a fresh block. *)
type block_kind =
  | Fresh of Operation_pool.pool
      (** A Fresh proposal contains the list its operations. *)
  | Reproposal of {
      consensus_operations : packed_operation list;
      payload_hash : Block_payload_hash.t;
      payload_round : Round.t;
      payload : Operation_pool.payload;
    }
      (** A Reproposal contains the consensus_operations as a proof that the
          prequorum was reached for this proposal. It also contains information
          about the payload of the previous proposal. *)

type block_to_bake = {
  predecessor : block_info;
  round : Round.t;
  delegate : Delegate.t;  (** Delegate that have the right to bake the block. *)
  kind : block_kind;  (** Either a reproposal or a fresh proposal *)
  force_apply : bool;
      (** if true, while baking the block, try and apply the block and its
          operations instead of only validating them. This can be set using the
          [--force-apply-from-round] flag (see [force_apply_from_round_arg] in
          [baking_commands.ml]). *)
}

(** [forge_request] type used to push a concurrent forging task in the forge
    worker. *)
type forge_request =
  | Forge_and_sign_block of block_to_bake
  | Forge_and_sign_preattestations of {
      unsigned_preattestations : unsigned_consensus_vote_batch;
    }
  | Forge_and_sign_attestations of {
      unsigned_attestations : unsigned_consensus_vote_batch;
    }

(** [manager_operations_infos] contains information about the number of manager
    operations in the forged block and the summing fees from these operations *)
type manager_operations_infos = {
  manager_operation_number : int;
  total_fees : Int64.t;
}

val manager_operations_infos_encoding : manager_operations_infos Data_encoding.t

(** [prepared_block] type returned by the forge worker and that contains all
    information useful for block injection. *)
type prepared_block = {
  signed_block_header : block_header;
  round : Round.t;
  delegate : Delegate.t;
  operations : Tezos_base.Operation.t list list;
  manager_operations_infos : manager_operations_infos option;
  baking_votes : Per_block_votes_repr.per_block_votes;
}

val pp_prepared_block : Format.formatter -> prepared_block -> unit

(** [forge_event] type used to return the result of a task completion in the
    forge worker. *)
type forge_event =
  | Block_ready of prepared_block
  | Preattestation_ready of signed_consensus_vote
  | Attestation_ready of signed_consensus_vote

val pp_forge_event : Format.formatter -> forge_event -> unit

(** Partial encoding for {!forge_event} that omits secret keys to
    avoid leaking them in event logs; see
    {!Baking_state_types.Key.encoding_for_logging__cannot_decode}.

    Warning: As a consequence, decoding from this encoding will always
    fail. *)
val forge_event_encoding_for_logging__cannot_decode :
  forge_event Data_encoding.t

(** [forge_worker_hooks] type that allows interactions with the forge worker.
    Hooks are needed in order to break a circular dependency. *)
type forge_worker_hooks = {
  push_request : forge_request -> unit;
  get_forge_event_stream : unit -> forge_event Lwt_stream.t;
  cancel_all_pending_tasks : unit -> unit;
}

(** {2 Global_state types and functions} *)

(** The validation mode specifies whether the baker (filters and) validates
    mempool operations via an RPC to the node, or if it does so "locally", by
    using the context. *)
type validation_mode = Node | Local of Abstract_context_index.t

val pp_validation_mode : Format.formatter -> validation_mode -> unit

(** Caches of timestamps *)
type cache = {
  known_timestamps : Timestamp.time Baking_cache.Timestamp_of_round_cache.t;
}

val create_cache : unit -> cache

(** A global state contains all information related to the chain and baker
    configuration. *)
type global_state = {
  cctxt : Protocol_client_context.full;
  chain_id : Chain_id.t;
  config : Baking_configuration.t;
  constants : Constants.t;
  round_durations : Round.round_durations;
  operation_worker : Operation_worker.t;
  mutable forge_worker_hooks : forge_worker_hooks;
  validation_mode : validation_mode;
  delegates : Key.t list;
  cache : cache;
  dal_node_rpc_ctxt : Tezos_rpc.Context.generic option;
}

val pp_global_state : Format.formatter -> global_state -> unit

type state = {
  global_state : global_state;
  level_state : level_state;
  round_state : round_state;
}

type t = state

val pp : Format.formatter -> t -> unit

(** [update_current_phase t new_phase] updates the round state in [t] with the
    [new_phase]. *)
val update_current_phase : t -> phase -> t

(** Returns, among our *own* delegates, the delegate (and its attesting slot)
    that has a proposer slot at the given round and the current or next level,
    if any. *)
val round_proposer :
  state -> level:[`Current | `Next] -> Round.t -> delegate_slot option

(** Memoization wrapper for {!Round.timestamp_of_round}. *)
val timestamp_of_round :
  state ->
  predecessor_timestamp:Time.Protocol.t ->
  predecessor_round:Round.t ->
  round:Round.t ->
  Time.Protocol.t tzresult

(** From the current [state], the function returns an optional association pair,
    which consists of the next round timestamp and its round. *)
val compute_next_round_time : state -> (Time.Protocol.t * Round.t) option

(** {2 Timeout type and functions} *)

(** timeout kind is either a timeout triggered at the end of a round or a
    timeout triggered when it is time to prepare the block at the next level. *)
type timeout_kind =
  | End_of_round of {ending_round : Round.t}
  | Time_to_prepare_next_level_block of {at_round : Round.t}

val timeout_kind_encoding : timeout_kind Data_encoding.t

val pp_timeout_kind : Format.formatter -> timeout_kind -> unit

(** {2 State_data type and functions} *)

(** [record_state t] writes the current level, the locked round and the
    attestable payload to the disk. *)
val record_state : t -> unit tzresult Lwt.t

(** [may_record_new_state ~previous_state ~new_state] Does nothing if the baker
    configuration is set to {!Baking_configuration.Memory} only. Otherwise,
    check that information from the [new_state] are consistent with
    [previous_state] and if the information has been updated, record the
    [new_state] to the disk. *)
val may_record_new_state :
  previous_state:t -> new_state:t -> unit tzresult Lwt.t

(** type used for data recorded on disk. *)
type state_data = {
  level_data : int32;
  locked_round_data : locked_round option;
  attestable_payload_data : attestable_payload option;
}

val state_data_encoding : state_data Data_encoding.t

(** [load_attestable_data cctxt location] tries to load the [state_data] from
    the [cctxt] at [location], returns [None] if the file does not exists. *)
val load_attestable_data :
  Protocol_client_context.full ->
  [`State] Baking_files.location ->
  state_data option tzresult Lwt.t

(** [may_load_attestable_data t] tries to load locked round and attestable
    payload from [t] if it is recorded, if not, return the state. *)
val may_load_attestable_data : t -> t tzresult Lwt.t

(** {2 Event type and functions} *)

(** Event type used in the [baking_scheduling] to trigger state transition. *)
type event =
  | New_valid_proposal of proposal
  | New_head_proposal of proposal
  | Prequorum_reached of
      Operation_worker.candidate * Kind.preattestation operation list
  | Quorum_reached of
      Operation_worker.candidate * Kind.attestation operation list
  | New_forge_event of forge_event
  | Timeout of timeout_kind

val pp_event : Format.formatter -> event -> unit

(** Prints event description in a few words. *)
val pp_short_event : Format.formatter -> event -> unit

(** Partial encoding for {!event} that omits secret keys to avoid
    leaking them in event logs; see
    {!Baking_state_types.Key.encoding_for_logging__cannot_decode}.

    Warning: As a consequence, decoding from this encoding will always
    fail. *)
val event_encoding_for_logging__cannot_decode : event Data_encoding.t
