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

(** {2 Consensus key type and functions} *)

type consensus_key = {
  alias : string option;
  public_key : Signature.public_key;
  public_key_hash : Signature.public_key_hash;
  secret_key_uri : Client_keys.sk_uri;
}

val consensus_key_encoding : consensus_key Data_encoding.t

val pp_consensus_key : Format.formatter -> consensus_key -> unit

type consensus_key_and_delegate = consensus_key * Signature.Public_key_hash.t

val consensus_key_and_delegate_encoding :
  consensus_key_and_delegate Data_encoding.t

val pp_consensus_key_and_delegate :
  Format.formatter -> consensus_key_and_delegate -> unit

(** {2 Delegates slots type and functions} *)

(** A delegate slot consists of the delegate's consensus key, its public key
    hash, its first slot, and its attesting power at some level. *)
type delegate_slot = {
  consensus_key_and_delegate : consensus_key_and_delegate;
  first_slot : Slot.t;
  attesting_power : int;
}

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

  (** Returns the voting power of the delegate whose first slot is the given
      slot. Returns [None] if the slot is not the first slot of any delegate. *)
  val voting_power : t -> slot:Slot.t -> int option
end

type delegate_slots = Delegate_slots.t

val pp_delegate_slots : Format.formatter -> delegate_slots -> unit

(** {2 Consensus operations types functions} *)

type consensus_vote_kind = Attestation | Preattestation

val consensus_vote_kind_encoding : consensus_vote_kind Data_encoding.t

val pp_consensus_vote_kind : Format.formatter -> consensus_vote_kind -> unit

type unsigned_consensus_vote = {
  vote_kind : consensus_vote_kind;
  vote_consensus_content : consensus_content;
  delegate : consensus_key_and_delegate;
  dal_content : dal_content option;
}

type signed_consensus_vote = {
  unsigned_consensus_vote : unsigned_consensus_vote;
  signed_operation : packed_operation;
}

type batch_content = {
  level : Raw_level.t;
  round : Round.t;
  block_payload_hash : Block_payload_hash.t;
}

type unsigned_consensus_vote_batch = private {
  batch_kind : consensus_vote_kind;
  batch_content : batch_content;
  batch_branch : Block_hash.t;
  unsigned_consensus_votes : unsigned_consensus_vote list;
}

val make_unsigned_consensus_vote_batch :
  consensus_vote_kind ->
  batch_content ->
  batch_branch:Block_hash.t ->
  (consensus_key_and_delegate * Slot.t) list ->
  unsigned_consensus_vote_batch

val dal_content_map_p :
  (unsigned_consensus_vote -> dal_content option tzresult Lwt.t) ->
  unsigned_consensus_vote_batch ->
  unsigned_consensus_vote_batch Lwt.t

type signed_consensus_vote_batch = private {
  batch_kind : consensus_vote_kind;
  batch_content : batch_content;
  batch_branch : Block_hash.t;
  signed_consensus_votes : signed_consensus_vote list;
}

val make_signed_consensus_vote_batch :
  consensus_vote_kind ->
  batch_content ->
  batch_branch:Block_hash.t ->
  signed_consensus_vote list ->
  signed_consensus_vote_batch tzresult

val make_singleton_consensus_vote_batch :
  signed_consensus_vote -> signed_consensus_vote_batch

(** {2 Block info types and functions}  *)

type prequorum = {
  level : int32;
  round : Round.t;
  block_payload_hash : Block_payload_hash.t;
  preattestations : Kind.preattestation operation list;
}

type block_info = {
  hash : Block_hash.t;
  shell : Block_header.shell_header;
  payload_hash : Block_payload_hash.t;
  payload_round : Round.t;
  round : Round.t;
  prequorum : prequorum option;
  quorum : Kind.attestation operation list;
  payload : Operation_pool.payload;
}

val block_info_encoding : block_info Data_encoding.t

val pp_block_info : Format.formatter -> block_info -> unit

(** {2 Proposal type and functions}  *)

type proposal = {block : block_info; predecessor : block_info}

val proposal_encoding : proposal Data_encoding.t

val pp_proposal : Format.formatter -> proposal -> unit

(** {2 Elected block type and functions} *)

type elected_block = {
  proposal : proposal;
  attestation_qc : Kind.attestation operation list;
}

val pp_elected_block : Format.formatter -> elected_block -> unit

(** Identify the first block of the protocol, ie. the block that
    activates the current protocol.

    This block should be baked by the baker of the previous protocol
    (that's why this same block is also referred to as the last block
    of the previous protocol). It is always considered final and
    therefore is not attested.*)
val is_first_block_in_protocol : proposal -> bool

(** {2 Locked_round type and functions} *)

type locked_round = {payload_hash : Block_payload_hash.t; round : Round.t}

val locked_round_encoding : locked_round Data_encoding.t

val pp_locked_round : Format.formatter -> locked_round -> unit

(** {2 Attestable_payload type and functions} *)

type attestable_payload = {proposal : proposal; prequorum : prequorum}

val attestable_payload_encoding : attestable_payload Data_encoding.t

val pp_attestable_payload : Format.formatter -> attestable_payload -> unit

(** {2 Level_state type and functions} *)

type level_state = {
  current_level : int32;
  latest_proposal : proposal;
  is_latest_proposal_applied : bool;
  locked_round : locked_round option;
  attestable_payload : attestable_payload option;
  elected_block : elected_block option;
  delegate_slots : delegate_slots;
  next_level_delegate_slots : delegate_slots;
  next_level_proposed_round : Round.t option;
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

type round_state = {
  current_round : Round.t;
  current_phase : phase;
  delayed_quorum : Kind.attestation operation list option;
  early_attestations : signed_consensus_vote list;
  awaiting_unlocking_pqc : bool;
}

val pp_round_state : Format.formatter -> round_state -> unit

(** {2 Forge types and functions} *)

type block_kind =
  | Fresh of Operation_pool.pool
  | Reproposal of {
      consensus_operations : packed_operation list;
      payload_hash : Block_payload_hash.t;
      payload_round : Round.t;
      payload : Operation_pool.payload;
    }

type block_to_bake = {
  predecessor : block_info;
  round : Round.t;
  delegate : consensus_key_and_delegate;
  kind : block_kind;
  force_apply : bool;
      (** if true, while baking the block, try and apply the block and its
          operations instead of only validating them. this can be permanently
          set using the [--force-apply] flag (see [force_apply_switch_arg] in
          [baking_commands.ml]). *)
}

(** [forge_request] type used to push a concurrent forging task in the
    forge worker. *)
type forge_request =
  | Forge_and_sign_block of block_to_bake
  | Forge_and_sign_preattestations of {
      unsigned_preattestations : unsigned_consensus_vote_batch;
    }
  | Forge_and_sign_attestations of {
      unsigned_attestations : unsigned_consensus_vote_batch;
    }

type prepared_block = {
  signed_block_header : block_header;
  round : Round.t;
  delegate : consensus_key_and_delegate;
  operations : Tezos_base.Operation.t list list;
  baking_votes : Per_block_votes_repr.per_block_votes;
}

val pp_prepared_block : Format.formatter -> prepared_block -> unit

(** [forge_event] type used to return the result of a task completion
    in the forge worker. *)
type forge_event =
  | Block_ready of prepared_block
  | Preattestation_ready of signed_consensus_vote
  | Attestation_ready of signed_consensus_vote

val forge_event_encoding : forge_event Data_encoding.t

val pp_forge_event : Format.formatter -> forge_event -> unit

(** [forge_worker_hooks] type that allows interactions with the forge
    worker. Hooks are needed in order to break a circular dependency. *)
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

type cache = {
  known_timestamps : Timestamp.time Baking_cache.Timestamp_of_round_cache.t;
  round_timestamps :
    (Timestamp.time * Round.t * consensus_key_and_delegate)
    Baking_cache.Round_timestamp_interval_cache.t;
}

val create_cache : unit -> cache

type global_state = {
  cctxt : Protocol_client_context.full;
  chain_id : Chain_id.t;
  config : Baking_configuration.t;
  constants : Constants.t;
  round_durations : Round.round_durations;
  operation_worker : Operation_worker.t;
  mutable forge_worker_hooks : forge_worker_hooks;
  validation_mode : validation_mode;
  delegates : consensus_key list;
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

val update_current_phase : t -> phase -> t

val record_state : t -> unit tzresult Lwt.t

val may_record_new_state :
  previous_state:t -> new_state:t -> unit tzresult Lwt.t

(** Returns, among our *own* delegates, the delegate (and its attesting slot)
    that has a proposer slot at the given round and the current or next level,
    if any. *)
val round_proposer :
  state -> level:[`Current | `Next] -> Round.t -> delegate_slot option

val may_load_attestable_data : t -> t tzresult Lwt.t

(** Memoization wrapper for [Round.timestamp_of_round]. *)
val timestamp_of_round :
  state ->
  predecessor_timestamp:Time.Protocol.t ->
  predecessor_round:Round.t ->
  round:Round.t ->
  Time.Protocol.t tzresult

(** @param block default to [`Head 0]*)
val compute_delegate_slots :
  Protocol_client_context.full ->
  ?block:Block_services.block ->
  level:int32 ->
  chain:Shell_services.chain ->
  consensus_key list ->
  delegate_slots tzresult Lwt.t

(** From the current [state], the function returns an optional
    association pair, which consists of the next round timestamp and its
    round. *)
val compute_next_round_time : state -> (Time.Protocol.t * Round.t) option

(** {2 Timeout type and functions} *)

type timeout_kind =
  | End_of_round of {ending_round : Round.t}
  | Time_to_prepare_next_level_block of {at_round : Round.t}

val timeout_kind_encoding : timeout_kind Data_encoding.t

val pp_timeout_kind : Format.formatter -> timeout_kind -> unit

(** {2 State_data type and functions} *)

type state_data = {
  level_data : int32;
  locked_round_data : locked_round option;
  attestable_payload_data : attestable_payload option;
}

val state_data_encoding : state_data Data_encoding.t

val load_attestable_data :
  Protocol_client_context.full ->
  [`State] Baking_files.location ->
  state_data option tzresult Lwt.t

(** {2 Event type and functions} *)

type event =
  | New_valid_proposal of proposal
  | New_head_proposal of proposal
  | Prequorum_reached of
      Operation_worker.candidate * Kind.preattestation operation list
  | Quorum_reached of
      Operation_worker.candidate * Kind.attestation operation list
  | New_forge_event of forge_event
  | Timeout of timeout_kind

val event_encoding : event Data_encoding.t

val pp_event : Format.formatter -> event -> unit
