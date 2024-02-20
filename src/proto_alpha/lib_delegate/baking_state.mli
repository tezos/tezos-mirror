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

(** The validation mode specifies whether the baker (filters and) validates
    mempool operations via an RPC to the node, or if it does so "locally", by
    using the context. *)
type validation_mode = Node | Local of Abstract_context_index.t

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

type cache = {
  known_timestamps : Timestamp.time Baking_cache.Timestamp_of_round_cache.t;
  round_timestamps :
    (Timestamp.time * Round.t * consensus_key_and_delegate)
    Baking_cache.Round_timestamp_interval_cache.t;
}

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

val block_info_encoding : block_info Data_encoding.t

val round_of_shell_header : Block_header.shell_header -> Round.t tzresult

module SlotMap : Map.S with type key = Slot.t

(** A delegate slot consists of the delegate's consensus key, its public key
    hash, its first slot, and its attesting power at some level. *)
type delegate_slot = {
  consensus_key_and_delegate : consensus_key_and_delegate;
  first_slot : Slot.t;
  attesting_power : int;
}

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

type proposal = {block : block_info; predecessor : block_info}

val proposal_encoding : proposal Data_encoding.t

(** Identify the first block of the protocol, ie. the block that
    activates the current protocol.

    This block should be baked by the baker of the previous protocol
    (that's why this same block is also referred to as the last block
    of the previous protocol). It is always considered final and
    therefore is not attested.*)
val is_first_block_in_protocol : proposal -> bool

type locked_round = {payload_hash : Block_payload_hash.t; round : Round.t}

val locked_round_encoding : locked_round Data_encoding.t

type attestable_payload = {proposal : proposal; prequorum : prequorum}

val attestable_payload_encoding : attestable_payload Data_encoding.t

type elected_block = {
  proposal : proposal;
  attestation_qc : Kind.attestation operation list;
}

type prepared_block = {
  signed_block_header : block_header;
  round : Round.t;
  delegate : consensus_key_and_delegate;
  operations : Tezos_base.Operation.t list list;
}

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
  next_forged_block : prepared_block option;
}

type phase =
  | Idle
  | Awaiting_preattestations
  | Awaiting_attestations
  | Awaiting_application

val phase_encoding : phase Data_encoding.t

type round_state = {
  current_round : Round.t;
  current_phase : phase;
  delayed_quorum : Kind.attestation operation list option;
}

(** [forge_event] type used to return the result of a task completion
    in the forge worker. *)
type forge_event

(** [forge_request] type used to push a concurrent forging task in the
    forge worker. *)
type forge_request

(** [forge_worker_hooks] type that allows interactions with the forge
    worker. Hooks are needed in order to break a circular dependency. *)
type forge_worker_hooks = {
  push_request : forge_request -> unit;
  get_forge_event_stream : unit -> forge_event Lwt_stream.t;
}

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

type state = {
  global_state : global_state;
  level_state : level_state;
  round_state : round_state;
}

type t = state

val update_current_phase : t -> phase -> t

(** Returns, among our *own* delegates, the delegate (and its attesting slot)
    that has a proposer slot at the given round and the current or next level,
    if any. *)
val round_proposer :
  state -> level:[`Current | `Next] -> Round.t -> delegate_slot option

type timeout_kind =
  | End_of_round of {ending_round : Round.t}
  | Time_to_bake_next_level of {at_round : Round.t}
  | Time_to_forge_block

val timeout_kind_encoding : timeout_kind Data_encoding.t

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

type state_data = {
  level_data : int32;
  locked_round_data : locked_round option;
  attestable_payload_data : attestable_payload option;
}

val state_data_encoding : state_data Data_encoding.t

val record_state : t -> unit tzresult Lwt.t

val may_record_new_state :
  previous_state:t -> new_state:t -> unit tzresult Lwt.t

val load_attestable_data :
  Protocol_client_context.full ->
  [`State] Baking_files.location ->
  state_data option tzresult Lwt.t

val may_load_attestable_data : t -> t tzresult Lwt.t

(** @param block default to [`Head 0]*)
val compute_delegate_slots :
  Protocol_client_context.full ->
  ?block:Block_services.block ->
  level:int32 ->
  chain:Shell_services.chain ->
  consensus_key list ->
  delegate_slots tzresult Lwt.t

val create_cache : unit -> cache

(** Memoization wrapper for [Round.timestamp_of_round]. *)
val timestamp_of_round :
  state ->
  predecessor_timestamp:Time.Protocol.t ->
  predecessor_round:Round.t ->
  round:Round.t ->
  Time.Protocol.t tzresult

(** From the current [state], the function returns an optional
    association pair, which consists of the next round timestamp and its
    round. *)
val compute_next_round_time : state -> (Time.Protocol.t * Round.t) option

val pp_validation_mode : Format.formatter -> validation_mode -> unit

val pp_global_state : Format.formatter -> global_state -> unit

val pp_option :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

val pp_block_info : Format.formatter -> block_info -> unit

val pp_proposal : Format.formatter -> proposal -> unit

val pp_locked_round : Format.formatter -> locked_round -> unit

val pp_attestable_payload : Format.formatter -> attestable_payload -> unit

val pp_elected_block : Format.formatter -> elected_block -> unit

val pp_delegate_slot : Format.formatter -> delegate_slot -> unit

val pp_delegate_slots : Format.formatter -> delegate_slots -> unit

val pp_level_state : Format.formatter -> level_state -> unit

val pp_phase : Format.formatter -> phase -> unit

val pp_round_state : Format.formatter -> round_state -> unit

val pp : Format.formatter -> t -> unit

val pp_timeout_kind : Format.formatter -> timeout_kind -> unit

val pp_event : Format.formatter -> event -> unit
