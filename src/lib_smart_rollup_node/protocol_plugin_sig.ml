(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** Protocol specific RPC directory, used as a dynamic directory in the protocol
    agnostic rollup node. *)
module type RPC_DIRECTORY = sig
  (** The RPC directory, specific to blocks of the protocol, for this rollup
      node. *)
  val block_directory :
    Node_context.rw ->
    (unit * Rollup_node_services.Arg.block_id) Tezos_rpc.Directory.t
end

(** Protocol specific functions to track attested DAL slots of L1 blocks. *)
module type DAL_SLOTS_TRACKER = sig
  (** [process_head node_ctxt head] performs the following operations for a
      given protocol:
      {ul
        {li it reads the attestations for headers published attestation_lag
        levels preceding [head] from the block metadata, determines which
        ones the rollup node will download, and stores the results in
        [Store.Dal_confirmed_slots].}
      }  *)
  val process_head :
    _ Node_context.rw_store -> Layer1.head -> unit tzresult Lwt.t
end

(** Protocol specific functions to reconstruct inboxes from L1 blocks. *)
module type INBOX = sig
  (** [process_head node_ctxt ~predecessor head] changes the state of
      the inbox on disk to react to the operations contained in the block [head]
      (where [predecessor] is the predecessor of [head] in the L1 chain). It
      returns a tuple [(inbox_hash, inbox, payload_hash, messages)] where
      [inbox] is the new inbox and [inbox_hash] its hash, [payload_hash] is the
      hash of the merkelized payload for this inbox and [messages] are the
      serialized messages present in the block (with the internal messages added
      by the protocol). *)
  val process_head :
    _ Node_context.rw_store ->
    predecessor:Layer1.header ->
    Layer1.header ->
    (Octez_smart_rollup.Inbox.Hash.t
    * Octez_smart_rollup.Inbox.t
    * Merkelized_payload_hashes_hash.t
    * string list)
    tzresult
    Lwt.t

  (** [same_as_layer_1 node_ctxt level node_inbox] ensures that the rollup node
      agrees with the L1 node that inbox for [level] is [node_inbox]. *)
  val same_as_layer_1 :
    _ Node_context.t ->
    int32 ->
    Octez_smart_rollup.Inbox.t ->
    unit tzresult Lwt.t

  (** Serialize an external messages to the protocol representation. NOTE: so
      far, in all available protocols, this adds a tag ['\001'] at the
      beginning. *)
  val serialize_external_message : string -> string tzresult

  (** Returns the initial global inbox where [level] is the first level of the
      protocol with smart rollups. *)
  val init :
    predecessor_timestamp:Time.Protocol.t ->
    predecessor:Block_hash.t ->
    level:int32 ->
    Octez_smart_rollup.Inbox.t

  (**/**)

  module Internal_for_tests : sig
    (** Only for tests. [process_messages node_ctxt ~is_first_block ~predecessor
        messages] reconstructs the inbox on disk for the [messages]. See
        {!val:process_head} for the return values. *)
    val process_messages :
      _ Node_context.rw_store ->
      is_first_block:bool ->
      predecessor:Layer1.header ->
      string list ->
      (Octez_smart_rollup.Inbox.Hash.t
      * Octez_smart_rollup.Inbox.t
      * Merkelized_payload_hashes_hash.t
      * string list)
      tzresult
      Lwt.t
  end
end

(** Protocol specific constants for the batcher. *)
module type BATCHER_CONSTANTS = sig
  (** Maximum size of an L2 message allowed by the prototcol, which is
      {!val:Protocol.Constants_repr.sc_rollup_message_size_limit}. *)
  val message_size_limit : int

  (** Maximum size in bytes of an batch of L2 messages that can fit in an
      operation on L1. It is protocol dependent. *)
  val protocol_max_batch_size : int
end

(** Protocol specific functions to interact with the L1 node. *)
module type LAYER1_HELPERS = sig
  (** [prefetch_tezos_blocks l1_ctxt blocks] prefetches the blocks
      asynchronously. NOTE: the number of blocks to prefetch must not be greater
      than the size of the blocks cache otherwise they will be lost. *)
  val prefetch_tezos_blocks : Layer1.t -> Layer1.head list -> unit

  val get_last_cemented_commitment :
    #Client_context.full -> Address.t -> Node_context.lcc tzresult Lwt.t

  val get_last_published_commitment :
    ?allow_unstake:bool ->
    #Client_context.full ->
    Address.t ->
    Signature.public_key_hash ->
    Commitment.t option tzresult Lwt.t

  val get_kind : #Client_context.full -> Address.t -> Kind.t tzresult Lwt.t

  val genesis_inbox :
    #Client_context.full ->
    genesis_level:int32 ->
    Octez_smart_rollup.Inbox.t tzresult Lwt.t

  (** Retrieve protocol agnotic constants for the head of the chain. *)
  val retrieve_constants :
    ?block:Tezos_shell_services.Block_services.block ->
    #Client_context.full ->
    Rollup_constants.protocol_constants tzresult Lwt.t

  val retrieve_genesis_info :
    #Client_context.full ->
    Address.t ->
    Node_context.genesis_info tzresult Lwt.t

  (** [get_boot_sector block_hash node_ctxt] retrieves the boot sector from the
      rollup origination operation in block [block_hash]. Precondition:
      [block_hash] has to be the block where the rollup was originated. *)
  val get_boot_sector :
    Block_hash.t -> _ Node_context.t -> string tzresult Lwt.t

  (** Find and retrieve the whitelist the rollup at a given block (if provided)
      or the head. *)
  val find_whitelist :
    #Client_context.full ->
    ?block:Block_hash.t ->
    Address.t ->
    Signature.public_key_hash list option tzresult Lwt.t

  (** Retrieve information about the last whitelist update on L1. *)
  val find_last_whitelist_update :
    #Client_context.full -> Address.t -> (Z.t * Int32.t) option tzresult Lwt.t

  (** Retrieve a commitment published on L1. *)
  val get_commitment :
    #Client_context.full ->
    Address.t ->
    Commitment.Hash.t ->
    Commitment.t tzresult Lwt.t

  (** Retrieve the balance in mutez for a given public key hash. *)
  val get_balance_mutez :
    #Client_context.full ->
    ?block:Block_hash.t ->
    Signature.public_key_hash ->
    int64 tzresult Lwt.t
end

(** Protocol specific functions for processing L1 blocks. *)
module type L1_PROCESSING = sig
  (** React to L1 operations included in a block of the chain. When
      [catching_up] is true, the process block is in the past and the
      failure condition of the process differs (e.g. if it detects a
      whitelist update execution, it does not fail when the operator
      is not in the whitelist).  *)
  val process_l1_block_operations :
    catching_up:bool -> Node_context.rw -> Layer1.header -> unit tzresult Lwt.t
end

(** Partial protocol plugin with just the PVM and the function to access the
    Layer1. This signature exists in order to build plugins for the interpreter
    and the refutation games while avoiding circular dependencies. *)
module type PARTIAL = sig
  (** The protocol for which this plugin is. *)
  val protocol : Protocol_hash.t

  module Layer1_helpers : LAYER1_HELPERS

  module Pvm : Pvm_plugin_sig.S
end

(** Protocol specific refutation helper functions.  *)
module type REFUTATION_GAME_HELPERS = sig
  (** [generate_proof node_ctxt (game) start_state] generates a serialized proof
    for the current [game] for the execution step starting with
    [start_state]. *)
  val generate_proof :
    _ Node_context.rw_context ->
    Game.t ->
    Access_mode.rw Context.pvmstate ->
    string tzresult Lwt.t

  (** [make_dissection plugin node_ctxt cache ~start_state ~start_chunk
      ~our_stop_chunk ~default_number_of_sections ~commitment_period_tick_offset
      ~last_level] computes a dissection from between [start_chunk] and
      [our_stop_chunk] at level [last_level]. [commitment_period_tick_offset] is
      the tick offset of the commitment period for the conflict/dissection. This
      dissection has [default_number_of_sections] if there are enough ticks. *)
  val make_dissection :
    (module PARTIAL) ->
    _ Node_context.t ->
    Pvm_plugin_sig.state_cache ->
    start_state:
      ( Fuel.Accounted.t,
        Access_mode.rw Context.pvmstate )
      Pvm_plugin_sig.eval_state
      option ->
    start_chunk:Game.dissection_chunk ->
    our_stop_chunk:Game.dissection_chunk ->
    default_number_of_sections:int ->
    commitment_period_tick_offset:Z.t ->
    last_level:int32 ->
    Game.dissection_chunk trace tzresult Lwt.t

  (** [timeout node_ctxt ~self ~opponent] returns the current state of the timeout
    if there is a refutation game between self and opponent. *)
  val timeout :
    _ Node_context.t ->
    self:Signature.public_key_hash ->
    opponent:Signature.public_key_hash ->
    Game.timeout option tzresult Lwt.t

  (** [timeout_reached node_ctxt ~self ~opponent] returns [true] if the
    timeout is reached against opponent in head of the L1 chain. *)
  val timeout_reached :
    _ Node_context.t ->
    self:Signature.public_key_hash ->
    opponent:Signature.public_key_hash ->
    bool tzresult Lwt.t

  (** [get_conflicts cctxt rollup signer] returns the conflicts for commitments
    staked on by [signer]. *)
  val get_conflicts :
    Client_context.full ->
    Address.t ->
    Signature.public_key_hash ->
    Game.conflict list tzresult Lwt.t

  (** [get_ongoing_games cctxt rollup signer] returns the games that [signer] is
    currently playing. *)
  val get_ongoing_games :
    Client_context.full ->
    Address.t ->
    Signature.public_key_hash ->
    (Game.t * Signature.public_key_hash * Signature.public_key_hash) list
    tzresult
    Lwt.t
end

(** Signature of protocol plugins for the rollup node. NOTE: the plugins have to
    be registered to be made available to the rollup node.  *)
module type S = sig
  include PARTIAL

  module RPC_directory : RPC_DIRECTORY

  module Dal_slots_tracker : DAL_SLOTS_TRACKER

  module Inbox : INBOX

  module Batcher_constants : BATCHER_CONSTANTS

  module L1_processing : L1_PROCESSING

  module Refutation_game_helpers : REFUTATION_GAME_HELPERS
end
