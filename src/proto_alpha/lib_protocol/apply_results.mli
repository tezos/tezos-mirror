(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Types representing results of applying an operation.

    These are used internally by [Apply], and can be used for experimenting
    with protocol updates, by clients to print out a summary of the
    operation at pre-injection simulation and at confirmation time,
    and by block explorers.
 *)

open Alpha_context
open Apply_operation_result
open Apply_internal_results

(** Result of applying a {!Operation.t}. Follows the same structure. *)
type 'kind operation_metadata = {contents : 'kind contents_result_list}

and packed_operation_metadata =
  | Operation_metadata : 'kind operation_metadata -> packed_operation_metadata
  | No_operation_metadata : packed_operation_metadata

(** Result of applying a {!Operation.contents_list}. Follows the same structure. *)
and 'kind contents_result_list =
  | Single_result : 'kind contents_result -> 'kind contents_result_list
  | Cons_result :
      'kind Kind.manager contents_result
      * 'rest Kind.manager contents_result_list
      -> ('kind * 'rest) Kind.manager contents_result_list

and packed_contents_result_list =
  | Contents_result_list :
      'kind contents_result_list
      -> packed_contents_result_list

(** Result of applying an {!Operation.contents}. Follows the same structure. *)
and 'kind contents_result =
  | Preendorsement_result : {
      balance_updates : Receipt.balance_updates;
      delegate : Signature.public_key_hash;
      consensus_key : Signature.public_key_hash;
      consensus_power : int;
    }
      -> Kind.preattestation contents_result
  | Endorsement_result : {
      balance_updates : Receipt.balance_updates;
      delegate : Signature.public_key_hash;
      consensus_key : Signature.public_key_hash;
      consensus_power : int;
    }
      -> Kind.attestation contents_result
  | Dal_attestation_result : {
      delegate : Signature.Public_key_hash.t;
    }
      -> Kind.dal_attestation contents_result
  | Seed_nonce_revelation_result :
      Receipt.balance_updates
      -> Kind.seed_nonce_revelation contents_result
  | Vdf_revelation_result :
      Receipt.balance_updates
      -> Kind.vdf_revelation contents_result
  | Double_endorsement_evidence_result :
      Receipt.balance_updates
      -> Kind.double_attestation_evidence contents_result
  | Double_preendorsement_evidence_result :
      Receipt.balance_updates
      -> Kind.double_preattestation_evidence contents_result
  | Double_baking_evidence_result :
      Receipt.balance_updates
      -> Kind.double_baking_evidence contents_result
  | Activate_account_result :
      Receipt.balance_updates
      -> Kind.activate_account contents_result
  | Proposals_result : Kind.proposals contents_result
  | Ballot_result : Kind.ballot contents_result
  | Drain_delegate_result : {
      balance_updates : Receipt.balance_updates;
      allocated_destination_contract : bool;
    }
      -> Kind.drain_delegate contents_result
  | Manager_operation_result : {
      balance_updates : Receipt.balance_updates;
      operation_result : 'kind manager_operation_result;
      internal_operation_results : packed_internal_operation_result list;
    }
      -> 'kind Kind.manager contents_result

and packed_contents_result =
  | Contents_result : 'kind contents_result -> packed_contents_result

and 'kind manager_operation_result =
  ( 'kind,
    'kind Kind.manager,
    'kind successful_manager_operation_result )
  operation_result

(** Result of applying a transaction. *)
and successful_transaction_result =
  Apply_internal_results.successful_transaction_result

(** Result of applying an origination. *)
and successful_origination_result =
  Apply_internal_results.successful_origination_result

(** Result of applying an external {!manager_operation_content}. *)
and _ successful_manager_operation_result =
  | Reveal_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.reveal successful_manager_operation_result
  | Transaction_result :
      successful_transaction_result
      -> Kind.transaction successful_manager_operation_result
  | Origination_result :
      successful_origination_result
      -> Kind.origination successful_manager_operation_result
  | Delegation_result : {
      consumed_gas : Gas.Arith.fp;
      balance_updates : Receipt.balance_updates;
    }
      -> Kind.delegation successful_manager_operation_result
  | Register_global_constant_result : {
      (* The manager submitting the operation must pay
          the cost of storage for the registered value.
          We include the balance update here. *)
      balance_updates : Receipt.balance_updates;
      (* Gas consumed while validating and storing the registered
          value. *)
      consumed_gas : Gas.Arith.fp;
      (* The size of the registered value in bytes.
          Currently, this is simply the number of bytes in the binary
          serialization of the Micheline value. *)
      size_of_constant : Z.t;
      (* The address of the newly registered value, being
          the hash of its binary serialization. This could be
          calulated on demand but we include it here in the
          receipt for flexibility in the future. *)
      global_address : Script_expr_hash.t;
    }
      -> Kind.register_global_constant successful_manager_operation_result
  | Increase_paid_storage_result : {
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.increase_paid_storage successful_manager_operation_result
  | Update_consensus_key_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.update_consensus_key successful_manager_operation_result
  | Transfer_ticket_result : {
      balance_updates : Receipt.balance_updates;
      ticket_receipt : Ticket_receipt.t;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.transfer_ticket successful_manager_operation_result
  | Dal_publish_slot_header_result : {
      slot_header : Dal.Slot.Header.t;
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.dal_publish_slot_header successful_manager_operation_result
  | Sc_rollup_originate_result : {
      balance_updates : Receipt.balance_updates;
      address : Sc_rollup.Address.t;
      genesis_commitment_hash : Sc_rollup.Commitment.Hash.t;
      consumed_gas : Gas.Arith.fp;
      size : Z.t;
    }
      -> Kind.sc_rollup_originate successful_manager_operation_result
  | Sc_rollup_add_messages_result : {
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.sc_rollup_add_messages successful_manager_operation_result
  | Sc_rollup_cement_result : {
      consumed_gas : Gas.Arith.fp;
      inbox_level : Raw_level.t;
      commitment_hash : Sc_rollup.Commitment.Hash.t;
    }
      -> Kind.sc_rollup_cement successful_manager_operation_result
  | Sc_rollup_publish_result : {
      consumed_gas : Gas.Arith.fp;
      staked_hash : Sc_rollup.Commitment.Hash.t;
      published_at_level : Raw_level.t;
      balance_updates : Receipt.balance_updates;
    }
      -> Kind.sc_rollup_publish successful_manager_operation_result
  | Sc_rollup_refute_result : {
      consumed_gas : Gas.Arith.fp;
      game_status : Sc_rollup.Game.status;
      balance_updates : Receipt.balance_updates;
    }
      -> Kind.sc_rollup_refute successful_manager_operation_result
  | Sc_rollup_timeout_result : {
      consumed_gas : Gas.Arith.fp;
      game_status : Sc_rollup.Game.status;
      balance_updates : Receipt.balance_updates;
    }
      -> Kind.sc_rollup_timeout successful_manager_operation_result
  | Sc_rollup_execute_outbox_message_result : {
      balance_updates : Receipt.balance_updates;
      ticket_receipt : Ticket_receipt.t;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.sc_rollup_execute_outbox_message
         successful_manager_operation_result
  | Sc_rollup_recover_bond_result : {
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
    }
      -> Kind.sc_rollup_recover_bond successful_manager_operation_result
  | Zk_rollup_origination_result : {
      balance_updates : Receipt.balance_updates;
      originated_zk_rollup : Zk_rollup.t;
      consumed_gas : Gas.Arith.fp;
      (* Number of bytes allocated by the ZKRU origination.
         Used to burn storage fees. *)
      storage_size : Z.t;
    }
      -> Kind.zk_rollup_origination successful_manager_operation_result
  | Zk_rollup_publish_result : {
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.zk_rollup_publish successful_manager_operation_result
  | Zk_rollup_update_result : {
      balance_updates : Receipt.balance_updates;
      consumed_gas : Gas.Arith.fp;
      paid_storage_size_diff : Z.t;
    }
      -> Kind.zk_rollup_update successful_manager_operation_result

and packed_successful_manager_operation_result =
  | Successful_manager_result :
      'kind successful_manager_operation_result
      -> packed_successful_manager_operation_result

val pack_migration_operation_results :
  Migration.origination_result list ->
  packed_successful_manager_operation_result list

(** Serializer for {!packed_operation_result}. *)
val operation_metadata_encoding : packed_operation_metadata Data_encoding.t

(** Operation metadata encoding that accepts legacy attestation name :
    `endorsement` (and preendorsement, double_<op>_evidence) in JSON

    https://gitlab.com/tezos/tezos/-/issues/5531

    This encoding is temporary and should be removed when the protocol no longer
    accepts JSON endorsements kinds
*)
val operation_metadata_encoding_with_legacy_attestation_name :
  packed_operation_metadata Data_encoding.t

val operation_data_and_metadata_encoding :
  (Operation.packed_protocol_data * packed_operation_metadata) Data_encoding.t

(** Operation data and metadata encoding that accepts legacy attestation name :
    `endorsement` (and preendorsement, double_<op>_evidence) in JSON

    https://gitlab.com/tezos/tezos/-/issues/5531

    This encoding is temporary and should be removed when the protocol no longer
    accepts JSON endorsements kinds
*)
val operation_data_and_metadata_encoding_with_legacy_attestation_name :
  (Operation.packed_protocol_data * packed_operation_metadata) Data_encoding.t

type 'kind contents_and_result_list =
  | Single_and_result :
      'kind Alpha_context.contents * 'kind contents_result
      -> 'kind contents_and_result_list
  | Cons_and_result :
      'kind Kind.manager Alpha_context.contents
      * 'kind Kind.manager contents_result
      * 'rest Kind.manager contents_and_result_list
      -> ('kind * 'rest) Kind.manager contents_and_result_list

type packed_contents_and_result_list =
  | Contents_and_result_list :
      'kind contents_and_result_list
      -> packed_contents_and_result_list

val contents_and_result_list_encoding :
  packed_contents_and_result_list Data_encoding.t

val contents_and_result_list_encoding_with_legacy_attestation_name :
  packed_contents_and_result_list Data_encoding.t

val pack_contents_list :
  'kind contents_list ->
  'kind contents_result_list ->
  'kind contents_and_result_list

val unpack_contents_list :
  'kind contents_and_result_list ->
  'kind contents_list * 'kind contents_result_list

val to_list : packed_contents_result_list -> packed_contents_result list

type ('a, 'b) eq = Eq : ('a, 'a) eq

val kind_equal_list :
  'kind contents_list ->
  'kind2 contents_result_list ->
  ('kind, 'kind2) eq option

type block_metadata = {
  proposer : Consensus_key.t;
  baker : Consensus_key.t;
  level_info : Level.t;
  voting_period_info : Voting_period.info;
  nonce_hash : Nonce_hash.t option;
  consumed_gas : Gas.Arith.fp;
  deactivated : Signature.Public_key_hash.t list;
  balance_updates : Receipt.balance_updates;
  liquidity_baking_toggle_ema : Per_block_votes.Liquidity_baking_toggle_EMA.t;
  adaptive_inflation_vote_ema : Per_block_votes.Adaptive_inflation_launch_EMA.t;
  adaptive_inflation_launch_cycle : Cycle.t option;
  implicit_operations_results : packed_successful_manager_operation_result list;
  dal_attestation : Dal.Attestation.t option;
}

val block_metadata_encoding : block_metadata Data_encoding.encoding
