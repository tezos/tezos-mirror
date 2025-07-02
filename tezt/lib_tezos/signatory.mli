(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** This module is a Tezt wrapper around the Tezos remote signer "Signatory".

    It allows to run a signatory signer and to interact with it. *)

(** The type of a generic operation that can be signed by the signatory. *)
type generic =
  [ `Activate_account
  | `Attestation
  | `Attestation_with_dal
  | `Ballot
  | `Dal_publish_commitment
  | `Delegation
  | `Double_attestation_evidence
  | `Double_baking_evidence
  | `Double_preattestation_evidence
  | `Drain_delegate
  | `Failing_noop
  | `Finalize_unstake
  | `Increase_paid_storage
  | `Origination
  | `Preattestation
  | `Proposals
  | `Register_global_constant
  | `Reveal
  | `Seed_nonce_revelation
  | `Set_delegate_parameters
  | `Set_deposits_limit
  | `Signature_prefix
  | `Smart_rollup_add_messages
  | `Smart_rollup_cement
  | `Smart_rollup_execute_outbox_message
  | `Smart_rollup_originate
  | `Smart_rollup_publish
  | `Smart_rollup_recover_bond
  | `Smart_rollup_refute
  | `Smart_rollup_timeout
  | `Stake
  | `Transaction
  | `Transfer_ticket
  | `Unstake
  | `Update_consensus_key
  | `Vdf_revelation
  | `Zk_rollup_origination
  | `Zk_rollup_publish
  | `Zk_rollup_update ]

(** The type of a restriction on the operations that can be signed by the
    signatory. *)
type restriction =
  [ `Block
  | `Endorsement
  | `Failing_noop
  | `Generic of generic list
  | `Preendorsement ]

(** The type of a key that can be used by the signatory. *)
type key = {
  account : Account.key;  (** The account associated with the key. *)
  active : bool;  (** Whether the key is active. *)
  log_payloads : bool;  (** Whether to log the payloads of the operations. *)
  restrictions : restriction list;
      (** The restrictions on the operations that can be signed by the key. *)
}

(** The type for a signatory signer. *)
type t

(** [port signatory] returns the port on which the signatory signer is
    listening. *)
val port : t -> int

(** [create ?name ?color ?base_dir ?port ?runner ?config_file ?vault_name keys]
    creates a new signatory signer instance. *)
val create :
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?port:int ->
  ?runner:Runner.t ->
  ?config_file:string ->
  ?vault_name:string ->
  key list ->
  t Lwt.t

(** [wait_for_ready signatory] waits for the signatory signer to be ready to
    accept requests. *)
val wait_for_ready : t -> unit Lwt.t

(** [run signatory] runs the signatory signer. *)
val run : t -> unit Lwt.t

(** [terminate ?timeout signatory] terminates the signatory signer. *)
val terminate : ?timeout:float -> t -> unit

(** [restart signatory] restarts the signatory signer. *)
val restart : t -> unit Lwt.t
