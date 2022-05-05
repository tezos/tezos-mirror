(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Implementation - Low level Repr. of Operations

    Defines kinds of operations that can be performed on chain:
    - preendorsement
    - endorsement
    - double baking evidence
    - double preendorsing evidence
    - double endorsing evidence
    - seed nonce revelation
    - account activation
    - proposal (see: [Voting_repr])
    - ballot (see: [Voting_repr])
    - failing noop
    - manager operation (which in turn has several types):
      - revelation
      - transaction
      - origination
      - delegation
      - set deposits limitation
      - tx rollup origination
      - tx rollup batch submission
      - tx rollup commit
      - tx rollup withdraw
      - tx rollup reveal withdrawals
      - smart contract rollup origination

    Each of them can be encoded as raw bytes. Operations are distinguished at
    type level using phantom type parameters. [packed_operation] type allows
    for unifying them when required, for instance to put them on a single
    list. *)

module Kind : sig
  type preendorsement_consensus_kind = Preendorsement_consensus_kind

  type endorsement_consensus_kind = Endorsement_consensus_kind

  type 'a consensus =
    | Preendorsement_kind : preendorsement_consensus_kind consensus
    | Endorsement_kind : endorsement_consensus_kind consensus

  type preendorsement = preendorsement_consensus_kind consensus

  type endorsement = endorsement_consensus_kind consensus

  type seed_nonce_revelation = Seed_nonce_revelation_kind

  type 'a double_consensus_operation_evidence =
    | Double_consensus_operation_evidence

  type double_endorsement_evidence =
    endorsement_consensus_kind double_consensus_operation_evidence

  type double_preendorsement_evidence =
    preendorsement_consensus_kind double_consensus_operation_evidence

  type double_baking_evidence = Double_baking_evidence_kind

  type activate_account = Activate_account_kind

  type proposals = Proposals_kind

  type ballot = Ballot_kind

  type reveal = Reveal_kind

  type transaction = Transaction_kind

  type origination = Origination_kind

  type delegation = Delegation_kind

  type set_deposits_limit = Set_deposits_limit_kind

  type failing_noop = Failing_noop_kind

  type register_global_constant = Register_global_constant_kind

  type tx_rollup_origination = Tx_rollup_origination_kind

  type tx_rollup_submit_batch = Tx_rollup_submit_batch_kind

  type tx_rollup_commit = Tx_rollup_commit_kind

  type tx_rollup_return_bond = Tx_rollup_return_bond_kind

  type tx_rollup_finalize_commitment = Tx_rollup_finalize_commitment_kind

  type tx_rollup_remove_commitment = Tx_rollup_remove_commitment_kind

  type tx_rollup_rejection = Tx_rollup_rejection_kind

  type tx_rollup_dispatch_tickets = Tx_rollup_dispatch_tickets_kind

  type transfer_ticket = Transfer_ticket_kind

  type sc_rollup_originate = Sc_rollup_originate_kind

  type sc_rollup_add_messages = Sc_rollup_add_messages_kind

  type sc_rollup_cement = Sc_rollup_cement_kind

  type sc_rollup_publish = Sc_rollup_publish_kind

  type 'a manager =
    | Reveal_manager_kind : reveal manager
    | Transaction_manager_kind : transaction manager
    | Origination_manager_kind : origination manager
    | Delegation_manager_kind : delegation manager
    | Register_global_constant_manager_kind : register_global_constant manager
    | Set_deposits_limit_manager_kind : set_deposits_limit manager
    | Tx_rollup_origination_manager_kind : tx_rollup_origination manager
    | Tx_rollup_submit_batch_manager_kind : tx_rollup_submit_batch manager
    | Tx_rollup_commit_manager_kind : tx_rollup_commit manager
    | Tx_rollup_return_bond_manager_kind : tx_rollup_return_bond manager
    | Tx_rollup_finalize_commitment_manager_kind
        : tx_rollup_finalize_commitment manager
    | Tx_rollup_remove_commitment_manager_kind
        : tx_rollup_remove_commitment manager
    | Tx_rollup_rejection_manager_kind : tx_rollup_rejection manager
    | Tx_rollup_dispatch_tickets_manager_kind
        : tx_rollup_dispatch_tickets manager
    | Transfer_ticket_manager_kind : transfer_ticket manager
    | Sc_rollup_originate_manager_kind : sc_rollup_originate manager
    | Sc_rollup_add_messages_manager_kind : sc_rollup_add_messages manager
    | Sc_rollup_cement_manager_kind : sc_rollup_cement manager
    | Sc_rollup_publish_manager_kind : sc_rollup_publish manager
end

type 'a consensus_operation_type =
  | Endorsement : Kind.endorsement consensus_operation_type
  | Preendorsement : Kind.preendorsement consensus_operation_type

val pp_operation_kind :
  Format.formatter -> 'kind consensus_operation_type -> unit

type consensus_content = {
  slot : Slot_repr.t;
  (* By convention, this is the validator's first slot. *)
  level : Raw_level_repr.t;
  (* The level of (pre)endorsed block. *)
  round : Round_repr.t;
  (* The round of (pre)endorsed block. *)
  block_payload_hash : Block_payload_hash.t;
      (* The payload hash of (pre)endorsed block. *)
}

val consensus_content_encoding : consensus_content Data_encoding.t

val pp_consensus_content : Format.formatter -> consensus_content -> unit

type consensus_watermark =
  | Endorsement of Chain_id.t
  | Preendorsement of Chain_id.t

val to_watermark : consensus_watermark -> Signature.watermark

val of_watermark : Signature.watermark -> consensus_watermark option

type raw = Operation.t = {shell : Operation.shell_header; proto : bytes}

val raw_encoding : raw Data_encoding.t

type transaction = {
  amount : Tez_repr.tez;
  parameters : Script_repr.lazy_expr;
  entrypoint : Entrypoint_repr.t;
  destination : Destination_repr.t;
}

type origination = {
  delegate : Signature.Public_key_hash.t option;
  script : Script_repr.t;
  credit : Tez_repr.tez;
}

(** An [operation] contains the operation header information in [shell]
    and all data related to the operation itself in [protocol_data]. *)
type 'kind operation = {
  shell : Operation.shell_header;
  protocol_data : 'kind protocol_data;
}

(** A [protocol_data] wraps together a signature for the operation and
    the contents of the operation itself. *)
and 'kind protocol_data = {
  contents : 'kind contents_list;
  signature : Signature.t option;
}

(** A [contents_list] is a list of contents, the GADT guarantees two
    invariants:
    - the list is not empty, and
    - if the list has several elements then it only contains manager
      operations. *)
and _ contents_list =
  | Single : 'kind contents -> 'kind contents_list
  | Cons :
      'kind Kind.manager contents * 'rest Kind.manager contents_list
      -> ('kind * 'rest) Kind.manager contents_list

(** A value of type [contents] an operation related to whether
    consensus, governance or contract management. *)
and _ contents =
  (* Preendorsement: About consensus, preendorsement of a block held by a
     validator (specific to Tenderbake). *)
  | Preendorsement : consensus_content -> Kind.preendorsement contents
  (* Endorsement: About consensus, endorsement of a block held by a
     validator. *)
  | Endorsement : consensus_content -> Kind.endorsement contents
  (* Seed_nonce_revelation: Nonces are created by bakers and are
     combined to create pseudo-random seeds. Bakers are urged to reveal their
     nonces after a given number of cycles to keep their block rewards
     from being forfeited. *)
  | Seed_nonce_revelation : {
      level : Raw_level_repr.t;
      nonce : Seed_repr.nonce;
    }
      -> Kind.seed_nonce_revelation contents
  (* Double_preendorsement_evidence: Double-preendorsement is a
     kind of malicious attack where a byzantine attempts to fork
     the chain by preendorsing blocks with different
     contents (at the same level and same round)
     twice. This behavior may be reported and the byzantine will have
     its security deposit forfeited. *)
  | Double_preendorsement_evidence : {
      op1 : Kind.preendorsement operation;
      op2 : Kind.preendorsement operation;
    }
      -> Kind.double_preendorsement_evidence contents
  (* Double_endorsement_evidence: Similar to double-preendorsement but
     for endorsements. *)
  | Double_endorsement_evidence : {
      op1 : Kind.endorsement operation;
      op2 : Kind.endorsement operation;
    }
      -> Kind.double_endorsement_evidence contents
  (* Double_baking_evidence: Similarly to double-endorsement but the
     byzantine attempts to fork by signing two different blocks at the
     same level. *)
  | Double_baking_evidence : {
      bh1 : Block_header_repr.t;
      bh2 : Block_header_repr.t;
    }
      -> Kind.double_baking_evidence contents
  (* Activate_account: Account activation allows to register a public
     key hash on the blockchain. *)
  | Activate_account : {
      id : Ed25519.Public_key_hash.t;
      activation_code : Blinded_public_key_hash.activation_code;
    }
      -> Kind.activate_account contents
  (* Proposals: A candidate protocol can be proposed for voting. *)
  | Proposals : {
      source : Signature.Public_key_hash.t;
      period : int32;
      proposals : Protocol_hash.t list;
    }
      -> Kind.proposals contents
  (* Ballot: The validators of the chain will then vote on proposals. *)
  | Ballot : {
      source : Signature.Public_key_hash.t;
      period : int32;
      proposal : Protocol_hash.t;
      ballot : Vote_repr.ballot;
    }
      -> Kind.ballot contents
  (* Failing_noop: An operation never considered by the state machine
     and which will always fail at [apply]. This allows end-users to
     sign arbitrary messages which have no computational semantics. *)
  | Failing_noop : string -> Kind.failing_noop contents
  (* Manager_operation: Operations, emitted and signed by
     a (revealed) implicit account, that describe management and
     interactions between contracts (whether implicit or
     smart). *)
  | Manager_operation : {
      source : Signature.Public_key_hash.t;
      fee : Tez_repr.tez;
      counter : counter;
      operation : 'kind manager_operation;
      gas_limit : Gas_limit_repr.Arith.integral;
      storage_limit : Z.t;
    }
      -> 'kind Kind.manager contents

(** A [manager_operation] describes management and interactions
    between contracts (whether implicit or smart). *)
and _ manager_operation =
  (* [Reveal] for the revelation of a public key, a one-time
     prerequisite to any signed operation, in order to be able to
     check the sender’s signature. *)
  | Reveal : Signature.Public_key.t -> Kind.reveal manager_operation
  (* [Transaction] of some amount to some destination contract. It can
     also be used to execute/call smart-contracts. *)
  | Transaction : transaction -> Kind.transaction manager_operation
  (* [Origination] of a contract using a smart-contract [script] and
     initially credited with the amount [credit]. *)
  | Origination : origination -> Kind.origination manager_operation
  (* [Delegation] to some staking contract (designated by its public
     key hash). When this value is None, delegation is reverted as it
     is set to nobody. *)
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation manager_operation
  (* [Register_global_constant] allows registration and substitution
     of a global constant available from any contract and registered in
     the context. *)
  | Register_global_constant : {
      value : Script_repr.lazy_expr;
    }
      -> Kind.register_global_constant manager_operation
  (* [Set_deposits_limit] sets an optional limit for frozen deposits
     of a contract at a lower value than the maximum limit.  When None,
     the limit in unset back to the default maximum limit. *)
  | Set_deposits_limit :
      Tez_repr.t option
      -> Kind.set_deposits_limit manager_operation
  (* [Tx_rollup_origination] allows an implicit contract to originate
     a new transactional rollup. *)
  | Tx_rollup_origination : Kind.tx_rollup_origination manager_operation
  (* [Tx_rollup_submit_batch] allows to submit batches of L2 operations on a
     transactional rollup. The content is a string, but stands for an immutable
     byte sequence. *)
  | Tx_rollup_submit_batch : {
      tx_rollup : Tx_rollup_repr.t;
      content : string;
      burn_limit : Tez_repr.t option;
    }
      -> Kind.tx_rollup_submit_batch manager_operation
  | Tx_rollup_commit : {
      tx_rollup : Tx_rollup_repr.t;
      commitment : Tx_rollup_commitment_repr.Full.t;
    }
      -> Kind.tx_rollup_commit manager_operation
  | Tx_rollup_return_bond : {
      tx_rollup : Tx_rollup_repr.t;
    }
      -> Kind.tx_rollup_return_bond manager_operation
  | Tx_rollup_finalize_commitment : {
      tx_rollup : Tx_rollup_repr.t;
    }
      -> Kind.tx_rollup_finalize_commitment manager_operation
  | Tx_rollup_remove_commitment : {
      tx_rollup : Tx_rollup_repr.t;
    }
      -> Kind.tx_rollup_remove_commitment manager_operation
  | Tx_rollup_rejection : {
      tx_rollup : Tx_rollup_repr.t;
      level : Tx_rollup_level_repr.t;
      message : Tx_rollup_message_repr.t;
      message_position : int;
      message_path : Tx_rollup_inbox_repr.Merkle.path;
      message_result_hash : Tx_rollup_message_result_hash_repr.t;
      message_result_path : Tx_rollup_commitment_repr.Merkle.path;
      previous_message_result : Tx_rollup_message_result_repr.t;
      previous_message_result_path : Tx_rollup_commitment_repr.Merkle.path;
      proof : Tx_rollup_l2_proof.t;
    }
      -> Kind.tx_rollup_rejection manager_operation
  | Tx_rollup_dispatch_tickets : {
      tx_rollup : Tx_rollup_repr.t;
          (** The rollup from where the tickets are retrieved *)
      level : Tx_rollup_level_repr.t;
          (** The level at which the withdrawal was enabled *)
      context_hash : Context_hash.t;
          (** The hash of the l2 context resulting from the execution of the
          inbox from where this withdrawal was enabled. *)
      message_index : int;
          (** Index of the message in the inbox at [level] where this withdrawal was enabled. *)
      message_result_path : Tx_rollup_commitment_repr.Merkle.path;
      tickets_info : Tx_rollup_reveal_repr.t list;
    }
      -> Kind.tx_rollup_dispatch_tickets manager_operation
      (** [Transfer_ticket] allows an implicit account (the "claimer") to
      receive [amount] tickets, pulled out of [tx_rollup], to the
      [entrypoint] of the smart contract [destination].

      The ticket must have been addressed to the
      claimer, who must be the source of this operation. It must have been
      pulled out at [level] and from the message at [message_index]. The ticket
      is composed of [ticketer; ty; contents]. *)
  | Transfer_ticket : {
      contents : Script_repr.lazy_expr;  (** Contents of the withdrawn ticket *)
      ty : Script_repr.lazy_expr;
          (** Type of the withdrawn ticket's contents *)
      ticketer : Contract_repr.t;  (** Ticketer of the withdrawn ticket *)
      amount : Z.t;
          (** Quantity of the withdrawn ticket. Must match the
          amount that was enabled.  *)
      destination : Contract_repr.t;
          (** The smart contract address that should receive the tickets. *)
      entrypoint : Entrypoint_repr.t;
          (** The entrypoint of the smart contract address that should receive the tickets. *)
    }
      -> Kind.transfer_ticket manager_operation
  (* [Sc_rollup_originate] allows an implicit account to originate a new
     smart contract rollup (initialized with a given boot
     sector). *)
  | Sc_rollup_originate : {
      kind : Sc_rollup_repr.Kind.t;
      boot_sector : string;
    }
      -> Kind.sc_rollup_originate manager_operation
  (* [Sc_rollup_add_messages] adds messages to a given rollup's
      inbox. *)
  | Sc_rollup_add_messages : {
      rollup : Sc_rollup_repr.t;
      messages : string list;
    }
      -> Kind.sc_rollup_add_messages manager_operation
  | Sc_rollup_cement : {
      rollup : Sc_rollup_repr.t;
      commitment : Sc_rollup_repr.Commitment_hash.t;
    }
      -> Kind.sc_rollup_cement manager_operation
  | Sc_rollup_publish : {
      rollup : Sc_rollup_repr.t;
      commitment : Sc_rollup_repr.Commitment.t;
    }
      -> Kind.sc_rollup_publish manager_operation

(** Counters are used as anti-replay protection mechanism in
    manager operations: each manager account stores a counter and
    each manager operation declares a value for the counter. When
    a manager operation is applied, the value of the counter of
    its manager is checked and incremented. *)
and counter = Z.t

type packed_manager_operation =
  | Manager : 'kind manager_operation -> packed_manager_operation

type packed_contents = Contents : 'kind contents -> packed_contents

type packed_contents_list =
  | Contents_list : 'kind contents_list -> packed_contents_list

val of_list : packed_contents list -> packed_contents_list tzresult

val to_list : packed_contents_list -> packed_contents list

type packed_protocol_data =
  | Operation_data : 'kind protocol_data -> packed_protocol_data

type packed_operation = {
  shell : Operation.shell_header;
  protocol_data : packed_protocol_data;
}

val pack : 'kind operation -> packed_operation

val manager_kind : 'kind manager_operation -> 'kind Kind.manager

val encoding : packed_operation Data_encoding.t

val contents_encoding : packed_contents Data_encoding.t

val contents_list_encoding : packed_contents_list Data_encoding.t

val protocol_data_encoding : packed_protocol_data Data_encoding.t

val unsigned_operation_encoding :
  (Operation.shell_header * packed_contents_list) Data_encoding.t

val raw : _ operation -> raw

val hash_raw : raw -> Operation_hash.t

val hash : _ operation -> Operation_hash.t

val hash_packed : packed_operation -> Operation_hash.t

val acceptable_passes : packed_operation -> int list

type error += Missing_signature (* `Permanent *)

type error += Invalid_signature (* `Permanent *)

val check_signature :
  Signature.Public_key.t -> Chain_id.t -> _ operation -> unit tzresult

type ('a, 'b) eq = Eq : ('a, 'a) eq

val equal : 'a operation -> 'b operation -> ('a, 'b) eq option

module Encoding : sig
  type 'b case =
    | Case : {
        tag : int;
        name : string;
        encoding : 'a Data_encoding.t;
        select : packed_contents -> 'b contents option;
        proj : 'b contents -> 'a;
        inj : 'a -> 'b contents;
      }
        -> 'b case

  val preendorsement_case : Kind.preendorsement case

  val endorsement_case : Kind.endorsement case

  val seed_nonce_revelation_case : Kind.seed_nonce_revelation case

  val double_preendorsement_evidence_case :
    Kind.double_preendorsement_evidence case

  val double_endorsement_evidence_case : Kind.double_endorsement_evidence case

  val double_baking_evidence_case : Kind.double_baking_evidence case

  val activate_account_case : Kind.activate_account case

  val proposals_case : Kind.proposals case

  val ballot_case : Kind.ballot case

  val failing_noop_case : Kind.failing_noop case

  val reveal_case : Kind.reveal Kind.manager case

  val transaction_case : Kind.transaction Kind.manager case

  val origination_case : Kind.origination Kind.manager case

  val delegation_case : Kind.delegation Kind.manager case

  val register_global_constant_case :
    Kind.register_global_constant Kind.manager case

  val set_deposits_limit_case : Kind.set_deposits_limit Kind.manager case

  val tx_rollup_origination_case : Kind.tx_rollup_origination Kind.manager case

  val tx_rollup_submit_batch_case :
    Kind.tx_rollup_submit_batch Kind.manager case

  val tx_rollup_commit_case : Kind.tx_rollup_commit Kind.manager case

  val tx_rollup_return_bond_case : Kind.tx_rollup_return_bond Kind.manager case

  val tx_rollup_finalize_commitment_case :
    Kind.tx_rollup_finalize_commitment Kind.manager case

  val tx_rollup_remove_commitment_case :
    Kind.tx_rollup_remove_commitment Kind.manager case

  val tx_rollup_rejection_case : Kind.tx_rollup_rejection Kind.manager case

  val tx_rollup_dispatch_tickets_case :
    Kind.tx_rollup_dispatch_tickets Kind.manager case

  val transfer_ticket_case : Kind.transfer_ticket Kind.manager case

  val sc_rollup_originate_case : Kind.sc_rollup_originate Kind.manager case

  val sc_rollup_add_messages_case :
    Kind.sc_rollup_add_messages Kind.manager case

  val sc_rollup_cement_case : Kind.sc_rollup_cement Kind.manager case

  val sc_rollup_publish_case : Kind.sc_rollup_publish Kind.manager case

  module Manager_operations : sig
    type 'b case =
      | MCase : {
          tag : int;
          name : string;
          encoding : 'a Data_encoding.t;
          select : packed_manager_operation -> 'kind manager_operation option;
          proj : 'kind manager_operation -> 'a;
          inj : 'a -> 'kind manager_operation;
        }
          -> 'kind case

    val reveal_case : Kind.reveal case

    val transaction_tag : int

    val transaction_case : Kind.transaction case

    val origination_tag : int

    val origination_case : Kind.origination case

    val delegation_tag : int

    val delegation_case : Kind.delegation case

    val register_global_constant_case : Kind.register_global_constant case

    val set_deposits_limit_case : Kind.set_deposits_limit case

    val tx_rollup_origination_case : Kind.tx_rollup_origination case

    val tx_rollup_submit_batch_case : Kind.tx_rollup_submit_batch case

    val tx_rollup_commit_case : Kind.tx_rollup_commit case

    val tx_rollup_return_bond_case : Kind.tx_rollup_return_bond case

    val tx_rollup_finalize_commitment_case :
      Kind.tx_rollup_finalize_commitment case

    val tx_rollup_remove_commitment_case : Kind.tx_rollup_remove_commitment case

    val tx_rollup_rejection_case : Kind.tx_rollup_rejection case

    val tx_rollup_dispatch_tickets_case : Kind.tx_rollup_dispatch_tickets case

    val transfer_ticket_case : Kind.transfer_ticket case

    val sc_rollup_originate_case : Kind.sc_rollup_originate case

    val sc_rollup_add_messages_case : Kind.sc_rollup_add_messages case

    val sc_rollup_cement_case : Kind.sc_rollup_cement case

    val sc_rollup_publish_case : Kind.sc_rollup_publish case
  end
end
