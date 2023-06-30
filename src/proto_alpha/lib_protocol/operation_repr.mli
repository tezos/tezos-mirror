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
      - smart rollup origination
      - smart rollup add messages
      - smart rollup publish
      - smart rollup cement
      - smart rollup refute
      - smart rollup timeout
      - smart rollup execute outbox message
      - smart rollup recover bond
      - zk rollup origination
      - zk rollup publish
      - zk rollup update

    Each of them can be encoded as raw bytes. Operations are distinguished at
    type level using phantom type parameters. [packed_operation] type allows
    for unifying them when required, for instance to put them on a single
    list. *)

module Kind : sig
  type preattestation_consensus_kind = Preattestation_consensus_kind

  type endorsement_consensus_kind = Endorsement_consensus_kind

  type 'a consensus =
    | Preattestation_kind : preattestation_consensus_kind consensus
    | Endorsement_kind : endorsement_consensus_kind consensus

  type preendorsement = preattestation_consensus_kind consensus

  type endorsement = endorsement_consensus_kind consensus

  type dal_attestation = Dal_attestation_kind

  type seed_nonce_revelation = Seed_nonce_revelation_kind

  type vdf_revelation = Vdf_revelation_kind

  type 'a double_consensus_operation_evidence =
    | Double_consensus_operation_evidence

  type double_endorsement_evidence =
    endorsement_consensus_kind double_consensus_operation_evidence

  type double_preendorsement_evidence =
    preattestation_consensus_kind double_consensus_operation_evidence

  type double_baking_evidence = Double_baking_evidence_kind

  type activate_account = Activate_account_kind

  type proposals = Proposals_kind

  type ballot = Ballot_kind

  type reveal = Reveal_kind

  type transaction = Transaction_kind

  type origination = Origination_kind

  type delegation = Delegation_kind

  type event = Event_kind

  type increase_paid_storage = Increase_paid_storage_kind

  type update_consensus_key = Update_consensus_key_kind

  type drain_delegate = Drain_delegate_kind

  type failing_noop = Failing_noop_kind

  type register_global_constant = Register_global_constant_kind

  type transfer_ticket = Transfer_ticket_kind

  type dal_publish_slot_header = Dal_publish_slot_header_kind

  type sc_rollup_originate = Sc_rollup_originate_kind

  type sc_rollup_add_messages = Sc_rollup_add_messages_kind

  type sc_rollup_cement = Sc_rollup_cement_kind

  type sc_rollup_publish = Sc_rollup_publish_kind

  type sc_rollup_refute = Sc_rollup_refute_kind

  type sc_rollup_timeout = Sc_rollup_timeout_kind

  type sc_rollup_execute_outbox_message =
    | Sc_rollup_execute_outbox_message_kind

  type sc_rollup_recover_bond = Sc_rollup_recover_bond_kind

  type zk_rollup_origination = Zk_rollup_origination_kind

  type zk_rollup_publish = Zk_rollup_publish_kind

  type zk_rollup_update = Zk_rollup_update_kind

  type 'a manager =
    | Reveal_manager_kind : reveal manager
    | Transaction_manager_kind : transaction manager
    | Origination_manager_kind : origination manager
    | Delegation_manager_kind : delegation manager
    | Event_manager_kind : event manager
    | Register_global_constant_manager_kind : register_global_constant manager
    | Increase_paid_storage_manager_kind : increase_paid_storage manager
    | Update_consensus_key_manager_kind : update_consensus_key manager
    | Transfer_ticket_manager_kind : transfer_ticket manager
    | Dal_publish_slot_header_manager_kind : dal_publish_slot_header manager
    | Sc_rollup_originate_manager_kind : sc_rollup_originate manager
    | Sc_rollup_add_messages_manager_kind : sc_rollup_add_messages manager
    | Sc_rollup_cement_manager_kind : sc_rollup_cement manager
    | Sc_rollup_publish_manager_kind : sc_rollup_publish manager
    | Sc_rollup_refute_manager_kind : sc_rollup_refute manager
    | Sc_rollup_timeout_manager_kind : sc_rollup_timeout manager
    | Sc_rollup_execute_outbox_message_manager_kind
        : sc_rollup_execute_outbox_message manager
    | Sc_rollup_recover_bond_manager_kind : sc_rollup_recover_bond manager
    | Zk_rollup_origination_manager_kind : zk_rollup_origination manager
    | Zk_rollup_publish_manager_kind : zk_rollup_publish manager
    | Zk_rollup_update_manager_kind : zk_rollup_update manager
end

type 'a consensus_operation_type =
  | Endorsement : Kind.endorsement consensus_operation_type
  | Preendorsement : Kind.preendorsement consensus_operation_type

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
  | Dal_attestation of Chain_id.t

val to_watermark : consensus_watermark -> Signature.watermark

val of_watermark : Signature.watermark -> consensus_watermark option

type raw = Operation.t = {shell : Operation.shell_header; proto : bytes}

val raw_encoding : raw Data_encoding.t

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
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3115

     Temporary operation to avoid modifying endorsement encoding. *)
  | Dal_attestation :
      Dal_attestation_repr.operation
      -> Kind.dal_attestation contents
  (* Seed_nonce_revelation: Nonces are created by bakers and are
     combined to create pseudo-random seeds. Bakers are urged to reveal their
     nonces after a given number of cycles to keep their block rewards
     from being forfeited. *)
  | Seed_nonce_revelation : {
      level : Raw_level_repr.t;
      nonce : Seed_repr.nonce;
    }
      -> Kind.seed_nonce_revelation contents
  (* Vdf_revelation: VDF are computed from the seed generated by the revealed
     nonces. *)
  | Vdf_revelation : {
      solution : Seed_repr.vdf_solution;
    }
      -> Kind.vdf_revelation contents
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
  (* [Drain_delegate { consensus_key ; delegate ; destination }]
     transfers the spendable balance of the [delegate] to [destination]
     when [consensus_key] is the active consensus key of [delegate].. *)
  | Drain_delegate : {
      consensus_key : Signature.Public_key_hash.t;
      delegate : Signature.Public_key_hash.t;
      destination : Signature.Public_key_hash.t;
    }
      -> Kind.drain_delegate contents
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
      counter : Manager_counter_repr.t;
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
  | Transaction : {
      amount : Tez_repr.tez;
      parameters : Script_repr.lazy_expr;
      entrypoint : Entrypoint_repr.t;
      destination : Contract_repr.t;
    }
      -> Kind.transaction manager_operation
  (* [Origination] of a contract using a smart-contract [script] and
     initially credited with the amount [credit]. *)
  | Origination : {
      delegate : Signature.Public_key_hash.t option;
      script : Script_repr.t;
      credit : Tez_repr.tez;
    }
      -> Kind.origination manager_operation
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
  (* [Increase_paid_storage] allows a sender to pay to increase the paid storage of
     some contract by some amount. *)
  | Increase_paid_storage : {
      amount_in_bytes : Z.t;
      destination : Contract_hash.t;
    }
      -> Kind.increase_paid_storage manager_operation
  (* [Update_consensus_key pk] updates the consensus key of
     the signing delegate to [pk]. *)
  | Update_consensus_key :
      Signature.Public_key.t
      -> Kind.update_consensus_key manager_operation
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
      amount : Ticket_amount.t;
          (** Quantity of the withdrawn ticket. Must match the
          amount that was enabled.  *)
      destination : Contract_repr.t;
          (** The smart contract address that should receive the tickets. *)
      entrypoint : Entrypoint_repr.t;
          (** The entrypoint of the smart contract address that should receive the tickets. *)
    }
      -> Kind.transfer_ticket manager_operation
  | Dal_publish_slot_header :
      Dal_operations_repr.Publish_slot_header.t
      -> Kind.dal_publish_slot_header manager_operation
      (** [Sc_rollup_originate] allows an implicit account to originate a new
          smart contract rollup (initialized with a given boot sector).
          The [parameters_ty] field allows to provide the expected interface
          of the rollup being originated (i.e. its entrypoints with their
          associated signatures) as a Michelson type.
      *)
  | Sc_rollup_originate : {
      kind : Sc_rollups.Kind.t;
      boot_sector : string;
      parameters_ty : Script_repr.lazy_expr;
    }
      -> Kind.sc_rollup_originate manager_operation
  (* [Sc_rollup_add_messages] adds messages to the smart rollups' inbox. *)
  | Sc_rollup_add_messages : {
      messages : string list;
    }
      -> Kind.sc_rollup_add_messages manager_operation
  | Sc_rollup_cement : {
      rollup : Sc_rollup_repr.t;
    }
      -> Kind.sc_rollup_cement manager_operation
  | Sc_rollup_publish : {
      rollup : Sc_rollup_repr.t;
      commitment : Sc_rollup_commitment_repr.t;
    }
      -> Kind.sc_rollup_publish manager_operation
  | Sc_rollup_refute : {
      rollup : Sc_rollup_repr.t;
      opponent : Sc_rollup_repr.Staker.t;
      refutation : Sc_rollup_game_repr.refutation;
    }
      -> Kind.sc_rollup_refute manager_operation
      (** [Sc_rollup_refute { rollup; opponent; refutation }] makes a move
          in a refutation game between the source of the operation and the
          [opponent] under the given [rollup]. Both players must be stakers
          on commitments in conflict. When [refutation = None], the game is
          initialized. Next, when [refutation = Some move], [move] is the
          next play for the current player. See {!Sc_rollup_game_repr} for
          details. **)
  | Sc_rollup_timeout : {
      rollup : Sc_rollup_repr.t;
      stakers : Sc_rollup_game_repr.Index.t;
    }
      -> Kind.sc_rollup_timeout manager_operation
  (* [Sc_rollup_execute_outbox_message] executes a message from the rollup's
      outbox. Messages may involve transactions to smart contract accounts on
      Layer 1. *)
  | Sc_rollup_execute_outbox_message : {
      rollup : Sc_rollup_repr.t;  (** The smart-contract rollup. *)
      cemented_commitment : Sc_rollup_commitment_repr.Hash.t;
          (** The hash of the last cemented commitment that the proof refers to. *)
      output_proof : string;
          (** A message along with a proof that it is included in the outbox
              at a given outbox level and message index.*)
    }
      -> Kind.sc_rollup_execute_outbox_message manager_operation
  | Sc_rollup_recover_bond : {
      sc_rollup : Sc_rollup_repr.t;
      staker : Signature.Public_key_hash.t;
    }
      -> Kind.sc_rollup_recover_bond manager_operation
  | Zk_rollup_origination : {
      public_parameters : Plonk.public_parameters;
      circuits_info : [`Public | `Private | `Fee] Zk_rollup_account_repr.SMap.t;
          (** Circuit names, alongside a tag indicating its kind. *)
      init_state : Zk_rollup_state_repr.t;
      nb_ops : int;
    }
      -> Kind.zk_rollup_origination manager_operation
  | Zk_rollup_publish : {
      zk_rollup : Zk_rollup_repr.t;
      ops : (Zk_rollup_operation_repr.t * Zk_rollup_ticket_repr.t option) list;
          (* See {!Zk_rollup_apply} *)
    }
      -> Kind.zk_rollup_publish manager_operation
  | Zk_rollup_update : {
      zk_rollup : Zk_rollup_repr.t;
      update : Zk_rollup_update_repr.t;
    }
      -> Kind.zk_rollup_update manager_operation

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

(** Operation encoding that accepts legacy attestation name : `endorsement`
    (and preendorsement, double_<op>_evidence) in JSON

    https://gitlab.com/tezos/tezos/-/issues/5529

    This encoding is temporary and should be removed when the endorsements kinds
    in JSON will not be accepted any more by the protocol.
*)
val encoding_with_legacy_attestation_name : packed_operation Data_encoding.t

val contents_encoding : packed_contents Data_encoding.t

val contents_encoding_with_legacy_attestation_name :
  packed_contents Data_encoding.t

val contents_list_encoding : packed_contents_list Data_encoding.t

val contents_list_encoding_with_legacy_attestation_name :
  packed_contents_list Data_encoding.t

val protocol_data_encoding : packed_protocol_data Data_encoding.t

val protocol_data_encoding_with_legacy_attestation_name :
  packed_protocol_data Data_encoding.t

val unsigned_operation_encoding :
  (Operation.shell_header * packed_contents_list) Data_encoding.t

val unsigned_operation_encoding_with_legacy_attestation_name :
  (Operation.shell_header * packed_contents_list) Data_encoding.t

val raw : _ operation -> raw

val hash_raw : raw -> Operation_hash.t

val hash : _ operation -> Operation_hash.t

val hash_packed : packed_operation -> Operation_hash.t

(** Each operation belongs to a validation pass that is an integer
   abstracting its priority in a block. Except Failing_noop. *)

(** The validation pass of consensus operations. *)
val consensus_pass : int

(** The validation pass of voting operations. *)
val voting_pass : int

(** The validation pass of anonymous operations. *)
val anonymous_pass : int

(** The validation pass of anonymous operations. *)
val manager_pass : int

(** [acceptable_pass op] returns either the validation_pass of [op]
   when defines and None when [op] is [Failing_noop]. *)
val acceptable_pass : packed_operation -> int option

(** [compare_by_passes] orders two operations in the reverse order of
   their acceptable passes. *)
val compare_by_passes : packed_operation -> packed_operation -> int

(** [compare (oph1,op1) (oph2,op2)] defines a total ordering relation
   on operations.

   The following requirements must be satisfied: [oph1] is the
   [Operation.hash op1], [oph2] is [Operation.hash op2], and that
   [op1] and [op2] are valid in the same context.

   [compare (oph1,op1) (oph2,op2) = 0] happens only if
   [Operation_hash.compare oph1 oph2 = 0], meaning when [op1] and
   [op2] are structurally identical.

   Two valid operations of different [validation_pass] are compared
   according to {!acceptable_passes}: the one with the smaller pass
   being the greater.

   Two valid operations of the same [validation_pass] are compared
   according to a [weight], computed thanks to their static
   information.

   The global order is as follows:

   {!Endorsement} and {!Preendorsement} > {!Dal_attestation} >
   {!Proposals} > {!Ballot} > {!Double_preendorsement_evidence} >
   {!Double_endorsement_evidence} > {!Double_baking_evidence} >
   {!Vdf_revelation} > {!Seed_nonce_revelation} > {!Activate_account}
   > {!Drain_delegate} > {!Manager_operation}.

   {!Endorsement} and {!Preendorsement} are compared by the pair of
   their [level] and [round] such as the farther to the current state
   [level] and [round] is greater; e.g. the greater pair in
   lexicographic order being the better. When equal and both
   operations being of the same kind, we compare their [slot]: the
   The smaller being the better, assuming that the more slots an endorser
   has, the smaller is its smallest [slot]. When the pair is equal
   and comparing an {!Endorsement] to a {!Preendorsement}, the
   {!Endorsement} is better.

   Two {!Dal_attestation} ops are compared in the lexicographic
   order of the pair of their number of endorsed slots as available
   and their endorsers.

   Two voting operations are compared in the lexicographic order of
   the pair of their [period] and [source]. A {!Proposals} is better
   than a {!Ballot}.

   Two denunciations of the same kind are compared such as the farther
   to the current state the better. For {!Double_baking_evidence}
   in the case of equality, they are compared by the hashes of their first
   denounced block_header.

   Two {!Vdf_revelation} ops are compared by their [solution].

   Two {!Seed_nonce_relevation} ops are compared by their [level].

   Two {!Activate_account} ops are compared by their [id].

   Two {!Drain_delegate} ops are compared by their [delegate].

   Two {!Manager_operation}s are compared in the lexicographic order of
   the pair of their [fee]/[gas_limit] ratios and [source]. *)
val compare :
  Operation_hash.t * packed_operation ->
  Operation_hash.t * packed_operation ->
  int

type error += Missing_signature (* `Permanent *)

type error += Invalid_signature (* `Permanent *)

(** Measuring the length of an operation, ignoring its signature.
    This is useful to define a gas model for the check of the
    signature. *)
val unsigned_operation_length : _ operation -> int

(** Check the signature of an operation. This function serializes the
    operation before calling the [Signature.check] function with the
    appropriate watermark. *)
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

  val preattestation_case : Kind.preendorsement case

  val endorsement_case : Kind.endorsement case

  val attestation_case : Kind.endorsement case

  val dal_attestation_case : Kind.dal_attestation case

  val seed_nonce_revelation_case : Kind.seed_nonce_revelation case

  val vdf_revelation_case : Kind.vdf_revelation case

  val double_preendorsement_evidence_case :
    Kind.double_preendorsement_evidence case

  val double_preattestation_evidence_case :
    Kind.double_preendorsement_evidence case

  val double_endorsement_evidence_case : Kind.double_endorsement_evidence case

  val double_attestation_evidence_case : Kind.double_endorsement_evidence case

  val double_baking_evidence_case : Kind.double_baking_evidence case

  val activate_account_case : Kind.activate_account case

  val proposals_case : Kind.proposals case

  val ballot_case : Kind.ballot case

  val drain_delegate_case : Kind.drain_delegate case

  val failing_noop_case : Kind.failing_noop case

  val reveal_case : Kind.reveal Kind.manager case

  val transaction_case : Kind.transaction Kind.manager case

  val origination_case : Kind.origination Kind.manager case

  val delegation_case : Kind.delegation Kind.manager case

  val update_consensus_key_case : Kind.update_consensus_key Kind.manager case

  val register_global_constant_case :
    Kind.register_global_constant Kind.manager case

  val increase_paid_storage_case : Kind.increase_paid_storage Kind.manager case

  val transfer_ticket_case : Kind.transfer_ticket Kind.manager case

  val dal_publish_slot_header_case :
    Kind.dal_publish_slot_header Kind.manager case

  val sc_rollup_originate_case : Kind.sc_rollup_originate Kind.manager case

  val sc_rollup_add_messages_case :
    Kind.sc_rollup_add_messages Kind.manager case

  val sc_rollup_cement_case : Kind.sc_rollup_cement Kind.manager case

  val sc_rollup_publish_case : Kind.sc_rollup_publish Kind.manager case

  val sc_rollup_refute_case : Kind.sc_rollup_refute Kind.manager case

  val sc_rollup_timeout_case : Kind.sc_rollup_timeout Kind.manager case

  val sc_rollup_execute_outbox_message_case :
    Kind.sc_rollup_execute_outbox_message Kind.manager case

  val sc_rollup_recover_bond_case :
    Kind.sc_rollup_recover_bond Kind.manager case

  val zk_rollup_origination_case : Kind.zk_rollup_origination Kind.manager case

  val zk_rollup_publish_case : Kind.zk_rollup_publish Kind.manager case

  val zk_rollup_update_case : Kind.zk_rollup_update Kind.manager case

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

    val transaction_case : Kind.transaction case

    val origination_case : Kind.origination case

    val delegation_case : Kind.delegation case

    val update_consensus_key_tag : int

    val update_consensus_key_case : Kind.update_consensus_key case

    val register_global_constant_case : Kind.register_global_constant case

    val increase_paid_storage_case : Kind.increase_paid_storage case

    val transfer_ticket_case : Kind.transfer_ticket case

    val dal_publish_slot_header_case : Kind.dal_publish_slot_header case

    val sc_rollup_originate_case : Kind.sc_rollup_originate case

    val sc_rollup_add_messages_case : Kind.sc_rollup_add_messages case

    val sc_rollup_cement_case : Kind.sc_rollup_cement case

    val sc_rollup_publish_case : Kind.sc_rollup_publish case

    val sc_rollup_refute_case : Kind.sc_rollup_refute case

    val sc_rollup_timeout_case : Kind.sc_rollup_timeout case

    val sc_rollup_execute_outbox_message_case :
      Kind.sc_rollup_execute_outbox_message case

    val sc_rollup_recover_bond_case : Kind.sc_rollup_recover_bond case

    val zk_rollup_origination_case : Kind.zk_rollup_origination case

    val zk_rollup_publish_case : Kind.zk_rollup_publish case

    val zk_rollup_update_case : Kind.zk_rollup_update case
  end
end

module Internal_for_benchmarking : sig
  (* Serialize an operation, ignoring its signature. *)
  val serialize_unsigned_operation : _ operation -> bytes
end
