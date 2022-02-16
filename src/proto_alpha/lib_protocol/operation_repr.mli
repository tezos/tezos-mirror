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

  type sc_rollup_originate = Sc_rollup_originate_kind

  type sc_rollup_add_messages = Sc_rollup_add_messages_kind

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
    | Sc_rollup_originate_manager_kind : sc_rollup_originate manager
    | Sc_rollup_add_messages_manager_kind : sc_rollup_add_messages manager
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

type 'kind operation = {
  shell : Operation.shell_header;
  protocol_data : 'kind protocol_data;
}

and 'kind protocol_data = {
  contents : 'kind contents_list;
  signature : Signature.t option;
}

and _ contents_list =
  | Single : 'kind contents -> 'kind contents_list
  | Cons :
      'kind Kind.manager contents * 'rest Kind.manager contents_list
      -> ('kind * 'rest) Kind.manager contents_list

and _ contents =
  | Preendorsement : consensus_content -> Kind.preendorsement contents
  | Endorsement : consensus_content -> Kind.endorsement contents
  | Seed_nonce_revelation : {
      level : Raw_level_repr.t;
      nonce : Seed_repr.nonce;
    }
      -> Kind.seed_nonce_revelation contents
  | Double_preendorsement_evidence : {
      op1 : Kind.preendorsement operation;
      op2 : Kind.preendorsement operation;
    }
      -> Kind.double_preendorsement_evidence contents
  | Double_endorsement_evidence : {
      op1 : Kind.endorsement operation;
      op2 : Kind.endorsement operation;
    }
      -> Kind.double_endorsement_evidence contents
  | Double_baking_evidence : {
      bh1 : Block_header_repr.t;
      bh2 : Block_header_repr.t;
    }
      -> Kind.double_baking_evidence contents
  | Activate_account : {
      id : Ed25519.Public_key_hash.t;
      activation_code : Blinded_public_key_hash.activation_code;
    }
      -> Kind.activate_account contents
  | Proposals : {
      source : Signature.Public_key_hash.t;
      period : int32;
      proposals : Protocol_hash.t list;
    }
      -> Kind.proposals contents
  | Ballot : {
      source : Signature.Public_key_hash.t;
      period : int32;
      proposal : Protocol_hash.t;
      ballot : Vote_repr.ballot;
    }
      -> Kind.ballot contents
  | Failing_noop : string -> Kind.failing_noop contents
  | Manager_operation : {
      source : Signature.Public_key_hash.t;
      fee : Tez_repr.tez;
      counter : counter;
      operation : 'kind manager_operation;
      gas_limit : Gas_limit_repr.Arith.integral;
      storage_limit : Z.t;
    }
      -> 'kind Kind.manager contents

and _ manager_operation =
  | Reveal : Signature.Public_key.t -> Kind.reveal manager_operation
  | Transaction : {
      amount : Tez_repr.tez;
      parameters : Script_repr.lazy_expr;
      entrypoint : Entrypoint_repr.t;
      destination : Destination_repr.t;
    }
      -> Kind.transaction manager_operation
  | Origination : {
      delegate : Signature.Public_key_hash.t option;
      script : Script_repr.t;
      credit : Tez_repr.tez;
      preorigination : Contract_repr.t option;
    }
      -> Kind.origination manager_operation
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation manager_operation
  | Register_global_constant : {
      value : Script_repr.lazy_expr;
    }
      -> Kind.register_global_constant manager_operation
  | Set_deposits_limit :
      Tez_repr.t option
      -> Kind.set_deposits_limit manager_operation
  | Tx_rollup_origination : Kind.tx_rollup_origination manager_operation
  | Tx_rollup_submit_batch : {
      tx_rollup : Tx_rollup_repr.t;
      content : string;
      burn_limit : Tez_repr.t option;
    }
      -> Kind.tx_rollup_submit_batch manager_operation
  | Tx_rollup_commit : {
      tx_rollup : Tx_rollup_repr.t;
      commitment : Tx_rollup_commitments_repr.Commitment.t;
    }
      -> Kind.tx_rollup_commit manager_operation
  | Sc_rollup_originate : {
      kind : Sc_rollup_repr.Kind.t;
      boot_sector : Sc_rollup_repr.PVM.boot_sector;
    }
      -> Kind.sc_rollup_originate manager_operation
  | Sc_rollup_add_messages : {
      rollup : Sc_rollup_repr.t;
      messages : string list;
    }
      -> Kind.sc_rollup_add_messages manager_operation

and counter = Z.t

type 'kind internal_operation = {
  source : Contract_repr.contract;
  operation : 'kind manager_operation;
  nonce : int;
}

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

type packed_internal_operation =
  | Internal_operation : 'kind internal_operation -> packed_internal_operation

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

val internal_operation_encoding : packed_internal_operation Data_encoding.t

type ('a, 'b) eq = Eq : ('a, 'a) eq

val equal : 'a operation -> 'b operation -> ('a, 'b) eq option

val packed_internal_operation_in_memory_size :
  packed_internal_operation -> Cache_memory_helpers.nodes_and_size

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

  val sc_rollup_originate_case : Kind.sc_rollup_originate Kind.manager case

  val sc_rollup_add_messages_case :
    Kind.sc_rollup_add_messages Kind.manager case

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

    val register_global_constant_case : Kind.register_global_constant case

    val set_deposits_limit_case : Kind.set_deposits_limit case

    val tx_rollup_origination_case : Kind.tx_rollup_origination case

    val tx_rollup_submit_batch_case : Kind.tx_rollup_submit_batch case

    val tx_rollup_commit_case : Kind.tx_rollup_commit case

    val sc_rollup_originate_case : Kind.sc_rollup_originate case

    val sc_rollup_add_messages_case : Kind.sc_rollup_add_messages case
  end
end
