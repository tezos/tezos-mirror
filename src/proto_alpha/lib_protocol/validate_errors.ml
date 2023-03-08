(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

open Alpha_context

type operation_conflict =
  | Operation_conflict of {
      existing : Operation_hash.t;
      new_operation : Operation_hash.t;
    }

let operation_conflict_encoding =
  let open Data_encoding in
  def
    "operation_conflict"
    ~title:"Conflict error"
    ~description:"Conflict between two operations"
  @@ conv
       (function
         | Operation_conflict {existing; new_operation} ->
             (existing, new_operation))
       (fun (existing, new_operation) ->
         Operation_conflict {existing; new_operation})
       (obj2
          (req "existing" Operation_hash.encoding)
          (req "new_operation" Operation_hash.encoding))

module Consensus = struct
  type error += Zero_frozen_deposits of Signature.Public_key_hash.t

  let () =
    register_error_kind
      `Permanent
      ~id:"validate.zero_frozen_deposits"
      ~title:"Zero frozen deposits"
      ~description:"The delegate has zero frozen deposits."
      ~pp:(fun ppf delegate ->
        Format.fprintf
          ppf
          "Delegate %a has zero frozen deposits; it is not allowed to \
           bake/preendorse/endorse."
          Signature.Public_key_hash.pp
          delegate)
      Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
      (function Zero_frozen_deposits delegate -> Some delegate | _ -> None)
      (fun delegate -> Zero_frozen_deposits delegate)

  (** This type is only used in consensus operation errors to make
      them more informative. *)
  type consensus_operation_kind =
    | Preendorsement
    | Endorsement
    | Dal_attestation

  let consensus_operation_kind_encoding =
    Data_encoding.string_enum
      [
        ("Preendorsement", Preendorsement);
        ("Endorsement", Endorsement);
        ("Dal_attestation", Dal_attestation);
      ]

  let consensus_operation_kind_pp fmt = function
    | Preendorsement -> Format.fprintf fmt "Preendorsement"
    | Endorsement -> Format.fprintf fmt "Endorsement"
    | Dal_attestation -> Format.fprintf fmt "Dal_attestation"

  (** Errors for preendorsements and endorsements. *)
  type error +=
    | Consensus_operation_for_old_level of {
        kind : consensus_operation_kind;
        expected : Raw_level.t;
        provided : Raw_level.t;
      }
    | Consensus_operation_for_future_level of {
        kind : consensus_operation_kind;
        expected : Raw_level.t;
        provided : Raw_level.t;
      }
    | Consensus_operation_for_old_round of {
        kind : consensus_operation_kind;
        expected : Round.t;
        provided : Round.t;
      }
    | Consensus_operation_for_future_round of {
        kind : consensus_operation_kind;
        expected : Round.t;
        provided : Round.t;
      }
    | Wrong_payload_hash_for_consensus_operation of {
        kind : consensus_operation_kind;
        expected : Block_payload_hash.t;
        provided : Block_payload_hash.t;
      }
    | Unexpected_preendorsement_in_block
    | Unexpected_endorsement_in_block
    | Preendorsement_round_too_high of {
        block_round : Round.t;
        provided : Round.t;
      }
    | Wrong_slot_used_for_consensus_operation of {
        kind : consensus_operation_kind;
      }
    | Conflicting_consensus_operation of {
        kind : consensus_operation_kind;
        conflict : operation_conflict;
      }
    | Consensus_operation_not_allowed

  let () =
    register_error_kind
      `Outdated
      ~id:"validate.consensus_operation_for_old_level"
      ~title:"Consensus operation for old level"
      ~description:"Consensus operation for old level."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a for old level (expected: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Raw_level.pp
          expected
          Raw_level.pp
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected" Raw_level.encoding)
          (req "provided" Raw_level.encoding))
      (function
        | Consensus_operation_for_old_level {kind; expected; provided} ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Consensus_operation_for_old_level {kind; expected; provided}) ;
    register_error_kind
      `Temporary
      ~id:"validate.consensus_operation_for_future_level"
      ~title:"Consensus operation for future level"
      ~description:"Consensus operation for future level."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a for future level (expected: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Raw_level.pp
          expected
          Raw_level.pp
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected" Raw_level.encoding)
          (req "provided" Raw_level.encoding))
      (function
        | Consensus_operation_for_future_level {kind; expected; provided} ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Consensus_operation_for_future_level {kind; expected; provided}) ;
    register_error_kind
      `Branch
      ~id:"validate.consensus_operation_for_old_round"
      ~title:"Consensus operation for old round"
      ~description:"Consensus operation for old round."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a for old round (expected_min: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Round.pp
          expected
          Round.pp
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected_min" Round.encoding)
          (req "provided" Round.encoding))
      (function
        | Consensus_operation_for_old_round {kind; expected; provided} ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Consensus_operation_for_old_round {kind; expected; provided}) ;
    register_error_kind
      `Temporary
      ~id:"validate.consensus_operation_for_future_round"
      ~title:"Consensus operation for future round"
      ~description:"Consensus operation for future round."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a for future round (expected: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Round.pp
          expected
          Round.pp
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected_max" Round.encoding)
          (req "provided" Round.encoding))
      (function
        | Consensus_operation_for_future_round {kind; expected; provided} ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Consensus_operation_for_future_round {kind; expected; provided}) ;
    register_error_kind
      (* Note: in Mempool mode this used to be
         Consensus_operation_on_competing_proposal (which was
         [`Branch] so we kept this classification). *)
      `Branch
      ~id:"validate.wrong_payload_hash_for_consensus_operation"
      ~title:"Wrong payload hash for consensus operation"
      ~description:"Wrong payload hash for consensus operation."
      ~pp:(fun ppf (kind, expected, provided) ->
        Format.fprintf
          ppf
          "%a with wrong payload hash (expected: %a, provided: %a)."
          consensus_operation_kind_pp
          kind
          Block_payload_hash.pp_short
          expected
          Block_payload_hash.pp_short
          provided)
      Data_encoding.(
        obj3
          (req "kind" consensus_operation_kind_encoding)
          (req "expected" Block_payload_hash.encoding)
          (req "provided" Block_payload_hash.encoding))
      (function
        | Wrong_payload_hash_for_consensus_operation {kind; expected; provided}
          ->
            Some (kind, expected, provided)
        | _ -> None)
      (fun (kind, expected, provided) ->
        Wrong_payload_hash_for_consensus_operation {kind; expected; provided}) ;
    register_error_kind
      `Permanent
      ~id:"validate.unexpected_preendorsement_in_block"
      ~title:"Unexpected preendorsement in block"
      ~description:"Unexpected preendorsement in block."
      ~pp:(fun ppf () ->
        Format.fprintf ppf "Unexpected preendorsement in block.")
      Data_encoding.empty
      (function Unexpected_preendorsement_in_block -> Some () | _ -> None)
      (fun () -> Unexpected_preendorsement_in_block) ;
    register_error_kind
      `Permanent
      ~id:"validate.unexpected_endorsement_in_block"
      ~title:"Unexpected endorsement in block"
      ~description:"Unexpected endorsement in block."
      ~pp:(fun ppf () -> Format.fprintf ppf "Unexpected endorsement in block.")
      Data_encoding.empty
      (function Unexpected_endorsement_in_block -> Some () | _ -> None)
      (fun () -> Unexpected_endorsement_in_block) ;
    register_error_kind
      `Permanent
      ~id:"validate.preendorsement_round_too_high"
      ~title:"Preendorsement round too high"
      ~description:"Preendorsement round too high."
      ~pp:(fun ppf (block_round, provided) ->
        Format.fprintf
          ppf
          "Preendorsement round too high (block_round: %a, provided: %a)."
          Round.pp
          block_round
          Round.pp
          provided)
      Data_encoding.(
        obj2 (req "block_round" Round.encoding) (req "provided" Round.encoding))
      (function
        | Preendorsement_round_too_high {block_round; provided} ->
            Some (block_round, provided)
        | _ -> None)
      (fun (block_round, provided) ->
        Preendorsement_round_too_high {block_round; provided}) ;
    register_error_kind
      `Permanent
      ~id:"validate.wrong_slot_for_consensus_operation"
      ~title:"Wrong slot for consensus operation"
      ~description:"Wrong slot used for a preendorsement or endorsement."
      ~pp:(fun ppf kind ->
        Format.fprintf
          ppf
          "Wrong slot used for a %a."
          consensus_operation_kind_pp
          kind)
      Data_encoding.(obj1 (req "kind" consensus_operation_kind_encoding))
      (function
        | Wrong_slot_used_for_consensus_operation {kind} -> Some kind
        | _ -> None)
      (fun kind -> Wrong_slot_used_for_consensus_operation {kind}) ;
    register_error_kind
      `Branch
      ~id:"validate.double_inclusion_of_consensus_operation"
      ~title:"Double inclusion of consensus operation"
      ~description:"Double inclusion of consensus operation."
      ~pp:(fun ppf (kind, Operation_conflict {existing; new_operation}) ->
        Format.fprintf
          ppf
          "%a operation %a conflicts with existing %a"
          consensus_operation_kind_pp
          kind
          Operation_hash.pp
          new_operation
          Operation_hash.pp
          existing)
      Data_encoding.(
        obj2
          (req "kind" consensus_operation_kind_encoding)
          (req "conflict" operation_conflict_encoding))
      (function
        | Conflicting_consensus_operation {kind; conflict} ->
            Some (kind, conflict)
        | _ -> None)
      (fun (kind, conflict) -> Conflicting_consensus_operation {kind; conflict}) ;
    register_error_kind
      `Branch
      ~id:"validate.consensus_operation_not_allowed"
      ~title:"Consensus operation not allowed"
      ~description:"Consensus operation not allowed."
      ~pp:(fun ppf () ->
        Format.fprintf ppf "Validation of consensus operation if forbidden ")
      Data_encoding.empty
      (function Consensus_operation_not_allowed -> Some () | _ -> None)
      (fun () -> Consensus_operation_not_allowed)
end

module Voting = struct
  type error +=
    | (* Shared voting errors *)
        Wrong_voting_period_index of {
        expected : int32;
        provided : int32;
      }
    | Wrong_voting_period_kind of {
        current : Voting_period.kind;
        expected : Voting_period.kind list;
      }
    | Source_not_in_vote_listings
    | (* Proposals errors *)
        Empty_proposals
    | Proposals_contain_duplicate of {proposal : Protocol_hash.t}
    | Already_proposed of {proposal : Protocol_hash.t}
    | Too_many_proposals of {previous_count : int; operation_count : int}
    | Conflicting_proposals of operation_conflict
    | Testnet_dictator_multiple_proposals
    | Proposals_from_unregistered_delegate of Signature.Public_key_hash.t
    | (* Ballot errors *)
        Ballot_for_wrong_proposal of {
        current : Protocol_hash.t;
        submitted : Protocol_hash.t;
      }
    | Already_submitted_a_ballot
    | Ballot_from_unregistered_delegate of Signature.Public_key_hash.t
    | Conflicting_ballot of operation_conflict

  let () =
    (* Shared voting errors *)
    register_error_kind
      `Temporary
      ~id:"validate.operation.wrong_voting_period_index"
      ~title:"Wrong voting period index"
      ~description:
        "The voting operation contains a voting period index different from \
         the current one."
      ~pp:(fun ppf (expected, provided) ->
        Format.fprintf
          ppf
          "The voting operation is meant for voting period %ld, whereas the \
           current period is %ld."
          provided
          expected)
      Data_encoding.(
        obj2 (req "current_index" int32) (req "provided_index" int32))
      (function
        | Wrong_voting_period_index {expected; provided} ->
            Some (expected, provided)
        | _ -> None)
      (fun (expected, provided) ->
        Wrong_voting_period_index {expected; provided}) ;
    register_error_kind
      `Temporary
      ~id:"validate.operation.wrong_voting_period_kind"
      ~title:"Wrong voting period kind"
      ~description:
        "The voting operation is incompatible the current voting period kind."
      ~pp:(fun ppf (current, expected) ->
        Format.fprintf
          ppf
          "The voting operation is only valid during a %a voting period, but \
           we are currently in a %a period."
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt " or ")
             Voting_period.pp_kind)
          expected
          Voting_period.pp_kind
          current)
      Data_encoding.(
        obj2
          (req "current" Voting_period.kind_encoding)
          (req "expected" (list Voting_period.kind_encoding)))
      (function
        | Wrong_voting_period_kind {current; expected} ->
            Some (current, expected)
        | _ -> None)
      (fun (current, expected) -> Wrong_voting_period_kind {current; expected}) ;
    let description = "The delegate is not in the vote listings." in
    register_error_kind
      `Temporary
      ~id:"validate.operation.source_not_in_vote_listings"
      ~title:"Source not in vote listings"
      ~description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
      Data_encoding.empty
      (function Source_not_in_vote_listings -> Some () | _ -> None)
      (fun () -> Source_not_in_vote_listings) ;

    (* Proposals errors *)
    let description = "Proposal list cannot be empty." in
    register_error_kind
      `Permanent
      ~id:"validate.operation.empty_proposals"
      ~title:"Empty proposals"
      ~description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
      Data_encoding.empty
      (function Empty_proposals -> Some () | _ -> None)
      (fun () -> Empty_proposals) ;
    register_error_kind
      `Permanent
      ~id:"validate.operation.proposals_contain_duplicate"
      ~title:"Proposals contain duplicate"
      ~description:"The list of proposals contains a duplicate element."
      ~pp:(fun ppf proposal ->
        Format.fprintf
          ppf
          "The list of proposals contains multiple occurrences of the proposal \
           %a."
          Protocol_hash.pp
          proposal)
      Data_encoding.(obj1 (req "proposal" Protocol_hash.encoding))
      (function
        | Proposals_contain_duplicate {proposal} -> Some proposal | _ -> None)
      (fun proposal -> Proposals_contain_duplicate {proposal}) ;
    register_error_kind
      `Branch
      ~id:"validate.operation.already_proposed"
      ~title:"Already proposed"
      ~description:
        "The delegate has already submitted one of the operation's proposals."
      ~pp:(fun ppf proposal ->
        Format.fprintf
          ppf
          "The delegate has already submitted the proposal %a."
          Protocol_hash.pp
          proposal)
      Data_encoding.(obj1 (req "proposal" Protocol_hash.encoding))
      (function Already_proposed {proposal} -> Some proposal | _ -> None)
      (fun proposal -> Already_proposed {proposal}) ;
    register_error_kind
      `Temporary
      ~id:"validate.operation.conflict_too_many_proposals"
      ~title:"Conflict too many proposals"
      ~description:
        "The delegate exceeded the maximum number of allowed proposals due to, \
         among others, previous Proposals operations in the current \
         block/mempool."
      ~pp:(fun ppf (previous_count, operation_count) ->
        Format.fprintf
          ppf
          "The delegate cannot submit too many protocol proposals: it \
           currently voted for %d and is trying to vote for %d more."
          previous_count
          operation_count)
      Data_encoding.(
        obj2 (req "previous_count" int8) (req "operation_count" int31))
      (function
        | Too_many_proposals {previous_count; operation_count} ->
            Some (previous_count, operation_count)
        | _ -> None)
      (fun (previous_count, operation_count) ->
        Too_many_proposals {previous_count; operation_count}) ;
    register_error_kind
      `Temporary
      ~id:"validate.operation.conflicting_proposals"
      ~title:"Conflicting proposals"
      ~description:
        "The current block/mempool already contains a testnest dictator \
         proposals operation, so it cannot have any other voting operation."
      ~pp:(fun ppf (Operation_conflict {existing; _}) ->
        Format.fprintf
          ppf
          "The current block/mempool already contains a conflicting operation \
           %a."
          Operation_hash.pp
          existing)
      Data_encoding.(obj1 (req "conflict" operation_conflict_encoding))
      (function Conflicting_proposals conflict -> Some conflict | _ -> None)
      (fun conflict -> Conflicting_proposals conflict) ;
    let description =
      "A testnet dictator cannot submit more than one proposal at a time."
    in
    register_error_kind
      `Permanent
      ~id:"validate.operation.testnet_dictator_multiple_proposals"
      ~title:"Testnet dictator multiple proposals"
      ~description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
      Data_encoding.empty
      (function Testnet_dictator_multiple_proposals -> Some () | _ -> None)
      (fun () -> Testnet_dictator_multiple_proposals) ;
    register_error_kind
      `Permanent
      ~id:"operation.proposals_from_unregistered_delegate"
      ~title:"Proposals from an unregistered delegate"
      ~description:"Cannot submit proposals with an unregistered delegate."
      ~pp:(fun ppf c ->
        Format.fprintf
          ppf
          "Cannot submit proposals with public key hash %a (unregistered \
           delegate)."
          Signature.Public_key_hash.pp
          c)
      Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
      (function Proposals_from_unregistered_delegate c -> Some c | _ -> None)
      (fun c -> Proposals_from_unregistered_delegate c) ;

    (* Ballot errors *)
    register_error_kind
      `Branch
      ~id:"validate.operation.ballot_for_wrong_proposal"
      ~title:"Ballot for wrong proposal"
      ~description:"Ballot provided for a proposal that is not the current one."
      ~pp:(fun ppf (current, submitted) ->
        Format.fprintf
          ppf
          "Ballot provided for proposal %a whereas the current proposal is %a."
          Protocol_hash.pp
          submitted
          Protocol_hash.pp
          current)
      Data_encoding.(
        obj2
          (req "current_proposal" Protocol_hash.encoding)
          (req "ballot_proposal" Protocol_hash.encoding))
      (function
        | Ballot_for_wrong_proposal {current; submitted} ->
            Some (current, submitted)
        | _ -> None)
      (fun (current, submitted) ->
        Ballot_for_wrong_proposal {current; submitted}) ;
    let description =
      "The delegate has already submitted a ballot for the current voting \
       period."
    in
    register_error_kind
      `Branch
      ~id:"validate.operation.already_submitted_a_ballot"
      ~title:"Already submitted a ballot"
      ~description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
      Data_encoding.empty
      (function Already_submitted_a_ballot -> Some () | _ -> None)
      (fun () -> Already_submitted_a_ballot) ;
    register_error_kind
      `Permanent
      ~id:"operation.ballot_from_unregistered_delegate"
      ~title:"Ballot from an unregistered delegate"
      ~description:"Cannot cast a ballot for an unregistered delegate."
      ~pp:(fun ppf c ->
        Format.fprintf
          ppf
          "Cannot cast a ballot for public key hash %a (unregistered delegate)."
          Signature.Public_key_hash.pp
          c)
      Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
      (function Ballot_from_unregistered_delegate c -> Some c | _ -> None)
      (fun c -> Ballot_from_unregistered_delegate c) ;
    register_error_kind
      `Temporary
      ~id:"validate.operation.conflicting_ballot"
      ~title:"Conflicting ballot"
      ~description:
        "The delegate has already submitted a ballot in a previously validated \
         operation of the current block or mempool."
      ~pp:(fun ppf (Operation_conflict {existing; _}) ->
        Format.fprintf
          ppf
          "The delegate has already submitted a ballot in the previously \
           validated operation %a of the current block or mempool."
          Operation_hash.pp
          existing)
      Data_encoding.(obj1 (req "conflict" operation_conflict_encoding))
      (function Conflicting_ballot conflict -> Some conflict | _ -> None)
      (fun conflict -> Conflicting_ballot conflict)
end

module Anonymous = struct
  type error +=
    | Invalid_activation of {pkh : Ed25519.Public_key_hash.t}
    | Conflicting_activation of {
        edpkh : Ed25519.Public_key_hash.t;
        conflict : operation_conflict;
      }

  let () =
    register_error_kind
      `Permanent
      ~id:"validate.operation.invalid_activation"
      ~title:"Invalid activation"
      ~description:
        "The given key and secret do not correspond to any existing \
         preallocated contract."
      ~pp:(fun ppf pkh ->
        Format.fprintf
          ppf
          "Invalid activation. The public key %a and accompanying secret do \
           not match any commitment."
          Ed25519.Public_key_hash.pp
          pkh)
      Data_encoding.(obj1 (req "pkh" Ed25519.Public_key_hash.encoding))
      (function Invalid_activation {pkh} -> Some pkh | _ -> None)
      (fun pkh -> Invalid_activation {pkh}) ;
    register_error_kind
      `Branch
      ~id:"validate.operation.conflicting_activation"
      ~title:"Account already activated in current validation_state"
      ~description:
        "The account has already been activated by a previous operation in the \
         current validation state."
      ~pp:(fun ppf (edpkh, Operation_conflict {existing; _}) ->
        Format.fprintf
          ppf
          "Invalid activation: the account %a has already been activated in \
           the current validation state by operation %a."
          Ed25519.Public_key_hash.pp
          edpkh
          Operation_hash.pp
          existing)
      Data_encoding.(
        obj2
          (req "edpkh" Ed25519.Public_key_hash.encoding)
          (req "conflict" operation_conflict_encoding))
      (function
        | Conflicting_activation {edpkh; conflict} -> Some (edpkh, conflict)
        | _ -> None)
      (fun (edpkh, conflict) -> Conflicting_activation {edpkh; conflict})

  type denunciation_kind = Preendorsement | Endorsement | Block

  let denunciation_kind_encoding =
    let open Data_encoding in
    string_enum
      [
        ("preendorsement", Preendorsement);
        ("endorsement", Endorsement);
        ("block", Block);
      ]

  let pp_denunciation_kind fmt : denunciation_kind -> unit = function
    | Preendorsement -> Format.fprintf fmt "preendorsement"
    | Endorsement -> Format.fprintf fmt "endorsement"
    | Block -> Format.fprintf fmt "block"

  type error +=
    | Invalid_double_baking_evidence of {
        hash1 : Block_hash.t;
        level1 : Raw_level.t;
        round1 : Round.t;
        hash2 : Block_hash.t;
        level2 : Raw_level.t;
        round2 : Round.t;
      }
    | Invalid_denunciation of denunciation_kind
    | Inconsistent_denunciation of {
        kind : denunciation_kind;
        delegate1 : Signature.Public_key_hash.t;
        delegate2 : Signature.Public_key_hash.t;
      }
    | Already_denounced of {
        kind : denunciation_kind;
        delegate : Signature.Public_key_hash.t;
        level : Level.t;
      }
    | Conflicting_denunciation of {
        kind : denunciation_kind;
        conflict : operation_conflict;
      }
    | Too_early_denunciation of {
        kind : denunciation_kind;
        level : Raw_level.t;
        current : Raw_level.t;
      }
    | Outdated_denunciation of {
        kind : denunciation_kind;
        level : Raw_level.t;
        last_cycle : Cycle.t;
      }

  let () =
    register_error_kind
      `Permanent
      ~id:"validate.block.invalid_double_baking_evidence"
      ~title:"Invalid double baking evidence"
      ~description:
        "A double-baking evidence is inconsistent (two distinct levels)"
      ~pp:(fun ppf (hash1, level1, round1, hash2, level2, round2) ->
        Format.fprintf
          ppf
          "Invalid double-baking evidence (hash: %a and %a, levels/rounds: \
           (%ld,%ld) and (%ld,%ld))"
          Block_hash.pp
          hash1
          Block_hash.pp
          hash2
          (Raw_level.to_int32 level1)
          (Round.to_int32 round1)
          (Raw_level.to_int32 level2)
          (Round.to_int32 round2))
      Data_encoding.(
        obj6
          (req "hash1" Block_hash.encoding)
          (req "level1" Raw_level.encoding)
          (req "round1" Round.encoding)
          (req "hash2" Block_hash.encoding)
          (req "level2" Raw_level.encoding)
          (req "round2" Round.encoding))
      (function
        | Invalid_double_baking_evidence
            {hash1; level1; round1; hash2; level2; round2} ->
            Some (hash1, level1, round1, hash2, level2, round2)
        | _ -> None)
      (fun (hash1, level1, round1, hash2, level2, round2) ->
        Invalid_double_baking_evidence
          {hash1; level1; round1; hash2; level2; round2}) ;
    register_error_kind
      `Permanent
      ~id:"validate.operation.block.invalid_denunciation"
      ~title:"Invalid denunciation"
      ~description:"A denunciation is malformed"
      ~pp:(fun ppf kind ->
        Format.fprintf
          ppf
          "Malformed double-%a evidence"
          pp_denunciation_kind
          kind)
      Data_encoding.(obj1 (req "kind" denunciation_kind_encoding))
      (function Invalid_denunciation kind -> Some kind | _ -> None)
      (fun kind -> Invalid_denunciation kind) ;
    register_error_kind
      `Permanent
      ~id:"validate.operation.block.inconsistent_denunciation"
      ~title:"Inconsistent denunciation"
      ~description:
        "A denunciation operation is inconsistent (two distinct delegates)"
      ~pp:(fun ppf (kind, delegate1, delegate2) ->
        Format.fprintf
          ppf
          "Inconsistent double-%a evidence (distinct delegate: %a and %a)"
          pp_denunciation_kind
          kind
          Signature.Public_key_hash.pp_short
          delegate1
          Signature.Public_key_hash.pp_short
          delegate2)
      Data_encoding.(
        obj3
          (req "kind" denunciation_kind_encoding)
          (req "delegate1" Signature.Public_key_hash.encoding)
          (req "delegate2" Signature.Public_key_hash.encoding))
      (function
        | Inconsistent_denunciation {kind; delegate1; delegate2} ->
            Some (kind, delegate1, delegate2)
        | _ -> None)
      (fun (kind, delegate1, delegate2) ->
        Inconsistent_denunciation {kind; delegate1; delegate2}) ;
    register_error_kind
      `Branch
      ~id:"validate.operation.already_denounced"
      ~title:"Already denounced"
      ~description:"The same denunciation has already been validated."
      ~pp:(fun ppf (kind, delegate, level) ->
        Format.fprintf
          ppf
          "Delegate %a at level %a has already been denounced for a double %a."
          pp_denunciation_kind
          kind
          Signature.Public_key_hash.pp
          delegate
          Level.pp
          level)
      Data_encoding.(
        obj3
          (req "denunciation_kind" denunciation_kind_encoding)
          (req "delegate" Signature.Public_key_hash.encoding)
          (req "level" Level.encoding))
      (function
        | Already_denounced {kind; delegate; level} ->
            Some (kind, delegate, level)
        | _ -> None)
      (fun (kind, delegate, level) -> Already_denounced {kind; delegate; level}) ;
    register_error_kind
      `Branch
      ~id:"validate.operation.conflicting_denunciation"
      ~title:"Conflicting denunciation in current validation state"
      ~description:
        "The same denunciation has already been validated in the current \
         validation state."
      ~pp:(fun ppf (kind, Operation_conflict {existing; _}) ->
        Format.fprintf
          ppf
          "Double %a evidence already exists in the current validation state \
           as operation %a."
          pp_denunciation_kind
          kind
          Operation_hash.pp
          existing)
      Data_encoding.(
        obj2
          (req "denunciation_kind" denunciation_kind_encoding)
          (req "conflict" operation_conflict_encoding))
      (function
        | Conflicting_denunciation {kind; conflict} -> Some (kind, conflict)
        | _ -> None)
      (fun (kind, conflict) -> Conflicting_denunciation {kind; conflict}) ;
    register_error_kind
      `Temporary
      ~id:"validate.operation.block.too_early_denunciation"
      ~title:"Too early denunciation"
      ~description:"A denunciation is too far in the future"
      ~pp:(fun ppf (kind, level, current) ->
        Format.fprintf
          ppf
          "A double-%a denunciation is too far in the future (current level: \
           %a, given level: %a)"
          pp_denunciation_kind
          kind
          Raw_level.pp
          current
          Raw_level.pp
          level)
      Data_encoding.(
        obj3
          (req "kind" denunciation_kind_encoding)
          (req "level" Raw_level.encoding)
          (req "current" Raw_level.encoding))
      (function
        | Too_early_denunciation {kind; level; current} ->
            Some (kind, level, current)
        | _ -> None)
      (fun (kind, level, current) ->
        Too_early_denunciation {kind; level; current}) ;
    register_error_kind
      `Permanent
      ~id:"validate.operation.block.outdated_denunciation"
      ~title:"Outdated denunciation"
      ~description:"A denunciation is outdated."
      ~pp:(fun ppf (kind, level, last_cycle) ->
        Format.fprintf
          ppf
          "A double-%a denunciation is outdated (last acceptable cycle: %a, \
           given level: %a)."
          pp_denunciation_kind
          kind
          Cycle.pp
          last_cycle
          Raw_level.pp
          level)
      Data_encoding.(
        obj3
          (req "kind" denunciation_kind_encoding)
          (req "level" Raw_level.encoding)
          (req "last" Cycle.encoding))
      (function
        | Outdated_denunciation {kind; level; last_cycle} ->
            Some (kind, level, last_cycle)
        | _ -> None)
      (fun (kind, level, last_cycle) ->
        Outdated_denunciation {kind; level; last_cycle})

  type error += Conflicting_nonce_revelation of operation_conflict

  let () =
    register_error_kind
      `Branch
      ~id:"validate.operation.conflicting_nonce_revelation"
      ~title:"Conflicting nonce revelation in the current validation state)."
      ~description:
        "A revelation for the same nonce has already been validated for the \
         current validation state."
      ~pp:(fun ppf (Operation_conflict {existing; _}) ->
        Format.fprintf
          ppf
          "This nonce revelation is conflicting with an existing revelation %a"
          Operation_hash.pp
          existing)
      Data_encoding.(obj1 (req "conflict" operation_conflict_encoding))
      (function
        | Conflicting_nonce_revelation conflict -> Some conflict | _ -> None)
      (fun conflict -> Conflicting_nonce_revelation conflict)

  type error += Conflicting_vdf_revelation of operation_conflict

  let () =
    register_error_kind
      `Branch
      ~id:"validate.operation.conflicting_vdf_revelation"
      ~title:"Conflicting vdf revelation in the current validation state)."
      ~description:
        "A revelation for the same vdf has already been validated for the \
         current validation state."
      ~pp:(fun ppf (Operation_conflict {existing; _}) ->
        Format.fprintf
          ppf
          "This vdf revelation is conflicting with an existing revelation %a"
          Operation_hash.pp
          existing)
      Data_encoding.(obj1 (req "conflict" operation_conflict_encoding))
      (function
        | Conflicting_vdf_revelation conflict -> Some conflict | _ -> None)
      (fun conflict -> Conflicting_vdf_revelation conflict)

  type error +=
    | Drain_delegate_on_unregistered_delegate of Signature.Public_key_hash.t
    | Invalid_drain_delegate_inactive_key of {
        delegate : Signature.Public_key_hash.t;
        consensus_key : Signature.Public_key_hash.t;
        active_consensus_key : Signature.Public_key_hash.t;
      }
    | Invalid_drain_delegate_no_consensus_key of Signature.Public_key_hash.t
    | Invalid_drain_delegate_noop of Signature.Public_key_hash.t
    | Invalid_drain_delegate_insufficient_funds_for_burn_or_fees of {
        delegate : Signature.Public_key_hash.t;
        destination : Signature.Public_key_hash.t;
        min_amount : Tez.t;
      }
    | Conflicting_drain_delegate of {
        delegate : Signature.Public_key_hash.t;
        conflict : operation_conflict;
      }

  let () =
    register_error_kind
      `Temporary
      ~id:"operation.drain_delegate_key_on_unregistered_delegate"
      ~title:"Drain delegate key on an unregistered delegate"
      ~description:"Cannot drain an unregistered delegate."
      ~pp:(fun ppf c ->
        Format.fprintf
          ppf
          "Cannot drain an unregistered delegate %a."
          Signature.Public_key_hash.pp
          c)
      Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
      (function
        | Drain_delegate_on_unregistered_delegate c -> Some c | _ -> None)
      (fun c -> Drain_delegate_on_unregistered_delegate c) ;
    register_error_kind
      `Temporary
      ~id:"operation.invalid_drain.inactive_key"
      ~title:"Drain delegate with an inactive consensus key"
      ~description:"Cannot drain with an inactive consensus key."
      ~pp:(fun ppf (delegate, consensus_key, active_consensus_key) ->
        Format.fprintf
          ppf
          "Consensus key %a is not the active consensus key for delegate %a. \
           The active consensus key is %a."
          Signature.Public_key_hash.pp
          consensus_key
          Signature.Public_key_hash.pp
          delegate
          Signature.Public_key_hash.pp
          active_consensus_key)
      Data_encoding.(
        obj3
          (req "delegate" Signature.Public_key_hash.encoding)
          (req "consensus_key" Signature.Public_key_hash.encoding)
          (req "active_consensus_key" Signature.Public_key_hash.encoding))
      (function
        | Invalid_drain_delegate_inactive_key
            {delegate; consensus_key; active_consensus_key} ->
            Some (delegate, consensus_key, active_consensus_key)
        | _ -> None)
      (fun (delegate, consensus_key, active_consensus_key) ->
        Invalid_drain_delegate_inactive_key
          {delegate; consensus_key; active_consensus_key}) ;
    register_error_kind
      `Temporary
      ~id:"operation.invalid_drain.no_consensus_key"
      ~title:"Drain a delegate without consensus key"
      ~description:"Cannot drain a delegate without consensus key."
      ~pp:(fun ppf delegate ->
        Format.fprintf
          ppf
          "There is no active consensus key for delegate %a."
          Signature.Public_key_hash.pp
          delegate)
      Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
      (function
        | Invalid_drain_delegate_no_consensus_key c -> Some c | _ -> None)
      (fun c -> Invalid_drain_delegate_no_consensus_key c) ;
    register_error_kind
      `Temporary
      ~id:"operation.invalid_drain.noop"
      ~title:"Invalid drain delegate: noop"
      ~description:"Cannot drain a delegate to itself."
      ~pp:(fun ppf delegate ->
        Format.fprintf
          ppf
          "The destination of a drain operation cannot be the delegate itself \
           (%a)."
          Signature.Public_key_hash.pp
          delegate)
      Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
      (function Invalid_drain_delegate_noop c -> Some c | _ -> None)
      (fun c -> Invalid_drain_delegate_noop c) ;
    register_error_kind
      `Temporary
      ~id:"operation.invalid_drain.insufficient_funds_for_burn_or_fees"
      ~title:
        "Drain delegate without enough balance for allocation burn or drain \
         fees"
      ~description:"Cannot drain without enough allocation burn and drain fees."
      ~pp:(fun ppf (delegate, destination, min_amount) ->
        Format.fprintf
          ppf
          "Cannot drain delegate from %a to %a: not enough funds for the drain \
           fees in the delegate account (minimum balance required: %a)."
          Signature.Public_key_hash.pp
          delegate
          Signature.Public_key_hash.pp
          destination
          Tez.pp
          min_amount)
      Data_encoding.(
        obj3
          (req "delegate" Signature.Public_key_hash.encoding)
          (req "destination" Signature.Public_key_hash.encoding)
          (req "min_amount" Tez.encoding))
      (function
        | Invalid_drain_delegate_insufficient_funds_for_burn_or_fees
            {delegate; destination; min_amount} ->
            Some (delegate, destination, min_amount)
        | _ -> None)
      (fun (delegate, destination, min_amount) ->
        Invalid_drain_delegate_insufficient_funds_for_burn_or_fees
          {delegate; destination; min_amount}) ;
    register_error_kind
      `Branch
      ~id:"validate.operation.conflicting_drain"
      ~title:"Conflicting drain in the current validation state)."
      ~description:
        "A manager operation or another drain operation is in conflict."
      ~pp:(fun ppf (delegate, Operation_conflict {existing; _}) ->
        Format.fprintf
          ppf
          "This drain operation conflicts with operation %a for the delegate %a"
          Operation_hash.pp
          existing
          Signature.Public_key_hash.pp
          delegate)
      Data_encoding.(
        obj2
          (req "delegate" Signature.Public_key_hash.encoding)
          (req "conflict" operation_conflict_encoding))
      (function
        | Conflicting_drain_delegate {delegate; conflict} ->
            Some (delegate, conflict)
        | _ -> None)
      (fun (delegate, conflict) ->
        Conflicting_drain_delegate {delegate; conflict})
end

module Manager = struct
  type error +=
    | Manager_restriction of {
        source : Signature.Public_key_hash.t;
        conflict : operation_conflict;
      }
    | Inconsistent_sources
    | Inconsistent_counters
    | Incorrect_reveal_position
    | Insufficient_gas_for_manager
    | Gas_quota_exceeded_init_deserialize
    | Tx_rollup_feature_disabled
    | Sc_rollup_feature_disabled
    | Sc_rollup_arith_pvm_disabled
    | Zk_rollup_feature_disabled

  let () =
    register_error_kind
      `Temporary
      ~id:"validate.operation.manager_restriction"
      ~title:"Manager restriction"
      ~description:
        "An operation with the same manager has already been validated in the \
         current block."
      ~pp:(fun ppf (source, Operation_conflict {existing; _}) ->
        Format.fprintf
          ppf
          "Manager %a already has the operation %a in the current block."
          Signature.Public_key_hash.pp
          source
          Operation_hash.pp
          existing)
      Data_encoding.(
        obj2
          (req "source" Signature.Public_key_hash.encoding)
          (req "conflict" operation_conflict_encoding))
      (function
        | Manager_restriction {source; conflict} -> Some (source, conflict)
        | _ -> None)
      (fun (source, conflict) -> Manager_restriction {source; conflict}) ;
    let inconsistent_sources_description =
      "The operation batch includes operations from different sources."
    in
    register_error_kind
      `Permanent
      ~id:"validate.operation.inconsistent_sources"
      ~title:"Inconsistent sources in operation batch"
      ~description:inconsistent_sources_description
      ~pp:(fun ppf () ->
        Format.fprintf ppf "%s" inconsistent_sources_description)
      Data_encoding.empty
      (function Inconsistent_sources -> Some () | _ -> None)
      (fun () -> Inconsistent_sources) ;
    let inconsistent_counters_description =
      "Inconsistent counters in operation. Counters of an operation must be \
       successive."
    in
    register_error_kind
      `Permanent
      ~id:"validate.operation.inconsistent_counters"
      ~title:"Inconsistent counters in operation"
      ~description:inconsistent_counters_description
      ~pp:(fun ppf () ->
        Format.fprintf ppf "%s" inconsistent_counters_description)
      Data_encoding.empty
      (function Inconsistent_counters -> Some () | _ -> None)
      (fun () -> Inconsistent_counters) ;
    let incorrect_reveal_description =
      "Incorrect reveal operation position in batch: only allowed in first \
       position."
    in
    register_error_kind
      `Permanent
      ~id:"validate.operation.incorrect_reveal_position"
      ~title:"Incorrect reveal position"
      ~description:incorrect_reveal_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" incorrect_reveal_description)
      Data_encoding.empty
      (function Incorrect_reveal_position -> Some () | _ -> None)
      (fun () -> Incorrect_reveal_position) ;
    register_error_kind
      `Permanent
      ~id:"validate.operation.insufficient_gas_for_manager"
      ~title:"Not enough gas for initial manager cost"
      ~description:
        (Format.asprintf
           "Gas limit is too low to cover the initial cost of manager \
            operations: a minimum of %a gas units is required."
           Gas.pp_cost_as_gas
           Michelson_v1_gas.Cost_of.manager_operation)
      Data_encoding.empty
      (function Insufficient_gas_for_manager -> Some () | _ -> None)
      (fun () -> Insufficient_gas_for_manager) ;
    let gas_deserialize_description =
      "Gas limit was not high enough to deserialize the transaction parameters \
       or origination script code or initial storage etc., making the \
       operation impossible to parse within the provided gas bounds."
    in
    register_error_kind
      `Permanent
      ~id:"validate.operation.gas_quota_exceeded_init_deserialize"
      ~title:"Not enough gas for initial deserialization of script expressions"
      ~description:gas_deserialize_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" gas_deserialize_description)
      Data_encoding.empty
      (function Gas_quota_exceeded_init_deserialize -> Some () | _ -> None)
      (fun () -> Gas_quota_exceeded_init_deserialize) ;
    register_error_kind
      `Permanent
      ~id:"validate.operation.tx_rollup_is_disabled"
      ~title:"Tx rollup is disabled"
      ~description:"Cannot originate a tx rollup as it is disabled."
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "Cannot apply a tx rollup operation as it is disabled. This feature \
           will be enabled in a future proposal")
      Data_encoding.unit
      (function Tx_rollup_feature_disabled -> Some () | _ -> None)
      (fun () -> Tx_rollup_feature_disabled) ;
    let scoru_disabled_description = "Smart rollups are disabled." in
    register_error_kind
      `Permanent
      ~id:"validate.operation.smart_rollup_disabled"
      ~title:"Smart rollups are disabled"
      ~description:scoru_disabled_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" scoru_disabled_description)
      Data_encoding.unit
      (function Sc_rollup_feature_disabled -> Some () | _ -> None)
      (fun () -> Sc_rollup_feature_disabled) ;

    let scoru_arith_pvm_disabled_description =
      "Arith PVM is disabled in this network."
    in
    register_error_kind
      `Permanent
      ~id:"operation.arith_pvm_disabled"
      ~title:"The Arith PVM is disabled"
      ~description:scoru_arith_pvm_disabled_description
      ~pp:(fun ppf () ->
        Format.fprintf ppf "%s" scoru_arith_pvm_disabled_description)
      Data_encoding.unit
      (function Sc_rollup_arith_pvm_disabled -> Some () | _ -> None)
      (fun () -> Sc_rollup_arith_pvm_disabled) ;
    let zkru_disabled_description =
      "ZK rollups will be enabled in a future proposal."
    in
    register_error_kind
      `Permanent
      ~id:"validate.operation.zk_rollup_disabled"
      ~title:"ZK rollups are disabled"
      ~description:zkru_disabled_description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" zkru_disabled_description)
      Data_encoding.unit
      (function Zk_rollup_feature_disabled -> Some () | _ -> None)
      (fun () -> Zk_rollup_feature_disabled)
end

type error += Failing_noop_error

let () =
  let description = "A failing_noop operation can never be validated." in
  register_error_kind
    `Permanent
    ~id:"validate.operation.failing_noop_error"
    ~title:"Failing_noop error"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Failing_noop_error -> Some () | _ -> None)
    (fun () -> Failing_noop_error)

module Block = struct
  type error +=
    | Not_enough_endorsements of {required : int; provided : int}
    | Inconsistent_validation_passes_in_block of {
        expected : int;
        provided : int;
      }

  let () =
    register_error_kind
      `Permanent
      ~id:"validate.block.not_enough_endorsements"
      ~title:"Not enough endorsements"
      ~description:
        "The block being validated does not include the required minimum \
         number of endorsements."
      ~pp:(fun ppf (required, provided) ->
        Format.fprintf
          ppf
          "Wrong number of endorsements (%i), at least %i are expected"
          provided
          required)
      Data_encoding.(obj2 (req "required" int31) (req "provided" int31))
      (function
        | Not_enough_endorsements {required; provided} ->
            Some (required, provided)
        | _ -> None)
      (fun (required, provided) -> Not_enough_endorsements {required; provided}) ;
    register_error_kind
      `Permanent
      ~id:"validate.block.inconsistent_validation_passes_in_block"
      ~title:"Inconsistent validation passes in block"
      ~description:
        "Validation of operation should be ordered by their validation passes \
         in a block."
      ~pp:(fun ppf (expected, provided) ->
        Format.fprintf
          ppf
          "Validation of operation should be ordered by their validation \
           passes in a block. Got an operation with validation pass: %d while \
           the last validated operation had the validation pass %d."
          provided
          expected)
      Data_encoding.(obj2 (req "expected" int31) (req "provided" int31))
      (function
        | Inconsistent_validation_passes_in_block {expected; provided} ->
            Some (expected, provided)
        | _ -> None)
      (fun (expected, provided) ->
        Inconsistent_validation_passes_in_block {expected; provided})
end
