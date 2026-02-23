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
open Baking_state

let section = Baking_events_section.section

let pp_int32 fmt n = Format.fprintf fmt "%ld" n

let pp_int64 fmt n = Format.fprintf fmt "%Ld" n

let waiting_color = Internal_event.Magenta

module Commands = struct
  include Internal_event.Simple

  let section = section @ ["commands"]

  let no_dal_node_provided =
    declare_0
      ~section
      ~name:"no_dal_node_provided"
      ~level:Warning
      ~msg:
        "No DAL node endpoint has been provided.\n\
         Not running a DAL node might result in losing a share of the \
         participation rewards.\n\
         For instructions on how to run a DAL node, please visit \
         https://docs.tezos.com/tutorials/join-dal-baker."
      ()

  let healthy_dal_node =
    declare_0
      ~section
      ~name:"healthy_dal_node"
      ~level:Notice
      ~msg:"The DAL node is healthy."
      ()

  let unhealthy_dal_node =
    declare_2
      ~section
      ~name:"unhealthy_dal_node"
      ~level:Error
      ~msg:
        "The DAL node running on {endpoint} is not healthy. DAL attestations \
         cannot be sent. Its health is {health}. Please check that your DAL \
         node is configured correctly."
      ~pp1:Uri.pp
      ("endpoint", Tezos_rpc.Encoding.uri_encoding)
      ~pp2:Tezos_dal_node_services.Types.Health.pp
      ("health", Tezos_dal_node_services.Types.Health.encoding)

  let unreachable_dal_node =
    declare_1
      ~section
      ~name:"unreachable_dal_node"
      ~level:Error
      ~msg:
        "The DAL node cannot be reached on endpoint: {endpoint}.\n\
         Please check your DAL node and possibly restart it."
      ~pp1:Uri.pp
      ("endpoint", Tezos_rpc.Encoding.uri_encoding)

  let deprecated_proto_specific_baker =
    declare_0
      ~section
      ~name:"deprecated_proto_specific_baker"
      ~level:Warning
      ~msg:
        "DEPRECATED: The `octez-baker-<protocol>` binaries are deprecated, and \
         will be removed in the next major version of Octez. Please use \
         `octez-baker` instead, which automatically handles protocol switches."
      ()

  let deprecated_proto_specific_accuser =
    declare_0
      ~section
      ~name:"deprecated_proto_specific_accuser"
      ~level:Warning
      ~msg:
        "DEPRECATED: The `octez-accuser-<protocol>` binaries are deprecated, \
         and will be removed in the next major version of Octez. Please use \
         `octez-accuser` instead, which automatically handles protocol \
         switches."
      ()

  let deprecated_adaptive_issuance_vote =
    declare_0
      ~section
      ~name:"deprecated_adaptive_issuance_vote"
      ~level:Warning
      ~msg:
        "DEPRECATED ARGUMENT: The 'adaptive-issuance-vote' argument \
         (placeholder 'vote') is deprecated. It is already ignored by the \
         baker, and will be removed in the next major version of Octez."
      ()

  let warn_if_adaptive_issuance_vote_present ~adaptive_issuance_vote =
    if Option.is_some adaptive_issuance_vote then
      emit deprecated_adaptive_issuance_vote ()
    else Lwt.return_unit
end

module State_transitions = struct
  include Internal_event.Simple

  let section = section @ ["transitions"]

  let new_valid_proposal =
    declare_3
      ~section
      ~name:"new_valid_proposal"
      ~level:Notice
      ~msg:"received new proposal {block} at level {level}, round {round}"
      ~pp1:Block_hash.pp
      ("block", Block_hash.encoding)
      ~pp2:pp_int32
      ("level", Data_encoding.int32)
      ~pp3:Round.pp
      ("round", Round.encoding)

  let new_head =
    declare_3
      ~section
      ~name:"new_head"
      ~level:Notice
      ~msg:"received new head {block} at level {level}, round {round}"
      ~pp1:Block_hash.pp
      ("block", Block_hash.encoding)
      ~pp2:pp_int32
      ("level", Data_encoding.int32)
      ~pp3:Round.pp
      ("round", Round.encoding)

  let new_head_with_increasing_level =
    declare_0
      ~section
      ~name:"new_head_with_increasing_level"
      ~level:Info
      ~msg:"received new head with level increasing"
      ()

  let new_forge_event =
    declare_1
      ~section
      ~name:"new_forge_event"
      ~level:Info
      ~msg:"received new forge event: {event}"
      ~pp1:pp_forge_event
      ("event", forge_event_encoding_for_logging__cannot_decode)

  let no_proposal_slot =
    declare_3
      ~section
      ~name:"no_proposal_slot"
      ~level:Info
      ~msg:
        "end of round {current_round}; no proposal slot at level {level}, \
         round {next_round}"
      ~pp1:Round.pp
      ("current_round", Round.encoding)
      ~pp2:pp_int32
      ("level", Data_encoding.int32)
      ~pp3:Round.pp
      ("next_round", Round.encoding)

  let proposal_slot =
    declare_4
      ~section
      ~name:"proposal_slot"
      ~level:Info
      ~msg:
        "end of round {current_round}; proposal slot at level {level}, round \
         {next_round} for {delegate}"
      ~pp1:Round.pp
      ("current_round", Round.encoding)
      ~pp2:pp_int32
      ("level", Data_encoding.int32)
      ~pp3:Round.pp
      ("next_round", Round.encoding)
      ~pp4:Delegate.pp_without_companion_key
      ("delegate", Delegate.encoding_for_logging__cannot_decode)

  let new_head_while_waiting_for_qc =
    declare_0
      ~section
      ~name:"new_head_while_waiting_for_qc"
      ~level:Info
      ~msg:"received new head while waiting for a quorum"
      ()

  let applied_expected_proposal_received =
    declare_1
      ~section
      ~name:"applied_expected_proposal_received"
      ~level:Info
      ~msg:"received the expected application notice for {proposal}"
      ~pp1:Block_hash.pp
      ("proposal", Block_hash.encoding)

  let unexpected_new_head_while_waiting_for_application =
    declare_0
      ~section
      ~name:"unexpected_new_head_while_waiting_for_application"
      ~level:Info
      ~msg:"received new head while waiting for another proposal's application"
      ()

  let new_valid_proposal_while_waiting_for_qc =
    declare_0
      ~section
      ~name:"new_valid_proposal_while_waiting_for_qc"
      ~level:Info
      ~msg:"received new valid proposal while waiting for a quorum"
      ()

  let valid_proposal_received_after_application =
    declare_0
      ~section
      ~name:"valid_proposal_received_after_application"
      ~level:Info
      ~msg:"received valid proposal for a block already applied"
      ()

  let unexpected_proposal_round =
    declare_2
      ~section
      ~name:"unexpected_proposal_round"
      ~level:Info
      ~msg:
        "unexpected proposal round, expected: {expected_round}, got: \
         {proposal_round}"
      ~pp1:Round.pp
      ("expected_round", Round.encoding)
      ~pp2:Round.pp
      ("proposal_round", Round.encoding)

  let proposal_for_round_already_seen =
    declare_3
      ~section
      ~name:"proposal_for_round_already_seen"
      ~level:Warning
      ~msg:
        "proposal {new_proposal} for current round ({current_round}) has \
         already been seen {previous_proposal}"
      ~pp1:Block_hash.pp
      ("new_proposal", Block_hash.encoding)
      ~pp2:Round.pp
      ("current_round", Round.encoding)
      ~pp3:Block_hash.pp
      ("previous_proposal", Block_hash.encoding)

  let updating_latest_proposal =
    declare_1
      ~section
      ~name:"updating_latest_proposal"
      ~msg:"updating latest proposal to {block_hash}"
      ~level:Info
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)

  let baker_is_ahead_of_node =
    declare_2
      ~section
      ~name:"baker_is_ahead"
      ~level:Info
      ~msg:
        "baker (level: {baker_level}) is ahead of the node (level: \
         {node_level})"
      ~pp1:pp_int32
      ("baker_level", Data_encoding.int32)
      ~pp2:pp_int32
      ("node_level", Data_encoding.int32)

  let new_proposal_is_on_another_branch =
    declare_2
      ~section
      ~name:"new_proposal_is_on_another_branch"
      ~level:Info
      ~msg:
        "received a proposal on another branch - current: current \
         pred{current_branch}, new pred {new_branch}"
      ~pp1:Block_hash.pp
      ("current_branch", Block_hash.encoding)
      ~pp2:Block_hash.pp
      ("new_branch", Block_hash.encoding)

  let switching_branch =
    declare_0
      ~section
      ~name:"switching_branch"
      ~level:Info
      ~msg:"switching branch"
      ()

  let branch_proposal_has_better_fitness =
    declare_0
      ~section
      ~name:"branch_proposal_has_better_fitness"
      ~level:Info
      ~msg:"different branch proposal has a better fitness than us"
      ()

  let branch_proposal_has_no_prequorum =
    declare_0
      ~section
      ~name:"branch_proposal_has_no_prequorum"
      ~level:Info
      ~msg:"different branch proposal has no prequorum but we do"
      ()

  let branch_proposal_has_lower_prequorum =
    declare_0
      ~section
      ~name:"branch_proposal_has_lower_prequorum"
      ~level:Info
      ~msg:"different branch proposal has a lower prequorum than us"
      ()

  let branch_proposal_has_better_prequorum =
    declare_0
      ~section
      ~name:"branch_proposal_has_better_prequorum"
      ~level:Info
      ~msg:"different branch proposal has a better prequorum"
      ()

  let branch_proposal_has_same_prequorum =
    declare_0
      ~section
      ~name:"branch_proposal_has_same_prequorum"
      ~level:Error
      ~msg:"different branch proposal has the same prequorum"
      ()

  let attempting_preattest_proposal =
    declare_1
      ~section
      ~name:"attempting_preattest_proposal"
      ~level:Info
      ~msg:"attempting to preattest proposal {block_hash}"
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)

  let attempting_vote_proposal =
    declare_1
      ~section
      ~name:"attempting_vote_proposal"
      ~level:Info
      ~msg:"attempting to vote for proposal {block_hash}"
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)

  let skipping_invalid_proposal =
    declare_0
      ~section
      ~name:"skipping_invalid_proposal"
      ~level:Info
      ~msg:"invalid proposal, skipping"
      ()

  let outdated_proposal =
    declare_1
      ~section
      ~name:"outdated_proposal"
      ~level:Debug
      ~msg:"outdated proposal {block_hash}"
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)

  let preparing_fresh_block =
    declare_2
      ~section
      ~name:"preparing_fresh_block"
      ~level:Info
      ~msg:"preparing fresh block at round {round}{delegate}"
      ~pp1:(fun fmt ->
        Format.fprintf
          fmt
          "@ for@ delegate@ %a"
          Delegate.pp_without_companion_key)
      ("delegate", Delegate.encoding_for_logging__cannot_decode)
      ~pp2:Round.pp
      ("round", Round.encoding)

  let no_attestable_payload_fresh_block =
    declare_0
      ~section
      ~name:"no_attestable_payload_fresh_block"
      ~level:Info
      ~msg:"no attestable payload, proposing fresh block"
      ()

  let repropose_block =
    declare_1
      ~section
      ~name:"repropose_block"
      ~level:Info
      ~msg:"repropose block with payload {payload}"
      ~pp1:Block_payload_hash.pp
      ("payload", Block_payload_hash.encoding)

  let unexpected_prequorum_received =
    declare_2
      ~section
      ~name:"unexpected_prequorum_received"
      ~level:Info
      ~msg:
        "unexpected prequorum received for {received_hash} instead of \
         {expected_hash}"
      ~pp1:Block_hash.pp
      ("received_hash", Block_hash.encoding)
      ~pp2:Block_hash.pp
      ("expected_hash", Block_hash.encoding)

  let unexpected_quorum_received =
    declare_2
      ~section
      ~name:"unexpected_quorum_received"
      ~level:Info
      ~msg:
        "unexpected quorum received for {received_hash} instead of \
         {expected_hash}"
      ~pp1:Block_hash.pp
      ("received_hash", Block_hash.encoding)
      ~pp2:Block_hash.pp
      ("expected_hash", Block_hash.encoding)

  let step_current_phase =
    declare_2
      ~section
      ~name:"step_current_phase"
      ~level:Debug
      ~msg:"automaton step: current phase {phase}, event {event}"
      ~pp1:pp_phase
      ("phase", phase_encoding)
      ~pp2:pp_event
      ("event", event_encoding_for_logging__cannot_decode)

  let discarding_attestation =
    declare_1
      ~section
      ~name:"discarding_attestation"
      ~level:Info
      ~msg:"discarding outdated {operation_information}"
      ~pp1:pp_signed_consensus_vote
      ( "operation_information",
        signed_consensus_vote_encoding_for_logging__cannot_decode )

  let discarding_unexpected_preattestation_with_different_payload =
    declare_3
      ~section
      ~name:"discarding_unexpected_preattestation_with_different_payload"
      ~level:Warning
      ~msg:
        "discarding {operation_information} with payload {operation_payload} \
         where the prequorum was locked on a different payload \
         {state_payload}."
      ~pp1:pp_signed_consensus_vote
      ( "operation_information",
        signed_consensus_vote_encoding_for_logging__cannot_decode )
      ~pp2:Block_payload_hash.pp
      ("operation_payload", Block_payload_hash.encoding)
      ~pp3:Block_payload_hash.pp
      ("state_payload", Block_payload_hash.encoding)

  let emit_discarding_unexpected_preattestation_with_different_payload
      signed_preattestation ~state_payload =
    let op_payload =
      signed_preattestation.unsigned_consensus_vote.vote_consensus_content
        .block_payload_hash
    in
    emit
      discarding_unexpected_preattestation_with_different_payload
      (signed_preattestation, op_payload, state_payload)

  let discarding_unexpected_attestation_not_matching_prequorum =
    declare_3
      ~section
      ~name:"discarding_unexpected_attestation_not_matching_prequorum"
      ~level:Warning
      ~msg:
        "discarding {operation_information} with payload {operation_payload} \
         where {prequorum_payload_if_any}."
      ~pp1:pp_signed_consensus_vote
      ( "operation_information",
        signed_consensus_vote_encoding_for_logging__cannot_decode )
      ~pp2:Block_payload_hash.pp
      ("operation_payload", Block_payload_hash.encoding)
      ~pp3:(fun fmt -> function
        | None -> Format.fprintf fmt "no prequorum was reached"
        | Some prequorum_payload ->
            Format.fprintf
              fmt
              "the prequorum was on a different payload %a"
              Block_payload_hash.pp
              prequorum_payload)
      ( "prequorum_payload_if_any",
        Data_encoding.option Block_payload_hash.encoding )

  let emit_discarding_unexpected_attestation_not_matching_prequorum
      signed_attestation ~attestable_payload_hash_opt =
    let attestation_payload =
      signed_attestation.unsigned_consensus_vote.vote_consensus_content
        .block_payload_hash
    in
    emit
      discarding_unexpected_attestation_not_matching_prequorum
      (signed_attestation, attestation_payload, attestable_payload_hash_opt)

  let discarding_unexpected_prequorum_reached =
    declare_2
      ~section
      ~name:"discarding_unexpected_prequorum_reached"
      ~level:Info
      ~msg:
        "discarding unexpected prequorum reached for {candidate} while in \
         {phase} phase."
      ~pp1:Block_hash.pp
      ("candidate", Block_hash.encoding)
      ~pp2:pp_phase
      ("phase", phase_encoding)

  let discarding_unexpected_quorum_reached =
    declare_2
      ~section
      ~name:"discarding_unexpected_quorum_reached"
      ~level:Info
      ~msg:
        "discarding unexpected quorum reached for {candidate} while in {phase} \
         phase."
      ~pp1:Block_hash.pp
      ("candidate", Block_hash.encoding)
      ~pp2:pp_phase
      ("phase", phase_encoding)
end

module Launch = struct
  include Internal_event.Simple

  let section = section @ ["launch"]

  let keys_used =
    declare_1
      ~section
      ~alternative_color:Internal_event.Cyan
      ~name:"keys_used"
      ~level:Notice
      ~msg:"Baker will run with the following keys:{keys}"
      ~pp1:(fun ppf keys ->
        Format.fprintf ppf "@[<v 2>@,%a@]" (Format.pp_print_list Key.pp) keys)
      ("keys", Data_encoding.list Key.encoding_for_logging__cannot_decode)
end

module Scheduling = struct
  include Internal_event.Simple

  let section = section @ ["scheduling"]

  let error_while_baking =
    declare_1
      ~section
      ~name:"error_while_baking"
      ~level:Warning
      ~msg:"error while baking {trace}"
      ~pp1:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let waiting_for_new_head =
    declare_0
      ~section
      ~alternative_color:waiting_color
      ~name:"waiting_for_new_head"
      ~level:Info
      ~msg:"no possible timeout, waiting for a new head to arrive..."
      ()

  let compute_next_timeout_elected_block =
    declare_2
      ~section
      ~name:"compute_next_timeout_elected_block"
      ~level:Debug
      ~msg:
        "found an elected block at level {level}, round {round}... checking \
         baking rights"
      ~pp1:pp_int32
      ("level", Data_encoding.int32)
      ~pp2:Round.pp
      ("round", Round.encoding)

  let next_potential_slot =
    declare_4
      ~section
      ~name:"next_potential_slot"
      ~level:Info
      ~msg:
        "next potential slot for level {level} is at round \
         {round}{timestamp}{delegate}"
      ~pp1:pp_int32
      ("level", Data_encoding.int32)
      ~pp2:Round.pp
      ("round", Round.encoding)
      ~pp3:(fun fmt -> Format.fprintf fmt "@ at %a" Timestamp.pp)
      ("timestamp", Timestamp.encoding)
      ~pp4:(fun fmt ->
        Format.fprintf
          fmt
          "@ for@ delegate@ %a"
          Delegate.pp_without_companion_key)
      ("delegate", Delegate.encoding_for_logging__cannot_decode)

  let waiting_end_of_round =
    declare_3
      ~section
      ~alternative_color:waiting_color
      ~name:"waiting_end_of_round"
      ~level:Info
      ~msg:"waiting {timespan} until end of round {round} at {timestamp}"
      ~pp1:Ptime.Span.pp
      ("timespan", Time.System.Span.encoding)
      ~pp2:pp_int32
      ("round", Data_encoding.int32)
      ~pp3:Timestamp.pp
      ("timestamp", Timestamp.encoding)

  let waiting_delayed_end_of_round =
    declare_4
      ~section
      ~alternative_color:waiting_color
      ~name:"waiting_delayed_end_of_round"
      ~level:Info
      ~msg:
        "waiting {timespan} until {timestamp} (end of round {round} plus \
         {delay}s delay)"
      ~pp1:Ptime.Span.pp
      ("timespan", Time.System.Span.encoding)
      ~pp2:pp_int32
      ("round", Data_encoding.int32)
      ~pp3:Timestamp.pp
      ("timestamp", Timestamp.encoding)
      ~pp4:pp_int64
      ("delay", Data_encoding.int64)

  let waiting_time_to_bake =
    declare_2
      ~section
      ~alternative_color:waiting_color
      ~name:"waiting_time_to_bake"
      ~level:Info
      ~msg:"waiting {timespan} until it's time to bake at {timestamp}"
      ~pp1:Ptime.Span.pp
      ("timespan", Time.System.Span.encoding)
      ~pp2:Timestamp.pp
      ("timestamp", Timestamp.encoding)

  let no_need_to_wait_for_proposal =
    declare_0
      ~section
      ~name:"no_need_to_wait_for_proposal"
      ~level:Info
      ~msg:"no need to wait to propose a block"
      ()

  let state_synchronized_to_round =
    declare_1
      ~section
      ~name:"state_synchronized_to_round"
      ~level:Debug
      ~msg:"state synchronized to round {round}"
      ~pp1:Round.pp
      ("round", Round.encoding)

  let proposal_in_the_future =
    declare_1
      ~section
      ~name:"proposal_in_the_future"
      ~level:Debug
      ~msg:"received proposal in the future {block_hash}"
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)

  let process_proposal_in_the_future =
    declare_1
      ~section
      ~name:"process_proposal_in_the_future"
      ~level:Debug
      ~msg:"process proposal received in the future with hash {block_hash}"
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)

  let waiting_to_forge_block =
    declare_2
      ~section
      ~alternative_color:waiting_color
      ~name:"waiting_to_forge_block"
      ~level:Info
      ~msg:"waiting {timespan} until it's time to forge block at {timestamp}"
      ~pp1:Ptime.Span.pp
      ("timespan", Time.System.Span.encoding)
      ~pp2:Timestamp.pp
      ("timestamp", Timestamp.encoding)

  let no_need_to_wait_to_forge_block =
    declare_0
      ~section
      ~name:"no_need_to_wait_to_forge_block"
      ~level:Info
      ~msg:"no need to wait to forge a block"
      ()

  let first_baker_of_next_level =
    declare_0
      ~section
      ~name:"first_baker_of_next_level"
      ~level:Info
      ~msg:
        "first baker of next level found among delegates. pre-emptively \
         forging block."
      ()

  let dal_node_no_attester_profile =
    declare_0
      ~section
      ~name:"dal_node_no_attester_profile"
      ~level:Warning
      ~msg:
        "The DAL node has no registered attester profile. It is recommended to \
         start the DAL node with '--attester-profiles <manager_key>'."
      ()
end

module Lib = struct
  include Internal_event.Simple

  let section = section @ ["lib"]

  let attempting_to_vote_for_proposal =
    declare_2
      ~section
      ~name:"attempting_preattest_proposal"
      ~level:Debug
      ~msg:"attempting to {action} proposal {proposal}"
      ("action", consensus_vote_kind_encoding)
      ~pp1:(fun fmt -> function
        | Preattestation -> Format.fprintf fmt "preattest"
        | Attestation -> Format.fprintf fmt "attest")
      ("proposal", proposal_encoding)
      ~pp2:pp_proposal

  let waiting_block_timestamp =
    declare_2
      ~section
      ~alternative_color:waiting_color
      ~name:"waiting_block_timestamp"
      ~level:Notice
      ~msg:"Waiting {diff_time} until block timestamp {timestamp}"
      ("timestamp", Time.Protocol.encoding)
      ("diff_time", Time.System.Span.encoding)
      ~pp1:Time.Protocol.pp_hum
      ~pp2:Time.System.Span.pp_hum
end

module Actions = struct
  include Internal_event.Simple

  let section = section @ ["actions"]

  let skipping_consensus_vote =
    declare_2
      ~section
      ~name:"skipping_consensus_vote"
      ~level:Error
      ~msg:"unable to sign {operation_information} -- {trace}"
      ~pp1:pp_unsigned_consensus_vote
      ( "operation_information",
        unsigned_consensus_vote_encoding_for_logging__cannot_decode )
      ~pp2:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let failed_to_inject_consensus_vote =
    declare_2
      ~section
      ~name:"failed_to_inject_consensus_vote"
      ~level:Error
      ~msg:"failed to inject {operation_information} -- {trace}"
      ~pp1:pp_signed_consensus_vote
      ( "operation_information",
        signed_consensus_vote_encoding_for_logging__cannot_decode )
      ~pp2:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let failed_to_forge_block =
    declare_2
      ~section
      ~name:"failed_to_forge_block"
      ~level:Error
      ~msg:"failed to forge block for {delegate} -- {trace}"
      ~pp1:Delegate.pp
      ("delegate", Delegate.encoding_for_logging__cannot_decode)
      ~pp2:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let potential_double_baking =
    declare_2
      ~section
      ~name:"potential_double_baking"
      ~level:Warning
      ~msg:"potential double baking detected at level {level}, round {round}"
      ~pp1:pp_int32
      ~pp2:Round.pp
      ("level", Data_encoding.int32)
      ("round", Round.encoding)

  let consensus_op_injected =
    declare_2
      ~section
      ~name:"consensus_operation_injected"
      ~level:Notice
      ~msg:"injected {operation_information}{operation_hash}"
      ~pp1:pp_signed_consensus_vote
      ( "operation_information",
        signed_consensus_vote_encoding_for_logging__cannot_decode )
      ~pp2:(fun fmt oph ->
        Format.fprintf fmt "@ (operation hash: %a)" Operation_hash.pp oph)
      ("operation_hash", Operation_hash.encoding)

  let attach_dal_attestation =
    declare_5
      ~section
      ~name:"attach_dal_attestation"
      ~level:Info
      ~msg:
        "ready to attach DAL attestation for level {attestation_level}, round \
         {round}, with bitset {bitset} for {delegate} to attest slots \
         published at levels {published_levels}"
      ("delegate", Delegate_id.encoding)
      ~pp2:pp_dal_content
      ("bitset", dal_content_encoding)
      ("published_levels", Data_encoding.(list int32))
      ("attestation_level", Data_encoding.int32)
      ("round", Round.encoding)

  let not_in_dal_committee =
    declare_2
      ~section
      ~name:"not_in_dal_committee"
      ~level:Notice
      ~msg:"{delegate} has no assigned DAL shards at level {attestation_level}"
      ("delegate", Delegate_id.encoding)
      ("attestation_level", Data_encoding.int32)

  let missing_companion_key_for_dal_with_bls =
    declare_2
      ~section
      ~name:"missing_companion_key_for_dal_with_bls"
      ~level:Warning
      ~msg:
        "Cannot issue an attestation with DAL because the BLS consensus key \
         has no corresponding companion key. Crafting the attestation without \
         DAL at level {attestation_level} for {delegate}"
      ("delegate", Delegate_id.encoding)
      ("attestation_level", Data_encoding.int32)

  let synchronizing_round =
    declare_1
      ~section
      ~name:"synchronizing_round"
      ~level:Info
      ~msg:"synchronizing round after block {block}"
      ~pp1:Block_hash.pp
      ("block", Block_hash.encoding)

  let prepare_forging_block =
    declare_3
      ~section
      ~name:"prepare_forging_block"
      ~level:Debug
      ~msg:
        "prepare forging block at level {level}, round {round} for {delegate}"
      ~pp1:pp_int32
      ~pp2:Round.pp
      ~pp3:Delegate.pp
      ("level", Data_encoding.int32)
      ("round", Round.encoding)
      ("delegate", Delegate.encoding_for_logging__cannot_decode)

  let forging_block =
    declare_4
      ~section
      ~name:"forging_block"
      ~level:Info
      ~msg:
        "forging block at level {level}, round {round}{delegate}(force apply: \
         {force_apply})"
      ~pp1:pp_int32
      ~pp2:Round.pp
      ~pp3:(fun fmt ->
        Format.fprintf
          fmt
          "@ for@ delegate@ %a@ "
          Delegate.pp_without_companion_key)
      ("level", Data_encoding.int32)
      ("round", Round.encoding)
      ("delegate", Delegate.encoding_for_logging__cannot_decode)
      ("force_apply", Data_encoding.bool)

  let delayed_block_injection =
    declare_4
      ~section
      ~name:"delayed_block_injection"
      ~level:Debug
      ~msg:
        "waiting {delay} before injecting block at level {level}, round \
         {round} for {delegate}"
      ("delay", Time.System.Span.encoding)
      ~pp1:Time.System.Span.pp_hum
      ("level", Data_encoding.int32)
      ~pp2:pp_int32
      ("round", Round.encoding)
      ~pp3:Round.pp
      ("delegate", Delegate.encoding_for_logging__cannot_decode)
      ~pp4:Delegate.pp

  let injecting_block =
    declare_3
      ~section
      ~name:"injecting_block"
      ~level:Debug
      ~msg:"injecting block at level {level}, round {round} for {delegate}"
      ~pp1:pp_int32
      ~pp2:Round.pp
      ~pp3:Delegate.pp
      ("level", Data_encoding.int32)
      ("round", Round.encoding)
      ("delegate", Delegate.encoding_for_logging__cannot_decode)

  let block_injected =
    declare_5
      ~alternative_color:Internal_event.Blue
      ~section
      ~name:"block_injected"
      ~level:Notice
      ~msg:
        "block {block} at level {level}, round {round} \
         injected{delegate}{manager_operations_infos}"
      ~pp1:Block_hash.pp
      ~pp2:pp_int32
      ~pp3:Round.pp
      ~pp4:(fun fmt ->
        Format.fprintf
          fmt
          "@ for@ delegate@ %a"
          Delegate.pp_without_companion_key)
      ~pp5:
        (Format.pp_print_option
           (fun fmt {manager_operation_number; total_fees} ->
             Format.fprintf
               fmt
               "@ with %d manager operations@ summing up to %a μtz in fees"
               manager_operation_number
               pp_int64
               total_fees))
      ("block", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("round", Round.encoding)
      ("delegate", Delegate.encoding_for_logging__cannot_decode)
      ( "manager_operations_infos",
        Data_encoding.option manager_operations_infos_encoding )

  let block_injection_failed =
    declare_2
      ~section
      ~name:"block_injection_failed"
      ~level:Error
      ~msg:"failed to inject block {block_hash} -- {trace}"
      ("block_hash", Block_hash.encoding)
      ~pp1:Block_hash.pp
      ("trace", Error_monad.trace_encoding)
      ~pp2:Error_monad.pp_print_trace

  let signing_consensus_op =
    declare_1
      ~section
      ~name:"signing_consensus_operation"
      ~level:Info
      ~msg:"signing {operation_information}"
      ~pp1:pp_unsigned_consensus_vote
      ( "operation_information",
        unsigned_consensus_vote_encoding_for_logging__cannot_decode )

  let signing_block =
    declare_3
      ~section
      ~name:"signing_block"
      ~level:Info
      ~msg:"signing block at level {level}, round {round} for {delegate}"
      ~pp1:pp_int32
      ~pp2:Round.pp
      ~pp3:Delegate.pp
      ("level", Data_encoding.int32)
      ("round", Round.encoding)
      ("delegate", Delegate.encoding_for_logging__cannot_decode)

  let invalid_json_file =
    declare_1
      ~section
      ~name:"invalid_json_file"
      ~level:Warning
      ~msg:"{filename} is not a valid JSON file"
      ("filename", Data_encoding.string)

  let no_operations_found_in_file =
    declare_1
      ~section
      ~name:"no_operations_found_in_file"
      ~level:Warning
      ~msg:"no operations found in file {filename}"
      ("filename", Data_encoding.string)

  let cannot_fetch_operations =
    declare_1
      ~section
      ~name:"cannot_fetch_operations"
      ~level:Error
      ~msg:"cannot fetch operations: {errs}"
      ("errs", Error_monad.(TzTrace.encoding error_encoding))

  let vote_for_liquidity_baking_toggle =
    declare_1
      ~section
      ~name:"vote_for_liquidity_baking_toggle"
      ~level:Notice
      ~msg:"Voting {value} for liquidity baking toggle vote"
      ( "value",
        Protocol.Alpha_context.Per_block_votes.liquidity_baking_vote_encoding )

  let signature_timeout =
    declare_1
      ~section
      ~name:"signature_timeout"
      ~level:Error
      ~msg:"Signature call reached a timeout of {timeout}"
      ("timeout", Data_encoding.float)

  let signature_error =
    declare_1
      ~section
      ~name:"signature_error"
      ~level:Error
      ~msg:"Signature call failed with {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let delegates_without_slots =
    declare_2
      ~section
      ~name:"delegates_without_slots"
      ~level:Notice
      ~msg:
        "The following delegates have no attesting rights at level {level}: \
         {delegates}"
      ~pp1:(Format.pp_print_list Key.pp)
      ("delegates", Data_encoding.list Key.encoding_for_logging__cannot_decode)
      ~pp2:pp_int32
      ("level", Data_encoding.int32)

  let no_dal_node_provided = Commands.no_dal_node_provided

  let unhealthy_dal_node = Commands.unhealthy_dal_node

  let unreachable_dal_node = Commands.unreachable_dal_node

  let stalling_signature =
    declare_4
      ~section
      ~name:"stalling_signature"
      ~level:Warning
      ~msg:
        "Signature call of {kind} for level {level} at round {round} has not \
         resolved in the last {seconds} seconds"
      ("kind", Baking_errors.signing_request_encoding)
      ("level", Data_encoding.int32)
      ("round", Round.encoding)
      ("seconds", Data_encoding.float)

  let stalling_forge_block =
    declare_3
      ~section
      ~name:"stalling_forge_block"
      ~level:Warning
      ~msg:
        "Forge block for level {level} at round {round} has not resolved in \
         the last {seconds} seconds"
      ("level", Data_encoding.int32)
      ("round", Round.encoding)
      ("seconds", Data_encoding.float)
end

module VDF = struct
  include Internal_event.Simple

  let section = section @ ["vdf"]

  let vdf_revelation_injected =
    declare_3
      ~section
      ~name:"vdf_revelation_injected"
      ~level:Notice
      ~msg:
        "Injected VDF revelation for cycle {cycle} (chain {chain} with \
         operation {ophash})"
      ~pp1:pp_int32
      ("cycle", Data_encoding.int32)
      ~pp2:Format.pp_print_string
      ("chain", Data_encoding.string)
      ~pp3:Operation_hash.pp
      ("ophash", Operation_hash.encoding)

  let vdf_daemon_error =
    declare_2
      ~section
      ~name:"vdf_daemon_error"
      ~level:Error
      ~msg:"{worker}: error while running VDF daemon: {errors}"
      ~pp1:Format.pp_print_string
      ("worker", Data_encoding.string)
      ~pp2:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let vdf_daemon_connection_lost =
    declare_1
      ~section
      ~name:"vdf_daemon_connection_lost"
      ~level:Error
      ~msg:"Connection to node lost, VDF daemon {worker} exiting"
      ~pp1:Format.pp_print_string
      ("worker", Data_encoding.string)

  let vdf_daemon_cannot_kill_computation =
    declare_1
      ~section
      ~name:"vdf_daemon_cannot_kill_computation"
      ~level:Error
      ~msg:"Error when killining running computation: {error}"
      ~pp1:Format.pp_print_string
      ("error", Data_encoding.string)

  let vdf_info =
    declare_1
      ~section
      ~name:"vdf_internal"
      ~level:Notice
      ~msg:"{msg}"
      ~pp1:Format.pp_print_string
      ("msg", Data_encoding.string)
end

module Nonces = struct
  include Internal_event.Simple

  let section = section @ ["nonces"]

  let found_nonce_to_reveal =
    declare_2
      ~section
      ~name:"found_nonce_to_reveal"
      ~level:Notice
      ~msg:"found nonce to reveal for block {block}, level {level}"
      ~pp1:Block_hash.pp
      ("block", Block_hash.encoding)
      ~pp2:Raw_level.pp
      ("level", Raw_level.encoding)

  let revealing_nonce =
    declare_3
      ~alternative_color:Internal_event.Cyan
      ~section
      ~name:"revealing_nonce"
      ~level:Notice
      ~msg:
        "revealing nonce of level {level} (chain {chain} with operation \
         {ophash})"
      ~pp1:pp_int32
      ("level", Data_encoding.int32)
      ~pp2:Format.pp_print_string
      ("chain", Data_encoding.string)
      ~pp3:Operation_hash.pp
      ("ophash", Operation_hash.encoding)

  let cannot_fetch_chain_head_level =
    declare_0
      ~section
      ~name:"cannot_fetch_chain_head_level"
      ~level:Error
      ~msg:"cannot fetch chain head level, aborting nonces filtering"
      ()

  let incoherent_nonce =
    declare_1
      ~section
      ~name:"incoherent_nonce"
      ~level:Error
      ~msg:"incoherent nonce for level {level}"
      ~pp1:(fun fmt -> Format.fprintf fmt "%a" Raw_level.pp)
      ("level", Raw_level.encoding)

  let cannot_read_nonces =
    declare_1
      ~section
      ~name:"cannot_read_nonces"
      ~level:Error
      ~msg:"cannot read nonces {trace}"
      ~pp1:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let cannot_retrieve_unrevealed_nonces =
    declare_1
      ~section
      ~name:"cannot_retrieve_unrevealed_nonces"
      ~level:Error
      ~msg:"cannot retrieve unrevealed nonces {trace}"
      ~pp1:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let cannot_inject_nonces =
    declare_1
      ~section
      ~name:"cannot_inject_nonces"
      ~level:Error
      ~msg:"cannot inject nonces {trace}"
      ~pp1:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let cant_retrieve_block_header_for_nonce =
    declare_2
      ~section
      ~name:"cant_retrieve_block_header_for_nonce"
      ~level:Warning
      ~msg:"cannot retrieve block header {header} associated with nonce {trace}"
      ("header", Data_encoding.string)
      ~pp2:Error_monad.pp_print_trace
      ("trace", Error_monad.trace_encoding)

  let registering_nonce =
    declare_1
      ~section
      ~name:"registering_nonce"
      ~level:Info
      ~msg:"registering nonce for block {block}"
      ~pp1:Block_hash.pp
      ("block", Block_hash.encoding)

  let nothing_to_reveal =
    declare_1
      ~section
      ~name:"nothing_to_reveal"
      ~level:Info
      ~msg:"nothing to reveal for block {block}"
      ~pp1:Block_hash.pp
      ("block", Block_hash.encoding)

  let revelation_worker_started =
    declare_0
      ~section
      ~name:"revelation_worker_started"
      ~level:Info
      ~msg:"revelation worker started"
      ()

  let success_migrate_nonces =
    declare_0
      ~section
      ~name:"success_migrate_nonces"
      ~level:Notice
      ~msg:"successfully migrated nonces: legacy nonces are safe to delete"
      ()

  let ignore_failed_nonce_migration =
    declare_3
      ~section
      ~name:"ignore_failed_nonce_migration"
      ~level:Warning
      ~msg:
        "Found orphaned nonces while migrating baking nonces to the new file \
         format. Please review the list of associated blocks. If the block is \
         older than the last cycle or if it was not included, the file at \
         '{legacy_nonces_file}' and '{orphaned_nonces_file}'should be archived \
         and then removed. If the block is in the current or last cycle, you \
         must start from a snapshot that is old enough to boostrap those \
         blocks to avoid losing some of your baking rewards. Blocks associated \
         with orphaned nonces:\n\
         {failed}"
      ~pp1:(Format.pp_print_list Block_hash.pp)
      ("failed", Data_encoding.list Block_hash.encoding)
      ("legacy_nonces_file", Data_encoding.string)
      ("orphaned_nonces_file", Data_encoding.string)

  let outdated_nonce =
    declare_1
      ~section
      ~name:"outdated_nonce"
      ~level:Info
      ~msg:"outdated nonce for block {block_hash} is safe to delete"
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)

  let unexpected_nonce =
    declare_1
      ~section
      ~name:"unexpected_nonce"
      ~level:Info
      ~msg:"unexpected nonce for block {block_hash} is safe to delete"
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)

  let revealed_nonce =
    declare_1
      ~section
      ~name:"revealed_nonce"
      ~level:Info
      ~msg:"revealed nonce for block {block_hash} is safe to delete"
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)

  let deterministic_nonce_timeout =
    declare_1
      ~section
      ~name:"deterministic_nonce_timeout"
      ~level:Error
      ~msg:
        "Call to generate a deterministic nonce reached a timeout of {timeout}"
      ("timeout", Data_encoding.float)

  let deterministic_nonce_error =
    declare_1
      ~section
      ~name:"deterministic_nonce_error"
      ~level:Error
      ~msg:"Call to deterministic nonce failed with {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))
end

module Per_block_votes = struct
  include Internal_event.Simple

  let reading_per_block_votes =
    declare_1
      ~section
      ~name:"reading_per_block_votes"
      ~level:Notice
      ~msg:"reading votes file: {path}"
      ("path", Data_encoding.string)

  let liquidity_baking_toggle_vote =
    declare_1
      ~section
      ~name:"read_liquidity_baking_toggle_vote"
      ~level:Notice
      ~msg:"read liquidity baking toggle vote = {value}"
      ( "value",
        Protocol.Alpha_context.Per_block_votes.liquidity_baking_vote_encoding )

  let per_block_vote_file_fail =
    declare_1
      ~section
      ~name:"per_block_vote_file_error"
      ~level:Error
      ~msg:"Error reading the block vote file: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))
end

module Selection = struct
  include Internal_event.Simple

  let section = section @ ["operation_selection"]

  let invalid_operation_filtered =
    declare_2
      ~section
      ~name:"invalid_operation_filtered"
      ~level:Warning
      ~msg:"filtered invalid operation {op}: {errors}"
      ~pp1:Operation_hash.pp
      ("op", Operation_hash.encoding)
      ~pp2:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let cannot_serialize_operation_metadata =
    declare_1
      ~section
      ~name:"cannot_serialize_operation_metadata"
      ~level:Warning
      ~msg:"cannot serialize operation {op} metadata"
      ~pp1:Operation_hash.pp
      ("op", Operation_hash.encoding)
end

module Forge_worker = struct
  include Internal_event.Simple

  let section = section @ ["forge_worker"]

  let error_while_processing_forge_request =
    declare_1
      ~section
      ~name:"error_while_processing_forge_request"
      ~level:Warning
      ~msg:"error while processing forge request: {errors}"
      ("errors", Error_monad.trace_encoding)
      ~pp1:pp_print_top_error_of_trace

  let error_while_authorizing_consensus_votes =
    declare_1
      ~section
      ~name:"error_while_authorizing_consensus_votes"
      ~level:Error
      ~msg:"error while authorizing consensus votes: {errors}"
      ("errors", Error_monad.(TzTrace.encoding error_encoding))
      ~pp1:pp_print_top_error_of_trace
end
