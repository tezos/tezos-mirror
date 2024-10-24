(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Micheline_helpers
open Tezos_micheline.Micheline

type proposal =
  | Sequencer of {sequencer_pk : string; pool_address : Bytes.t}
  | Kernel of Bytes.t

let decode_proposal_list =
  let open Result_syntax in
  function
  | [payload] ->
      let* payload = decode_bytes payload in
      Ok (Kernel payload)
  | [seq_pk; pool] ->
      let* sequencer_pk = decode_string seq_pk in
      let* pool_address = decode_bytes pool in
      Ok (Sequencer {sequencer_pk; pool_address})
  | _ -> error_with "DecodeProposalListError"

let decode_proposal micheline =
  let open Result_syntax in
  match micheline with
  | Prim _ ->
      (* If the node micheline is a prim it should be a pair *)
      decode_pair decode_proposal_list micheline
  | Bytes _ ->
      let* payload = decode_bytes micheline in
      Ok (Kernel payload)
  | _ -> error_with "DecodeProposal"

type period_type = Proposal | Promotion

let decode_period_type = function
  | Prim (_, "Left", [l], _) when l |> is_unit -> Ok Proposal
  | Prim (_, "Right", [r], _) when r |> is_unit -> Ok Promotion
  | _ -> error_with "DecodePeriodTypeError"

type period = {index : Z.t; type_ : period_type}

type proposal_object = {
  payload : bytes;
  proposer : string; (* address *)
  upvoted_proposal : (string * Z.t) list;
}

type proposal_period = {
  proposals : (bytes * proposal_object) list;
  total_voting_power : Z.t;
}

type promotion_period = {
  payload : bytes;
  votes : (string * (string * Z.t)) list;
  total_voting_power : Z.t;
}

type ('a, 'b) network = Mainnet of 'a | Ghostnet of 'b | Missing

type 'a mainnet = ('a, unit) network

type 'a ghostnet = (unit, 'a) network

let apply_mainnet f = function Mainnet a -> f a | _ -> ()

type finished_voting = {
  finished_at : (period, Z.t) network;
  proposal : proposal_period ghostnet;
  promotion : promotion_period option ghostnet;
  winner_proposal_payload : proposal option;
}

let decode_proposal_object proposal_object =
  let open Result_syntax in
  let decode = function
    | [payload; proposer; upvoted_proposal] ->
        let* payload = decode_bytes payload in
        let* proposer = decode_string proposer in
        let* upvoted_proposal =
          decode_map
            ~decode_key:decode_string
            ~decode_elt:decode_nat
            upvoted_proposal
        in
        Ok {payload; proposer; upvoted_proposal}
    | _ -> error_with "DecodeProposalObjectError"
  in
  decode_pair decode proposal_object

let decode_proposal_period proposal_period =
  let open Result_syntax in
  let decode = function
    | [proposals; total_voting_power] ->
        let* proposals =
          decode_map
            ~decode_key:decode_bytes
            ~decode_elt:decode_proposal_object
            proposals
        in
        let* total_voting_power = decode_nat total_voting_power in
        Ok {proposals; total_voting_power}
    | _ -> error_with "DecodeProposalsPeriod"
  in
  decode_pair decode proposal_period

let decode_vote vote =
  let open Result_syntax in
  let decode = function
    | [vote; voting_power] ->
        let* vote = decode_string vote in
        let* voting_power = decode_nat voting_power in
        Ok (vote, voting_power)
    | _ -> error_with "DecodeVote"
  in
  decode_pair decode vote

let decode_promotion_period promotion_period =
  let open Result_syntax in
  let decode = function
    | [payload; votes; total_voting_power] ->
        let* payload = decode_bytes payload in
        let* votes =
          decode_map ~decode_key:decode_string ~decode_elt:decode_vote votes
        in
        let* total_voting_power = decode_nat total_voting_power in
        Ok {payload; votes; total_voting_power}
    | _ -> error_with "DecodePromotionPeriod"
  in
  decode_pair decode promotion_period

let decode_finish_vote finished_voting =
  let open Result_syntax in
  let decode = function
    | [period_index; period_type; winner_proposal] ->
        let* index = decode_nat period_index in
        let* type_ = decode_period_type period_type in
        let* winner_proposal_payload =
          decode_option decode_proposal winner_proposal
        in
        let period = {index; type_} in
        Ok
          {
            finished_at = Mainnet period;
            proposal = Missing;
            promotion = Missing;
            winner_proposal_payload;
          }
    | [period_index; proposal_period; promotion_period; winner_proposal] ->
        let* index = decode_nat period_index in
        let* winner_proposal_payload =
          decode_option decode_proposal winner_proposal
        in
        let* proposal_period = decode_proposal_period proposal_period in
        let* promotion_period =
          decode_option decode_promotion_period promotion_period
        in
        Ok
          {
            finished_at = Ghostnet index;
            proposal = Ghostnet proposal_period;
            promotion = Ghostnet promotion_period;
            winner_proposal_payload;
          }
    | _ -> error_with "DecodeFinishVoteError"
  in
  decode_pair decode finished_voting

type voting_context = {
  current_period : period;
  proposal_period : proposal_period ghostnet;
  promotion_period : promotion_period option ghostnet;
  last_winner_payload : bytes option ghostnet;
  remaining_blocks : Z.t mainnet;
}

type voting_state = {
  voting_context : voting_context;
  finished_voting : finished_voting option;
}

let decode_voting_context voting_context =
  let open Result_syntax in
  let decode = function
    | [period_index; period_type; remaining_blocks] ->
        let* index = decode_nat period_index in
        let* type_ = decode_period_type period_type in
        let* remaining_blocks = decode_nat remaining_blocks in
        let current_period = {index; type_} in
        Ok
          {
            current_period;
            proposal_period = Missing;
            promotion_period = Missing;
            last_winner_payload = Missing;
            remaining_blocks = Mainnet remaining_blocks;
          }
    | [
        period_index;
        period_type;
        proposal_period;
        promotion_period;
        last_winner_payload;
      ] ->
        let* index = decode_nat period_index in
        let* type_ = decode_period_type period_type in
        let* proposal_period = decode_proposal_period proposal_period in
        let* promotion_period =
          decode_option decode_promotion_period promotion_period
        in
        let* last_winner_payload =
          decode_option decode_bytes last_winner_payload
        in
        let current_period = {index; type_} in
        Ok
          {
            current_period;
            proposal_period = Ghostnet proposal_period;
            promotion_period = Ghostnet promotion_period;
            last_winner_payload = Ghostnet last_winner_payload;
            remaining_blocks = Missing;
          }
    | l -> error_with "DecodeVotingContextError %d" (List.length l)
  in
  decode_pair decode voting_context

let decode_voting_state voting_state =
  let open Result_syntax in
  let decode = function
    | [period_index; period_type; remaining_blocks; finish_vote] ->
        let* index = decode_nat period_index in
        let* type_ = decode_period_type period_type in
        let* remaining_blocks = decode_nat remaining_blocks in
        let* finished_voting = decode_option decode_finish_vote finish_vote in
        let current_period = {index; type_} in
        let voting_context =
          {
            current_period;
            proposal_period = Missing;
            promotion_period = Missing;
            last_winner_payload = Missing;
            remaining_blocks = Mainnet remaining_blocks;
          }
        in
        Ok {voting_context; finished_voting}
    | [voting_context; finish_vote] ->
        let* voting_context = decode_voting_context voting_context in
        let* finished_voting = decode_option decode_finish_vote finish_vote in
        Ok {voting_context; finished_voting}
    | _ -> error_with "DecodeVotingStateError"
  in
  decode_pair decode voting_state

type config = {
  started_at_level : Z.t;
  period_length : Z.t;
  adoption_period_sec : Z.t;
  upvoting_limit : Z.t;
  allowed_proposer : string list ghostnet;
  scale : Z.t;
  proposal_quorum : Z.t;
  promotion_quorum : Z.t;
  promotion_supermajority : Z.t;
}

let decode_config micheline =
  let open Result_syntax in
  let aux_decode_config = function
    | [
        started_at_level;
        period_length;
        adoption_period_sec;
        upvoting_limit;
        scale;
        proposal_quorum;
        promotion_quorum;
        promotion_supermajority;
      ] ->
        let* started_at_level = decode_nat started_at_level in
        let* period_length = decode_nat period_length in
        let* adoption_period_sec = decode_nat adoption_period_sec in
        let* upvoting_limit = decode_nat upvoting_limit in
        let* scale = decode_nat scale in
        let* proposal_quorum = decode_nat proposal_quorum in
        let* promotion_quorum = decode_nat promotion_quorum in
        let* promotion_supermajority = decode_nat promotion_supermajority in
        Ok
          {
            started_at_level;
            period_length;
            adoption_period_sec;
            upvoting_limit;
            allowed_proposer = Missing;
            scale;
            proposal_quorum;
            promotion_quorum;
            promotion_supermajority;
          }
    | [
        started_at_level;
        period_length;
        adoption_period_sec;
        upvoting_limit;
        allowed_proposer;
        scale;
        proposal_quorum;
        promotion_quorum;
        promotion_supermajority;
      ] ->
        let* started_at_level = decode_nat started_at_level in
        let* period_length = decode_nat period_length in
        let* adoption_period_sec = decode_nat adoption_period_sec in
        let* upvoting_limit = decode_nat upvoting_limit in
        let* allowed_proposer = decode_set decode_string allowed_proposer in
        let* scale = decode_nat scale in
        let* proposal_quorum = decode_nat proposal_quorum in
        let* promotion_quorum = decode_nat promotion_quorum in
        let* promotion_supermajority = decode_nat promotion_supermajority in
        Ok
          {
            started_at_level;
            period_length;
            adoption_period_sec;
            upvoting_limit;
            allowed_proposer = Ghostnet allowed_proposer;
            scale;
            proposal_quorum;
            promotion_quorum;
            promotion_supermajority;
          }
    | _ -> error_with "DecodeConfigError"
  in
  decode_pair aux_decode_config micheline

type storage = {config : config}

let decode_storage micheline =
  let open Result_syntax in
  let aux_decode_storage = function
    | config :: _ ->
        let* config = decode_config config in
        Ok {config}
    | _ -> error_with "DecodeStorageError"
  in
  decode_pair aux_decode_storage micheline
