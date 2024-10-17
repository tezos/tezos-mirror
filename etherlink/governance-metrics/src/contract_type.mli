(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_micheline

(** Depending on the governance (kernel or sequencer) a proposal can be
    two different micheline object. *)
type proposal =
  | Sequencer of {sequencer_pk : string; pool_address : Bytes.t}
  | Kernel of Bytes.t

(** Configuration of a governance contract. *)
type config = {
  started_at_level : Z.t;
  period_length : Z.t;
  adoption_period_sec : Z.t;
  upvoting_limit : Z.t;
  scale : Z.t;
  proposal_quorum : Z.t;
  promotion_quorum : Z.t;
  promotion_supermajority : Z.t;
}

type period_type = Proposal | Promotion

type period = {index : Z.t; type_ : period_type}

(** Storage structure.
    NB: It only contains config as it's the only field in the storage that
    is relevant to us (for now). *)
type storage = {config : config}

type finish_vote = {
  finished_at : period;
  winner_proposal_payload : proposal option;
}

type voting_state = {
  current_period : period;
  remaining_blocks : Z.t;
  finished_voting : finish_vote option;
}

(** [decode_voting_state] decode the voting state returned by the view of a
    contract. *)
val decode_voting_state : Micheline_parser.node -> voting_state tzresult

(** [decode_storage micheline] decode the storage of a smart contract *)
val decode_storage : Micheline_parser.node -> storage tzresult
