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

(** Some of the argument of voting state are specific to the mainnet version
    (the opposite is also true). 
    With this type, we don't have to duplicate all the types for ghostnet and
    mainnet. *)
type ('a, 'b) network = Mainnet of 'a | Ghostnet of 'b | Missing

type 'a mainnet = ('a, unit) network

type 'a ghostnet = (unit, 'a) network

(** [apply_mainnet f] apply the function [f] only if the argument is meant for mainnet. *)
val apply_mainnet : ('a -> unit) -> 'a mainnet -> unit

(** Configuration of a governance contract. *)
type config = {
  started_at_level : Z.t;
  period_length : Z.t;
  adoption_period_sec : Z.t;
  upvoting_limit : Z.t;
  allowed_proposer : string list ghostnet; (* address list *)
  scale : Z.t;
  proposal_quorum : Z.t;
  promotion_quorum : Z.t;
  promotion_supermajority : Z.t;
}

type period_type = Proposal | Promotion

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

(** Storage structure.
    NB: It only contains config as it's the only field in the storage that
    is relevant to us (for now). *)
type storage = {config : config}

type finished_voting = {
  finished_at : (period, Z.t) network;
  proposal : proposal_period ghostnet;
  promotion : promotion_period option ghostnet;
  winner_proposal_payload : proposal option;
}

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

(** [decode_voting_state] decode the voting state returned by the view of a
    contract. *)
val decode_voting_state : Micheline_parser.node -> voting_state tzresult

(** [decode_storage micheline] decode the storage of a smart contract *)
val decode_storage : Micheline_parser.node -> storage tzresult
