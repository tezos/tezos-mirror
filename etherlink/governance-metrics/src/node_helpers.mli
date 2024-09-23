(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type sequencer_proposal_args = {sequencer_pk : string; pool_address : string}

type sequencer_proposal_args_opt = {
  sequencer_pk : string option;
  pool_address : string option;
}

type sequencer_entrypoints_parameters =
  | Trigger_committee_upgrade of string
  | Vote of string
  | Upvote_proposal of sequencer_proposal_args
  | New_proposal of sequencer_proposal_args

type kernel_entrypoint_parameters =
  | Trigger_kernel_upgrade of string
  | Vote of string
  | Upvote_proposal of string
  | New_proposal of string

type entrypoint_parameters =
  | Sequencer of sequencer_entrypoints_parameters
  | Kernel of kernel_entrypoint_parameters
  | Security_kernel of kernel_entrypoint_parameters

type contract_parameters = {value : entrypoint_parameters}

type operation_result = {status : string}

type contract_transaction = {source : string; parameters : contract_parameters}

type header = {level : int}

module RPC : sig
  (** [chain_id base] returns the L1 chain identifier. *)
  val chain_id : Uri.t -> string tzresult Lwt.t

  (** [micheline_view ~chain_id ~contract ~view ~level ~decode base] returns the decoded
      view of a smart contract. *)
  val micheline_view :
    chain_id:string ->
    contract:string ->
    view:string ->
    level:int ->
    decode:(Tezos_micheline.Micheline_parser.node -> 'a tzresult) ->
    Uri.t ->
    'a tzresult Lwt.t

  (** [storage ~contract ~decode base] returns the decoded storage of a smart
      contract. *)
  val storage :
    contract:string ->
    decode:(Tezos_micheline.Micheline_parser.node -> 'a tzresult) ->
    Uri.t ->
    'a tzresult Lwt.t

  (** [monitor_heads ~process base] will monitor each head (streamed chunk) and
      apply [process] on the received chunk. *)
  val monitor_heads :
    process:(header -> unit tzresult Lwt.t) -> Uri.t -> unit tzresult Lwt.t

  (** [governance_operations ~hash base] will get the (potential) governance operations
      from a block by its [hash]. *)
  val governance_operations :
    config:Configuration.configuration ->
    level:string ->
    contract_transaction trace tzresult Lwt.t
end
