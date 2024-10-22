(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [start_server ()] will start a new metric server based on the configuration
    given when executing the program. *)
val start_server : config:Configuration.configuration -> unit -> unit Lwt.t

(** [set_finalized_l1_level value] will set the metric related to the finalized L1
    level with [value]. *)
val set_finalized_l1_level : int -> unit

type governance_contract = Sequencer | Kernel | Security_kernel

(** Helper to provide the actual contract address associated to the governance
    contract type based on the configuration given when executing the program. *)
val governance_to_contract :
  config:Configuration.configuration -> governance_contract -> string

(** Helper to provide the string representation of the governance contract type. *)
val governance_to_string : governance_contract -> string

module GovernanceMetrics : sig
  module Storage : sig
    (** [set_configuration ~config contract] will set the configuration metrics
        based on [config] and the [contract] type. *)
    val set_configuration :
      config:Contract_type.config -> governance_contract -> unit

    (** [set_remaining_blocks contract value] will set the remaining blocks of the
        current period of the [contract] type to [value]. *)
    val set_remaining_blocks : governance_contract -> Z.t -> unit

    (** [set_current_period_type contract value] will set the current period type
        period of the [contract] type to [value]. *)
    val set_current_period_type : governance_contract -> int -> unit

    (** [set_current_period_index contract value] will set the current period index
        period of the [contract] type to [value]. *)
    val set_current_period_index : governance_contract -> Z.t -> unit
  end

  (** The [Entrypoints] module defines the API to use the metrics related to the governance contract call.
      All the governance contracts use the same model:
      - first an upgrade is proposed, and a first voting period decides which proposal is to be considered,
      - then there is a second voting period to determine if the chosen proposal is accepted,
      - finally the upgrade is triggered.
      
      A metric is created for each of those three phases. Voting an trigger upgrade are gauges, 
      but all the information is in the labels rather than the value of the gauge (which will always
      be set to `1.0`).
      Example: 
      When a proposal for a new kernel [0xUp1..] is made by [tz1Alice..], a new time series is created for the 
      corresponding metrics: `kernel_proposal{source="tz1Alice..", proposal="0xUp1.."}`. An upvote by another
      address [tz1Bob..] would be another time series `kernel_proposal{source="tz1Bob..", proposal="0xUp1.."}`.
      Similarly, votes would be times series `kernel_vote{source="tz1Alice..", value="yay"}` and 
      `kernel_vote{source="tz1Bob..", value="pass"}`, etc.
  *)
  module Entrypoints : sig
    (** [vote ~source ~value contract] registers the vote of [source] as [value] (yea|nay|pass)
        for the targeted governance [contract]. *)
    val vote : source:string -> value:string -> governance_contract -> unit

    (** [trigger_upgrade ~source ~address contract] registers that [source] triggered an 
        upgrade on the rollup [address] for [contract]. *)
    val trigger_upgrade :
      source:string -> address:string -> governance_contract -> unit

    (** [proposal ~source ?sequencer_pk ?pool_address ?proposal contract] registers a proposal 
        (or upvote) made by [source] on [contract]. If [contract] is a sequencer governance 
        contract, then [sequencer_pk] and [pool_address] are used as the proposal value, else 
        [proposal] is used. *)
    val proposal :
      source:string ->
      ?sequencer_pk:string ->
      ?pool_address:string ->
      ?proposal:string ->
      governance_contract ->
      unit

    (** [clear_vote contract] will clear every voting metrics targetting
        [contract]. *)
    val clear_vote : governance_contract -> unit

    (** [clear_proposal contract] will clear every proposal metrics
        targetting [contract]. *)
    val clear_proposal : governance_contract -> unit
  end
end
