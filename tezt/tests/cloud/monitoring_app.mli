(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Monitoring helpers for scheduling reports and raising alerts. *)

(** Scheduled Reports via Chronos *)
module Tasks : sig
  (** [register_chronos_task ~cloud ~network endpoint] registers a recurring
      Chronos task that will gather the latest metrics, format them and send
      them to the configured Slack channel. *)
  val register_chronos_task :
    Cloud.t -> network:Network.t -> Endpoint.t Lwt.t -> unit
end

module Alert : sig
  (** [check_for_lost_dal_rewards ~cloud ~network ~metadata]
      examines Tezos block [metadata] for any lost DAL attesting rewards
      balance updates. If found, posts a Slack alert. *)
  val check_for_lost_dal_rewards :
    cloud:Cloud.t -> network:Network.t -> metadata:JSON.t -> unit Lwt.t

  (** [check_for_dal_accusations ~cloud ~network ~cycle ~level ~operations ~endpoint]
      scans the block’s consensus operations for any DAL entrapment evidence. If any
      are found, posts a Slack alert describing each accusation. *)
  val check_for_dal_accusations :
    cloud:Cloud.t ->
    network:[< Network.t] ->
    cycle:int ->
    level:int ->
    operations:JSON.t ->
    endpoint:Endpoint.t ->
    unit Lwt.t

  val check_for_recently_missed_a_lot :
    cloud:Cloud.t ->
    endpoint:Endpoint.t ->
    network:Network.t ->
    level:int ->
    metadata:JSON.t ->
    unit Lwt.t

  val report_funds_are_getting_short :
    cloud:Cloud.t ->
    network:Network.t ->
    fee:int ->
    pkh:string ->
    balance:int ->
    unit Lwt.t
end
