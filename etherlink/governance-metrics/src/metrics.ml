(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Prometheus
open Configuration

let etherlink_governance_registry = CollectorRegistry.create ()

let namespace = "etherlink"

let subsystem = "governance"

let v_labels_counter =
  Counter.v_labels ~registry:etherlink_governance_registry ~namespace ~subsystem

let v_labels_gauge =
  Gauge.v_labels ~registry:etherlink_governance_registry ~namespace ~subsystem

let v_gauge ?(label_names = []) ~help name =
  v_labels_gauge ~help ~label_names name

let set_counter t ?(labels = []) () = ignore @@ Counter.labels t labels

let set_gauge t ?(labels = []) f x =
  let m = Gauge.labels t labels in
  Gauge.set m @@ f x

let incr_counter t ?(labels = []) () =
  let m = Counter.labels t labels in
  Counter.inc_one m

module Cohttp (Server : Cohttp_lwt.S.Server) = struct
  let callback _conn req _body =
    let open Cohttp in
    let open Lwt_syntax in
    let uri = Request.uri req in
    match (Request.meth req, Uri.path uri) with
    | `GET, "/metrics" ->
        let* data_sc =
          CollectorRegistry.(collect etherlink_governance_registry)
        in
        let body =
          Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data_sc
        in
        let headers =
          Header.init_with "Content-Type" "text/plain; version=0.0.4"
        in
        Server.respond_string ~status:`OK ~headers ~body ()
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()
end

module Metrics_server = Cohttp (Cohttp_lwt_unix.Server)

let start_server ~config () =
  let open Lwt_result_syntax in
  let*! ctx = Conduit_lwt_unix.init ~src:config.prometheus.metrics_addr () in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  let mode = `TCP (`Port config.prometheus.metrics_port) in
  let callback = Metrics_server.callback in
  let*! () = Event.starting_metrics_server () in
  Cohttp_lwt_unix.Server.create
    ~ctx
    ~mode
    (Cohttp_lwt_unix.Server.make ~callback ())

let finalized_l1_level_register =
  v_gauge ~help:"Current L1 Level" "current_l1_level"

let set_finalized_l1_level = set_gauge finalized_l1_level_register Int.to_float

type governance_contract = Sequencer | Kernel | Security_kernel

let governance_to_contract ~config = function
  | Sequencer -> config.contracts.sequencer_governance
  | Kernel -> config.contracts.kernel_governance
  | Security_kernel -> config.contracts.security_kernel_governance

let governance_to_string = function
  | Sequencer -> "sequencer"
  | Kernel -> "kernel"
  | Security_kernel -> "security kernel"

let get_helper_txt ~helper = function
  | Sequencer -> "Sequencer: " ^ helper
  | Kernel -> "Kernel: " ^ helper
  | Security_kernel -> "Security_kernel: " ^ helper

let get_metric_txt ~metric = function
  | Sequencer -> "sequencer_" ^ metric
  | Kernel -> "kernel_" ^ metric
  | Security_kernel -> "security_kernel_" ^ metric

(* This normalization is necessary because two contracts on Ghostnet are
   using "yay" instead of "yea" to vote yes, normalizing help with Grafana
   and Prometheus to have consistant values on the dashboard. *)
let normalize_vote ~value = match value with "yay" -> "yea" | str -> str

module MetricsRegistration = struct
  (* This module will set every metric **ONCE** at top level to avoid
     a fatal error from Grafana: registering the same metric multiple
     times is forbidden. *)

  module type Contract = sig
    val gov_type : governance_contract
  end

  module type Storage = sig
    val configuration : Counter.family

    val remaining_blocks : Gauge.family

    val current_period_type : Gauge.family

    val current_period_index : Gauge.family
  end

  module type Entrypoint = sig
    val vote : Gauge.family

    val trigger_upgrade : Gauge.family

    val proposal : Counter.family
  end

  module MakeStorage (Contract : Contract) : Storage = struct
    let config_label_names =
      [
        "start_level";
        "period_length";
        "adoption_period";
        "upvoting_limit";
        "scale";
        "proposal_quorum";
        "promotion_quorum";
        "promotion_supermajority";
      ]

    let configuration_name =
      get_metric_txt ~metric:"configuration" Contract.gov_type

    let configuration =
      v_labels_counter
        ~label_names:config_label_names
        ~help:(get_helper_txt ~helper:"configuration" Contract.gov_type)
        configuration_name

    let remaining_blocks_name =
      get_metric_txt ~metric:"remaining_blocks" Contract.gov_type

    let remaining_blocks =
      v_gauge
        ~help:(get_helper_txt ~helper:"remaining blocks" Contract.gov_type)
        remaining_blocks_name

    let current_period_type_name =
      get_metric_txt ~metric:"current_period_type" Contract.gov_type

    let current_period_type =
      v_gauge
        ~help:(get_helper_txt ~helper:"current period type" Contract.gov_type)
        current_period_type_name

    let current_period_index_name =
      get_metric_txt ~metric:"current_period_index" Contract.gov_type

    let current_period_index =
      v_gauge
        ~help:(get_helper_txt ~helper:"current period index" Contract.gov_type)
        current_period_index_name
  end

  module SequencerStorageRegistration = MakeStorage (struct
    let gov_type = Sequencer
  end)

  module KernelStorageRegistration = MakeStorage (struct
    let gov_type = Kernel
  end)

  module KernelSecurityStorageRegistration = MakeStorage (struct
    let gov_type = Security_kernel
  end)

  module MakeEntrypoints (Contract : Contract) : Entrypoint = struct
    let vote_name = get_metric_txt ~metric:"vote" Contract.gov_type

    let vote =
      v_gauge
        ~label_names:["source"; "value"]
        ~help:(get_helper_txt ~helper:"vote" Contract.gov_type)
        vote_name

    let trigger_upgrade_name =
      match Contract.gov_type with
      | Sequencer ->
          get_metric_txt ~metric:"trigger_commitee_upgrade" Contract.gov_type
      | Kernel | Security_kernel ->
          get_metric_txt ~metric:"trigger_upgrade" Contract.gov_type

    let trigger_upgrade =
      match Contract.gov_type with
      | Sequencer ->
          v_gauge
            ~label_names:["source"; "address"]
            ~help:
              (get_helper_txt
                 ~helper:"trigger comittee upgrade"
                 Contract.gov_type)
            trigger_upgrade_name
      | Kernel | Security_kernel ->
          v_gauge
            ~label_names:["source"; "address"]
            ~help:(get_helper_txt ~helper:"trigger upgrade" Contract.gov_type)
            trigger_upgrade_name

    let proposal_name = get_metric_txt ~metric:"proposal" Contract.gov_type

    let proposal =
      (* A new proposal can be treated as an upvote proposal: proposing also
         means upvoting the proposal. This will ease the computation related
         to the metric from PromQL and Grafana's perspective. *)
      match Contract.gov_type with
      | Sequencer ->
          v_labels_counter
            ~label_names:["source"; "sequencer_pk"; "pool_address"]
            ~help:(get_helper_txt ~helper:"proposal" Contract.gov_type)
            proposal_name
      | Kernel | Security_kernel ->
          v_labels_counter
            ~label_names:["source"; "proposal"]
            ~help:(get_helper_txt ~helper:"proposal" Contract.gov_type)
            proposal_name
  end

  module SequencerEntrypointsRegistration = MakeEntrypoints (struct
    let gov_type = Sequencer
  end)

  module KernelEntrypointsRegistration = MakeEntrypoints (struct
    let gov_type = Kernel
  end)

  module SecurityKernelEntrypointsRegistration = MakeEntrypoints (struct
    let gov_type = Security_kernel
  end)
end

module GovernanceMetrics = struct
  module Storage = struct
    let get_config_labels ~config =
      Contract_type.
        [
          Z.to_string config.started_at_level;
          Z.to_string config.period_length;
          Z.to_string config.adoption_period_sec;
          Z.to_string config.upvoting_limit;
          Z.to_string config.scale;
          Z.to_string config.proposal_quorum;
          Z.to_string config.promotion_quorum;
          Z.to_string config.promotion_supermajority;
        ]

    let set_configuration ~config = function
      | Sequencer ->
          set_counter
            MetricsRegistration.SequencerStorageRegistration.configuration
            ~labels:(get_config_labels ~config)
            ()
      | Kernel ->
          set_counter
            MetricsRegistration.KernelStorageRegistration.configuration
            ~labels:(get_config_labels ~config)
            ()
      | Security_kernel ->
          set_counter
            MetricsRegistration.KernelSecurityStorageRegistration.configuration
            ~labels:(get_config_labels ~config)
            ()

    let set_remaining_blocks = function
      | Sequencer ->
          set_gauge
            MetricsRegistration.SequencerStorageRegistration.remaining_blocks
            Z.to_float
      | Kernel ->
          set_gauge
            MetricsRegistration.KernelStorageRegistration.remaining_blocks
            Z.to_float
      | Security_kernel ->
          set_gauge
            MetricsRegistration.KernelSecurityStorageRegistration
            .remaining_blocks
            Z.to_float

    let set_current_period_type = function
      | Sequencer ->
          set_gauge
            MetricsRegistration.SequencerStorageRegistration.current_period_type
            Int.to_float
      | Kernel ->
          set_gauge
            MetricsRegistration.KernelStorageRegistration.current_period_type
            Int.to_float
      | Security_kernel ->
          set_gauge
            MetricsRegistration.KernelSecurityStorageRegistration
            .current_period_type
            Int.to_float

    let set_current_period_index = function
      | Sequencer ->
          set_gauge
            MetricsRegistration.SequencerStorageRegistration
            .current_period_index
            Z.to_float
      | Kernel ->
          set_gauge
            MetricsRegistration.KernelStorageRegistration.current_period_index
            Z.to_float
      | Security_kernel ->
          set_gauge
            MetricsRegistration.KernelSecurityStorageRegistration
            .current_period_index
            Z.to_float
  end

  module Entrypoints = struct
    let vote ~source ~value contract_type =
      let value = normalize_vote ~value in
      match contract_type with
      | Sequencer ->
          set_gauge
            MetricsRegistration.SequencerEntrypointsRegistration.vote
            ~labels:[source; value]
            Fun.id
            1.
      | Kernel ->
          set_gauge
            MetricsRegistration.KernelEntrypointsRegistration.vote
            ~labels:[source; value]
            Fun.id
            1.
      | Security_kernel ->
          set_gauge
            MetricsRegistration.SecurityKernelEntrypointsRegistration.vote
            ~labels:[source; value]
            Fun.id
            1.

    let trigger_upgrade ~source ~address = function
      | Sequencer ->
          set_gauge
            MetricsRegistration.SequencerEntrypointsRegistration.trigger_upgrade
            ~labels:[source; address]
            Fun.id
            1.
      | Kernel ->
          set_gauge
            MetricsRegistration.KernelEntrypointsRegistration.trigger_upgrade
            ~labels:[source; address]
            Fun.id
            1.
      | Security_kernel ->
          set_gauge
            MetricsRegistration.SecurityKernelEntrypointsRegistration
            .trigger_upgrade
            ~labels:[source; address]
            Fun.id
            1.

    let proposal ~source ?(sequencer_pk = "") ?(pool_address = "")
        ?(proposal = "") = function
      | Sequencer ->
          incr_counter
            MetricsRegistration.SequencerEntrypointsRegistration.proposal
            ~labels:[source; sequencer_pk; pool_address]
            ()
      | Kernel ->
          incr_counter
            MetricsRegistration.KernelEntrypointsRegistration.proposal
            ~labels:[source; proposal]
            ()
      | Security_kernel ->
          incr_counter
            MetricsRegistration.SecurityKernelEntrypointsRegistration.proposal
            ~labels:[source; proposal]
            ()

    let clear_vote = function
      | Sequencer ->
          Gauge.clear MetricsRegistration.SequencerEntrypointsRegistration.vote
      | Kernel ->
          Gauge.clear MetricsRegistration.KernelEntrypointsRegistration.vote
      | Security_kernel ->
          Gauge.clear
            MetricsRegistration.SecurityKernelEntrypointsRegistration.vote

    let clear_proposal = function
      | Sequencer ->
          Counter.clear
            MetricsRegistration.SequencerEntrypointsRegistration.proposal
      | Kernel ->
          Counter.clear
            MetricsRegistration.KernelEntrypointsRegistration.proposal
      | Security_kernel ->
          Counter.clear
            MetricsRegistration.SecurityKernelEntrypointsRegistration.proposal
  end
end
