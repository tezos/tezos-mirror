(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Node_helpers
open Configuration
open Metrics

let process_contracts_configuration ~config =
  let open Lwt_result_syntax in
  let* {config = config_sequencer} =
    RPC.storage
      ~contract:config.contracts.sequencer_governance
      ~decode:Contract_type.decode_storage
      config.endpoint
  in
  let*! () =
    GovernanceMetrics.Storage.set_configuration
      ~config:config_sequencer
      Sequencer ;
    Event.contract_metrics "sequencer configuration"
  in
  let* {config = config_kernel} =
    RPC.storage
      ~contract:config.contracts.kernel_governance
      ~decode:Contract_type.decode_storage
      config.endpoint
  in
  let*! () =
    GovernanceMetrics.Storage.set_configuration ~config:config_kernel Kernel ;
    Event.contract_metrics "kernel configuration"
  in
  let* {config = config_security_kernel} =
    RPC.storage
      ~contract:config.contracts.security_kernel_governance
      ~decode:Contract_type.decode_storage
      config.endpoint
  in
  GovernanceMetrics.Storage.set_configuration
    ~config:config_security_kernel
    Security_kernel ;
  let*! () = Event.contract_metrics "security kernel configuration" in
  return_unit

let contract_storage_state_process ~config ~contract ~chain_id ~level =
  let open Lwt_result_syntax in
  let* voting_state =
    RPC.micheline_view
      ~chain_id
      ~contract:(governance_to_contract ~config contract)
      ~view:"get_voting_state"
      ~level
      ~decode:Contract_type.decode_voting_state
      config.endpoint
  in
  let*! () =
    let type_ =
      match voting_state.voting_context.current_period.type_ with
      | Proposal -> 0
      | Promotion -> 1
    in
    Contract_type.apply_mainnet
      (GovernanceMetrics.Storage.set_remaining_blocks contract)
      voting_state.voting_context.remaining_blocks ;
    GovernanceMetrics.Storage.set_current_period_type contract type_ ;
    GovernanceMetrics.Storage.set_current_period_index
      contract
      voting_state.voting_context.current_period.index ;
    Event.contract_metrics @@ "voting state for "
    ^ governance_to_string contract
  in
  return_unit

let process_governance_contracts_entrypoints ~governance_operations ~level =
  let open Lwt_result_syntax in
  let* operations = governance_operations ~level in
  List.iter
    (fun {source; parameters} ->
      match parameters.value with
      | Sequencer (Trigger_committee_upgrade address) ->
          GovernanceMetrics.Entrypoints.trigger_upgrade
            ~source
            ~address
            Sequencer ;
          GovernanceMetrics.Entrypoints.clear_proposal Sequencer ;
          GovernanceMetrics.Entrypoints.clear_vote Sequencer
      | Sequencer (Vote value) ->
          GovernanceMetrics.Entrypoints.vote ~source ~value Sequencer
      | Sequencer (Upvote_proposal {sequencer_pk; pool_address}) ->
          GovernanceMetrics.Entrypoints.proposal
            ~source
            ~sequencer_pk
            ~pool_address
            Sequencer
      | Sequencer (New_proposal {sequencer_pk; pool_address}) ->
          GovernanceMetrics.Entrypoints.proposal
            ~source
            ~sequencer_pk
            ~pool_address
            Sequencer
      | Kernel (Trigger_kernel_upgrade address) ->
          GovernanceMetrics.Entrypoints.trigger_upgrade ~source ~address Kernel ;
          GovernanceMetrics.Entrypoints.clear_proposal Kernel ;
          GovernanceMetrics.Entrypoints.clear_vote Kernel
      | Kernel (Vote value) ->
          GovernanceMetrics.Entrypoints.vote ~source ~value Kernel
      | Kernel (Upvote_proposal proposal) ->
          GovernanceMetrics.Entrypoints.proposal ~source ~proposal Kernel
      | Kernel (New_proposal proposal) ->
          GovernanceMetrics.Entrypoints.proposal ~source ~proposal Kernel
      | Security_kernel (Trigger_kernel_upgrade address) ->
          GovernanceMetrics.Entrypoints.trigger_upgrade
            ~source
            ~address
            Security_kernel ;
          GovernanceMetrics.Entrypoints.clear_proposal Security_kernel ;
          GovernanceMetrics.Entrypoints.clear_vote Security_kernel
      | Security_kernel (Vote value) ->
          GovernanceMetrics.Entrypoints.vote ~source ~value Security_kernel
      | Security_kernel (Upvote_proposal proposal) ->
          GovernanceMetrics.Entrypoints.proposal
            ~source
            ~proposal
            Security_kernel
      | Security_kernel (New_proposal proposal) ->
          GovernanceMetrics.Entrypoints.proposal
            ~source
            ~proposal
            Security_kernel)
    operations ;
  return_unit

(* This value is used in order to avoid fetching the same block twice in case of
   a reorganization. *)
let latest_finalized_level = ref 0

let main ~config () =
  let open Lwt_result_syntax in
  let*! () = Tezos_base_unix.Internal_event_unix.init () in
  let*! () = Event.starting_observer () in
  let () =
    Lwt.dont_wait
      (fun () -> Metrics.start_server ~config ())
      (fun exn ->
        Format.eprintf "Metrics server error: %s\n@." (Printexc.to_string exn))
  in
  let* () = process_contracts_configuration ~config in
  let* chain_id = RPC.chain_id config.endpoint in
  let governance_operations = RPC.governance_operations ~config in
  (* Head monitoring.
     This part will be the clock of our execution. Each detected
     fresh head will trigger a process to update several metrics. *)
  let process {level} =
    (* We fetch the informations for [level - 2] to avoid reorg. problems. *)
    let level = level - 2 in
    if !latest_finalized_level != level then (
      let* () =
        contract_storage_state_process
          ~config
          ~contract:Sequencer
          ~chain_id
          ~level
      in
      let* () =
        contract_storage_state_process ~config ~contract:Kernel ~chain_id ~level
      in
      let* () =
        contract_storage_state_process
          ~config
          ~contract:Security_kernel
          ~chain_id
          ~level
      in
      let* () =
        process_governance_contracts_entrypoints
          ~governance_operations
          ~level:(Int.to_string level)
      in
      set_finalized_l1_level level ;
      latest_finalized_level := level ;
      return_unit)
    else return_unit
  in
  let rec governance_obsv_process () =
    let* () = RPC.monitor_heads ~process config.endpoint in
    let*! () = Event.monitor_head_restart () in
    governance_obsv_process ()
  in
  governance_obsv_process ()

let run =
  let open Tezos_clic in
  command
    ~desc:"Run Etherlink's governance observer."
    (args5
       Args.metrics_addr
       Args.metrics_port
       Args.endpoint
       Args.etherlink_mainnet
       Args.etherlink_ghostnet)
    (prefixes ["run"] stop)
    (fun
      ( metrics_addr,
        metrics_port,
        endpoint,
        etherlink_mainnet_arg,
        etherlink_ghostnet_arg )
      ()
    ->
      let contracts =
        match (etherlink_mainnet_arg, etherlink_ghostnet_arg) with
        | true, true -> network_argument_not_found ~error:Both_args
        | false, false -> network_argument_not_found ~error:No_args
        | true, false -> mainnet_contracts
        | false, true -> ghostnet_contracts
      in
      let config =
        {
          prometheus = {metrics_addr; metrics_port};
          endpoint = Uri.of_string endpoint;
          contracts;
        }
      in
      main ~config ())

let commands = [run]

let global_options = Tezos_clic.no_options

let executable_name = Filename.basename Sys.executable_name

let argv () = Array.to_list Sys.argv |> List.tl |> Stdlib.Option.get

let parse_options () =
  let open Lwt_result_syntax in
  let args = argv () in
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stdout)
        Format.std_formatter
        Short) ;
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stderr)
        Format.err_formatter
        Short) ;
  let commands =
    Tezos_clic.add_manual
      ~executable_name
      ~global_options
      (if Unix.isatty Unix.stdout then Tezos_clic.Ansi else Tezos_clic.Plain)
      Format.std_formatter
      commands
  in
  let* (), remaining_args =
    Tezos_clic.parse_global_options global_options () args
  in
  Tezos_clic.dispatch commands () remaining_args

let handle_error = function
  | Ok _ -> ()
  | Error [Tezos_clic.Version] ->
      let evm_node_version =
        (* reuse octez evm node version, this is a small hack because
           we need a version so we can add this bin into
           `experimental` *)
        Tezos_version_value.Bin_version.octez_evm_node_version_string
      in
      Format.printf "%s\n" evm_node_version ;
      exit 0
  | Error [Tezos_clic.Help command] ->
      Tezos_clic.usage
        Format.std_formatter
        ~executable_name
        ~global_options
        (match command with None -> [] | Some c -> [c]) ;
      Stdlib.exit 0
  | Error errs ->
      Tezos_clic.pp_cli_errors
        Format.err_formatter
        ~executable_name
        ~global_options
        ~default:Error_monad.pp
        errs ;
      Stdlib.exit 1

let () = Tezos_base_unix.Event_loop.main_run parse_options |> handle_error
