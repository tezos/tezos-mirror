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

let rec contract_storage_state_process ~config ~contract ~chain_id =
  let open Lwt_result_syntax in
  let* voting_state =
    RPC.micheline_view
      ~chain_id
      ~contract:(governance_to_contract ~config contract)
      ~view:"get_voting_state"
      ~decode:Contract_type.decode_voting_state
      config.endpoint
  in
  let*! () =
    let type_ =
      match voting_state.current_period.type_ with
      | Proposal -> 0
      | Promotion -> 1
    in
    GovernanceMetrics.Storage.set_remaining_blocks
      contract
      voting_state.remaining_blocks ;
    GovernanceMetrics.Storage.set_current_period_type contract type_ ;
    GovernanceMetrics.Storage.set_current_period_index
      contract
      voting_state.current_period.index ;
    Event.contract_metrics @@ "voting state for "
    ^ governance_to_string contract
  in
  let*! () = Lwt_unix.sleep 5. in
  contract_storage_state_process ~config ~contract ~chain_id

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
  let*! chain_id = RPC.chain_id config.endpoint in
  match chain_id with
  | Ok chain_id ->
      let () =
        Lwt.dont_wait
          (fun () ->
            let*! _ =
              contract_storage_state_process
                ~config
                ~contract:Sequencer
                ~chain_id
            in
            Lwt.return_unit)
          (fun exn ->
            Format.eprintf
              "Sequencer storage state process error: %s\n@."
              (Printexc.to_string exn))
      in
      let () =
        Lwt.dont_wait
          (fun () ->
            let*! _ =
              contract_storage_state_process ~config ~contract:Kernel ~chain_id
            in
            Lwt.return_unit)
          (fun exn ->
            Format.eprintf
              "Kernel storage state process error: %s\n@."
              (Printexc.to_string exn))
      in
      contract_storage_state_process ~config ~contract:Security_kernel ~chain_id
  | Error trace ->
      let*! () = Event.storage_state_error "Failed to GET chain ID" in
      fail trace

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
  let _ =
    Tezos_clic.(
      setup_formatter
        Format.std_formatter
        (if Unix.isatty Unix.stdout then Ansi else Plain)
        Short)
  in
  let _ =
    Tezos_clic.(
      setup_formatter
        Format.err_formatter
        (if Unix.isatty Unix.stderr then Ansi else Plain)
        Short)
  in
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

let () =
  Lwt_main.run (parse_options ())
  |> Result.iter_error (Format.printf "ERROR: %a%!" Error_monad.pp_print_trace)
