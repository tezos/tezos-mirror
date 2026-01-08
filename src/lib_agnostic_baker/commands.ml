(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Dal = struct
  open Tezos_dal_node_lib
  open Cli.Term

  let arg_to_clic
      ({
         default = _;
         short;
         long;
         extra_long = _;
         parse;
         doc;
         placeholder;
         pp = _;
         env;
       } :
        'a arg) =
    let open Tezos_clic in
    arg
      ~doc
      ?short
      ~long
      ~placeholder
      ?env:(Option.map (fun (env : env) -> env.name) env)
      (parameter (fun _ctxt s ->
           let open Lwt_result_syntax in
           match parse s with
           | Ok s -> return s
           | Error msg -> failwith "%s" msg))

  let switch_to_clic {long; extra_long = _; doc} =
    let open Tezos_clic in
    switch ~doc ~long ()

  let data_dir = arg_to_clic data_dir_arg

  let config_file = arg_to_clic config_file_arg

  let rpc_addr = arg_to_clic rpc_addr_arg

  let expected_pow = arg_to_clic expected_pow_arg

  let net_addr = arg_to_clic net_addr_arg

  let public_addr = arg_to_clic public_addr_arg

  let endpoint = arg_to_clic endpoint_arg

  let slots_backup_uris = arg_to_clic slots_backup_uris_arg

  let trust_slots_backup_uris = switch_to_clic trust_slots_backup_uris_switch

  let ignore_l1_config_peers = switch_to_clic ignore_l1_config_peers_switch

  let attester_profile = arg_to_clic attester_profile_arg

  let operator_profile = arg_to_clic operator_profile_arg

  let observer_profile = arg_to_clic observer_profile_arg

  let bootstrap_profile = switch_to_clic bootstrap_profile_switch

  let peers = arg_to_clic peers_arg

  let metrics_addr = arg_to_clic metrics_addr_arg

  let history_mode = arg_to_clic history_mode_arg

  let service_name = arg_to_clic service_name_arg

  let service_namespace = arg_to_clic service_namespace_arg

  let fetch_trusted_setup = arg_to_clic fetch_trusted_setup_arg

  let disable_shard_validation = switch_to_clic disable_shard_validation_switch

  let disable_amplification = switch_to_clic disable_amplification_switch

  let verbose = switch_to_clic verbose_switch

  let ignore_topics = arg_to_clic ignore_topics_arg

  let batching_configuration = arg_to_clic batching_configuration_arg

  let publish_slots_regularly = arg_to_clic publish_slots_regularly_arg

  let commands =
    let open Tezos_clic in
    let group = {name = "dal"; title = "Commands related to the DAL daemon."} in
    let debug_print_store_schemas =
      let open Tezos_clic in
      let args = Tezos_clic.no_options in
      command
        ~group
        ~desc:"Print SQL statements describing the tables created in the store."
        args
        (prefixes ["debug"; "dal"; "print"; "store"; "schemas"] @@ stop)
        (fun () _cctxt -> Cli.Action.debug_print_store_schemas ())
    in
    let run =
      let open Tezos_clic in
      let args =
        Tezos_clic.args26
          data_dir
          config_file
          rpc_addr
          expected_pow
          net_addr
          public_addr
          endpoint
          slots_backup_uris
          trust_slots_backup_uris
          metrics_addr
          attester_profile
          operator_profile
          observer_profile
          bootstrap_profile
          peers
          history_mode
          service_name
          service_namespace
          fetch_trusted_setup
          disable_shard_validation
          verbose
          ignore_l1_config_peers
          disable_amplification
          ignore_topics
          batching_configuration
          publish_slots_regularly
      in
      command
        ~group
        ~desc:"Run the Octez DAL"
        args
        (prefixes ["run"; "dal"] @@ stop)
        (fun ( data_dir,
               config_file,
               rpc_addr,
               expected_pow,
               net_addr,
               public_addr,
               endpoint,
               slots_backup_uris,
               trust_slots_backup_uris,
               metrics_addr,
               attesters,
               operators,
               observers,
               bootstrap,
               peers,
               history_mode,
               service_name,
               service_namespace,
               fetch_trusted_setup,
               disable_shard_validation,
               verbose,
               ignore_l1_config_peers,
               disable_amplification,
               ignore_topics,
               batching_configuration,
               publish_slots_regularly )
             _cctxt
           ->
          Cli.Action.run
            ?data_dir
            ?config_file
            ?rpc_addr
            ?expected_pow
            ?listen_addr:net_addr
            ?public_addr
            ?endpoint
            ?slots_backup_uris
            ~trust_slots_backup_uris
            ?metrics_addr
            ?attesters
            ?operators
            ?observers
            ~bootstrap
            ?peers
            ?history_mode
            ?service_name
            ?service_namespace
            ?fetch_trusted_setup
            ~disable_shard_validation
            ~verbose
            ~ignore_l1_config_peers
            ~disable_amplification
            ?ignore_topics
            ?batching_configuration
            ?publish_slots_regularly
            ())
    in
    let mk_config_command ~prefix:p ~desc action =
      let open Tezos_clic in
      let args =
        Tezos_clic.args23
          data_dir
          config_file
          rpc_addr
          expected_pow
          net_addr
          public_addr
          endpoint
          slots_backup_uris
          trust_slots_backup_uris
          metrics_addr
          attester_profile
          operator_profile
          observer_profile
          bootstrap_profile
          peers
          history_mode
          service_name
          service_namespace
          fetch_trusted_setup
          verbose
          ignore_l1_config_peers
          disable_amplification
          batching_configuration
      in
      command
        ~group
        ~desc
        args
        (prefixes ["config"; "dal"; p] @@ stop)
        (fun ( data_dir,
               config_file,
               rpc_addr,
               expected_pow,
               net_addr,
               public_addr,
               endpoint,
               slots_backup_uris,
               trust_slots_backup_uris,
               metrics_addr,
               attesters,
               operators,
               observers,
               bootstrap,
               peers,
               history_mode,
               service_name,
               service_namespace,
               fetch_trusted_setup,
               verbose,
               ignore_l1_config_peers,
               disable_amplification,
               batching_configuration )
             _cctxt
           ->
          action
            ?data_dir
            ?config_file
            ?rpc_addr
            ?expected_pow
            ?listen_addr:net_addr
            ?public_addr
            ?endpoint
            ?slots_backup_uris
            ?trust_slots_backup_uris:(Some trust_slots_backup_uris)
            ?metrics_addr
            ?attesters
            ?operators
            ?observers
            ?bootstrap:(Some bootstrap)
            ?peers
            ?history_mode
            ?service_name
            ?service_namespace
            ?fetch_trusted_setup
            ?verbose:(Some verbose)
            ?ignore_l1_config_peers:(Some ignore_l1_config_peers)
            ?disable_amplification:(Some disable_amplification)
            ?batching_configuration
            ())
    in
    let config_init =
      mk_config_command
        ~desc:
          "This command creates a configuration file with the parameters \
           provided on the command-line, if no configuration file exists \
           already in the specified or default location. Otherwise, the \
           command-line parameters override the existing ones, and old \
           parameters are lost. This configuration is then used by the run \
           command."
        ~prefix:"init"
        Cli.Action.config_init
    in
    let config_update =
      mk_config_command
        ~desc:
          "This command updates the configuration file with the parameters \
           provided on the command-line. If no configuration file exists \
           already, the command will fail."
        ~prefix:"update"
        Cli.Action.config_update
    in
    [run; config_init; config_update; debug_print_store_schemas]
end

module Baker = struct
  let init_logging (module C : Client_main_run.M) ~base_dir =
    let open Lwt_result_syntax in
    let full =
      new Client_context_unix.unix_full
        ~chain:C.default_chain
        ~block:C.default_block
        ~confirmations:None
        ~password_filename:None
        ~base_dir:C.default_base_dir
        ~rpc_config:Tezos_rpc_http_client_unix.RPC_client_unix.default_config
        ~verbose_rpc_error_diagnostics:false
    in
    let* parsed, _remaining =
      C.parse_config_args
        full
        (match Array.to_list Sys.argv with
        | _ :: original_args -> original_args
        | [] -> [])
    in
    let parsed_config_file = parsed.Client_config.parsed_config_file in
    let parsed_args = parsed.Client_config.parsed_args in
    let*! () =
      Client_main_run.init_logging
        (module C)
        ?parsed_args
        ?parsed_config_file
        ~base_dir
        ()
    in
    return_unit

  let commands client_config =
    let open Lwt_result_syntax in
    let open Configuration in
    let open Tezos_clic in
    let group =
      {
        name = "delegate.baker";
        title = "Commands related to the agnostic baker daemon.";
      }
    in
    [
      command
        ~group
        ~desc:"Launch the baker daemon"
        baker_args
        (prefixes ["run"; "with"; "local"; "node"]
        @@ param
             ~name:"node_data_path"
             ~desc:"Path to the node data directory (e.g. $HOME/.tezos-node)"
             directory_parameter
        @@ sources_param)
        (fun args data_dir sources cctxt ->
          let* () = init_logging client_config ~base_dir:cctxt#get_base_dir in
          let args = Configuration.create_config args in
          Daemon.Baker.run
            ~keep_alive:args.keep_alive
            ~command:(Daemon.Run_with_local_node {data_dir; args; sources})
            cctxt);
      command
        ~group
        ~desc:"Launch the baker daemon using RPCs only."
        baker_args
        (prefixes ["run"; "remotely"] @@ sources_param)
        (fun args sources cctxt ->
          let* () = init_logging client_config ~base_dir:cctxt#get_base_dir in
          let args = Configuration.create_config args in
          Daemon.Baker.run
            ~keep_alive:args.keep_alive
            ~command:(Daemon.Run_remotely {args; sources})
            cctxt);
      command
        ~group
        ~desc:"Launch the VDF daemon"
        (args2 pidfile_arg keep_alive_arg)
        (prefixes ["run"; "vdf"] @@ stop)
        (fun (pidfile, keep_alive) cctxt ->
          let* () = init_logging client_config ~base_dir:cctxt#get_base_dir in
          Daemon.Baker.run
            ~keep_alive
            ~command:(Daemon.Run_vdf {pidfile; keep_alive})
            cctxt);
      command
        ~group
        ~desc:"Launch the accuser daemon"
        (args3 pidfile_arg preserved_levels_arg keep_alive_arg)
        (prefixes ["run"; "accuser"] @@ stop)
        (fun (pidfile, preserved_levels, keep_alive) cctxt ->
          let (module C) = client_config in
          let client_config : (module Client_main_run.M) =
            (module struct
              include C

              let default_daily_logs_path = Some "octez-accuser"
            end)
          in
          let* () = init_logging client_config ~base_dir:cctxt#get_base_dir in
          Daemon.Baker.run
            ~keep_alive
            ~command:
              (Daemon.Run_accuser {pidfile; preserved_levels; keep_alive})
            cctxt);
    ]
    @ Dal.commands
end

module Accuser = struct
  let commands =
    let open Configuration in
    let open Tezos_clic in
    let group =
      {
        name = "delegate.accuser";
        title = "Commands related to the agnostic accuser daemon.";
      }
    in
    [
      command
        ~group
        ~desc:"Launch the accuser daemon"
        (args3 pidfile_arg preserved_levels_arg keep_alive_arg)
        (prefix "run" @@ stop)
        (fun (pidfile, preserved_levels, keep_alive) cctxt ->
          Daemon.Accuser.run
            ~keep_alive
            ~command:
              (Daemon.Run_accuser {pidfile; preserved_levels; keep_alive})
            cctxt);
    ]
end

let baker_commands = Baker.commands

let accuser_commands = Accuser.commands
