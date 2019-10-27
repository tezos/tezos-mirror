open Internal_pervasives
open Console

let run state ~protocol ~size ~base_port ~no_daemons_for ?external_peer_ports
    ~nodes_history_mode_edits ~with_baking ?generate_kiln_config node_exec
    client_exec baker_exec endorser_exec accuser_exec () =
  Helpers.System_dependencies.precheck state `Or_fail
    ~executables:
      [node_exec; client_exec; baker_exec; endorser_exec; accuser_exec]
  >>= fun () ->
  Test_scenario.network_with_protocol ?external_peer_ports ~protocol ~size
    ~nodes_history_mode_edits ~base_port state ~node_exec ~client_exec
  >>= fun (nodes, protocol) ->
  Tezos_client.rpc state
    ~client:(Tezos_client.of_node (List.hd_exn nodes) ~exec:client_exec)
    `Get ~path:"/chains/main/chain_id"
  >>= fun chain_id_json ->
  let network_id =
    match chain_id_json with `String s -> s | _ -> assert false in
  Asynchronous_result.map_option generate_kiln_config ~f:(fun kiln_config ->
      Kiln.Configuration_directory.generate state kiln_config
        ~peers:(List.map nodes ~f:(fun {Tezos_node.p2p_port; _} -> p2p_port))
        ~sandbox_json:(Tezos_protocol.sandbox_path ~config:state protocol)
        ~nodes:
          (List.map nodes ~f:(fun {Tezos_node.rpc_port; _} ->
               sprintf "http://localhost:%d" rpc_port))
        ~bakers:
          (List.map protocol.Tezos_protocol.bootstrap_accounts
             ~f:(fun (account, _) ->
               Tezos_protocol.Account.(name account, pubkey_hash account)))
        ~network_string:network_id ~node_exec ~client_exec
        ~protocol_execs:
          [(protocol.Tezos_protocol.hash, baker_exec, endorser_exec)])
  >>= fun (_ : unit option) ->
  let keys_and_daemons =
    let pick_a_node_and_client idx =
      match List.nth nodes ((1 + idx) mod List.length nodes) with
      | Some node -> (node, Tezos_client.of_node node ~exec:client_exec)
      | None -> assert false in
    Tezos_protocol.bootstrap_accounts protocol
    |> List.filter_mapi ~f:(fun idx acc ->
           let node, client = pick_a_node_and_client idx in
           let key = Tezos_protocol.Account.name acc in
           if List.mem ~equal:String.equal no_daemons_for key then None
           else
             Some
               ( acc
               , client
               , [ Tezos_daemon.baker_of_node ~exec:baker_exec ~client node ~key
                 ; Tezos_daemon.endorser_of_node ~exec:endorser_exec ~client
                     node ~key ] )) in
  ( if with_baking then
    let accusers =
      List.map nodes ~f:(fun node ->
          let client = Tezos_client.of_node node ~exec:client_exec in
          Tezos_daemon.accuser_of_node ~exec:accuser_exec ~client node) in
    List_sequential.iter accusers ~f:(fun acc ->
        Running_processes.start state (Tezos_daemon.process acc ~state)
        >>= fun {process= _; lwt= _} -> return ())
    >>= fun () ->
    List_sequential.iter keys_and_daemons ~f:(fun (acc, client, daemons) ->
        Tezos_client.bootstrapped ~state client
        >>= fun () ->
        let key, priv = Tezos_protocol.Account.(name acc, private_key acc) in
        Tezos_client.import_secret_key ~state client key priv
        >>= fun () ->
        say state
          EF.(
            desc_list
              (haf "Registration-as-delegate:")
              [ desc (af "Client:") (af "%S" client.Tezos_client.id)
              ; desc (af "Key:") (af "%S" key) ])
        >>= fun () ->
        Tezos_client.register_as_delegate ~state client key
        >>= fun () ->
        say state
          EF.(
            desc_list (haf "Starting daemons:")
              [ desc (af "Client:") (af "%S" client.Tezos_client.id)
              ; desc (af "Key:") (af "%S" key) ])
        >>= fun () ->
        List_sequential.iter daemons ~f:(fun daemon ->
            Running_processes.start state (Tezos_daemon.process daemon ~state)
            >>= fun {process= _; lwt= _} -> return ()))
  else
    List.fold ~init:(return []) keys_and_daemons
      ~f:(fun prev_m (acc, client, _) ->
        prev_m
        >>= fun prev ->
        Tezos_client.bootstrapped ~state client
        >>= fun () ->
        let key, priv = Tezos_protocol.Account.(name acc, private_key acc) in
        let keyed_client =
          Tezos_client.Keyed.make client ~key_name:key ~secret_key:priv in
        Tezos_client.Keyed.initialize state keyed_client
        >>= fun _ -> return (keyed_client :: prev))
    >>= fun clients ->
    Interactive_test.Pauser.add_commands state
      Interactive_test.Commands.[bake_command state ~clients] ;
    return () )
  >>= fun () ->
  Interactive_test.Pauser.add_commands state
    Interactive_test.Commands.(
      all_defaults state ~nodes
      @ [secret_keys state ~protocol]
      @ arbitrary_commands_for_each_and_all_clients state
          ~clients:(List.map nodes ~f:(Tezos_client.of_node ~exec:client_exec))) ;
  Interactive_test.Pauser.generic ~force:true state
    EF.[haf "Sandbox is READY \\o/"]

let cmd ~pp_error () =
  let open Cmdliner in
  let open Term in
  Test_command_line.Run_command.make ~pp_error
    ( pure
        (fun size
             base_port
             (`External_peers external_peer_ports)
             (`No_daemons_for no_daemons_for)
             (`With_baking with_baking)
             protocol
             bnod
             bcli
             bak
             endo
             accu
             generate_kiln_config
             nodes_history_mode_edits
             state
             ->
          let actual_test =
            run state ~size ~base_port ~protocol bnod bcli bak endo accu
              ~nodes_history_mode_edits ~with_baking ?generate_kiln_config
              ~external_peer_ports ~no_daemons_for in
          (state, Interactive_test.Pauser.run_test ~pp_error state actual_test))
    $ Arg.(
        value & opt int 5
        & info ["size"; "S"] ~doc:"Set the size of the network.")
    $ Arg.(
        value & opt int 20_000
        & info ["base-port"; "P"] ~doc:"Base port number to build upon.")
    $ Arg.(
        pure (fun l -> `External_peers l)
        $ value
            (opt_all int []
               (info ["add-external-peer-port"] ~docv:"PORT-NUMBER"
                  ~doc:"Add $(docv) to the peers of the network nodes.")))
    $ Arg.(
        pure (fun l -> `No_daemons_for l)
        $ value
            (opt_all string []
               (info ["no-daemons-for"] ~docv:"ACCOUNT-NAME"
                  ~doc:"Do not start daemons for $(docv).")))
    $ Arg.(
        pure (fun x -> `With_baking (not x))
        $ value
            (flag
               (info ["no-baking"]
                  ~doc:
                    "Completely disable baking/endorsing/accusing (you need \
                     to bake manually to make the chain advance).")))
    $ Tezos_protocol.cli_term ()
    $ Tezos_executable.cli_term `Node "tezos"
    $ Tezos_executable.cli_term `Client "tezos"
    $ Tezos_executable.cli_term `Baker "tezos"
    $ Tezos_executable.cli_term `Endorser "tezos"
    $ Tezos_executable.cli_term `Accuser "tezos"
    $ Kiln.Configuration_directory.cli_term ()
    $ Tezos_node.History_modes.cmdliner_term ()
    $ Test_command_line.cli_state ~name:"mininet" () )
    (let doc = "Small network sandbox with bakers, endorsers, and accusers." in
     let man : Manpage.block list =
       [ `P
           "This test builds a small sandbox network, start various daemons, \
            and then gives the user an interactive command prompt to inspect \
            the network." ] in
     info "mini-network" ~man ~doc)
