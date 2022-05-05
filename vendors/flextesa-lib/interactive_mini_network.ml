open Internal_pervasives
open Console

(** A.k.a the [chain_id] *)
module Genesis_block_hash = struct
  let path state = Paths.root state // "genesis.json"

  let to_json _state genesis =
    Ezjsonm.dict [("genesis-block-hash", `String genesis)]

  (** See implementation of {!Tezos_node}, this corresponds to the Chain-id
      ["NetXKMbjQL2SBox"] *)
  let default = "BLdZYwNF8Rn6zrTWkuRRNyrj6bQWPkfBog2YKhWhn5z3ApmpzBf"

  module Choice = struct
    type t = [`Random | `Force of string | `Default]

    let pp : t Fmt.t =
     fun ppf ->
      let open Fmt in
      function
      | `Random -> pf ppf "Random"
      | `Default -> pf ppf "Default:%s" default
      | `Force v -> pf ppf "Forced:%s" v

    let pp_short : t Fmt.t =
     fun ppf ->
      let open Fmt in
      function
      | `Random -> pf ppf "Random"
      | `Default -> pf ppf "Default"
      | `Force _ -> pf ppf "Forced"

    let cmdliner_term () : t Cmdliner.Term.t =
      let open Cmdliner in
      let open Term in
      ret
        ( pure (function
            | None | Some "default" -> `Ok `Default
            | Some "random" -> `Ok `Random
            | Some force -> `Ok (`Force force) )
        $ Arg.(
            let doc =
              Fmt.str
                "Set the genesis block hash (from which the chain-id is \
                 derived). The default (or the string %S) is `%s...`, %S means \
                 pick-one-at-random. This option is ignored when the \
                 `--keep-root` option allows the chain to resume (the \
                 previously chosen genesis-hash will be still in effect)."
                "default"
                (String.sub default ~pos:0 ~len:8)
                "random" in
            value
              (opt (some string) None
                 (info ["genesis-block-hash"] ~docv:"BLOCK-HASH|random|default"
                    ~doc ) )) )
  end

  let chain_id_of_hash hash =
    let open Tezos_crypto in
    Option.map (Block_hash.of_b58check_opt hash) ~f:(fun bh ->
        bh |> Chain_id.of_block_hash |> Chain_id.to_b58check )

  let process_choice state choice =
    let json_file = path state in
    let pp_hash_fancily ppf h =
      let open More_fmt in
      pf ppf "`%s` (corresponding chain-id: `%s`)" h
        (chain_id_of_hash h |> Option.value ~default:"ERROR-WRONG-HASH") in
    match Caml.Sys.file_exists json_file with
    | true ->
        System.read_file state json_file
        >>= fun json_str ->
        System_error.catch_exn
          ~attach:[("json-content", `Verbatim [json_str])]
          (fun () ->
            match Ezjsonm.value_from_string json_str with
            | `O [("genesis-block-hash", `String hash)] -> hash
            | _ ->
                Fmt.failwith "invalid json for genesis-block-hash: %S" json_str
            )
        >>= fun hash ->
        Console.sayf state
          More_fmt.(
            fun ppf () ->
              wf ppf "Genesis-block-hash already set: %a%a" pp_hash_fancily hash
                (fun ppf -> function
                  | `Default -> pf ppf "."
                  | choice ->
                      pf ppf " (user choice “%a” is then ignored)."
                        Choice.pp choice )
                choice)
        >>= fun () -> return hash
    | false ->
        let hash =
          match choice with
          | `Default -> default
          | `Force v -> v
          | `Random ->
              let seed =
                Fmt.str "%d:%f" (Random.int 1_000_000) (Unix.gettimeofday ())
              in
              let open Tezos_crypto in
              let block_hash = Block_hash.hash_string [seed] in
              Block_hash.to_b58check block_hash in
        Console.sayf state
          More_fmt.(
            fun ppf () ->
              wf ppf
                "Genesis-block-hash not set, using: %a (from user choice: \
                 “%a”)."
                pp_hash_fancily hash Choice.pp_short choice)
        >>= fun () ->
        Running_processes.run_successful_cmdf state "mkdir -p %s"
          Caml.Filename.(dirname json_file |> quote)
        >>= fun _ ->
        System.write_file state json_file
          ~content:(to_json state hash |> Ezjsonm.value_to_string)
        >>= fun () -> return hash
end

let run state ~protocol ~size ~base_port ~clear_root ~no_daemons_for ?hard_fork
    ~genesis_block_choice ?external_peer_ports ~nodes_history_mode_edits
    ~with_baking ?generate_kiln_config node_exec client_exec baker_exec
    endorser_exec accuser_exec test_kind () =
  ( if clear_root then
    Console.say state EF.(wf "Clearing root: `%s`" (Paths.root state))
    >>= fun () -> Helpers.clear_root state
  else Console.say state EF.(wf "Keeping root: `%s`" (Paths.root state)) )
  >>= fun () ->
  Genesis_block_hash.process_choice state genesis_block_choice
  >>= fun genesis_block_hash ->
  Helpers.System_dependencies.precheck state `Or_fail
    ~executables:
      ( [node_exec; client_exec]
      @ (if with_baking then [baker_exec; endorser_exec; accuser_exec] else [])
      @ Option.value_map hard_fork ~default:[] ~f:Hard_fork.executables )
  >>= fun () ->
  Console.say state EF.(wf "Starting up the network.")
  >>= fun () ->
  let node_custom_network =
    let base =
      Tezos_node.Config_file.network ~genesis_hash:genesis_block_hash () in
    `Json
      (Ezjsonm.dict
         ( base
         @ Option.value_map ~default:[] hard_fork ~f:(fun hf ->
               [Hard_fork.node_network_config hf] ) ) ) in
  Test_scenario.network_with_protocol ?external_peer_ports ~protocol ~size
    ~do_activation:clear_root ~nodes_history_mode_edits ~base_port state
    ~node_exec ~client_exec ~node_custom_network
  >>= fun (nodes, protocol) ->
  Console.say state EF.(wf "Network started, preparing scenario.")
  >>= fun () ->
  Tezos_client.rpc state
    ~client:(Tezos_client.of_node (List.hd_exn nodes) ~exec:client_exec)
    `Get ~path:"/chains/main/chain_id"
  >>= fun chain_id_json ->
  let network_id =
    match chain_id_json with `String s -> s | _ -> assert false in
  Asynchronous_result.map_option generate_kiln_config ~f:(fun kiln_config ->
      Kiln.Configuration_directory.generate state kiln_config
        ~peers:(List.map nodes ~f:(fun {Tezos_node.p2p_port; _} -> p2p_port))
        ~sandbox_json:(Tezos_protocol.sandbox_path state protocol)
        ~nodes:
          (List.map nodes ~f:(fun {Tezos_node.rpc_port; _} ->
               sprintf "http://localhost:%d" rpc_port ) )
        ~bakers:
          (List.map protocol.Tezos_protocol.bootstrap_accounts
             ~f:(fun (account, _) ->
               Tezos_protocol.Account.(name account, pubkey_hash account) ) )
        ~network_string:network_id ~node_exec ~client_exec
        ~protocol_execs:
          [(protocol.Tezos_protocol.hash, baker_exec, endorser_exec)] )
  >>= fun (_ : unit option) ->
  let to_keyed acc client =
    let key, priv = Tezos_protocol.Account.(name acc, private_key acc) in
    let keyed_client =
      Tezos_client.Keyed.make client ~key_name:key ~secret_key:priv in
    keyed_client in
  let lb_vote = match protocol.kind with `Alpha -> Some "pass" | _ -> None in
  let hard_fork_lb_vote = function
    | "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK" -> Some "pass"
    | _ -> None
  in
  let keys_and_daemons =
    let pick_a_node_and_client idx =
      match List.nth nodes (Int.rem (1 + idx) (List.length nodes)) with
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
               , to_keyed acc client
               , Option.value_map hard_fork ~default:[]
                   ~f:(Hard_fork.keyed_daemons ~client ~node ~key
                         ~lb_vote:hard_fork_lb_vote)
                 @ [ Tezos_daemon.baker_of_node ~exec:baker_exec ~client node
                       ~key ~lb_vote
                   ; Tezos_daemon.endorser_of_node ~exec:endorser_exec ~client
                       node ~key ] ) ) in
  List_sequential.iter keys_and_daemons ~f:(fun (_, _, kc, _) ->
      Tezos_client.Keyed.initialize state kc >>= fun _ -> return () )
  >>= fun () ->
  Interactive_test.Pauser.add_commands state
    Interactive_test.Commands.
      [ generate_traffic_command state
          ~clients:(List.map keys_and_daemons ~f:(fun (_, _, kc, _) -> kc)) ] ;
  ( if with_baking then
    let accusers =
      List.map nodes ~f:(fun node ->
          let client = Tezos_client.of_node node ~exec:client_exec in
          Tezos_daemon.accuser_of_node ~exec:accuser_exec ~client node ) in
    List_sequential.iter accusers ~f:(fun acc ->
        Running_processes.start state (Tezos_daemon.process state acc)
        >>= fun {process= _; lwt= _} -> return () )
    >>= fun () ->
    List_sequential.iter keys_and_daemons ~f:(fun (_acc, client, kc, daemons) ->
        Tezos_client.wait_for_node_bootstrap state client
        >>= fun () ->
        let key_name = kc.Tezos_client.Keyed.key_name in
        say state
          EF.(
            desc_list
              (haf "Registration-as-delegate:")
              [ desc (af "Client:") (af "%S" client.Tezos_client.id)
              ; desc (af "Key:") (af "%S" key_name) ])
        >>= fun () ->
        Tezos_client.register_as_delegate state client ~key_name
        >>= fun () ->
        say state
          EF.(
            desc_list (haf "Starting daemons:")
              [ desc (af "Client:") (af "%S" client.Tezos_client.id)
              ; desc (af "Key:") (af "%S" key_name) ])
        >>= fun () ->
        List_sequential.iter daemons ~f:(fun daemon ->
            Running_processes.start state (Tezos_daemon.process state daemon)
            >>= fun {process= _; lwt= _} -> return () ) )
  else
    List.fold ~init:(return []) keys_and_daemons
      ~f:(fun prev_m (_acc, client, keyed, _) ->
        prev_m
        >>= fun prev ->
        Tezos_client.wait_for_node_bootstrap state client
        >>= fun () -> return (keyed :: prev) )
    >>= fun clients ->
    Interactive_test.Pauser.add_commands state
      Interactive_test.Commands.[bake_command state ~clients] ;
    return () )
  >>= fun () ->
  let clients = List.map keys_and_daemons ~f:(fun (_, c, _, _) -> c) in
  Helpers.Shell_environment.(
    let path = Paths.root state // "shell.env" in
    let env = build state ~clients in
    write state env ~path >>= fun () -> return (help_command state env ~path))
  >>= fun shell_env_help ->
  Interactive_test.Pauser.add_commands state
    Interactive_test.Commands.(
      shell_env_help :: all_defaults state ~nodes
      @ [secret_keys state ~protocol]
      @ arbitrary_commands_for_each_and_all_clients state ~clients) ;
  match test_kind with
  | `Interactive ->
      Interactive_test.Pauser.generic ~force:true state
        EF.[haf "Sandbox is READY \\o/"]
  | `Random_traffic (`Any, `Until level) ->
      System.sleep 10.
      >>= fun () ->
      Traffic_generation.Random.run state ~protocol ~nodes ~clients
        ~until_level:level `Any
  | `Wait_level (`At_least lvl as opt) ->
      let seconds =
        let multiplier = 10 in
        let tbb =
          protocol.Tezos_protocol.time_between_blocks |> List.hd
          |> Option.value ~default:10 in
        Float.of_int (tbb * multiplier) in
      let attempts = 2 * lvl in
      Test_scenario.Queries.wait_for_all_levels_to_be state ~attempts ~seconds
        nodes opt

let cmd () =
  let open Cmdliner in
  let open Term in
  let pp_error = Test_command_line.Common_errors.pp in
  let base_state =
    Test_command_line.Command_making_state.make ~application_name:"Flextesa"
      ~command_name:"mininet" () in
  let docs = Manpage_builder.section_test_scenario base_state in
  let term =
    pure
      (fun
        test_kind
        (`Clear_root clear_root)
        size
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
        hard_fork
        genesis_block_choice
        generate_kiln_config
        nodes_history_mode_edits
        state
      ->
        let actual_test =
          run state ~size ~base_port ~protocol bnod bcli bak endo accu
            ?hard_fork ~clear_root ~nodes_history_mode_edits ~with_baking
            ?generate_kiln_config ~external_peer_ports ~no_daemons_for
            ~genesis_block_choice test_kind in
        Test_command_line.Run_command.or_hard_fail state ~pp_error
          (Interactive_test.Pauser.run_test ~pp_error state actual_test) )
    $ term_result ~usage:true
        Arg.(
          pure
            Result.(
              fun level_opt random_traffic ->
                match (level_opt, random_traffic) with
                | Some l, None -> return (`Wait_level (`At_least l))
                | None, None -> return `Interactive
                | Some l, Some kind -> return (`Random_traffic (kind, `Until l))
                | None, Some _ ->
                    fail
                      (`Msg
                        "Error: option `--random-traffic` requires also \
                         `--until-level`." ))
          $ value
              (opt (some int) None
                 (info ["until-level"] ~docs
                    ~doc:"Run the sandbox until a given level (not interactive)" ) )
          $ value
              (opt
                 (some (enum [("any", `Any)]))
                 None
                 (info ["random-traffic"] ~docs
                    ~doc:"Generate random traffic (requires `--until-level`)." ) ))
    $ Arg.(
        pure (fun kr -> `Clear_root (not kr))
        $ value
            (flag
               (info ["keep-root"] ~docs
                  ~doc:
                    "Do not erase the root path before starting (this also \
                     makes the sandbox start-up bypass the protocol-activation \
                     step)." ) ))
    $ Arg.(
        value & opt int 5
        & info ["size"; "S"] ~docs ~doc:"Set the size of the network.")
    $ Arg.(
        value & opt int 20_000
        & info ["base-port"; "P"] ~docs ~doc:"Base port number to build upon.")
    $ Arg.(
        pure (fun l -> `External_peers l)
        $ value
            (opt_all int []
               (info ["add-external-peer-port"] ~docv:"PORT-NUMBER" ~docs
                  ~doc:"Add $(docv) to the peers of the network nodes." ) ))
    $ Arg.(
        pure (fun l -> `No_daemons_for l)
        $ value
            (opt_all string []
               (info ["no-daemons-for"] ~docv:"ACCOUNT-NAME" ~docs
                  ~doc:"Do not start daemons for $(docv)." ) ))
    $ Arg.(
        pure (fun x -> `With_baking (not x))
        $ value
            (flag
               (info ["no-baking"] ~docs
                  ~doc:
                    "Completely disable baking/endorsing/accusing (you need to \
                     bake manually to make the chain advance)." ) ))
    $ Tezos_protocol.cli_term base_state
    $ Tezos_executable.cli_term base_state `Node "tezos"
    $ Tezos_executable.cli_term base_state `Client "tezos"
    $ Tezos_executable.cli_term base_state `Baker "tezos"
    $ Tezos_executable.cli_term base_state `Endorser "tezos"
    $ Tezos_executable.cli_term base_state `Accuser "tezos"
    $ Hard_fork.cmdliner_term ~docs base_state ()
    $ Genesis_block_hash.Choice.cmdliner_term ()
    $ Kiln.Configuration_directory.cli_term base_state
    $ Tezos_node.History_modes.cmdliner_term base_state
    $ Test_command_line.Full_default_state.cmdliner_term base_state () in
  let info =
    let doc = "Small network sandbox with bakers, endorsers, and accusers." in
    let man : Manpage.block list =
      Manpage_builder.make base_state
        ~intro_blob:
          "This test builds a small sandbox network, start various daemons, \
           and then gives the user an interactive command prompt to inspect \
           the network."
        [ `P
            "One can also run this sandbox with `--no-baking` to make baking \
             interactive-only."
        ; `P
            "There is also the option of running the sandbox non-interactively \
             for a given number of blocks, cf. `--until-level LEVEL`." ] in
    Cmd.info "mini-network" ~man ~doc in
  Cmd.v info term
