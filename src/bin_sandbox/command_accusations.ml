open Flextesa
open Internal_pervasives
open Console

let default_attempts = 35

let little_mesh_with_bakers ?base_port ?generate_kiln_config state ~protocol
    ~starting_level ~node_exec ~client_exec ~bakers () =
  let* () = Helpers.clear_root state in
  let* () =
    Interactive_test.Pauser.generic
      state
      EF.[af "Ready to start"; af "Root path deleted."]
  in
  let block_interval = 1 in
  let (protocol, baker_list) =
    let open Tezos_protocol in
    let bakers = List.take protocol.bootstrap_accounts bakers in
    let timestamp_delay =
      let year = 3600 * 24 * 365 in
      Some (-year)
    in
    ( {
        protocol with
        time_between_blocks = [block_interval; 0];
        minimal_block_delay = block_interval;
        timestamp_delay;
        bootstrap_accounts =
          List.map protocol.bootstrap_accounts ~f:(fun (n, v) ->
              if List.exists bakers ~f:(fun baker -> Poly.equal n (fst baker))
              then (n, v)
              else (n, 1_000L));
      },
      bakers )
  in
  let net_size = 3 in
  let topology = Test_scenario.Topology.(mesh "Simple" net_size) in
  let all_nodes =
    Test_scenario.Topology.build
      topology
      ?base_port
      ~make_node:(fun id ~expected_connections ~rpc_port ~p2p_port peers ->
        Tezos_node.make
          ~exec:node_exec
          ~protocol
          id
          ~expected_connections
          ~rpc_port
          ~p2p_port
          peers)
  in
  let* () = Helpers.dump_connections state all_nodes in
  Interactive_test.Pauser.add_commands
    state
    Interactive_test.Commands.(
      all_defaults state ~nodes:all_nodes
      @ [secret_keys state ~protocol; Log_recorder.Operations.show_all state]) ;
  let* () =
    Test_scenario.Network.(start_up state ~client_exec (make all_nodes))
  in
  let baker nth_node =
    let nth_baker = nth_node % List.length baker_list in
    let key_name = sprintf "b%d" nth_baker in
    let node = List.nth_exn all_nodes nth_node in
    let client = Tezos_client.of_node node ~exec:client_exec in
    let baker_account = List.nth_exn baker_list nth_baker in
    let bak =
      Tezos_client.Keyed.make
        client
        ~key_name
        ~secret_key:(Tezos_protocol.Account.private_key (fst baker_account))
    in
    let* _ = Tezos_client.Keyed.initialize state bak in
    return (client, bak)
  in
  let* (client_0, baker_0) = baker 0 in
  let* (client_1, baker_1) = baker 1 in
  let* (client_2, baker_2) = baker 2 in
  Interactive_test.Pauser.add_commands
    state
    Interactive_test.Commands.(
      arbitrary_commands_for_each_and_all_clients
        state
        ~clients:[client_0; client_1; client_2]) ;
  let* _ =
    Asynchronous_result.map_option generate_kiln_config ~f:(fun kiln_config ->
        let* chain_id_json =
          Tezos_client.rpc
            state
            ~client:client_0
            `Get
            ~path:"/chains/main/chain_id"
        in
        let network_id =
          match chain_id_json with `String s -> s | _ -> assert false
        in
        let* () =
          Kiln.Configuration_directory.generate
            state
            kiln_config
            ~peers:
              (List.map all_nodes ~f:(fun {Tezos_node.p2p_port; _} -> p2p_port))
            ~sandbox_json:(Tezos_protocol.sandbox_path state protocol)
            ~nodes:
              (List.map all_nodes ~f:(fun {Tezos_node.rpc_port; _} ->
                   sprintf "http://localhost:%d" rpc_port))
            ~bakers:
              (List.map
                 protocol.Tezos_protocol.bootstrap_accounts
                 ~f:(fun (account, _) ->
                   Tezos_protocol.Account.(name account, pubkey_hash account)))
            ~network_string:network_id
            ~node_exec
            ~client_exec
        in
        return EF.(wf "Kiln was configured at `%s`" kiln_config.path))
  in
  let bake msg baker = Tezos_client.Keyed.bake state baker msg in
  let* () =
    List.fold
      (List.init (starting_level - 1) ~f:(fun n -> n))
      ~init:(return ()) (* We are already at level 1, we bake 7 times: *)
      ~f:(fun pm n ->
        let* () = pm in
        Helpers.wait_for
          state
          ~attempts:default_attempts
          ~seconds:8.
          (fun _attempt ->
            Asynchronous_result.bind_on_result
              (bake
                 (sprintf "first bakes: [%d/%d]" (n + 1) (starting_level - 1))
                 baker_0)
              ~f:(function
                | Ok () -> return (`Done ())
                | Error _ -> return (`Not_done "not baked yet"))))
  in
  let* () =
    Test_scenario.Queries.wait_for_all_levels_to_be
      state
      ~attempts:default_attempts
      ~seconds:8.
      all_nodes
      (`Equal_to starting_level)
  in
  let* () =
    Interactive_test.Pauser.generic
      state
      EF.
        [
          af "Clients ready";
          af "Node 0 baked %d times." (starting_level - 1);
          af "All nodes should be at level %d." starting_level;
        ]
  in
  return (all_nodes, client_0, baker_0, client_1, baker_1, client_2, baker_2)

let wait_for_operation_in_mempools state ~nodes:all_nodes ~kind ~client_exec how
    =
  let (init, combine) =
    match how with `At_least_one -> (false, ( || )) | `All -> (true, ( && ))
  in
  Helpers.wait_for state ~attempts:default_attempts ~seconds:8. (fun _ ->
      let* done_ =
        List.fold ~init:(return init) all_nodes ~f:(fun prev_m node ->
            let* prev = prev_m in
            let client = Tezos_client.of_node node ~exec:client_exec in
            let* client_result =
              Tezos_client.mempool_has_operation state ~client ~kind
            in
            return (combine client_result prev))
      in
      if done_ then return (`Done ())
      else
        return
          (`Not_done (sprintf "Waiting for %S to show up in the mempool" kind)))

let simple_double_baking ~starting_level ?generate_kiln_config ~state ~protocol
    ~base_port node_exec client_exec () =
  let* (all_nodes, client_0, baker_0, client_1, baker_1, client_2, baker_2) =
    little_mesh_with_bakers
      ~bakers:1
      ~protocol
      state
      ~node_exec
      ~client_exec
      ()
      ~base_port
      ~starting_level
      ?generate_kiln_config
  in
  let kill_nth nth = List.nth_exn all_nodes nth |> Helpers.kill_node state in
  let restart_nth nth =
    List.nth_exn all_nodes nth |> Helpers.restart_node ~client_exec state
  in
  let number_of_lonely_bakes = 1 in
  let* () = kill_nth 1 in
  let* () = kill_nth 2 in
  (* Bake one block less i.e., (number_of_lonely_bakes - 1) inject an operation
     (here a transfer) to generate a different block hash for the next baking
     command *)
  let* () =
    Loop.n_times (number_of_lonely_bakes - 1) (fun _ ->
        Tezos_client.Keyed.bake state baker_0 "Bake-on-0")
  in
  let transfer client =
    match protocol.Tezos_protocol.bootstrap_accounts with
    | (src, _) :: (dst, _) :: _ ->
        let src_key = Tezos_protocol.Account.pubkey_hash src
        and dst_key = Tezos_protocol.Account.pubkey_hash dst in
        let* _ =
          Tezos_client.successful_client_cmd
            state
            ~client
            [
              "--wait";
              "none";
              "transfer";
              "1";
              "from";
              src_key;
              "to";
              dst_key;
              "--fee";
              "0.05";
            ]
        in
        Asynchronous_result.return ()
    | _ ->
        (* This case should not happen with bootstrap_accounts *)
        Asynchronous_result.return ()
  in
  let* () = transfer client_0 in
  let* () = Tezos_client.Keyed.bake state baker_0 "Bake-on-0" in
  let* baking_0_header =
    Tezos_client.get_block_header state ~client:client_0 `Head
  in
  (* This baking will have better fitness so other nodes will have to fetch it. *)
  let* () =
    Tezos_client.Keyed.endorse state baker_0 "endorsing lonely bake-on-0"
  in
  let* () = System.sleep 1. in
  let* () = kill_nth 0 in
  let* () = restart_nth 1 in
  let* () = restart_nth 2 in
  let* () =
    Loop.n_times number_of_lonely_bakes (fun _ ->
        Tezos_client.Keyed.bake state baker_1 "Bake-on-1")
  in
  let* baking_1_header =
    Tezos_client.get_block_header state ~client:client_1 `Head
  in
  let* () = restart_nth 0 in
  let* () = Tezos_client.Keyed.bake state baker_0 "Bake-on-0" in
  let* () =
    Test_scenario.Queries.wait_for_all_levels_to_be
      state
      ~attempts:default_attempts
      ~seconds:8.
      all_nodes
      (`At_least (starting_level + number_of_lonely_bakes + 1))
  in
  let* head_hash_json =
    Tezos_client.rpc
      state
      ~client:client_1
      `Get
      ~path:"/chains/main/blocks/head/hash"
  in
  let* () =
    Interactive_test.Pauser.generic
      state
      EF.
        [
          af "About to forge";
          ef_json "Baking 0" baking_0_header;
          ef_json "Baking 1" baking_1_header;
          ef_json "Head hash" head_hash_json;
        ]
  in
  let clean header =
    let open Jqo in
    remove_field header ~name:"hash"
    |> remove_field ~name:"chain_id"
    |> remove_field ~name:"protocol"
  in
  let json =
    `O
      [
        ("branch", head_hash_json);
        ( "contents",
          `A
            [
              `O
                [
                  ("kind", `String "double_baking_evidence");
                  ("bh1", clean baking_0_header);
                  ("bh2", clean baking_1_header);
                ];
            ] );
      ]
  in
  let* result = Tezos_client.Keyed.forge_and_inject state baker_1 ~json in
  let next_level = starting_level + number_of_lonely_bakes + 1 in
  let* () =
    Interactive_test.Pauser.generic
      state
      EF.
        [
          af "Waiting for accuser to notice double baking";
          ef_json "Result of injection" result;
          af "All nodes reaching level %d" next_level;
        ]
  in
  let* () =
    wait_for_operation_in_mempools
      state
      ~nodes:all_nodes
      ~kind:"double_baking_evidence"
      ~client_exec
      `All
  in
  let* () =
    Tezos_client.Keyed.bake state baker_2 (sprintf "all at lvl %d" next_level)
  in
  let next_level = next_level + 1 in
  let* () =
    Tezos_client.Keyed.bake state baker_2 (sprintf "all at lvl %d" next_level)
  in
  let last_level = next_level + 1 in
  let* () =
    Interactive_test.Pauser.generic
      state
      EF.[af "Just baked level %d" last_level]
  in
  let* () =
    Test_scenario.Queries.wait_for_all_levels_to_be
      state
      ~attempts:default_attempts
      ~seconds:8.
      all_nodes
      (`At_least last_level)
  in
  let* () =
    Helpers.wait_for state ~attempts:5 ~seconds:3. (fun _ ->
        let* done_ =
          Tezos_client.block_has_operation
            state
            ~client:client_2
            ~level:last_level
            ~kind:"double_baking_evidence"
        in
        if done_ then return (`Done ())
        else
          return
            (`Not_done
              (sprintf
                 "Waiting for accusation to show up in block %d"
                 last_level)))
  in
  say state EF.(af "Test done.")

let find_endorsement_in_mempool state ~client =
  let open Poly in
  Helpers.wait_for state ~attempts:4 ~seconds:2. (fun _ ->
      let* endorsement_opt =
        Tezos_client.find_applied_in_mempool state ~client ~f:(fun o ->
            Jqo.field o ~k:"contents"
            |> Jqo.list_exists ~f:(fun op ->
                   (* Dbg.e EF.(ef_json "op" op) ; *)
                   Jqo.field op ~k:"kind" = `String "endorsement_with_slot"))
      in
      match endorsement_opt with
      | None -> return (`Not_done (sprintf "No endorsement so far"))
      | Some e -> return (`Done e))

let simple_double_endorsement ~starting_level ?generate_kiln_config ~state
    ~protocol ~base_port node_exec client_exec () =
  (* skip for alpha/Tenderbake *)
  match protocol.Tezos_protocol.kind with
  | `Ithaca | `Alpha ->
      (* The same reason as for tests_python/tests_alpha/test_double_endorsement.py applies here *)
      let* () =
        say
          state
          (EF.atom
             "Skipping simple_double_endorsement test: irrelevant to \
              Tenderbake (alpha)")
      in
      Asynchronous_result.return ()
  | _ ->
      let* (all_nodes, client_0, baker_0, client_1, baker_1, client_2, baker_2)
          =
        little_mesh_with_bakers
          ~bakers:2
          ~protocol
          state
          ~node_exec
          ~client_exec
          ()
          ~starting_level
          ~base_port
          ?generate_kiln_config
      in
      (* 2 bakers ⇒ baker_0 and baker_2 are for the same key on ≠ nodes *)
      assert (
        Tezos_client.Keyed.(
          String.equal baker_0.key_name baker_2.key_name
          && String.equal baker_0.secret_key baker_2.secret_key)) ;
      let node_0 = List.nth_exn all_nodes 0 in
      let node_1 = List.nth_exn all_nodes 1 in
      let node_2 = List.nth_exn all_nodes 2 in
      let baker_1_n0 =
        let open Tezos_client.Keyed in
        let {key_name; secret_key; _} = baker_1 in
        make client_0 ~key_name ~secret_key
      in
      let* _ = Tezos_client.Keyed.initialize state baker_1_n0 in
      let* () = Helpers.kill_node state node_1 in
      let* () = Helpers.kill_node state node_2 in
      (* Inject an operation to generate a different block's hash *)
      let* () =
        Tezos_client.Keyed.endorse state baker_0 "endorsing lonely bake-on-0"
      in
      let* () =
        Tezos_client.Keyed.bake state baker_0 "baker-0 baking with node 0"
      in
      let* () =
        Tezos_client.Keyed.endorse state baker_0 "baker-0 endorsing with node 0"
      in
      let* endorsement_0 = find_endorsement_in_mempool state ~client:client_0 in
      let* () =
        Tezos_client.Keyed.endorse
          state
          baker_1_n0
          "baker-1 endorsing with node 0"
      in
      let* () = Helpers.kill_node state node_0 in
      let* () = Helpers.restart_node state node_2 ~client_exec in
      let* () =
        Tezos_client.Keyed.bake state baker_2 "baker-0 baking with node 2"
      in
      let* () =
        Tezos_client.Keyed.endorse state baker_2 "baker-0 endorsing with node 2"
      in
      let* endorsement_1 = find_endorsement_in_mempool state ~client:client_2 in
      let* () =
        say
          state
          EF.(
            list
              [
                ef_json "Endorsement 0:" endorsement_0;
                ef_json "Endorsement 1:" endorsement_1;
              ])
      in
      let* () = Helpers.restart_node state node_1 ~client_exec in
      let* () =
        Test_scenario.Queries.wait_for_all_levels_to_be
          state
          ~attempts:default_attempts
          ~seconds:8.
          [node_1; node_2]
          (`Equal_to (starting_level + 1))
      in
      let* () = Helpers.restart_node state node_0 ~client_exec in
      (* TODO: understand why this kick in the butt is necessary for node
         2 (seems like the node was not getting to level starting+2 without
         this). *)
      let* () = Helpers.kill_node state node_2 in
      let* () = Helpers.restart_node state node_2 ~client_exec in
      let* () =
        Test_scenario.Queries.wait_for_all_levels_to_be
          state
          ~attempts:default_attempts
          ~seconds:8.
          all_nodes
          (`Equal_to (starting_level + 1))
      in
      let* head_hash_json =
        Tezos_client.rpc
          state
          ~client:client_1
          `Get
          ~path:"/chains/main/blocks/head/hash"
      in
      let double_endorsement =
        let transform_endorsement endorsement =
          match Jqo.field ~k:"contents" endorsement with
          | `A [one] -> (Jqo.field ~k:"endorsement" one, Jqo.field ~k:"slot" one)
          | _ -> assert false
        in
        let (inlined_endorsement_1, slot) =
          transform_endorsement endorsement_0
        in
        let (inlined_endorsement_2, _) = transform_endorsement endorsement_1 in
        `O
          [
            ("branch", head_hash_json);
            ( "contents",
              `A
                [
                  `O
                    [
                      ("kind", `String "double_endorsement_evidence");
                      ("op1", inlined_endorsement_1);
                      ("op2", inlined_endorsement_2);
                      ("slot", slot);
                    ];
                ] );
          ]
      in
      let* () =
        Interactive_test.Pauser.generic
          state
          EF.[ef_json "About to forge" double_endorsement]
      in
      let* result =
        Tezos_client.Keyed.forge_and_inject
          state
          baker_1
          ~json:double_endorsement
      in
      let* () =
        Interactive_test.Pauser.generic
          state
          EF.[ef_json "Result of injection" result]
      in
      let* () =
        wait_for_operation_in_mempools
          state
          ~nodes:[node_1]
          ~kind:"double_endorsement_evidence"
          ~client_exec
          `All
      in
      let last_level = starting_level + 2 in
      let* () =
        Tezos_client.Keyed.bake state baker_1 (sprintf "level %d" last_level)
      in
      let* () =
        Tezos_client.Keyed.endorse
          state
          baker_1
          (sprintf "endorse level %d" last_level)
      in
      let* () =
        Test_scenario.Queries.wait_for_all_levels_to_be
          state
          ~attempts:default_attempts
          ~seconds:8.
          all_nodes
          (`Equal_to last_level)
      in
      let* () =
        Helpers.wait_for state ~attempts:10 ~seconds:4. (fun _ ->
            (* We check that client-2 sees the evidence from baker-1 *)
            let* done_ =
              Tezos_client.block_has_operation
                state
                ~client:client_2
                ~level:last_level
                ~kind:"double_endorsement_evidence"
            in
            if done_ then return (`Done ())
            else
              return
                (`Not_done
                  (sprintf
                     "Waiting for accusation to show up in block %d"
                     last_level)))
      in
      say state EF.(af "Test done.")

let with_accusers ~state ~protocol ~base_port node_exec accuser_exec client_exec
    () =
  let* () = Helpers.clear_root state in
  let block_interval = 2 in
  let (protocol, baker_0_account) =
    let d = protocol in
    let open Tezos_protocol in
    let baker = List.hd_exn d.bootstrap_accounts in
    ( {
        d with
        time_between_blocks = [block_interval; block_interval * 2];
        minimal_block_delay = block_interval;
        bootstrap_accounts =
          List.map d.bootstrap_accounts ~f:(fun (n, v) ->
              if Poly.(n = fst baker) then (n, v) else (n, 1_000L));
      },
      baker )
  in
  let topology =
    Test_scenario.Topology.(
      net_in_the_middle "AT-" (mesh "Mid" 3) (mesh "Main" 4) (mesh "Acc" 4))
  in
  let (mesh_nodes, intermediary_nodes, accuser_nodes) =
    Test_scenario.Topology.build
      topology
      ~base_port
      ~make_node:(fun id ~expected_connections ~rpc_port ~p2p_port peers ->
        Tezos_node.make
          ~exec:node_exec
          ~protocol
          id
          ~expected_connections
          ~rpc_port
          ~p2p_port
          peers)
  in
  let all_nodes = mesh_nodes @ intermediary_nodes @ accuser_nodes in
  let* () = Helpers.dump_connections state all_nodes in
  let* () =
    Test_scenario.Network.(start_up state ~client_exec (make all_nodes))
  in
  let start_accuser nod =
    let client = Tezos_client.of_node nod ~exec:client_exec in
    let acc = Tezos_daemon.accuser_of_node ~exec:accuser_exec ~client nod in
    let* _ = Running_processes.start state (Tezos_daemon.process state acc) in
    return ()
  in
  let* () = List_sequential.iter accuser_nodes ~f:start_accuser in
  let key_name = "b0" in
  let baker nth =
    let node = List.nth_exn all_nodes nth in
    let client = Tezos_client.of_node node ~exec:client_exec in
    let bak =
      Tezos_client.Keyed.make
        client
        ~key_name
        ~secret_key:(Tezos_protocol.Account.private_key (fst baker_0_account))
    in
    let* _ = Tezos_client.Keyed.initialize state bak in
    return (client, bak)
  in
  let* (client_0, baker_0) = baker 0 in
  let* (client_1, baker_1) = baker 1 in
  let* (client_2, baker_2) = baker 2 in
  Interactive_test.Pauser.add_commands
    state
    Interactive_test.Commands.(
      all_defaults state ~nodes:all_nodes
      @ [secret_keys state ~protocol; Log_recorder.Operations.show_all state]
      @ arbitrary_commands_for_each_and_all_clients
          state
          ~clients:[client_0; client_1; client_2]) ;
  let pause ?force msgs = Interactive_test.Pauser.generic state ?force msgs in
  let starting_level = 10 in
  let* () =
    List.fold
      (List.init (starting_level - 1) ~f:(fun n -> n))
      ~init:(return ()) (* We are already at level 1, we bake 7 times: *)
      ~f:(fun pm n ->
        let* () = pm in
        Tezos_client.Keyed.bake
          state
          baker_0
          (sprintf "first bakes: [%d/%d]" (n + 1) (starting_level - 1)))
  in
  let* () =
    Test_scenario.Queries.wait_for_all_levels_to_be
      state
      ~attempts:default_attempts
      ~seconds:8.
      all_nodes
      (`Equal_to starting_level)
  in
  let* () =
    pause
      EF.
        [
          af "Two clients ready";
          af "Node 0 baked %d times." (starting_level - 1);
          af "All nodes should be at level %d." starting_level;
        ]
  in
  let transfer _msg client =
    let dest =
      List.random_element_exn protocol.Tezos_protocol.bootstrap_accounts
      |> fst |> Tezos_protocol.Account.pubkey_hash
    in
    let* res =
      Tezos_client.successful_client_cmd
        state
        ~client
        [
          "--wait";
          "none";
          "transfer";
          "1";
          "from";
          key_name;
          "to";
          dest;
          "--fee";
          "0.05";
        ]
    in
    say
      state
      EF.(
        desc
          (af "Successful transfer (%s):" client.Tezos_client.id)
          (ocaml_string_list res#out))
  in
  let* () =
    List_sequential.iter intermediary_nodes ~f:(fun x ->
        Helpers.kill_node state x)
  in
  let kill_all_but nodes iths =
    List_sequential.iteri nodes ~f:(fun ith n ->
        if List.mem iths ith ~equal:Int.equal then return ()
        else Helpers.kill_node state n)
  in
  let kill_nth_node nodes nth =
    Helpers.kill_node
      state
      (Option.value_exn ~message:"kill_nth_node" (List.nth nodes nth))
  in
  let restart_nth_node nodes nth =
    Helpers.restart_node
      state
      ~client_exec
      (Option.value_exn ~message:"restart_nth_node" (List.nth nodes nth))
  in
  let get_block_header ~client block =
    let path =
      sprintf
        "/chains/main/blocks/%s/header"
        (match block with `Head -> "head" | `Level i -> Int.to_string i)
    in
    Tezos_client.rpc state ~client `Get ~path
  in
  let* () = kill_all_but mesh_nodes [0] in
  let number_of_lonely_bakes = 1 in
  let* () = pause EF.[af "Node 0 is the only one alive"] in
  let* () = transfer "node0 only alive" client_0 in
  let* () =
    Loop.n_times number_of_lonely_bakes (fun n ->
        Tezos_client.Keyed.bake state baker_0 (sprintf "n0 only alive: %d" n))
  in
  let* _baking_0_header = get_block_header ~client:client_0 `Head in
  let* () = Tezos_client.Keyed.endorse state baker_0 "self-endorsing" in
  let* () = Tezos_client.Keyed.bake state baker_0 "baking self-endorsement" in
  let* () = kill_nth_node mesh_nodes 0 in
  let* () = restart_nth_node mesh_nodes 1 in
  let* () = transfer "node1 only one alive" client_1 in
  let* () =
    Loop.n_times number_of_lonely_bakes (fun _ ->
        Tezos_client.Keyed.bake state baker_1 "after transfer")
  in
  let* _baking_1_header = get_block_header ~client:client_1 `Head in
  let* () = kill_nth_node mesh_nodes 1 in
  let* () =
    pause
      EF.
        [
          af "Node 0 was killed";
          af "Node 1 was restarted";
          af "Node 1 transferred";
          af "Node 1 baked";
          af "Node 1 was killed";
        ]
  in
  let* () =
    List.fold ~init:(return ()) intermediary_nodes ~f:(fun prev x ->
        let* () = prev in
        Helpers.restart_node state ~client_exec x)
  in
  let node_0 = List.nth_exn mesh_nodes 0 in
  let except_0 l =
    List.filter l ~f:Tezos_node.(fun n -> Poly.(n.id <> node_0.id))
  in
  let* () =
    List_sequential.iter
      (except_0 mesh_nodes)
      ~f:(Helpers.restart_node state ~client_exec)
  in
  let* () = pause EF.[af "All nodes restarted Except 0"] in
  let* () =
    Test_scenario.Queries.wait_for_all_levels_to_be
      state
      ~attempts:default_attempts
      ~seconds:8.
      (except_0 all_nodes)
      (`At_least (starting_level + number_of_lonely_bakes))
  in
  let* () = Helpers.restart_node state ~client_exec node_0 in
  let* () = pause EF.[af "Restarted 0"] in
  let* () =
    Helpers.wait_for state ~attempts:default_attempts ~seconds:8. (fun _ ->
        let* done_ =
          List.fold ~init:(return false) accuser_nodes ~f:(fun prev_m node ->
              let* prev = prev_m in
              let client = Tezos_client.of_node node ~exec:client_exec in
              let* client_result =
                Tezos_client.mempool_has_operation
                  state
                  ~client
                  ~kind:"double_baking_evidence"
              in
              return (client_result || prev))
        in
        if done_ then return (`Done ())
        else
          return
            (`Not_done
              (sprintf "Waiting for accusation to show up in the mempool")))
  in
  let* () =
    Tezos_client.Keyed.bake
      state
      baker_2
      (sprintf "all at lvl %d" (starting_level + number_of_lonely_bakes + 1))
  in
  let* () =
    Helpers.wait_for state ~attempts:10 ~seconds:4. (fun _ ->
        let level = starting_level + number_of_lonely_bakes + 2 in
        let* done_ =
          Tezos_client.block_has_operation
            state
            ~client:client_2
            ~level
            ~kind:"double_baking_evidence"
        in
        if done_ then return (`Done ())
        else
          return
            (`Not_done
              (sprintf "Waiting for accusation to show up in block %d" level)))
  in
  let* () =
    pause
      EF.
        [
          af "One more baking (level should include accusation)";
          af
            "All nodes reaching level %d"
            (starting_level + number_of_lonely_bakes + 2);
        ]
  in
  let* () = Tezos_client.Keyed.bake state baker_1 "a couple more" in
  Test_scenario.Queries.wait_for_all_levels_to_be
    state
    ~attempts:default_attempts
    ~seconds:8.
    all_nodes
    (`At_least (starting_level + number_of_lonely_bakes + 1))

let cmd () =
  let open Cmdliner in
  let open Term in
  let pp_error = Test_command_line.Common_errors.pp in
  let base_state =
    Test_command_line.Command_making_state.make
      ~application_name:"Flextesa"
      ~command_name:"mininet"
      ()
  in
  let docs = Manpage_builder.section_test_scenario base_state in
  let pf fmt = ksprintf (fun s -> `P s) fmt in
  let tests =
    let test variant name title man = (variant, name, title, man) in
    [
      test
        `With_accusers
        "with-accusers"
        "Network With Accusers"
        (pf
           "This test builds a network with 3 interconnected meshes: Main, \
            Intermediate, and Accuser.");
      test
        `Simple_double_baking
        "simple-double-baking"
        "Simple Network With Manual Double Baking Accusation"
        (pf
           "This test builds a very simple 3-piece network, makes a baker \
            double bake and $(i,manually) inserts a double-baking accusation.");
      test
        `Simple_double_endorsing
        "simple-double-endorsing"
        "Simple Network With Manual Double Endorsing Accusation"
        (pf
           "This test builds a very simple 3-piece network, makes a baker \
            double endorse and $(i,manually) inserts a double-endorsement \
            accusation.");
    ]
  in
  let term =
    const
      (fun
        test
        base_port
        (`Starting_level starting_level)
        bnod
        bcli
        accex
        generate_kiln_config
        protocol
        state
      ->
        let checks () =
          let acc = if Poly.(test = `With_accusers) then [accex] else [] in
          Helpers.System_dependencies.precheck
            state
            `Or_fail
            ~executables:(acc @ [bnod; bcli])
        in
        let actual_test () =
          match test with
          | `With_accusers ->
              let* () = checks () in
              with_accusers ~state bnod accex bcli ~base_port () ~protocol
          | `Simple_double_baking ->
              let* () = checks () in
              simple_double_baking
                ~state
                bnod
                bcli
                ~base_port
                ?generate_kiln_config
                ~starting_level
                ~protocol
                ()
          | `Simple_double_endorsing ->
              let* () = checks () in
              simple_double_endorsement
                ~state
                bnod
                bcli
                ~base_port
                ?generate_kiln_config
                ~starting_level
                ~protocol
                ()
        in
        Test_command_line.Run_command.or_hard_fail
          state
          ~pp_error
          (Interactive_test.Pauser.run_test ~pp_error state actual_test))
    $ Arg.(
        required
          (pos
             0
             (some (enum (List.map tests ~f:(fun (v, n, _, _) -> (n, v)))))
             None
             (info [] ~docs ~docv:"TEST-NAME" ~doc:"Choose which test to run.")))
    $ Arg.(
        value & opt int 30_000
        & info ["base-port"] ~docs ~doc:"Base port number to build upon.")
    $ Arg.(
        const (fun l -> `Starting_level l)
        $ value
            (opt
               int
               5
               (info
                  ~docs
                  ["starting-level"]
                  ~doc:
                    "Initial block-level to reach before actually starting the \
                     test.")))
    $ Tezos_executable.cli_term base_state `Node "tezos"
    $ Tezos_executable.cli_term base_state `Client "tezos"
    $ Tezos_executable.cli_term base_state `Accuser "tezos"
    $ Kiln.Configuration_directory.cli_term base_state
    $ Tezos_protocol.cli_term base_state
    $ Test_command_line.cli_state ~name:"accusing" ()
  in
  let info =
    let doc = "Sandbox networks which record double-bakings." in
    let man : Manpage.block list =
      [
        `S "ACCUSATION TESTS";
        pf
          "This command provides %d tests which use network sandboxes to make \
           double-bakings and double-endorsements happen."
          (List.length tests);
        `Blocks
          (List.map tests ~f:(fun (_, n, tit, m) ->
               `Blocks [pf "* $(b,`%s`): $(i,%s)." n tit; `Noblank; m]));
      ]
    in
    info ~man ~doc "accusations"
  in
  (term, info)
