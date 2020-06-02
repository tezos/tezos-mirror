(* Semi-interactive test for voting *)
open Flextesa
open Internal_pervasives
module Counter_log = Helpers.Counter_log

let register_baker state ~client ~alias ~consensus_key ~owner_key ~balance ~src
    =
  Tezos_client.successful_client_cmd
    state
    ~client
    [ "--wait";
      "none";
      "register";
      "baker";
      alias;
      "transferring";
      sprintf "%Ld" balance;
      "from";
      src;
      "with";
      "consensus";
      "key";
      consensus_key;
      "and";
      "threshold";
      "1";
      "and";
      "owner";
      "keys";
      owner_key;
      "--burn-cap";
      "0.312" ]

let ledger_prompt_notice state ef =
  Console.say
    state
    EF.(
      desc
        (shout "Ledger-prompt")
        (list [ef; wf "Please hit “✔” on the ledger."]))

let setup_baking_ledger state uri ~client ~src =
  Interactive_test.Pauser.generic
    state
    EF.
      [ wf "Setting up the ledger device %S" uri;
        haf
          "Please make sure the ledger is on the Baking app and quit (`q`) \
           this prompt to continue." ]
    ~force:true
  >>= fun () ->
  let key_name = "ledgered" in
  let baker_key_name = "ledgered-baker" in
  register_baker
    state
    ~client
    ~alias:baker_key_name
    ~consensus_key:("text:" ^ uri)
    ~owner_key:baker_key_name
    ~balance:2_000_000L
    ~src
  >>= fun res ->
  let re =
    Re.(
      compile
        (seq [str " New contract "; group (rep1 alnum); str " originated."]))
  in
  let matches = Re.exec re (String.concat ~sep:" " res#out) in
  let baker_hash = Re.Group.get matches 1 in
  let baker =
    Tezos_client.Keyed.make client ~key_name ~secret_key:uri ~baker_hash
  in
  Console.say
    state
    EF.(wf "Registered \"%s\" baker %s" baker.baker_name baker_hash)
  >>= fun () ->
  ledger_prompt_notice
    state
    EF.(
      wf
        "Registering baker for %S in client `%s`. The ledger should be \
         prompting for acknowledgment to provide the public key."
        uri
        client.Tezos_client.id)
  >>= fun () ->
  Tezos_client.Keyed.initialize state baker
  >>= fun _ ->
  ledger_prompt_notice
    state
    EF.(
      wf
        "Setting up %S for baking. The ledger should be showing the setup \
         parameters (Address, Main chain, HWMs)."
        uri)
  >>= fun () ->
  Tezos_client.successful_client_cmd
    state
    ~client
    [ "setup";
      "ledger";
      "to";
      "bake";
      "for";
      key_name;
      "--main-hwm";
      "0";
      "--test-hwm";
      "0" ]
  >>= fun _ -> return baker

let failf fmt = ksprintf (fun s -> fail (`Scenario_error s)) fmt

let transfer state ~client ~src ~dst ~amount =
  Tezos_client.successful_client_cmd
    state
    ~client
    [ "--wait";
      "none";
      "transfer";
      sprintf "%Ld" amount;
      "from";
      src;
      "to";
      dst;
      "--fee";
      "0.05";
      "--burn-cap";
      "0.3" ]

let baker_transfer state ~client ~src ~dst ~amount ~owner_key =
  Tezos_client.successful_client_cmd
    state
    ~client
    [ "--wait";
      "none";
      "from";
      "baker";
      "contract";
      src;
      "transfer";
      sprintf "%Ld" amount;
      "to";
      dst;
      "with";
      "key";
      owner_key;
      "--fee";
      "0.05";
      "--burn-cap";
      "1.846" ]

let set_baker_active state ~client ~(keyed : Tezos_client.Keyed.t) =
  match keyed.baker_hash with
  | None ->
      failf "No baker known for Keyed.key_name=%s" keyed.key_name
  | Some _ ->
      (* This command may fail when the baker is already active *)
      Tezos_client.client_cmd
        state
        ~client
        ["--wait"; "none"; "set"; "baker"; keyed.baker_name; "active"]

let bake_until_voting_period ?keep_alive_delegate state ~baker ~attempts period
    =
  let client = baker.Tezos_client.Keyed.client in
  let period_name = Tezos_protocol.Voting_period.to_string period in
  Helpers.wait_for state ~attempts ~seconds:0.5 (fun nth ->
      Tezos_client.rpc
        state
        ~client
        `Get
        ~path:"/chains/main/blocks/head/votes/current_period_kind"
      >>= function
      | `String p when String.equal p period_name ->
          return (`Done (nth - 1))
      | _ ->
          Asynchronous_result.map_option keep_alive_delegate ~f:(fun keyed ->
              set_baker_active state ~client ~keyed)
          >>= fun _ ->
          ksprintf
            (Tezos_client.Keyed.bake state baker)
            "Baker %s bakes %d/%d waiting for %S voting period"
            client.id
            nth
            attempts
            period_name
          >>= fun () ->
          return (`Not_done (sprintf "Waiting for %S period" period_name)))

let check_understood_protocols state ~chain ~client ~protocol_hash
    ~expect_clueless_client =
  Asynchronous_result.bind_on_result
    (Tezos_client.successful_client_cmd
       state
       ~client
       ["--chain"; chain; "list"; "understood"; "protocols"])
    ~f:(function
      | Ok client_protocols_result -> (
        match
          List.find client_protocols_result#out ~f:(fun prefix ->
              String.is_prefix protocol_hash ~prefix)
        with
        | Some _ ->
            return `Proper_understanding
        | None when expect_clueless_client ->
            return `Expected_misunderstanding
        | None ->
            return `Failure_to_understand )
      | Error (`Process_error _) when expect_clueless_client ->
          return `Expected_misunderstanding
      | Error e ->
          fail e)

let run state ~winner_path ~demo_path ~protocol ~node_exec ~client_exec
    ~clueless_winner ~admin_exec ~winner_client_exec ~size ~base_port
    ~serialize_proposals ?with_ledger () =
  let default_attempts = 50 in
  Helpers.clear_root state
  >>= fun () ->
  Helpers.System_dependencies.precheck
    state
    `Or_fail
    ~executables:[node_exec; client_exec; admin_exec; winner_client_exec]
    ~protocol_paths:[winner_path; demo_path]
  >>= fun () ->
  Interactive_test.Pauser.generic
    state
    EF.[af "Ready to start"; af "Root path deleted."]
  >>= fun () ->
  let open Tezos_protocol in
  let (bootstrap_0_account, _balance) =
    List.nth_exn protocol.bootstrap_accounts 0
  in
  let (baker_0_hash, _balance, baker_0_key_account) =
    List.nth_exn protocol.bootstrap_bakers 0
  in
  let protocol = {protocol with time_between_blocks = [1; 0]} in
  Test_scenario.network_with_protocol
    ~protocol
    ~size
    ~base_port
    state
    ~node_exec
    ~client_exec
  >>= fun (nodes, protocol) ->
  let make_admin = Tezos_admin_client.of_client ~exec:admin_exec in
  Interactive_test.Pauser.add_commands
    state
    Interactive_test.Commands.(
      all_defaults state ~nodes
      @ [secret_keys state ~protocol; Log_recorder.Operations.show_all state]
      @ arbitrary_commands_for_each_and_all_clients
          state
          ~make_admin
          ~clients:(List.map nodes ~f:(Tezos_client.of_node ~exec:client_exec))) ;
  Interactive_test.Pauser.generic state EF.[af "About to really start playing"]
  >>= fun () ->
  let client n =
    Tezos_client.of_node ~exec:client_exec (List.nth_exn nodes n)
  in
  let baker_0 =
    Tezos_client.Keyed.make
      (client 0)
      ~baker_hash:baker_0_hash
      ~key_name:"baker-0"
      ~secret_key:(Tezos_protocol.Account.private_key baker_0_key_account)
  in
  Tezos_client.Keyed.initialize state baker_0
  >>= fun _ ->
  let bootstrap_0 =
    Tezos_client.Keyed.make
      (client 0)
      ~key_name:"bootstrap-0"
      ~secret_key:(Tezos_protocol.Account.private_key bootstrap_0_account)
  in
  Tezos_client.Keyed.initialize state bootstrap_0
  >>= fun _ ->
  let level_counter = Counter_log.create () in
  let first_bakes = 5 in
  Loop.n_times first_bakes (fun nth ->
      ksprintf (Tezos_client.Keyed.bake state baker_0) "initial-bake %d" nth)
  >>= fun () ->
  let initial_level = first_bakes + 1 in
  Counter_log.add level_counter "initial_level" initial_level ;
  ( match with_ledger with
  | None ->
      Console.say state EF.(wf "No ledger.")
      >>= fun () ->
      let client = client 0 in
      let key_alias = "special-baker-key" in
      Tezos_client.successful_client_cmd
        state
        ~client
        ["gen"; "keys"; key_alias]
      >>= fun _ ->
      let alias = "special-baker" in
      register_baker
        state
        ~client
        ~alias
        ~consensus_key:key_alias
        ~owner_key:key_alias
        ~balance:2_000_000L
        ~src:baker_0.key_name
      >>= fun res ->
      let re =
        Re.(
          compile
            (seq [str " New contract "; group (rep1 alnum); str " originated."]))
      in
      let matches = Re.exec re (String.concat ~sep:" " res#out) in
      let baker_hash = Re.Group.get matches 1 in
      Console.say state EF.(wf "Registered \"special-baker\" %s" baker_hash)
      >>= fun () ->
      return
      @@ Tezos_client.Keyed.make
           client
           ~baker_hash
           ~key_name:baker_0.key_name
           ~baker_name:alias
           ~secret_key:(Tezos_protocol.Account.private_key baker_0_key_account)
  | Some uri ->
      setup_baking_ledger
        state
        ~client:(client 0)
        ~src:bootstrap_0.key_name
        uri )
  >>= fun special_baker ->
  let winner_client = {baker_0.client with exec = winner_client_exec} in
  let winner_baker_0 =
    let open Tezos_client.Keyed in
    {baker_0 with client = winner_client}
  in
  let winner_special_baker =
    let open Tezos_client.Keyed in
    {special_baker with client = winner_client}
  in
  Interactive_test.Pauser.add_commands
    state
    Interactive_test.Commands.
      [ arbitrary_command_on_all_clients
          state
          ~command_names:["wc"; "winner-client"]
          ?make_admin:None
          ~clients:[winner_client] ] ;
  Interactive_test.Pauser.generic
    state
    EF.[wf "You can now try the new-client"]
  >>= fun () ->
  Interactive_test.Pauser.add_commands
    state
    Interactive_test.Commands.
      [ arbitrary_command_on_all_clients
          state
          ~command_names:["baker"]
          ~make_admin
          ~clients:[special_baker.Tezos_client.Keyed.client] ] ;
  let after_registration_bakes = 2 in
  Loop.n_times after_registration_bakes (fun nth ->
      ksprintf
        (Tezos_client.Keyed.bake state baker_0)
        "after-registration-bake %d"
        nth)
  >>= fun () ->
  Counter_log.add
    level_counter
    "after-registration-bakes"
    after_registration_bakes ;
  Test_scenario.Queries.wait_for_all_levels_to_be
    state
    ~attempts:default_attempts
    ~seconds:8.
    nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  let activation_bakes =
    let open Tezos_protocol in
    protocol.blocks_per_cycle * (protocol.preserved_cycles + 2)
  in
  Loop.n_times activation_bakes (fun nth ->
      ksprintf
        (Tezos_client.Keyed.bake state baker_0)
        "Baking after new delegate registered: %d/%d"
        nth
        activation_bakes
      >>= fun () ->
      Tezos_client.successful_client_cmd
        state
        ~client:(client 0)
        ["rpc"; "get"; "/chains/main/blocks/head/helpers/baking_rights"]
      >>= fun res ->
      Console.say
        state
        EF.(
          desc
            (haf "Baking rights")
            (markdown_verbatim (String.concat ~sep:"\n" res#out))))
  >>= fun () ->
  Counter_log.add level_counter "activation-bakes" activation_bakes ;
  Tezos_client.Keyed.bake state special_baker "Baked by Special Baker™"
  >>= fun () ->
  Counter_log.incr level_counter "special-baker-first-bake" ;
  let attempts =
    Tezos_protocol.(
      (* If we are right after the proposal period, we need to get to
         the next one *)
      3 * protocol.blocks_per_voting_period)
  in
  bake_until_voting_period
    state
    ~baker:special_baker
    ~attempts
    `Proposal
    ~keep_alive_delegate:baker_0
  >>= fun extra_bakes_waiting_for_proposal_period ->
  Counter_log.add
    level_counter
    "wait-for-proposal-period"
    extra_bakes_waiting_for_proposal_period ;
  Test_scenario.Queries.wait_for_all_levels_to_be
    state
    ~attempts:default_attempts
    ~seconds:8.
    nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  let admin_0 = Tezos_admin_client.of_client ~exec:admin_exec (client 0) in
  Tezos_admin_client.successful_command admin_0 state ["list"; "protocols"]
  >>= fun res ->
  let default_protocols = res#out in
  let make_and_inject_protocol ?(make_different = false) name path =
    let tmpdir = Paths.root state // sprintf "protocol-%s" name in
    Console.say state EF.(wf "Injecting protocol from %s" tmpdir)
    >>= fun () ->
    Running_processes.run_successful_cmdf
      state
      "cp -L -R %s %s"
      (Caml.Filename.quote path)
      (Caml.Filename.quote tmpdir)
    >>= fun _ ->
    ( if make_different then
      Running_processes.run_successful_cmdf
        state
        "echo '(* Protocol %s *)' >> %s/main.mli"
        name
        (Caml.Filename.quote tmpdir)
      >>= fun _ -> return ()
    else return () )
    >>= fun () ->
    Tezos_admin_client.inject_protocol admin_0 state ~path:tmpdir
    >>= fun (res, hash) ->
    Interactive_test.Pauser.generic
      state
      EF.
        [ af "Just injected %s (%s): %s" name path hash;
          markdown_verbatim (String.concat ~sep:"\n" res#out) ]
    >>= fun () -> return hash
  in
  make_and_inject_protocol "winner" winner_path
  >>= fun winner_hash ->
  make_and_inject_protocol
    ~make_different:Poly.(winner_path = demo_path)
    "demo"
    demo_path
  >>= fun demo_hash ->
  Tezos_admin_client.successful_command admin_0 state ["list"; "protocols"]
  >>= fun res ->
  let after_injections_protocols = res#out in
  Interactive_test.Pauser.generic
    state
    EF.
      [ af "Network up";
        desc (haf "Protocols")
        @@ list
             (List.map after_injections_protocols ~f:(fun p ->
                  af
                    "`%s` (%s)"
                    p
                    ( if List.mem default_protocols p ~equal:String.equal then
                      "previously known"
                    else
                      match p with
                      | _ when String.equal p winner_hash ->
                          "injected winner"
                      | _ when String.equal p demo_hash ->
                          "injected demo"
                      | _ ->
                          "injected unknown" ))) ]
  >>= fun () ->
  Asynchronous_result.map_option with_ledger ~f:(fun _ ->
      Interactive_test.Pauser.generic
        state
        EF.
          [ af "About to VOTE";
            haf "Please switch to the Wallet app and quit (`q`) this prompt."
          ]
        ~force:true)
  >>= fun (_ : unit option) ->
  let submit_proposals baker props =
    Asynchronous_result.map_option with_ledger ~f:(fun _ ->
        ledger_prompt_notice
          state
          EF.(
            wf
              "Submitting proposal%s: %s"
              (if List.length props = 1 then "" else "s")
              (String.concat ~sep:", " props)))
    >>= fun _ ->
    Tezos_client.Keyed.submit_proposals state baker props
    >>= fun _ -> return ()
  in
  let to_submit_first = [winner_hash; demo_hash] in
  ( match serialize_proposals with
  | false ->
      submit_proposals special_baker to_submit_first
      >>= fun () ->
      Tezos_client.Keyed.bake state baker_0 "bake-submit-proposals"
  | true ->
      List_sequential.iteri to_submit_first ~f:(fun i one ->
          submit_proposals special_baker [one]
          >>= fun () ->
          ksprintf
            (Tezos_client.Keyed.bake state baker_0)
            "bake-submit-proposals-%d"
            i) )
  >>= fun () ->
  Tezos_client.Keyed.submit_proposals state baker_0 [winner_hash]
  >>= fun _ ->
  bake_until_voting_period
    state
    ~baker:baker_0
    ~attempts:protocol.blocks_per_voting_period
    `Testing_vote
    ~keep_alive_delegate:special_baker
  >>= fun extra_bakes_waiting_for_testing_vote_period ->
  Counter_log.add
    level_counter
    "wait-for-testing-vote-period"
    extra_bakes_waiting_for_testing_vote_period ;
  Test_scenario.Queries.wait_for_all_levels_to_be
    state
    ~attempts:default_attempts
    ~seconds:8.
    nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  Helpers.wait_for state ~attempts:default_attempts ~seconds:2. (fun _ ->
      Tezos_client.rpc
        state
        ~client:(client 1)
        `Get
        ~path:"/chains/main/blocks/head/votes/current_proposal"
      >>= fun current_proposal_json ->
      if Poly.(current_proposal_json <> `String winner_hash) then
        return
          (`Not_done
            (sprintf
               "Waiting for current_proposal_json to be %s (%s)"
               winner_hash
               Ezjsonm.(to_string (wrap current_proposal_json))))
      else return (`Done ()))
  >>= fun () ->
  Tezos_client.Keyed.submit_ballot state baker_0 winner_hash 100 0 0
  >>= fun _ ->
  Asynchronous_result.map_option with_ledger ~f:(fun _ ->
      ledger_prompt_notice
        state
        EF.(
          wf "Submitting (yay=100, nay=0, pass=0)) ballot for %S" winner_hash))
  >>= fun (_ : unit option) ->
  Tezos_client.Keyed.bake state baker_0 "bake-submit-ballot-1"
  >>= fun () ->
  Tezos_client.Keyed.submit_ballot state special_baker winner_hash 100 0 0
  >>= fun _ ->
  Interactive_test.Pauser.generic
    state
    EF.[af "Ballots are in (not baked though)"]
  >>= fun () ->
  bake_until_voting_period
    state
    ~baker:baker_0
    ~attempts:(1 + protocol.blocks_per_voting_period)
    ~keep_alive_delegate:special_baker
    `Testing
  >>= fun extra_bakes_waiting_for_testing_period ->
  Counter_log.add
    level_counter
    "wait-for-testing-period"
    extra_bakes_waiting_for_testing_period ;
  Test_scenario.Queries.wait_for_all_levels_to_be
    state
    ~attempts:default_attempts
    ~seconds:8.
    nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  check_understood_protocols
    state
    ~client:winner_client
    ~chain:"main"
    ~protocol_hash:winner_hash
    ~expect_clueless_client:clueless_winner
  >>= (function
        | `Proper_understanding ->
            let chain = "test" in
            Asynchronous_result.map_option with_ledger ~f:(fun _ ->
                Interactive_test.Pauser.generic
                  state
                  EF.
                    [ af "About to bake on the test chain.";
                      haf
                        "Please switch back to the Baking app and quit (`q`) \
                         this prompt." ]
                  ~force:true)
            >>= fun (_ : unit option) ->
            let testing_bakes = 5 in
            Loop.n_times testing_bakes (fun ith ->
                let baker =
                  if ith % 2 = 0 then winner_baker_0 else winner_special_baker
                in
                Tezos_client.Keyed.bake
                  ~chain
                  state
                  baker
                  (sprintf
                     "Baking on the test chain [%d/%d]"
                     (ith + 1)
                     testing_bakes))
            >>= fun () ->
            Test_scenario.Queries.wait_for_all_levels_to_be
              state
              ~chain
              ~attempts:default_attempts
              ~seconds:8.
              nodes
              (`At_least (Counter_log.sum level_counter + testing_bakes))
            >>= fun () ->
            Interactive_test.Pauser.generic
              state
              EF.[wf "Testing period, with proper winner-client, have fun."]
            >>= fun () -> return ()
        | `Expected_misunderstanding ->
            Console.say
              state
              EF.(wf "Winner-Client cannot bake on test chain (expected)")
        | `Failure_to_understand ->
            failf "Winner-Client cannot bake on test chain!")
  >>= fun () ->
  Helpers.wait_for state ~attempts:default_attempts ~seconds:0.3 (fun _ ->
      Tezos_client.rpc
        state
        ~client:(client 1)
        `Get
        ~path:"/chains/main/blocks/head/metadata"
      >>= fun metadata_json ->
      try
        match
          Jqo.field metadata_json ~k:"test_chain_status"
          |> Jqo.field ~k:"protocol"
        with
        | `String s when String.equal s winner_hash ->
            return (`Done ())
        | other ->
            return
              (`Not_done
                (sprintf "Wrong protocol: %s" Ezjsonm.(to_string (wrap other))))
      with e ->
        return
          (`Not_done
            (sprintf
               "Cannot get test-chain protocol: %s → %s"
               (Exn.to_string e)
               Ezjsonm.(to_string (wrap metadata_json)))))
  >>= fun () ->
  bake_until_voting_period
    state
    ~baker:baker_0
    ~attempts:(1 + protocol.blocks_per_voting_period)
    ~keep_alive_delegate:special_baker
    `Promotion_vote
  >>= fun extra_bakes_waiting_for_promotion_period ->
  Counter_log.add
    level_counter
    "wait-for-promotion-period"
    extra_bakes_waiting_for_promotion_period ;
  Test_scenario.Queries.wait_for_all_levels_to_be
    state
    ~attempts:default_attempts
    ~seconds:8.
    nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  Interactive_test.Pauser.generic state EF.[haf "Before ballots"]
  >>= fun () ->
  Tezos_client.Keyed.submit_ballot state baker_0 winner_hash 100 0 0
  >>= fun _ ->
  Asynchronous_result.map_option with_ledger ~f:(fun _ ->
      Interactive_test.Pauser.generic
        state
        EF.
          [ af "About to cast approval ballot.";
            haf
              "Please switch back to the Wallet app and quit (`q`) this prompt."
          ]
        ~force:true
      >>= fun () ->
      ledger_prompt_notice
        state
        EF.(wf "Submitting (yay=100, nay=0, pass=0) ballot for %S" winner_hash))
  >>= fun (_ : unit option) ->
  Tezos_client.Keyed.bake state baker_0 "bake-submit-ballot-2"
  >>= fun () ->
  Tezos_client.Keyed.submit_ballot state special_baker winner_hash 100 0 0
  >>= fun _ ->
  Interactive_test.Pauser.generic
    state
    EF.[af "Final ballot(s) are in (not baked though)"]
  >>= fun () ->
  let ballot_bakes = 1 in
  Loop.n_times ballot_bakes (fun _ ->
      Tezos_client.Keyed.bake state baker_0 "Baking the promotion vote ballots")
  >>= fun () ->
  Counter_log.add level_counter "bake-the-ballots" ballot_bakes ;
  Tezos_client.successful_client_cmd
    state
    ~client:(client 0)
    ["list"; "understood"; "protocols"]
  >>= fun client_protocols_result ->
  Interactive_test.Pauser.generic
    state
    EF.
      [ af "Final ballot(s) are baked in.";
        af
          "The client `%s` understands the following protocols: %s"
          Tezos_executable.(
            Option.value
              ~default:(default_binary client_exec)
              client_exec.binary)
          (String.concat ~sep:", " client_protocols_result#out) ]
  >>= fun () ->
  Loop.n_times protocol.blocks_per_voting_period (fun _ ->
      Tezos_client.Keyed.bake state baker_0 "Baking to Adoption period")
  >>= fun () ->
  Helpers.wait_for
    state
    ~seconds:0.5
    ~attempts:(1 + protocol.blocks_per_voting_period)
    (fun nth ->
      let client = baker_0.client in
      Running_processes.run_successful_cmdf
        state
        "curl http://localhost:%d/chains/main/blocks/head/metadata"
        client.port
      >>= fun curl_res ->
      let json_string = curl_res#out |> String.concat ~sep:"\n" in
      let json_metadata = Ezjsonm.from_string json_string in
      match Jqo.field json_metadata ~k:"next_protocol" with
      | `String p when String.equal p winner_hash ->
          return (`Done (nth - 1))
      | other ->
          transfer
            state
            ~client
            ~amount:1L
            ~src:baker_0.Tezos_client.Keyed.key_name
            ~dst:special_baker.Tezos_client.Keyed.key_name
          >>= fun _ ->
          ksprintf
            (Tezos_client.Keyed.bake state baker_0)
            "Baker %s bakes %d/%d waiting for next protocol: %S"
            client.id
            nth
            attempts
            winner_hash
          >>= fun () ->
          return
            (`Not_done
              (sprintf
                 "Waiting for next_protocol: %S (≠ %s)"
                 winner_hash
                 Ezjsonm.(to_string (wrap other)))))
  >>= fun extra_bakes_waiting_for_next_protocol ->
  Counter_log.add
    level_counter
    "wait-for-next-protocol"
    extra_bakes_waiting_for_next_protocol ;
  check_understood_protocols
    state
    ~client:winner_client
    ~chain:"main"
    ~protocol_hash:winner_hash
    ~expect_clueless_client:clueless_winner
  >>= (function
        | `Expected_misunderstanding ->
            Console.say
              state
              EF.(
                wf "As expected, the client does not know about %s" winner_hash)
        | `Failure_to_understand ->
            failf "The winner-client does not know about `%s`" winner_hash
        | `Proper_understanding -> (
            Console.say state EF.(wf "The client knows about %s" winner_hash)
            >>= fun () ->
            (* This actually depends on the protocol upgrade. *)
            Asynchronous_result.bind_on_result
              (Tezos_client.successful_client_cmd
                 state
                 ~client:winner_client
                 ["upgrade"; "baking"; "state"])
              ~f:(function
                | Ok _ ->
                    return ()
                | Error _ ->
                    Console.say
                      state
                      EF.(
                        desc
                          (shout "Warning")
                          (wf
                             "Command `upgrade baking state` failed, but we \
                              keep going with the baking.")))
            >>= fun () ->
            Asynchronous_result.map_option with_ledger ~f:(fun _ ->
                Interactive_test.Pauser.generic
                  state
                  EF.
                    [ af "About to bake on the new winning protocol.";
                      haf
                        "Please switch to the Baking app and quit (`q`) this \
                         prompt." ]
                  ~force:true
                >>= fun () ->
                Console.say state EF.(wf "Sleeping for a couple of seconds…")
                (* USB thing is often slower than humans hitting `q` *)
                >>= fun () -> System.sleep 4.)
            >>= fun (_ : unit option) ->
            Tezos_client.Keyed.bake
              state
              winner_baker_0
              "First bake on new protocol !!"
            >>= fun () ->
            Counter_log.incr level_counter "baker-0-bakes-on-new-protocol" ;
            Tezos_client.Keyed.bake
              state
              winner_special_baker
              "Second bake on new protocol !!"
            >>= fun () ->
            Counter_log.incr
              level_counter
              "special-baker-bakes-on-new-protocol" ;
            Tezos_client.rpc
              state
              ~client:winner_client
              `Get
              ~path:"/chains/main/blocks/head/metadata"
            >>= fun json_metadata ->
            match Jqo.field json_metadata ~k:"protocol" with
            | `String p when String.equal p winner_hash ->
                return ()
            | other ->
                failf
                  "Protocol is not `%s` but `%s`"
                  winner_hash
                  Ezjsonm.(to_string (wrap other)) ))
  >>= fun () ->
  Interactive_test.Pauser.generic
    state
    EF.
      [ haf "End of the Voting test: SUCCESS \\o/";
        desc
          (af "Estimated level: %d" (Counter_log.sum level_counter))
          (markdown_verbatim (Counter_log.to_table_string level_counter)) ]
  >>= fun () -> return ()

let cmd () =
  let open Cmdliner in
  let open Term in
  let pp_error = Test_command_line.Common_errors.pp in
  let base_state =
    Test_command_line.Command_making_state.make
      ~application_name:"Flextesa"
      ~command_name:"voting"
      ()
  in
  let docs = Manpage_builder.section_test_scenario base_state in
  let term =
    const
      (fun winner_path
           demo_path
           node_exec
           client_exec
           admin_exec
           winner_client_exec
           size
           (`Clueless_winner clueless_winner)
           (`Base_port base_port)
           (`With_ledger with_ledger)
           (`Serialize_proposals serialize_proposals)
           protocol
           state
           ->
        Test_command_line.Run_command.or_hard_fail
          state
          ~pp_error
          (Interactive_test.Pauser.run_test
             state
             ~pp_error
             (run
                state
                ~serialize_proposals
                ~winner_path
                ~clueless_winner
                ~demo_path
                ~node_exec
                ~size
                ~admin_exec
                ~base_port
                ~client_exec
                ~winner_client_exec
                ~protocol
                ?with_ledger)))
    $ Arg.(
        const Caml.Filename.dirname
        $ required
            (pos
               0
               (some string)
               None
               (info
                  ~docs
                  []
                  ~docv:"WINNER-PROTOCOL-PATH"
                  ~doc:
                    "The protocol to inject and make win the election, e.g. \
                     `src/proto_004_Pt24m4xi/lib_protocol/src/TEZOS_PROTOCOL`.")))
    $ Arg.(
        const Caml.Filename.dirname
        $ required
            (pos
               1
               (some string)
               None
               (info
                  ~docs
                  []
                  ~docv:"LOSER-PROTOCOL-PATH"
                  ~doc:
                    "The protocol to inject and down-vote, e.g. \
                     `./src/bin_client/test/proto_test_injection/TEZOS_PROTOCOL` \
                     (if same as `WINNER-PROTOCOL-PATH` the scenario will \
                     make them automatically & artificially different).")))
    $ Tezos_executable.cli_term base_state `Node "current"
    $ Tezos_executable.cli_term base_state `Client "current"
    $ Tezos_executable.cli_term base_state `Admin "current"
    $ Tezos_executable.cli_term base_state `Client "winner"
    $ Arg.(value (opt int 5 (info ["size"; "S"] ~doc:"Size of the Network.")))
    $ Arg.(
        const (fun b -> `Clueless_winner b)
        $ value
            (flag
               (info
                  ~docs
                  ["winning-client-is-clueless"]
                  ~doc:
                    "Do not fail if the client does not know about “next” \
                     protocol.")))
    (*
$ Arg.(
        const (fun p -> `Hash p)
        $ value
            (opt
               (some string)
               None
               (info
                  ["current-hash"]
                  ~doc:"The hash to advertise as the current protocol.")))
 *)
    $ Arg.(
        const (fun p -> `Base_port p)
        $ value
            (opt
               int
               46_000
               (info ~docs ["base-port"] ~doc:"Base port number to build upon.")))
    $ Arg.(
        const (fun x -> `With_ledger x)
        $ value
            (opt
               (some string)
               None
               (info
                  ["with-ledger"]
                  ~docs
                  ~docv:"ledger://..."
                  ~doc:
                    "Do the test with a Ledger Nano device as one of the \
                     bakers/voters.")))
    $ Arg.(
        const (fun x -> `Serialize_proposals x)
        $ value
            (flag
               (info
                  ["serialize-proposals"]
                  ~docs
                  ~doc:
                    "Run the proposals one-by-one instead of all together \
                     (preferred by the Ledger).")))
    $ Tezos_protocol.cli_term base_state
    $ Test_command_line.cli_state ~name:"voting" ()
  in
  let info =
    let doc = "Sandbox network with a full round of voting." in
    let man : Manpage.block list =
      [ `S "VOTING TEST";
        `P
          "This command provides a test which uses a network sandbox to \
           perform a full round of protocol vote and upgrade, including \
           voting and baking on the test chain with or without a Ledger Nano \
           device.";
        `P "There are two main test behaviors:";
        `P
          "* $(b,SIMPLE:) The simple one does as much as possible with any \
           dummy protocol candidates and a Tezos code-base which doesn't \
           handle them: it tests all the voting periods until baking the last \
           block of the currently understood protocol.";
        `Noblank;
        `P
          "To allow the test to succeed in this case, the option \
           `--winning-client-is-clueless` is required; it is meant to signal \
           that the “winner” `tezos-client` executable (from the \
           `--winner-client-binary` option) is expected to not understand the \
           winning protocol.";
        `Noblank;
        `P "This is the version running in Gitlab-CI, see `bin_flextesa/dune`.";
        `P
          "* $(b,FULL:) Without the `--winning-client-is-clueless` option, \
           the test will try to bake on the test chain as well as after the \
           protocol switch (with the winner-client). This requires the \
           winning protocol to be a working one and, of course, the \
           winning-client to understand it.";
        `P
          "The test can run fully automated unless one uses the \
           `\"--with-ledger=ledger://...\"` option in which case some steps \
           have to be interactive. In this case, the option \
           `--serialize-proposals` is recommended, because if it is not \
           provided, the proposal vote will be a “Sign Unverified” \
           operation." ]
    in
    info ~doc ~man "voting"
  in
  (term, info)
