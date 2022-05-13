open Flextesa
open Internal_pervasives

let failf fmt = ksprintf (fun s -> fail (`Scenario_error s)) fmt

let ledger_prompt_notice state ~ef ?(button = `Checkmark) () =
  let button_str =
    match button with
    | `Checkmark -> "✔"
    | `X -> "❌"
    | `Both -> "❌ and ✔ at the same time"
  in
  Console.say
    state
    EF.(
      desc
        (shout "Ledger-prompt")
        (list [ef; wf "Press %s on the ledger." button_str]))

let assert_failure state msg f =
  let* () = Console.say state EF.(wf "Asserting %s" msg) in
  let* result =
    Asynchronous_result.bind_on_error
      (let* _ = f () in
       return `Worked)
      ~f:(fun ~result:_ _ -> return `Didn'tWork)
  in
  match result with `Worked -> failf "%s" msg | `Didn'tWork -> return ()

let failf fmt = ksprintf (fun s -> fail (`Scenario_error s)) fmt

let assert_ a = if a then return () else failf "Assertion failed"

let assert_eq to_string ~expected ~actual =
  if Poly.equal expected actual then return ()
  else
    failf
      "Assertion failed: expected %s but got %s"
      (to_string expected)
      (to_string actual)

let rec ask state ef =
  let* () = Console.say state EF.(list [ef; wf " (y/n)?"]) in
  let* char = System_error.catch Lwt_io.read_char Lwt_io.stdin in
  match char with
  | 'y' | 'Y' -> return true
  | 'n' | 'N' -> return false
  | _ -> ask state ef

let ask_assert state ef =
  let* asked = ask state ef in
  assert_ asked

let with_ledger_prompt state message expectation ~f =
  let* () =
    ledger_prompt_notice
      state
      ()
      ~button:(match expectation with `Succeeds -> `Checkmark | `Fails -> `X)
      ~ef:
        EF.(
          list
            [
              message;
              wf "\n\n";
              wf
                (match expectation with
                | `Succeeds -> ">> ACCEPT THIS <<"
                | `Fails -> ">> REJECT THIS <<");
            ])
  in
  match expectation with
  | `Succeeds ->
      let* _ = f () in
      Console.say state EF.(wf "> Got response: ACCEPTED")
  | `Fails ->
      let* () = assert_failure state "expected failure" f in
      Console.say state EF.(wf "> Got response: REJECTED")

let with_ledger_test_reject_and_succeed state ef f =
  let* () = with_ledger_prompt state ef `Fails ~f in
  with_ledger_prompt state ef `Succeeds ~f

let assert_hwms state ~client ~uri ~main ~test =
  let* () =
    Console.say
      state
      EF.(wf "Asserting main HWM = %d and test HWM = %d" main test)
  in
  let* {main = main_actual; test = test_actual; _} =
    Tezos_client.Ledger.get_hwm state ~client ~uri
  in
  let* () = assert_eq Int.to_string ~actual:main_actual ~expected:main in
  assert_eq Int.to_string ~actual:test_actual ~expected:test

let get_chain_id_string state ~client =
  let* json =
    Tezos_client.rpc state ~client `Get ~path:"/chains/main/chain_id"
  in
  match json with
  | `String x -> return x
  | _ -> failf "Failed to parse chain_id JSON from node"

let get_chain_id state ~client =
  let* chain_id_string = get_chain_id_string state ~client in
  return (Tezos_crypto.Chain_id.of_b58check_exn chain_id_string)

let get_head_block_hash state ~client () =
  let* json =
    Tezos_client.rpc state ~client `Get ~path:"/chains/main/blocks/head/hash"
  in
  match json with
  | `String x -> return x
  | _ -> failf "Failed to parse block hash JSON from node"

let forge_endorsement state ~client ~chain_id ~level =
  let* branch = get_head_block_hash state ~client () in
  let json =
    `O
      [
        ("branch", `String branch);
        ( "contents",
          `A
            [
              `O
                [
                  ("kind", `String "endorsement");
                  ("level", `Float (Float.of_int level));
                ];
            ] );
      ]
  in
  let* json =
    Tezos_client.rpc
      state
      ~client
      ~path:"/chains/main/blocks/head/helpers/forge/operations"
      (`Post (Ezjsonm.to_string json))
  in
  match json with
  | `String operation_bytes ->
      let endorsement_magic_byte = "02" in
      return
        (endorsement_magic_byte
        ^ (chain_id |> Tezos_crypto.Chain_id.to_hex |> Hex.show)
        ^ operation_bytes)
  | _ -> failf "Failed to forge operation or parse result"

let forge_delegation state ~client ~src ~dest ?(fee = 0.00126) () =
  let* branch = get_head_block_hash state ~client () in
  let json =
    `O
      [
        ("branch", `String branch);
        ( "contents",
          `A
            [
              `O
                [
                  ("kind", `String "delegation");
                  ("source", `String src);
                  ( "fee",
                    `String (Int.to_string (Int.of_float (fee *. 1000000.))) );
                  ("counter", `String (Int.to_string 30713));
                  ("gas_limit", `String (Int.to_string 10100));
                  ("delegate", `String dest);
                  ("storage_limit", `String (Int.to_string 277));
                ];
            ] );
      ]
  in
  let* json =
    Tezos_client.rpc
      state
      ~client
      ~path:"/chains/main/blocks/head/helpers/forge/operations"
      (`Post (Ezjsonm.to_string json))
  in
  match json with
  | `String operation_bytes ->
      let magic_byte = "03" in
      return (magic_byte ^ operation_bytes)
  | _ -> failf "Failed to forge operation or parse result"

let sign state ~client ~bytes () =
  let* _ =
    Tezos_client.successful_client_cmd
      state
      ~client:client.Tezos_client.Keyed.client
      ["sign"; "bytes"; "0x" ^ bytes; "for"; client.Tezos_client.Keyed.key_name]
  in
  return ()

let originate_account_from state ~client ~account =
  let orig_account_name = "-originated-account" in
  let id_script parameter =
    Fmt.str
      "parameter %s;\n\
       storage %s;\n\
       code\n\
      \  {\n\
      \    %s\n\
      \    { CAR; NIL operation; PAIR }\n\
      \  };\n"
      parameter
      parameter
      "# No push-drops"
  in
  let tmp = Caml.Filename.temp_file "little-id-script" ".tz" in
  let* () = System.write_file state tmp ~content:(id_script "unit") in
  let* _ =
    Tezos_client.successful_client_cmd
      state
      ~client
      [
        "originate";
        "contract";
        orig_account_name;
        "transferring";
        Int.to_string 1000;
        "from";
        Tezos_protocol.Account.pubkey_hash account;
        "running";
        tmp;
        "--init";
        "Unit";
        "--burn-cap";
        Float.to_string 0.300;
      ]
  in
  return orig_account_name

let setup_baking_ledger state uri ~client ~protocol =
  let* () = Console.say state EF.(wf "Setting up the ledger device %S" uri) in
  let key_name = "ledgered" in
  let baker = Tezos_client.Keyed.make client ~key_name ~secret_key:uri in
  let assert_baking_key x =
    let to_string = function Some x -> x | None -> "<none>" in
    let* () =
      Console.say
        state
        EF.(wf "Asserting that the authorized key is %s" (to_string x))
    in
    let* auth_key = Tezos_client.Ledger.get_authorized_key state ~client ~uri in
    assert_eq to_string ~expected:x ~actual:auth_key
  in
  let* () =
    Tezos_client.Ledger.deauthorize_baking state ~client ~uri
    (* TODO: The following assertion doesn't confirm anything if the ledger was already not authorized to bake. *)
  in
  let* () = assert_baking_key None in
  let* account = Tezos_client.Ledger.show_ledger state ~client ~uri in
  let* () =
    with_ledger_test_reject_and_succeed
      state
      EF.(
        wf
          "Importing %S in client `%s`. The ledger should be prompting for \
           acknowledgment to provide the public key of %s"
          uri
          client.Tezos_client.id
          (Tezos_protocol.Account.pubkey_hash account))
      (fun () ->
        let* _ = Tezos_client.Keyed.initialize state baker in
        return ())
  in
  let* () =
    assert_failure state "baking before setup should fail" (fun () ->
        Tezos_client.Keyed.bake state baker "Baked by ledger")
  in
  let* () =
    assert_failure state "endorsing before setup should fail" (fun () ->
        Tezos_client.Keyed.endorse state baker "Endorsed by ledger")
  in
  let test_invalid_delegations () =
    let ledger_pkh = Tezos_protocol.Account.pubkey_hash account in
    let other_pkh =
      Tezos_protocol.Account.pubkey_hash
        (fst (List.last_exn protocol.Tezos_protocol.bootstrap_accounts))
    in
    let cases =
      [
        (ledger_pkh, other_pkh, "ledger to another account");
        (other_pkh, ledger_pkh, "another account to ledger");
        (other_pkh, other_pkh, "another account to another account");
      ]
    in
    List_sequential.iter cases ~f:(fun (src, dest, msg) ->
        let* forged_delegation_bytes =
          forge_delegation state ~client ~src ~dest ()
        in
        assert_failure
          state
          (sprintf
             "signing a delegation from %s (%s to %s) should fail"
             msg
             src
             dest)
          (sign state ~client:baker ~bytes:forged_delegation_bytes))
  in
  let* () = test_invalid_delegations () in
  let* cid = get_chain_id_string state ~client in
  let* () =
    with_ledger_test_reject_and_succeed
      state
      EF.(
        list
          [
            wf "Setting up %S for baking" uri;
            wf "Setup Baking?";
            wf "Address: %S\n" (Tezos_protocol.Account.pubkey_hash account);
            wf "Chain: %S" cid;
            wf "Main Chain HWM: 0";
            wf "Test Chain HWM: 0";
          ])
      (fun () ->
        Tezos_client.successful_client_cmd
          state
          ~client
          [
            "setup";
            "ledger";
            "to";
            "bake";
            "for";
            key_name;
            "--main-hwm";
            "0";
            "--test-hwm";
            "0";
          ])
  in
  let* () = assert_baking_key (Some uri) in
  let* () = test_invalid_delegations () in
  return (baker, account)

let run state ~protocol ~node_exec ~client_exec ~admin_exec ~size ~base_port
    ~uri ~enable_deterministic_nonce_tests () =
  let* () = Helpers.clear_root state in
  let* () =
    Interactive_test.Pauser.generic
      state
      EF.[af "Ready to start"; af "Root path deleted."]
  in
  let ledger_client = Tezos_client.no_node_client ~exec:client_exec in
  let* ledger_account =
    Tezos_client.Ledger.show_ledger state ~client:ledger_client ~uri
  in
  let protocol =
    let open Tezos_protocol in
    {
      protocol with
      time_between_blocks = [1; 2];
      minimal_block_delay = 1;
      bootstrap_accounts =
        (ledger_account, 1_000_000_000_000L) :: protocol.bootstrap_accounts;
    }
  in
  let other_baker_account =
    fst (List.nth_exn protocol.Tezos_protocol.bootstrap_accounts 1)
  in
  let* nodes, protocol =
    Test_scenario.network_with_protocol
      ~protocol
      ~size
      ~base_port
      state
      ~node_exec
      ~client_exec
  in
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
  let* () =
    Interactive_test.Pauser.generic
      state
      EF.[af "About to really start playing"]
  in
  let client n =
    Tezos_client.of_node ~exec:client_exec (List.nth_exn nodes n)
  in
  let* _ =
    Tezos_client.successful_client_cmd
      state
      ~client:(client 0)
      Tezos_protocol.Account.
        [
          "import";
          "secret";
          "key";
          name other_baker_account;
          private_key other_baker_account;
        ]
  in
  let* _ =
    Tezos_client.successful_client_cmd
      state
      ~client:(client 0)
      Tezos_protocol.Account.["bake"; "for"; name other_baker_account]
  in
  let assert_hwms_ ~main ~test =
    assert_hwms state ~client:(client 0) ~uri ~main ~test
  in
  let set_hwm_ level =
    with_ledger_prompt
      state
      EF.(list [wf "Reset HWM"; wf "%d" level])
      `Succeeds
      ~f:(fun () ->
        Tezos_client.Ledger.set_hwm state ~client:(client 0) ~uri ~level)
  in
  let* chain_id = get_chain_id state ~client:(client 0) in
  let* baker, ledger_account =
    setup_baking_ledger state uri ~client:(client 0) ~protocol
  in
  Interactive_test.Pauser.add_commands
    state
    Interactive_test.Commands.
      [
        arbitrary_command_on_all_clients
          state
          ~command_names:["baker"]
          ~make_admin
          ~clients:[baker.Tezos_client.Keyed.client];
      ] ;
  let bake () = Tezos_client.Keyed.bake state baker "Baked by ledger" in
  let endorse () =
    Tezos_client.Keyed.endorse state baker "Endorsed by ledger"
  in
  let* cid = get_chain_id_string state ~client:(client 0) in
  let ask_hwm ~main ~test =
    let* () = assert_hwms_ ~main ~test in
    ask_assert
      state
      EF.(wf "Is 'Chain' = %S and 'Last Block Level' = %d" cid main)
  in
  let* () =
    if enable_deterministic_nonce_tests then
      (* Test determinism of nonces *)
      let* thisNonce1 = Tezos_client.Keyed.generate_nonce state baker "this" in
      let* thatNonce1 = Tezos_client.Keyed.generate_nonce state baker "that" in
      let* thisNonce2 = Tezos_client.Keyed.generate_nonce state baker "this" in
      let* thatNonce2 = Tezos_client.Keyed.generate_nonce state baker "that" in
      let* () =
        assert_eq (fun x -> x) ~expected:thisNonce1 ~actual:thisNonce2
      in
      let* () =
        assert_eq (fun x -> x) ~expected:thatNonce1 ~actual:thatNonce2
      in
      assert_ Poly.(thisNonce1 <> thatNonce1)
    else return ()
  in
  let* () =
    assert_failure
      state
      "originating an account from the Tezos Baking app should fail"
      (fun () ->
        let* _ =
          originate_account_from
            state
            ~client:(client 0)
            ~account:ledger_account
        in
        return ())
  in
  let fee = 0.00126 in
  let ledger_pkh = Tezos_protocol.Account.pubkey_hash ledger_account in
  let* forged_delegation_bytes =
    forge_delegation
      state
      ~client:(client 0)
      ()
      ~src:ledger_pkh
      ~dest:ledger_pkh
      ~fee
  in
  let* () =
    with_ledger_test_reject_and_succeed
      state
      EF.(
        list
          [
            wf "Register as delegate?";
            wf "Address %s" ledger_pkh;
            wf "Fee %f" fee;
          ])
      (sign state ~client:baker ~bytes:forged_delegation_bytes)
  in
  let* () = bake () in
  let* () = ask_hwm ~main:3 ~test:0 in
  let* () =
    let level = 1 in
    with_ledger_test_reject_and_succeed
      state
      EF.(list [wf "Reset HWM"; wf "%d" level])
      (fun () ->
        Tezos_client.Ledger.set_hwm state ~client:(client 0) ~uri ~level)
  in
  let* () = assert_hwms_ ~main:1 ~test:1 in
  let* () = bake () in
  let* () = assert_hwms_ ~main:4 ~test:1 in
  let* () = set_hwm_ 5 in
  let* () = assert_hwms_ ~main:5 ~test:5 in
  let* () =
    assert_failure state "endorsing a level beneath HWM should fail" endorse
  in
  let* () =
    assert_failure state "baking a level beneath HWM should fail" bake
  in
  let* () = set_hwm_ 4 in
  let* () = bake () in
  let* () = assert_hwms_ ~main:5 ~test:4 in
  let* () = endorse () (* does not increase level since we just baked *) in
  let* () =
    assert_failure state "endorsing same block twice should not work" endorse
  in
  let* () = assert_hwms_ ~main:5 ~test:4 in
  let* () = bake () in
  let* () = assert_hwms_ ~main:6 ~test:4 in
  let* endorsement_at_low_level_bytes =
    forge_endorsement state ~client:baker.client ~chain_id ~level:1
  in
  let* () =
    assert_failure
      state
      "endorsing-after-baking a level beneath HWM should fail"
      (sign state ~client:baker ~bytes:endorsement_at_low_level_bytes)
  in
  let* () = assert_hwms_ ~main:6 ~test:4 (* HWM has not changed *) in
  let* () = endorse () (* HWM still has not changed *) in
  let* () =
    assert_hwms_ ~main:6 ~test:4 (* Forge an endorsement on a different chain *)
  in
  let other_chain_id = "NetXSzLHKwSumh7" in
  let* () =
    Console.say
      state
      EF.(
        wf
          "Signing a forged endorsement on a different chain: %s"
          other_chain_id)
  in
  let* endorsement_on_different_chain_bytes =
    forge_endorsement
      state
      ~client:baker.client
      ~chain_id:(Tezos_crypto.Chain_id.of_b58check_exn other_chain_id)
      ~level:5
  in
  let* () =
    sign state ~client:baker ~bytes:endorsement_on_different_chain_bytes ()
    (* Only the test HWM has changed *)
  in
  let* () = assert_hwms_ ~main:6 ~test:5 in
  let* () = Loop.n_times 5 (fun _ -> bake ()) in
  let* () = ask_hwm ~main:11 ~test:5 in
  let* () =
    Tezos_client.Ledger.deauthorize_baking state ~client:(client 0) ~uri
  in
  let* () =
    assert_failure state "baking after deauthorization should fail" bake
  in
  assert_failure state "endorsing after deauthorization should fail" endorse

let cmd () =
  let open Cmdliner in
  let open Term in
  let pp_error = Test_command_line.Common_errors.pp in
  let base_state =
    Test_command_line.Command_making_state.make
      ~application_name:"Flextesa"
      ~command_name:"ledger-baking"
      ()
  in
  let docs = Manpage_builder.section_test_scenario base_state in
  let term =
    const
      (fun
        uri
        node_exec
        client_exec
        admin_exec
        size
        (`Base_port base_port)
        no_deterministic_nonce_tests
        protocol
        state
      ->
        Test_command_line.Run_command.or_hard_fail
          state
          ~pp_error
          (Interactive_test.Pauser.run_test
             ~pp_error
             state
             (run
                state
                ~protocol
                ~node_exec
                ~size
                ~admin_exec
                ~base_port
                ~client_exec
                ~enable_deterministic_nonce_tests:
                  (not no_deterministic_nonce_tests)
                ~uri)))
    $ Arg.(
        required
          (pos
             0
             (some string)
             None
             (info [] ~docv:"LEDGER-URI" ~doc:"ledger:// URI")))
    $ Tezos_executable.cli_term base_state `Node "tezos"
    $ Tezos_executable.cli_term base_state `Client "tezos"
    $ Tezos_executable.cli_term base_state `Admin "tezos"
    $ Arg.(
        value (opt int 5 (info ["size"; "S"] ~docs ~doc:"Size of the Network")))
    $ Arg.(
        const (fun p -> `Base_port p)
        $ value
            (opt
               int
               46_000
               (info
                  ["base-port"; "P"]
                  ~docs
                  ~doc:"Base port number to build upon")))
    $ Arg.(
        value
          (flag
             (info
                ["no-deterministic-nonce-tests"]
                ~docs
                ~doc:"Disable tests for deterministic nonces")))
    $ Tezos_protocol.cli_term base_state
    $ Test_command_line.cli_state ~name:"ledger-baking" ()
  in
  let info =
    let doc = "Interactive test exercising the Ledger Baking app features" in
    Cmd.info ~doc "ledger-baking"
  in
  Cmd.v info term
