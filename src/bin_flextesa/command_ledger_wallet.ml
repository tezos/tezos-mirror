open Tezos_network_sandbox
open Internal_pervasives

let failf fmt = ksprintf (fun s -> fail (`Scenario_error s)) fmt

let ledger_prompt_notice state ~ef ?(button = `Checkmark) () =
  let button_str =
    match button with
    | `Checkmark -> "✔"
    | `X -> "❌"
    | `Both -> "❌ and ✔ at the same time"
  in
  Console.say state
    EF.(
      desc (shout "Ledger-prompt")
        (list [ef; wf "Press %s on the ledger." button_str]))

let assert_failure state msg f () =
  Console.say state EF.(wf "Asserting %s" msg)
  >>= fun () ->
  Asynchronous_result.bind_on_error
    (f () >>= fun _ -> return `Worked)
    ~f:(fun _ -> return `Didn'tWork)
  >>= function `Worked -> failf "%s" msg | `Didn'tWork -> return ()

let failf fmt = ksprintf (fun s -> fail (`Scenario_error s)) fmt
let assert_ a = if a then return () else failf "Assertion failed"

let assert_eq to_string ~expected ~actual =
  if expected = actual then return ()
  else
    failf "Assertion failed: expected %s but got %s" (to_string expected)
      (to_string actual)

let rec ask state ef =
  Console.say state EF.(list [ef; wf " (y/n)?"])
  >>= fun () ->
  Lwt_exception.catch Lwt_io.read_char Lwt_io.stdin
  >>= function
  | 'y' | 'Y' -> return true | 'n' | 'N' -> return false | _ -> ask state ef

let ask_assert state ef () = ask state ef >>= fun b -> assert_ b

let with_ledger_prompt state message expectation ~f =
  ledger_prompt_notice state ()
    ~button:(match expectation with `Succeeds -> `Checkmark | `Fails -> `X)
    ~ef:
      EF.(
        list
          [ message; wf "\n\n"
          ; wf
              ( match expectation with
              | `Succeeds -> ">> ACCEPT THIS <<"
              | `Fails -> ">> REJECT THIS <<" ) ])
  >>= fun () ->
  match expectation with
  | `Succeeds ->
      f () >>= fun _ -> Console.say state EF.(wf "> Got response: ACCEPTED")
  | `Fails ->
      assert_failure state "expected failure" f ()
      >>= fun () -> Console.say state EF.(wf "> Got response: REJECTED")

let with_ledger_test_reject_and_succeed state ef f =
  with_ledger_prompt state ef `Fails ~f
  >>= fun () -> with_ledger_prompt state ef `Succeeds ~f

let get_chain_id state ~client =
  Tezos_client.rpc state ~client `Get ~path:"/chains/main/chain_id"
  >>= (function
        | `String x -> return x
        | _ -> failf "Failed to parse chain_id JSON from node")
  >>= fun chain_id_string ->
  return (Tezos_crypto.Chain_id.of_b58check_exn chain_id_string)

let get_head_block_hash state ~client () =
  Tezos_client.rpc state ~client `Get ~path:"/chains/main/blocks/head/hash"
  >>= function
  | `String x -> return x
  | _ -> failf "Failed to parse block hash JSON from node"

let forge_batch_transactions state ~client ~src ~dest ~n ?(fee = 0.00126) () =
  get_head_block_hash state ~client ()
  >>= fun branch ->
  let json =
    `O
      [ ("branch", `String branch)
      ; ( "contents"
        , `A
            (List.map (List.range 0 n) ~f:(fun i ->
                 `O
                   [ ("kind", `String "transaction")
                   ; ("source", `String src)
                   ; ( "destination"
                     , `String "tz2KZPgf2rshxNUBXFcTaCemik1LH1v9qz3F" )
                   ; ("amount", `String (string_of_int 100))
                   ; ( "fee"
                     , `String (string_of_int (int_of_float (fee *. 1000000.)))
                     )
                   ; ("counter", `String (string_of_int i))
                   ; ("gas_limit", `String (string_of_int 127))
                   ; ("storage_limit", `String (string_of_int 277)) ] )) ) ]
  in
  Tezos_client.rpc state ~client
    ~path:"/chains/main/blocks/head/helpers/forge/operations"
    (`Post (Ezjsonm.to_string json))
  >>= function
  | `String operation_bytes ->
      let magic_byte = "03" in
      return (magic_byte ^ operation_bytes)
  | _ -> failf "Failed to forge operation or parse result"

let sign state ~client ~bytes () =
  Tezos_client.successful_client_cmd state
    ~client:client.Tezos_client.Keyed.client
    ["sign"; "bytes"; "0x" ^ bytes; "for"; client.Tezos_client.Keyed.key_name]
  >>= fun _ -> return ()

let run state ~node_exec ~client_exec ~admin_exec ~size ~base_port ~uri () =
  Helpers.clear_root state
  >>= fun () ->
  Interactive_test.Pauser.generic state
    EF.[af "Ready to start"; af "Root path deleted."]
  >>= fun () ->
  let ledger_client = Tezos_client.no_node_client ~exec:client_exec in
  Tezos_client.Ledger.show_ledger state ~client:ledger_client ~uri
  >>= fun ledger_account ->
  Test_scenario.network_with_protocol
    ~protocol:(Tezos_protocol.default ())
    ~size ~base_port state ~node_exec ~client_exec
  >>= fun (nodes, protocol) ->
  let make_admin = Tezos_admin_client.of_client ~exec:admin_exec in
  Interactive_test.Pauser.add_commands state
    Interactive_test.Commands.(
      all_defaults state ~nodes
      @ [ secret_keys state ~protocol
        ; Log_recorder.Operations.show_all state
        ; arbitrary_command_on_clients state ~command_names:["all-clients"]
            ~make_admin
            ~clients:
              (List.map nodes ~f:(Tezos_client.of_node ~exec:client_exec)) ]) ;
  Interactive_test.Pauser.generic state EF.[af "About to really start playing"]
  >>= fun () ->
  let client n =
    Tezos_client.of_node ~exec:client_exec (List.nth_exn nodes n)
  in
  let signer =
    Tezos_client.Keyed.make (client 0) ~key_name:"ledgered" ~secret_key:uri
  in
  Tezos_client.Ledger.show_ledger state ~client:(client 0) ~uri
  >>= fun ledger_account ->
  with_ledger_test_reject_and_succeed state
    EF.(
      wf
        "Importing %S in client `%s`. The ledger should be prompting for \
         acknowledgment to provide the public key of %s"
        uri (client 0).Tezos_client.id
        (Tezos_protocol.Account.pubkey_hash ledger_account))
    (fun () ->
      Tezos_client.Keyed.initialize state signer >>= fun _ -> return () )
  >>= fun _ ->
  forge_batch_transactions state ~client:(client 0)
    ~src:(Tezos_protocol.Account.pubkey_hash ledger_account)
    ~dest:"tz2KZPgf2rshxNUBXFcTaCemik1LH1v9qz3F" ~n:50 ()
  >>= fun batch_transaction_bytes ->
  with_ledger_test_reject_and_succeed state
    EF.(
      wf
        "Signing batch of transaction: Unrecognized Operation - Sign Unverified")
    (sign state ~client:signer ~bytes:batch_transaction_bytes)

let cmd ~pp_error () =
  let open Cmdliner in
  let open Term in
  Test_command_line.Run_command.make ~pp_error
    ( pure
        (fun uri
        node_exec
        client_exec
        admin_exec
        size
        (`Base_port base_port)
        state
        ->
          ( state
          , Interactive_test.Pauser.run_test ~pp_error state
              (run state ~node_exec ~size ~admin_exec ~base_port ~client_exec
                 ~uri) ) )
    $ Arg.(
        required
          (pos 0 (some string) None
             (info [] ~docv:"LEDGER-URI" ~doc:"ledger:// URI")))
    $ Tezos_executable.cli_term `Node "tezos"
    $ Tezos_executable.cli_term `Client "tezos"
    $ Tezos_executable.cli_term `Admin "tezos"
    $ Arg.(value (opt int 5 (info ["size"; "S"] ~doc:"Size of the Network")))
    $ Arg.(
        pure (fun p -> `Base_port p)
        $ value
            (opt int 46_000
               (info ["base-port"; "P"] ~doc:"Base port number to build upon")))
    $ Test_command_line.cli_state ~name:"ledger-wallet" () )
    (let doc = "Interactive test exercising the Ledger Wallet app features" in
     info ~doc "ledger-wallet")
