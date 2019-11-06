open Tezos_network_sandbox
open Internal_pervasives

let failf fmt = ksprintf (fun s -> fail (`Scenario_error s)) fmt

let client_async_cmd state ~client args ~f =
  Running_processes.run_async_cmdf
    state
    f
    "sh -c %s"
    ( Tezos_client.client_command client ~state args
    |> Genspio.Compile.to_one_liner |> Filename.quote )
  >>= fun (status, res) -> return (status = Lwt_unix.WEXITED 0, res)

let ledger_hash_re () =
  Re.(
    compile
      (seq
         [ str "* Blake 2B Hash (ledger-style, with operation watermark):";
           rep1 (alt [space; eol]);
           group (rep1 alnum);
           rep1 (alt [space; eol]) ]))

(* Searches a stream for an expected ledger hash from `tezos-client --verbose-signing`*)
let find_and_print_signature_hash state stream =
  let re = ledger_hash_re () in
  let check lines =
    Re.(
      match exec_opt re lines with
      | None ->
          None
      | Some matches ->
          Some (Group.get matches 1))
  in
  Asynchronous_result.Stream.fold
    (Lwt_io.read_lines stream)
    ~init:("", false)
    ~f:(fun (all_output_prev, showed_message_prev) line ->
      let all_output = all_output_prev ^ "\n" ^ line in
      ( if not showed_message_prev then
        match check all_output with
        | None ->
            return false
        | Some x ->
            Console.say state EF.(wf "Hash should be: %s" x)
            >>= fun () -> return true
      else return true )
      >>= fun showed_message -> return (all_output, showed_message))
  >>= fun (output, _) -> return output

let ledger_prompt_notice state ~ef ?(button = `Checkmark) () =
  let button_str =
    match button with
    | `Checkmark ->
        "✔"
    | `X ->
        "❌"
    | `Both ->
        "❌ and ✔ at the same time"
  in
  Console.say
    state
    EF.(
      desc
        (shout "Ledger-prompt")
        (list [ef; wf "Press %s on the ledger." button_str]))

let ledger_prompt_notice_expectation state message expectation =
  ledger_prompt_notice
    state
    ()
    ~button:(match expectation with `Succeeds -> `Checkmark | `Fails -> `X)
    ~ef:
      EF.(
        list
          [ message;
            wf "\n\n";
            wf
              ( match expectation with
              | `Succeeds ->
                  ">> ACCEPT THIS <<"
              | `Fails ->
                  ">> REJECT THIS <<" ) ])

let run_with_status f =
  Asynchronous_result.bind_on_error
    (f () >>= fun x -> return (`Worked x))
    ~f:(fun ~result x -> return (`Didn'tWork x))

let assert_failure state msg f () =
  Console.say state EF.(wf "Asserting %s" msg)
  >>= fun () ->
  run_with_status f
  >>= function `Worked _ -> failf "%s" msg | `Didn'tWork x -> return x

let failf fmt = ksprintf (fun s -> fail (`Scenario_error s)) fmt

let assert_ a = if a then return () else failf "Assertion failed"

let assert_eq to_string ~expected ~actual =
  if expected = actual then return ()
  else
    failf
      "Assertion failed: expected %s but got %s"
      (to_string expected)
      (to_string actual)

let rec ask state ef =
  Console.say state EF.(list [ef; wf " (y/n)?"])
  >>= fun () ->
  Lwt_exception.catch Lwt_io.read_char Lwt_io.stdin
  >>= function
  | 'y' | 'Y' -> return true | 'n' | 'N' -> return false | _ -> ask state ef

let ask_assert state ef () = ask state ef >>= fun b -> assert_ b

let with_ledger_prompt state message expectation ~f =
  ledger_prompt_notice_expectation state message expectation
  >>= fun () ->
  match expectation with
  | `Succeeds ->
      f () >>= fun _ -> Console.say state EF.(wf "> Got response: ACCEPTED")
  | `Fails ->
      assert_failure state "expected failure" f ()
      >>= fun _ -> Console.say state EF.(wf "> Got response: REJECTED")

let with_ledger_test_reject_and_succeed state ef f =
  with_ledger_prompt state ef `Fails ~f
  >>= fun () -> with_ledger_prompt state ef `Succeeds ~f

let get_chain_id state ~client =
  Tezos_client.rpc state ~client `Get ~path:"/chains/main/chain_id"
  >>= (function
        | `String x ->
            return x
        | _ ->
            failf "Failed to parse chain_id JSON from node")
  >>= fun chain_id_string ->
  return (Tezos_crypto.Chain_id.of_b58check_exn chain_id_string)

let get_head_block_hash state ~client () =
  Tezos_client.rpc state ~client `Get ~path:"/chains/main/blocks/head/hash"
  >>= function
  | `String x ->
      return x
  | _ ->
      failf "Failed to parse block hash JSON from node"

let forge_batch_transactions state ~client ~src ~dest ~n ?(fee = 0.00126) () =
  get_head_block_hash state ~client ()
  >>= fun branch ->
  let json =
    `O
      [ ("branch", `String branch);
        ( "contents",
          `A
            (List.map (List.range 0 n) ~f:(fun i ->
                 `O
                   [ ("kind", `String "transaction");
                     ("source", `String src);
                     ( "destination",
                       `String "tz2KZPgf2rshxNUBXFcTaCemik1LH1v9qz3F" );
                     ("amount", `String (string_of_int 100));
                     ( "fee",
                       `String (string_of_int (int_of_float (fee *. 1000000.)))
                     );
                     ("counter", `String (string_of_int i));
                     ("gas_limit", `String (string_of_int 127));
                     ("storage_limit", `String (string_of_int 277)) ])) ) ]
  in
  Tezos_client.rpc
    state
    ~client
    ~path:"/chains/main/blocks/head/helpers/forge/operations"
    (`Post (Ezjsonm.to_string json))
  >>= function
  | `String operation_bytes ->
      let magic_byte = "03" in
      return (magic_byte ^ operation_bytes)
  | _ ->
      failf "Failed to forge operation or parse result"

let sign state ~client ~bytes () =
  Tezos_client.successful_client_cmd
    state
    ~client:client.Tezos_client.Keyed.client
    ["sign"; "bytes"; "0x" ^ bytes; "for"; client.Tezos_client.Keyed.key_name]
  >>= fun _ -> return ()

let run state ~node_exec ~client_exec ~admin_exec ~size ~base_port ~uri () =
  Helpers.clear_root state
  >>= fun () ->
  Interactive_test.Pauser.generic
    state
    EF.[af "Ready to start"; af "Root path deleted."]
  >>= fun () ->
  let ledger_client = Tezos_client.no_node_client ~exec:client_exec in
  Tezos_client.Ledger.show_ledger state ~client:ledger_client ~uri
  >>= fun ledger_account ->
  Test_scenario.network_with_protocol
    ~protocol:(Tezos_protocol.default ())
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
      @ [ secret_keys state ~protocol;
          Log_recorder.Operations.show_all state;
          arbitrary_command_on_clients
            state
            ~command_names:["all-clients"]
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
  with_ledger_test_reject_and_succeed
    state
    EF.(
      wf
        "Importing %S in client `%s`. The ledger should be prompting for \
         acknowledgment to provide the public key of %s"
        uri
        (client 0).Tezos_client.id
        (Tezos_protocol.Account.pubkey_hash ledger_account))
    (fun () ->
      Tezos_client.Keyed.initialize state signer >>= fun _ -> return ())
  >>= fun _ ->
  let submit_proposals () =
    client_async_cmd
      state
      ~client:(client 0)
      ~f:(fun proc -> find_and_print_signature_hash state proc#stdout)
      [ "submit";
        "proposals";
        "for";
        Tezos_protocol.Account.pubkey_hash ledger_account;
        "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS";
        "Psd1ynUBhMZAeajwcZJAeq5NrxorM6UCU4GJqxZ7Bx2e9vUWB6z";
        "--force";
        "--verbose-signing" ]
  in
  ledger_prompt_notice_expectation
    state
    EF.(wf "Submitting multi-protocol proposal submission")
    `Fails
  >>= submit_proposals
  >>= fun (success, stdout) ->
  assert_ (not success)
  >>= fun () ->
  ( match
      String.substr_index stdout ~pattern:"Conditions of use not satisfied"
    with
  | None ->
      failf "expected rejection %s" stdout
  | Some _ ->
      return () )
  >>= fun () ->
  ledger_prompt_notice_expectation
    state
    EF.(wf "Submitting multi-protocol proposal submission")
    `Succeeds
  >>= submit_proposals
  >>= fun (success, stdout) ->
  assert_ (not success)
  >>= fun () ->
  ( match
      String.substr_index
        stdout
        ~pattern:"not registered as valid delegate key"
    with
  | None ->
      failf "expected error that key is not registered as valid delegate"
  | Some _ ->
      return () )
  >>= fun _ ->
  forge_batch_transactions
    state
    ~client:(client 0)
    ~src:(Tezos_protocol.Account.pubkey_hash ledger_account)
    ~dest:"tz2KZPgf2rshxNUBXFcTaCemik1LH1v9qz3F"
    ~n:50
    ()
  >>= fun batch_transaction_bytes ->
  let bytes_hash =
    Tezos_crypto.(
      `Hex batch_transaction_bytes |> Hex.to_bytes
      |> Tezos_stdlib.MBytes.of_bytes
      |> (fun x -> [x])
      |> Blake2B.hash_bytes |> Blake2B.to_string |> Base58.raw_encode)
  in
  with_ledger_test_reject_and_succeed
    state
    EF.(
      wf
        "Signing batch of transaction: Unrecognized Operation - Sign Hash %s"
        bytes_hash)
    (* Todo blake2b hash here *)
    (sign state ~client:signer ~bytes:batch_transaction_bytes)

let cmd ~pp_error () =
  let open Cmdliner in
  let open Term in
  Test_command_line.Run_command.make
    ~pp_error
    ( pure
        (fun uri
             node_exec
             client_exec
             admin_exec
             size
             (`Base_port base_port)
             state
             ->
          ( state,
            Interactive_test.Pauser.run_test
              ~pp_error
              state
              (run
                 state
                 ~node_exec
                 ~size
                 ~admin_exec
                 ~base_port
                 ~client_exec
                 ~uri) ))
    $ Arg.(
        required
          (pos
             0
             (some string)
             None
             (info [] ~docv:"LEDGER-URI" ~doc:"ledger:// URI")))
    $ Tezos_executable.cli_term `Node "tezos"
    $ Tezos_executable.cli_term `Client "tezos"
    $ Tezos_executable.cli_term `Admin "tezos"
    $ Arg.(value (opt int 5 (info ["size"; "S"] ~doc:"Size of the Network")))
    $ Arg.(
        pure (fun p -> `Base_port p)
        $ value
            (opt
               int
               46_000
               (info ["base-port"; "P"] ~doc:"Base port number to build upon")))
    $ Test_command_line.cli_state ~name:"ledger-wallet" () )
    (let doc = "Interactive test exercising the Ledger Wallet app features" in
     info ~doc "ledger-wallet")
