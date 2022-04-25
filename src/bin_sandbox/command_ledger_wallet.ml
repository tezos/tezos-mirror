open Flextesa
open Internal_pervasives

(********************* TEST UTILS **********************)

let client_async_cmd state ~client args ~f =
  let* (status, res) =
    Running_processes.Async.run_cmdf
      ~id_base:"client_async_cmd"
      state
      ~f
      "sh -c %s"
      (Tezos_client.client_command state client args
      |> Genspio.Compile.to_one_liner |> Caml.Filename.quote)
  in
  return
    (object
       method out = fst res

       method err = snd res

       method status = status
     end
      : Process_result.t)

let ledger_hash_re =
  lazy
    Re.(
      compile
        (seq
           [
             str "* Blake 2B Hash (ledger-style, with operation watermark):";
             rep1 (alt [space; eol]);
             group (rep1 alnum);
             rep1 (alt [space; eol]);
           ]))

(* Searches a stream for an expected ledger hash from `tezos-client --verbose-signing`*)
let find_and_print_signature_hash ?(display_expectation = true) state process =
  let re = Lazy.force ledger_hash_re in
  let check lines =
    Re.(
      match exec_opt re lines with
      | None -> None
      | Some matches -> Some (Group.get matches 1))
  in
  (* Dbg.e EF.(wf "find_and_print_signature_hash") ; *)
  let* (output, error, _) =
    Running_processes.Async.fold_process
      process
      ~init:("", "", not display_expectation)
      ~f:(fun (all_output_prev, all_error_prev, showed_message_prev) out err ->
        (* Dbg.e EF.(wf "find_and_print_signature_hash.fold_process %S %S" out err) ; *)
        let all_output = all_output_prev ^ out in
        let all_error = all_error_prev ^ err in
        let* showed_message =
          if not showed_message_prev then
            match check all_output with
            | None -> return false
            | Some x ->
                let* () =
                  Console.say state EF.(wf "Displayed hash should be: `%s`" x)
                in
                return true
          else return true
        in
        return (`Continue (all_output, all_error, showed_message)))
  in
  return (String.split ~on:'\n' output, String.split ~on:'\n' error)

module MFmt = More_fmt

let failf ?attach fmt = ksprintf (fun s -> fail ?attach (`Scenario_error s)) fmt

let ledger_prompt_notice state ~msgs ?(button = `Checkmark) () =
  let button_str =
    match button with
    | `Checkmark -> "✔"
    | `X -> "❌"
    | `Both -> "❌ and ✔ at the same time"
  in
  Console.sayf
    state
    MFmt.(
      fun ppf () ->
        vertical_box ~indent:4 ppf (fun ppf ->
            shout ppf (fun ppf -> const string "Ledger-prompt:" ppf ()) ;
            cut ppf () ;
            List.iter msgs ~f:(fun f ->
                f ppf () ;
                cut ppf ()) ;
            wf ppf "→ Press %s on the ledger." button_str))

let ledger_prompt_notice_expectation state ~messages ~user_answer =
  ledger_prompt_notice
    state
    ()
    ~button:(match user_answer with `Accept -> `Checkmark | `Reject -> `X)
    ~msgs:
      (messages
      @ MFmt.
          [
            cut;
            (fun ppf () ->
              match user_answer with
              | `Accept -> shout ppf (fun ppf -> pf ppf ">> ACCEPT THIS <<")
              | `Reject -> shout ppf (fun ppf -> pf ppf ">> REJECT THIS <<"));
          ])

let with_ledger_test_reject_and_accept ?(only_success = false) state ~messages f
    =
  let with_ledger_prompt state ~messages ~user_answer ~f =
    let* () = ledger_prompt_notice_expectation state ~messages ~user_answer in
    f ~user_answer
  in
  let* () =
    if only_success then return ()
    else with_ledger_prompt state ~messages ~user_answer:`Reject ~f
  in
  with_ledger_prompt state ~messages ~user_answer:`Accept ~f

let get_chain_id state ~client =
  let* chain_id_string =
    let* json =
      Tezos_client.rpc state ~client `Get ~path:"/chains/main/chain_id"
    in
    match json with
    | `String x -> return x
    | _ -> failf "Failed to parse chain_id JSON from node"
  in
  return (Tezos_crypto.Chain_id.of_b58check_exn chain_id_string)

let get_head_block_hash state ~client () =
  let* json =
    Tezos_client.rpc state ~client `Get ~path:"/chains/main/blocks/head/hash"
  in
  match json with
  | `String x -> return x
  | _ -> failf "Failed to parse block hash JSON from node"

let please_check_the_hash ppf () =
  let open MFmt in
  tag "prompt" ppf (fun ppf ->
      wf ppf "The ledger cannot parse this operation, please verify the hash.")

let expect_from_output ~expectation ~message (proc_res : Process_result.t) =
  (* let expect_rejection msg (success, (stdout, stderr)) = *)
  let exp =
    match expectation with
    | `Ledger_reject_or_timeout -> "rejection"
    | `Not_a_delegate -> "not-delegate-error"
    | `Success -> "success"
    | `Origination_failed -> "origination-failure"
  in
  let nope s =
    failf
      ~attach:
        [("stdout", `Verbatim proc_res#out); ("stderr", `Verbatim proc_res#err)]
      "%s, expected %s: %s."
      message
      exp
      s
  in
  let success = Poly.equal proc_res#status (Unix.WEXITED 0) in
  match expectation with
  | `Success when success -> return ()
  | `Success -> nope "did not succeed"
  | (`Ledger_reject_or_timeout | `Not_a_delegate | `Origination_failed) as e
    -> (
      let pattern =
        match e with
        | `Ledger_reject_or_timeout -> "Conditions of use not satisfied"
        | `Not_a_delegate -> "not registered as valid delegate key"
        | `Origination_failed -> "origination simulation failed"
      in
      let all_output = String.concat ~sep:"\n" (proc_res#out @ proc_res#err) in
      match (success, String.substr_index all_output ~pattern) with
      | (false, Some _) -> return ()
      | (false, None) -> nope "cannot find the right error message"
      | (true, _) -> nope "command succeeded??")

(********************* TEST SECTIONS ***************************)

let voting_tests state ~client ~src ~with_rejections ~protocol_kind
    ~ledger_account ~tested_proposal ~go_to_next_period () =
  let expect_failure message v =
    expect_from_output ~expectation:`Not_a_delegate ~message v
  in
  let expect_rejection message v =
    expect_from_output ~expectation:`Ledger_reject_or_timeout ~message v
  in
  let test_reject_and_accept name ~messages action =
    let* () =
      if with_rejections then
        let* () =
          ledger_prompt_notice_expectation state ~messages ~user_answer:`Reject
        in
        let* res = action () in
        expect_rejection name res
      else return ()
    in
    let* () =
      ledger_prompt_notice_expectation state ~messages ~user_answer:`Accept
    in
    let* res = action () in
    expect_failure name res
  in
  let source_display = Tezos_protocol.Account.pubkey_hash ledger_account in
  let submit_proposals ~display_expectation proposals () =
    client_async_cmd
      state
      ~client:(client 0)
      ~f:(fun _ proc ->
        find_and_print_signature_hash ~display_expectation state proc)
      (["submit"; "proposals"; "for"; src]
      @ proposals
      @ ["--force"; "--verbose-signing"])
  in
  let* () =
    test_reject_and_accept
      "single-proposal"
      ~messages:
        MFmt.
          [
            (fun ppf () ->
              wf ppf "Submitting single proposal %s" tested_proposal);
            (fun ppf () ->
              match protocol_kind with
              | `Athens -> ()
              | `Babylon | `Carthage | `Delphi | `Edo | `Florence | `Granada
              | `Hangzhou | `Ithaca | `Jakarta | `Alpha ->
                  wf
                    ppf
                    "From Babylon on, You will first be asked to provide the \
                     public key." ;
                  cut ppf () ;
                  wf
                    ppf
                    "Accept this prompt, regardless of below, then continue.");
            (fun ppf () ->
              vertical_box ppf ~indent:4 (fun ppf ->
                  wf
                    ppf
                    "Protocol is %a, the ledger should be able to display \
                     voting parameters:"
                    Tezos_protocol.Protocol_kind.pp
                    protocol_kind ;
                  cut ppf () ;
                  wf ppf "* Confirm Proposal" ;
                  cut ppf () ;
                  wf ppf "* Source: `%s`" source_display ;
                  cut ppf () ;
                  wf ppf "* Period: `0`" ;
                  cut ppf () ;
                  wf ppf "* Protocol: `%s`" tested_proposal));
          ]
      (submit_proposals ~display_expectation:false [tested_proposal])
  in
  let* () =
    test_reject_and_accept
      "multiple-proposal"
      ~messages:
        MFmt.
          [
            (fun ppf () -> wf ppf "Submitting 2 proposals together");
            please_check_the_hash;
          ]
      (submit_proposals
         ~display_expectation:true
         [
           tested_proposal; "Psd1ynUBhMZAeajwcZJAeq5NrxorM6UCU4GJqxZ7Bx2e9vUWB6z";
         ])
  in
  let* () = go_to_next_period () in
  List_sequential.iter ["yea"; "nay"] ~f:(fun vote ->
      let* _ =
        Console.say state EF.(haf "EXPECTING THE NEXT COMMAND TO FAIL")
      in
      test_reject_and_accept
        (Fmt.str "vote-%s" vote)
        ~messages:
          MFmt.
            [
              (fun ppf () ->
                match protocol_kind with
                | `Athens -> ()
                | `Babylon | `Carthage | `Delphi | `Edo | `Florence | `Granada
                | `Hangzhou | `Ithaca | `Jakarta | `Alpha ->
                    wf
                      ppf
                      "From Babylon on, you will first be asked to provide the \
                       public key." ;
                    cut ppf () ;
                    wf
                      ppf
                      "Accept this prompt, regardless of below, then continue.");
              (fun ppf () -> wf ppf "Voting %s for %s" vote tested_proposal);
              (fun ppf () -> wf ppf "Source: `%s`" source_display);
              (fun ppf () -> wf ppf "Protocol: `%s`" tested_proposal);
              (fun ppf () -> wf ppf "Period: `%i`" 1);
            ]
        (fun () ->
          let* (_, proc) =
            Tezos_client.client_cmd
              state
              ~client:(client 0)
              ["submit"; "ballot"; "for"; src; tested_proposal; vote]
          in
          return proc))

let ledger_should_display ?title ppf l =
  let open MFmt in
  vertical_box ~indent:4 ppf (fun ppf ->
      wf ppf "Ledger should display:" ;
      cut ppf () ;
      (match title with None -> () | Some t -> wf ppf t) ;
      List.iter l ~f:(fun (s, f) ->
          cut ppf () ;
          pf ppf "* %s: %a." s f ()))

let show_command_message command =
  MFmt.(
    fun ppf () ->
      wrapping_box ~indent:2 ppf (fun ppf ->
          wf ppf "Command:" ;
          sp ppf () ;
          const
            (list ~sep:sp string)
            ("<tezos-client>" :: command |> List.map ~f:Caml.Filename.quote)
            ppf
            ()))

let originate_manager_tz_script state ~client ~name ~from ~bake ~protocol_kind
    ~ledger_account =
  let prepare_origination_of_manager_tz_script ?(spendable = false)
      ?(delegatable = false) ?delegate state ~name ~from ~protocol_kind
      ~ledger_account =
    let ledger_pkh = Tezos_protocol.Account.pubkey_hash ledger_account in
    let manager_tz_script =
      "\n\
      \      parameter\n\
      \        (or\n\
      \           (lambda %do unit (list operation))\n\
      \           (unit %default));\n\
      \      storage key_hash;\n\
      \      code\n\
      \        { UNPAIR ;\n\
      \          IF_LEFT\n\
      \            { # 'do' entrypoint\n\
      \              # Assert no token was sent:\n\
      \              # to send tokens, the default entry point should be used\n\
      \              PUSH mutez 0 ;\n\
      \              AMOUNT ;\n\
      \              ASSERT_CMPEQ ;\n\
      \              # Assert that the sender is the manager\n\
      \              DUUP ;\n\
      \              IMPLICIT_ACCOUNT ;\n\
      \              ADDRESS ;\n\
      \              SENDER ;\n\
      \              ASSERT_CMPEQ ;\n\
      \              # Execute the lambda argument\n\
      \              UNIT ;\n\
      \              EXEC ;\n\
      \              PAIR ;\n\
      \            }\n\
      \            { # 'default' entrypoint\n\
      \              DROP ;\n\
      \              NIL operation ;\n\
      \              PAIR ;\n\
      \            }\n\
      \        };"
    in
    let tmp = Caml.Filename.temp_file "manager" ".tz" in
    let* () = System.write_file state tmp ~content:manager_tz_script in
    let origination =
      let opt = Option.value_map ~default:[] in
      ["--wait"; "none"; "originate"; "contract"; name]
      @ (match protocol_kind with
        | `Athens -> ["for"; from]
        | `Babylon | `Carthage | `Delphi | `Edo | `Florence | `Granada
        | `Hangzhou | `Ithaca | `Jakarta | `Alpha ->
            [])
      @ [
          "transferring";
          "350";
          "from";
          from;
          "running";
          tmp;
          "--init";
          sprintf "\"%s\"" ledger_pkh;
          "--force";
          "--burn-cap";
          "300000000000";
          (* ; "--fee-cap" ; "20000000000000" *)
          "--gas-limit";
          "1000000000000000";
          "--storage-limit";
          "20000000000000";
          "--verbose-signing";
        ]
      @ opt delegate ~f:(fun s -> (* Baby & Aths *) ["--delegate"; s])
      @ (if delegatable then [(* Aths *) "--delegatable"] else [])
      @ if spendable then [(* Aths *) "--spendable"] else []
    in
    return origination
  in
  let* origination =
    prepare_origination_of_manager_tz_script
      state
      ~name
      ~from
      ~protocol_kind
      ~ledger_account
  in
  let* _ = Tezos_client.successful_client_cmd state ~client origination in
  Fmt.kstr bake "baking `%s` in" name

let manager_tz_delegation_tests state ~client ~ledger_key ~ledger_account
    ~with_rejections ~protocol_kind ~delegate_pkh ~random_contract_pkh ~bake ()
    =
  let manager_tz_kt1_account = "manager-tz" in
  let* () =
    with_ledger_test_reject_and_accept
      ~only_success:true
      state
      ~messages:
        MFmt.
          [
            (fun ppf () -> wf ppf "Originating manager.tz contract ");
            (fun ppf () -> wf ppf "Sign Hash");
            (fun ppf () ->
              wf
                ppf
                "The ledger should be prompting for acknowledgment to provide \
                 a signature of an unknown operation. THIS IS NOT AN ACTUAL \
                 TEST, and is only part of the test setup. There is no need \
                 for a verification of this hash. Please accept this \
                 Unrecognized Operation.");
          ]
      (fun ~user_answer:_ ->
        originate_manager_tz_script
          state
          ~client
          ~name:manager_tz_kt1_account
          ~from:ledger_key
          ~bake
          ~protocol_kind
          ~ledger_account)
  in
  let* (_, proc_result) =
    Tezos_client.client_cmd
      state
      ~client
      ["show"; "known"; "contract"; manager_tz_kt1_account]
  in
  let contract_address = proc_result#out |> String.concat ~sep:"" in
  let ledger_pkh = Tezos_protocol.Account.pubkey_hash ledger_account in
  let random_implicit_account =
    Tezos_protocol.Account.of_name "random-account-for-manager-test"
  in
  let random_acc_pkh =
    Tezos_protocol.Account.pubkey_hash random_implicit_account
  in
  let only_success = not with_rejections in
  let manager_tz_test ~contract_arg ~expected ~output_msg =
    let command =
      [
        "--wait";
        "none";
        "transfer";
        "0";
        "from";
        ledger_key;
        "to";
        manager_tz_kt1_account;
        "--entrypoint";
        "do";
        "--arg";
        contract_arg;
        "--burn-cap";
        "1";
        "--storage-limit";
        "500";
        "--verbose-signing";
      ]
    in
    with_ledger_test_reject_and_accept
      state
      ~only_success
      ~messages:
        MFmt.
          [
            (fun ppf () ->
              wf ppf "Managing account of manager.tz contract `%s`" ledger_pkh);
            show_command_message command;
            (fun ppf () ->
              wf
                ppf
                "Note that X is a placeholder for some value that will vary \
                 between runs");
            (fun ppf () -> ledger_should_display ppf expected);
          ]
      (fun ~user_answer ->
        let* res =
          client_async_cmd
            state
            ~client
            ~f:(fun _ proc ->
              find_and_print_signature_hash
                ~display_expectation:Poly.(protocol_kind = `Babylon)
                state
                proc)
            command
        in
        expect_from_output
          ~message:output_msg
          res
          ~expectation:
            (match user_answer with
            | `Reject -> `Ledger_reject_or_timeout
            | `Accept -> `Success))
  in
  let set_delegation () =
    manager_tz_test
      ~contract_arg:
        (sprintf
           "{ DROP ; NIL operation ; PUSH key_hash \"%s\" ; SOME ; \
            SET_DELEGATE ; CONS }"
           delegate_pkh)
      ~expected:
        MFmt.
          [
            ("Confirm Delegation", const string "");
            ("Fee", const string "0.00XXX");
            ("Source", const string contract_address);
            ("Delegate", const string delegate_pkh);
            ( "Delegate Name",
              const string "Custom Delegate: please verify the addresss" );
            ("Storage Limit", const int 500);
          ]
      ~output_msg:"delegation"
  in
  let remove_delegation () =
    manager_tz_test
      ~contract_arg:
        "{ DROP ; NIL operation ; NONE key_hash ; SET_DELEGATE ; CONS }"
      ~expected:
        MFmt.
          [
            ("Withdraw Delegation", const string "");
            ("Fee", const string "0.00XXX");
            ("Source", const string contract_address);
            ("Delegate", const string "None");
            ( "Delegate Name",
              const string "Custom Delegate: please verify the address" );
            ("Storage Limit", const int 500);
          ]
      ~output_msg:"delegate-removal"
  in
  let manager_tz_to_implicit () =
    manager_tz_test
      ~contract_arg:
        (sprintf
           "{ DROP ; NIL operation ; PUSH key_hash \"%s\" ; IMPLICIT_ACCOUNT ; \
            PUSH mutez %d ; UNIT ; TRANSFER_TOKENS ; CONS }"
           random_acc_pkh
           10000000)
      ~expected:
        MFmt.
          [
            ("Confirm Transaction", const string "");
            ("Amount", const int 10);
            ("Fee", const string "0.00XXX");
            ("Source", const string contract_address);
            ("Destination", const string random_acc_pkh);
            ("Storage Limit", const int 500);
          ]
      ~output_msg:"transfer-from-manager-tz-to-implicit"
  in
  let manager_tz_to_originated_large () =
    manager_tz_test
      ~contract_arg:
        (sprintf
           "{ DROP ; NIL operation ; PUSH address \"%s\" ; CONTRACT unit ; \
            ASSERT_SOME ; PUSH mutez %d ; UNIT ; TRANSFER_TOKENS ; CONS }"
           random_contract_pkh
           (300 * 1000000))
      ~expected:
        MFmt.
          [
            ("Confirm Transaction", const string "");
            ("Amount", const int 300);
            ("Fee", const string "0.00XXX");
            ("Source", const string contract_address);
            ("Destination", const string random_contract_pkh);
            ("Storage Limit", const int 500);
          ]
      ~output_msg:"transfer-from-manager-tz-to-originated"
  in
  let manager_tz_to_originated_small () =
    manager_tz_test
      ~contract_arg:
        (sprintf
           "{ DROP ; NIL operation ; PUSH address \"%s\" ; CONTRACT unit ; \
            ASSERT_SOME ; PUSH mutez %d ; UNIT ; TRANSFER_TOKENS ; CONS }"
           random_contract_pkh
           3)
      ~expected:
        MFmt.
          [
            ("Confirm Transaction", const string "");
            ("Amount", const string "0.000003");
            ("Fee", const string "0.00XXX");
            ("Source", const string contract_address);
            ("Destination", const string random_contract_pkh);
            ("Storage Limit", const int 500);
          ]
      ~output_msg:"transfer-from-manager-tz-to-originated"
  in
  let* () = return () in
  let* _ = set_delegation () in
  let* () = ksprintf bake "setting delegate of %s" contract_address in
  let* _ = remove_delegation () in
  let* () = ksprintf bake "removing delegate of %s" contract_address in
  let* _ = manager_tz_to_implicit () in
  let* () =
    ksprintf bake "transferring from %s to %s" contract_address random_acc_pkh
  in
  let* _ = manager_tz_to_originated_large () in
  let* () =
    ksprintf
      bake
      "transfering from %s to %s"
      contract_address
      random_contract_pkh
  in
  let* _ = manager_tz_to_originated_small () in
  ksprintf bake "transfering from %s to %s" contract_address random_contract_pkh

let delegation_tests state ~client ~src ~with_rejections ~protocol_kind
    ~ledger_account ~delegate ~delegate_pkh ~bake () =
  let ledger_pkh = Tezos_protocol.Account.pubkey_hash ledger_account in
  let only_success = not with_rejections in
  let self_delegation () =
    (* Which is equivalent to registration as delegate. *)
    let command =
      [
        "--wait";
        "none";
        "set";
        "delegate";
        "for";
        src;
        "to";
        src;
        "--verbose-signing";
      ]
    in
    let* _ =
      with_ledger_test_reject_and_accept
        state
        ~only_success
        ~messages:
          MFmt.
            [
              (fun ppf () -> wf ppf "Self-delegating account `%s`" ledger_pkh);
              show_command_message command;
              (fun ppf () ->
                wf
                  ppf
                  "Note that X is a placeholder for some value that will vary \
                   between runs");
              (fun ppf () ->
                ledger_should_display
                  ~title:"Confirm Delegation"
                  ppf
                  [
                    ("Fee", const string "0.00XXX");
                    ("Source", const string ledger_pkh);
                    ("Delegate", const string ledger_pkh);
                    ("Delegate Name", const string "Obsidian");
                    ("Storage", const int 0);
                  ]);
            ]
        (fun ~user_answer ->
          let* res =
            client_async_cmd
              state
              ~client
              ~f:(fun _ proc ->
                find_and_print_signature_hash
                  ~display_expectation:Poly.(protocol_kind = `Babylon)
                  state
                  proc)
              command
          in
          expect_from_output
            ~message:"self-delegation"
            res
            ~expectation:
              (match user_answer with
              | `Reject -> `Ledger_reject_or_timeout
              | `Accept -> `Success))
    in
    ksprintf bake "setting self-delegate of %s" src
    (* Self-delegate deletion is forbidden for both Athens and Babylon *)
  in
  let tz_account_delegation () =
    let command =
      [
        "--wait";
        "none";
        "set";
        "delegate";
        "for";
        src;
        "to";
        delegate;
        "--verbose-signing";
      ]
    in
    let* _ =
      with_ledger_test_reject_and_accept
        state
        ~only_success
        ~messages:
          MFmt.
            [
              (fun ppf () ->
                wf ppf "Delegating account `%s` to `%s`" ledger_pkh delegate);
              show_command_message command;
              (fun ppf () ->
                wf
                  ppf
                  "Note that X is a placeholder for some value that will vary \
                   between runs");
              (fun ppf () ->
                ledger_should_display
                  ~title:"Confirm Delegation"
                  ppf
                  [
                    ("Fee", const string "0.00XXX");
                    ("Source", const string ledger_pkh);
                    ("Delegate", const string delegate_pkh);
                    ( "Delegate Name",
                      const string "Custom Delegate: please verify the address"
                    );
                    ("Storage Limit", const int 0);
                  ]);
            ]
        (fun ~user_answer ->
          let* res =
            client_async_cmd
              state
              ~client
              ~f:(fun _ proc ->
                find_and_print_signature_hash
                  ~display_expectation:Poly.(protocol_kind = `Babylon)
                  state
                  proc)
              command
          in
          expect_from_output
            ~message:"tz123-delegation"
            res
            ~expectation:
              (match user_answer with
              | `Reject -> `Ledger_reject_or_timeout
              | `Accept -> `Success))
    in
    ksprintf bake "setting delegate of %s" src
    (* Self-delegate deletion is forbidden for both Athens and Babylon *)
  in
  match protocol_kind with
  | `Athens -> self_delegation ()
  | `Babylon | `Carthage | `Delphi | `Edo | `Florence | `Granada | `Hangzhou
  | `Ithaca | `Jakarta | `Alpha ->
      let* () = tz_account_delegation () in
      self_delegation ()

let transaction_tests state ~client ~src ~with_rejections ~protocol_kind
    ~pair_string_nat_kt1_account ~ledger_account ~unit_kt1_account ~bake () =
  let ledger_pkh = Tezos_protocol.Account.pubkey_hash ledger_account in
  let only_success = not with_rejections in
  let test_transaction ?storage_limit ?arguments ~name ~dst_name ~dst_pkh
      ?entrypoint ?(amount = 15) ?fee () =
    let command =
      let opt = Option.value_map ~default:[] in
      let storage =
        match storage_limit with
        | None -> []
        | Some limit -> ["--storage-limit"; sprintf "%d" limit]
      in
      [
        "--wait";
        "none";
        "transfer";
        sprintf "%i" amount;
        "from";
        src;
        "to";
        dst_name;
      ]
      @ Option.value_map ~default:[] arguments ~f:(fun a -> ["--arg"; a])
      @ Option.value_map ~default:[] fee ~f:(fun f -> ["--fee"; sprintf "%d" f])
      @ storage @ ["--fee-cap"; "10"]
      @ opt entrypoint ~f:(fun e -> ["--entrypoint"; e])
      @ ["--burn-cap"; "100"; "--verbose-signing"]
    in
    let* _ =
      with_ledger_test_reject_and_accept
        state
        ~only_success
        ~messages:
          MFmt.
            [
              (fun ppf () -> wf ppf "%s with account `%s`" name ledger_pkh);
              show_command_message command;
              (fun ppf () ->
                wf
                  ppf
                  "Note that X is a placeholder for some value that will vary \
                   between runs");
              (fun ppf () ->
                match arguments with
                | None ->
                    ledger_should_display
                      ppf
                      [
                        ("Amount", const int amount);
                        ( "Fee",
                          const string
                          @@ Option.value_map
                               ~default:"0.00XXX"
                               fee
                               ~f:(sprintf "%d") );
                        ("Source", const string ledger_pkh);
                        ("Destination", const string dst_pkh);
                        ( "Storage Limit",
                          const int
                          @@ match storage_limit with None -> 0 | Some s -> s );
                      ]
                | _ (* some arguments *) -> please_check_the_hash ppf ());
            ]
        (fun ~user_answer ->
          let* res =
            client_async_cmd
              state
              ~client
              ~f:(fun _ proc ->
                find_and_print_signature_hash
                  ~display_expectation:
                    Poly.(protocol_kind = `Babylon || arguments <> None)
                  state
                  proc)
              command
          in
          expect_from_output
            ~message:name
            res
            ~expectation:
              (match user_answer with
              | `Reject -> `Ledger_reject_or_timeout
              | `Accept -> `Success))
    in
    ksprintf bake "%s with %s" name src
  in
  let* () =
    test_transaction
      ~name:"Self-transaction"
      ~dst_pkh:ledger_pkh
      ~dst_name:src
      ()
  in
  let module Acc = Tezos_protocol.Account in
  let random_account = Acc.of_name "random-account-for-transaction-test" in
  let* () =
    test_transaction
      ~name:"transaction-to-random-tz1"
      ~dst_pkh:(Acc.pubkey_hash random_account)
      ~dst_name:(Acc.pubkey_hash random_account)
      ~storage_limit:277
      (* First time: there is a reveal *) ()
  in
  let* () =
    test_transaction
      ~name:"transaction-to-random-tz1-again"
      ~dst_pkh:(Acc.pubkey_hash random_account)
      ~dst_name:(Acc.pubkey_hash random_account)
      ~storage_limit:0
      (* no moa reveal *) ()
  in
  let* () =
    test_transaction
      ~name:"transaction-to-random-tz1-fee-no-storage-limit"
      ~dst_pkh:(Acc.pubkey_hash random_account)
      ~dst_name:(Acc.pubkey_hash random_account)
      ~fee:2
      ()
  in
  let* () =
    test_transaction
      ~name:"transaction-to-random-tz1-fee-storage-limit"
      ~dst_pkh:(Acc.pubkey_hash random_account)
      ~dst_name:(Acc.pubkey_hash random_account)
      ~fee:2
      ~storage_limit:500
      ()
  in
  let* () =
    test_transaction
      ~name:"parameterless-transaction-to-kt1"
      ~dst_pkh:"KT1XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
      ~dst_name:unit_kt1_account
      ()
  in
  test_transaction
    ~name:"parameterfull-transaction-to-kt1"
    ~dst_pkh:"KT1XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ~arguments:"Pair \"hello from the ledger\" 51"
    ~dst_name:pair_string_nat_kt1_account
    ()

let prepare_origination_of_id_script ?(spendable = false) ?(delegatable = false)
    ?delegate ?(push_drops = 0) ?(amount = "2") state ~client:_ ~name ~from
    ~protocol_kind ~parameter ~init_storage =
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
      (match push_drops with
      | 0 -> "# No push-drops"
      | n ->
          Fmt.str
            "# %d push-drop%s\n    %s"
            n
            (if n > 1 then "s" else "")
            (List.init push_drops ~f:(fun ith ->
                 Fmt.str
                   "{ PUSH string %S ; DROP } ;"
                   (Fmt.str
                      "push-dropping %d adds stupid bytes to the contract"
                      ith))
            |> String.concat ~sep:"\n    "))
  in
  let tmp = Caml.Filename.temp_file "little-id-script" ".tz" in
  let* () = System.write_file state tmp ~content:(id_script parameter) in
  Dbg.e EF.(wf "id_script %s: %s" parameter tmp) ;
  let origination =
    let opt = Option.value_map ~default:[] in
    ["--wait"; "none"; "originate"; "contract"; name]
    @ (match protocol_kind with
      | `Athens -> ["for"; from]
      | `Babylon | `Carthage | `Delphi | `Edo | `Florence | `Granada | `Hangzhou
      | `Ithaca | `Jakarta | `Alpha ->
          [])
    @ [
        "transferring";
        amount;
        "from";
        from;
        "running";
        tmp;
        "--init";
        init_storage;
        "--force";
        "--burn-cap";
        "300000000000";
        (* ; "--fee-cap" ; "20000000000000" *)
        "--gas-limit";
        "1000000000000000";
        "--storage-limit";
        "20000000000000";
        "--verbose-signing";
      ]
    @ opt delegate ~f:(fun s -> (* Baby & Aths *) ["--delegate"; s])
    @ (if delegatable then [(* Aths *) "--delegatable"] else [])
    @ if spendable then [(* Aths *) "--spendable"] else []
  in
  return origination

let originate_id_script ?push_drops state ~client ~name ~from ~bake
    ~protocol_kind ~parameter ~init_storage =
  let* origination =
    prepare_origination_of_id_script
      state
      ~client
      ~name
      ~from
      ~protocol_kind
      ?push_drops
      ~parameter
      ~init_storage
  in
  let* _ = Tezos_client.successful_client_cmd state ~client origination in
  Fmt.kstr bake "baking `%s` in" name

let pp_warning_ledger_takes_a_while ~adjective =
  let open MFmt in
  fun ppf () ->
    cut ppf () ;
    let prompt = "WARNING: " in
    let warning1 = "The ledger will take a few seconds to show" in
    let warning2 = str "the hash for such a %s operation." adjective in
    let wl = String.length prompt + String.length warning1 in
    tag "shout" ppf (fun ppf -> string ppf ("/" ^ String.make wl '=' ^ "\\")) ;
    cut ppf () ;
    tag "shout" ppf (fun ppf -> pf ppf "|%s" prompt) ;
    string ppf warning1 ;
    tag "shout" ppf (fun ppf -> string ppf "|") ;
    cut ppf () ;
    tag "shout" ppf (fun ppf -> pf ppf "|") ;
    string ppf String.(make (length prompt) ' ') ;
    string ppf warning2 ;
    string ppf String.(make (length warning1 - length warning2) ' ') ;
    tag "shout" ppf (fun ppf -> string ppf "|") ;
    cut ppf () ;
    tag "shout" ppf (fun ppf -> string ppf ("\\" ^ String.make wl '=' ^ "/"))

let basic_contract_operations_tests state ~client ~src ~with_rejections
    ~protocol_kind ~ledger_account ~bake ~delegate () =
  let ledger_pkh = Tezos_protocol.Account.pubkey_hash ledger_account in
  let only_success = not with_rejections in
  let test_origination ?delegate ?delegatable ?spendable ?push_drops ~name
      ~amount ~parameter ~init_storage () =
    let* origination =
      prepare_origination_of_id_script
        ~amount
        ?push_drops
        state
        ~client
        ~name
        ~from:src
        ?delegate
        ?delegatable
        ?spendable
        ~protocol_kind
        ~parameter
        ~init_storage
    in
    let* _ =
      with_ledger_test_reject_and_accept
        state
        ~only_success
        ~messages:
          MFmt.
            [
              (fun ppf () ->
                wf ppf "Origination: %s (ledger: %s)" name ledger_pkh);
              show_command_message origination;
              please_check_the_hash;
              (if Poly.(push_drops <> None) then
               pp_warning_ledger_takes_a_while ~adjective:"huge"
              else const string "");
            ]
        (fun ~user_answer ->
          let* res =
            client_async_cmd
              state
              ~client
              ~f:(fun _ proc ->
                find_and_print_signature_hash
                  ~display_expectation:true
                  state
                  proc)
              origination
          in
          expect_from_output
            ~message:name
            res
            ~expectation:
              (match user_answer with
              | `Reject -> `Ledger_reject_or_timeout
              | `Accept -> `Success))
    in
    ksprintf bake "%s with %s" name src
  in
  let* () =
    test_origination
      ~name:"ID-unit"
      ~amount:"0"
      ~parameter:"unit"
      ~init_storage:"Unit"
      ()
  in
  let* () =
    test_origination
      ~name:"ID-string"
      ~amount:"10"
      ~parameter:"string"
      ~init_storage:"\"some string\""
      ()
  in
  let* () =
    test_origination
      ~name:"ID-string-nat-mutez"
      ~amount:"10"
      ~parameter:"(pair string (pair nat mutez))"
      ~init_storage:"Pair \"hello\" (Pair 12 1)"
      ()
  in
  let* () =
    test_origination
      ~name:"ID-address+delegate"
      ~amount:"1"
      ~parameter:"address"
      ~delegate
      ~init_storage:"\"tz1YPSCGWXwBdTncK2aCctSZAXWvGsGwVJqU\""
      ()
  in
  let* () =
    match protocol_kind with
    | `Athens ->
        test_origination
          ~name:"ID-string+delegatable"
          ~amount:"0"
          ~parameter:"string"
          ~delegate
          ~init_storage:"\"delegatable contract\""
          ~delegatable:true
          ()
    | `Babylon | `Carthage | `Delphi | `Edo | `Florence | `Granada | `Hangzhou
    | `Ithaca | `Jakarta | `Alpha ->
        return ()
  in
  let push_drops =
    (* Found by dichotomic trial-and-error :)
       240 works, 250 fails at 16870 bytes, … *)
    242
  in
  test_origination
    ~push_drops
    ~name:"giant-contract"
    ~amount:"10"
    ~parameter:"(pair string nat)"
    ~init_storage:"Pair \"the answer is: \" 42"
    ()

module Wallet_scenario = struct
  type root =
    [ `All
    | `Voting
    | `Batch_transactions
    | `Manager
    | `Delegation
    | `Transactions
    | `Contracts
    | `None ]

  type t = [root | `Without_rejections of root]

  let with_rejections : t -> bool = function
    | `Without_rejections _ -> false
    | _ -> true

  let enum_assoc : (string * root) list =
    [
      ("everything", `All);
      ("voting", `Voting);
      ("none", `None);
      ("delegation", `Delegation);
      ("transactions", `Transactions);
      ("contracts", `Contracts);
      ("manager", `Manager);
      ("batch-transactions", `Batch_transactions);
    ]

  let root (ws : t) =
    match ws with `Without_rejections r -> r | #root as r -> r

  let run_if v t ~yes ~no =
    let with_rejections = with_rejections t in
    match root t with
    | `All -> yes ~with_rejections
    | other when Poly.(other = v) -> yes ~with_rejections
    | _other ->
        no
          (List.find_map_exn enum_assoc ~f:(function
              | (k, this) when Poly.(v = this) -> Some k
              | _ -> None))

  let if_voting t = run_if `Voting t

  let if_batch_transactions t = run_if `Batch_transactions t

  let if_delegation t = run_if `Delegation t

  let if_manager t = run_if `Manager t

  let if_transactions t = run_if `Transactions t

  let if_contracts t = run_if `Contracts t

  let cli_term () =
    let make no_rejections v =
      if no_rejections then `Without_rejections v else (v :> t)
    in
    let open Cmdliner in
    let open Term in
    const make
    $ Arg.(
        value
          (flag (info ["no-rejections"] ~doc:"Do not test ledger rejections.")))
    $ Arg.(
        value
          (opt
             (enum ([("all", `All)] @ enum_assoc))
             `All
             (info
                ["only-test"]
                ~doc:
                  (Fmt.str
                     "Limit to a family of tests (one of: %s)."
                     (List.map enum_assoc ~f:(fun (n, _) -> sprintf "`%s`" n)
                     |> String.concat ~sep:", ")))))
end

let run state ~pp_error ~protocol ~protocol_kind ~node_exec ~client_exec
    ~admin_exec ~wallet_scenario ~size ~base_port ~uri () =
  let* () = Helpers.clear_root state in
  let* () =
    Helpers.System_dependencies.precheck
      state
      `Or_fail
      ~executables:[node_exec; client_exec; admin_exec]
  in
  let* () =
    Interactive_test.Pauser.generic
      state
      EF.[af "Ready to start"; af "Root path deleted."]
  in
  let ledger_client = Tezos_client.no_node_client ~exec:client_exec in
  let* _ledger_account =
    Tezos_client.Ledger.show_ledger state ~client:ledger_client ~uri
  in
  let (protocol, baker_0_account, _baker_0_balance) =
    let open Tezos_protocol in
    let d = protocol in
    let baker = List.nth_exn d.bootstrap_accounts 0 in
    ( {
        d with
        kind = protocol_kind;
        time_between_blocks = [1; 0];
        minimal_block_delay = 1;
        bootstrap_accounts =
          List.map d.bootstrap_accounts ~f:(fun (n, v) ->
              if Poly.(fst baker = n) then (n, v) else (n, 1_000L));
      },
      fst baker,
      snd baker )
  in
  let* (nodes, protocol) =
    Test_scenario.network_with_protocol
      ~protocol
      ~size
      ~base_port
      state
      ~node_exec
      ~client_exec
  in
  let client n =
    Tezos_client.of_node ~exec:client_exec (List.nth_exn nodes n)
  in
  let client_0 = client 0 in
  let baker_0 =
    Tezos_client.Keyed.make
      client_0
      ~key_name:"baker-0"
      ~secret_key:(Tezos_protocol.Account.private_key baker_0_account)
  in
  let* _ = Tezos_client.Keyed.initialize state baker_0 in
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
  let first_bakes = 3 in
  let* () =
    Loop.n_times first_bakes (fun nth ->
        ksprintf (Tezos_client.Keyed.bake state baker_0) "initial-bake %d" nth)
  in
  let* () =
    Interactive_test.Pauser.generic
      state
      EF.[af "About to really start playing"]
  in
  let signer =
    Tezos_client.Keyed.make (client 0) ~key_name:"ledgered" ~secret_key:uri
  in
  let* ledger_account =
    Tezos_client.Ledger.show_ledger state ~client:client_0 ~uri
  in
  let* _ =
    Tezos_client.successful_client_cmd
      state
      ~client:client_0
      [
        "--wait";
        "none";
        "transfer";
        "20000";
        "from";
        baker_0.Tezos_client.Keyed.key_name;
        "to" (*  *);
        Tezos_protocol.Account.pubkey_hash ledger_account;
        "--burn-cap";
        "100";
      ]
  in
  let bake msg = Tezos_client.Keyed.bake state baker_0 msg in
  let* () = bake "After transferring tez to the ledger account" in
  let* () =
    with_ledger_test_reject_and_accept
      ~only_success:(Wallet_scenario.with_rejections wallet_scenario |> not)
      state
      ~messages:
        MFmt.
          [
            (fun ppf () ->
              wf ppf "Importing %S in client `%s`." uri client_0.Tezos_client.id);
            (fun ppf () ->
              wf
                ppf
                "The ledger should be prompting for acknowledgment to provide \
                 the public key of `%s`."
                (Tezos_protocol.Account.pubkey_hash ledger_account));
          ]
      (fun ~user_answer ->
        let* (_, proc) =
          Tezos_client.client_cmd
            state
            ~client:client_0
            [
              "import";
              "secret";
              "key";
              signer.key_name;
              signer.secret_key;
              "--force";
            ]
        in
        expect_from_output
          ~message:"importing key"
          proc
          ~expectation:
            (match user_answer with
            | `Accept -> `Success
            | `Reject -> `Ledger_reject_or_timeout))
  in
  let skipping s = Console.say state EF.(haf "Skipping %s tests" s) in
  let voting_test ~with_rejections =
    let tested_proposal =
      "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
    in
    voting_tests
      state
      ~client
      ~ledger_account
      ~src:signer.key_name
      ()
      ~with_rejections
      ~protocol_kind
      ~tested_proposal
      ~go_to_next_period:(fun () ->
        let* _ =
          Tezos_client.successful_client_cmd
            state
            ~client:client_0
            [
              "--wait";
              "none";
              "submit";
              "proposals";
              "for";
              baker_0.Tezos_client.Keyed.key_name;
              tested_proposal;
              "--force";
            ]
        in
        let blocks = protocol.Tezos_protocol.blocks_per_voting_period in
        let* () =
          Loop.n_times blocks (fun nth ->
              ksprintf
                (Tezos_client.Keyed.bake state baker_0)
                "going to testing-vote period %d/%d"
                (nth + 1)
                blocks)
        in
        return ())
  in
  let batch_test ~with_rejections =
    let n = 50 in
    let forge_batch_transactions state ~client ~src ~dest:_ ~n ?(fee = 0.00126)
        () =
      let* branch = get_head_block_hash state ~client () in
      let json =
        `O
          [
            ("branch", `String branch);
            ( "contents",
              `A
                (List.map (List.range 0 n) ~f:(fun i ->
                     `O
                       [
                         ("kind", `String "transaction");
                         ("source", `String src);
                         ( "destination",
                           `String "tz2KZPgf2rshxNUBXFcTaCemik1LH1v9qz3F" );
                         ("amount", `String (Int.to_string 100));
                         ( "fee",
                           `String
                             (Int.to_string (Float.to_int (fee *. 1000000.))) );
                         ("counter", `String (Int.to_string i));
                         ("gas_limit", `String (Int.to_string 127));
                         ("storage_limit", `String (Int.to_string 277));
                       ])) );
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
    in
    let* batch_transaction_bytes =
      forge_batch_transactions
        state
        ~client:(client 0)
        ~src:(Tezos_protocol.Account.pubkey_hash ledger_account)
        ~dest:"tz2KZPgf2rshxNUBXFcTaCemik1LH1v9qz3F"
        ~n
        ()
    in
    let bytes_hash =
      Tezos_crypto.(
        `Hex batch_transaction_bytes |> Tezos_stdlib.Hex.to_bytes_exn
        |> (fun x -> [x])
        |> Blake2B.hash_bytes |> Blake2B.to_string |> Base58.raw_encode)
    in
    let sign state ~client ~bytes =
      Tezos_client.client_cmd
        state
        ~client:client.Tezos_client.Keyed.client
        [
          "sign";
          "bytes";
          "0x" ^ bytes;
          "for";
          client.Tezos_client.Keyed.key_name;
        ]
    in
    with_ledger_test_reject_and_accept
      state
      ~only_success:(not with_rejections)
      ~messages:
        MFmt.
          [
            (fun ppf () -> wf ppf "Signing batch of %d transactions" n);
            (fun ppf () ->
              wf ppf "Ledger should display “Sign Hash” → `%s`" bytes_hash);
            pp_warning_ledger_takes_a_while ~adjective:"big";
          ]
      (fun ~user_answer ->
        let* (_, proc) =
          sign state ~client:signer ~bytes:batch_transaction_bytes
        in
        expect_from_output
          ~message:"Signing batch operation"
          proc
          ~expectation:
            (match user_answer with
            | `Accept -> `Success
            | `Reject -> `Ledger_reject_or_timeout))
  in
  let delegation_tests ~with_rejections =
    delegation_tests
      state
      ~client:client_0
      ~ledger_account
      ~delegate:baker_0.Tezos_client.Keyed.key_name
      ~delegate_pkh:(Tezos_protocol.Account.pubkey_hash baker_0_account)
      ~src:signer.key_name
      ()
      ~bake
      ~with_rejections
      ~protocol_kind
  in
  let unit_kt1_account = "unit-kt1-of-the-baker" in
  let* () =
    originate_id_script
      state
      ~client:client_0
      ~name:unit_kt1_account
      ~from:baker_0.Tezos_client.Keyed.key_name
      ~bake
      ~protocol_kind
      ~parameter:"unit"
      ~init_storage:"Unit"
  in
  let* (_, proc_result) =
    Tezos_client.client_cmd
      state
      ~client:client_0
      ["show"; "known"; "contract"; unit_kt1_account]
  in
  let unit_kt1_contract_address = proc_result#out |> String.concat ~sep:"" in
  let pair_string_nat_kt1_account = "pair-string-nat-kt1-of-the-baker" in
  let* () =
    originate_id_script
      state
      ~client:client_0
      ~name:pair_string_nat_kt1_account
      ~push_drops:10
      ~from:baker_0.Tezos_client.Keyed.key_name
      ~bake
      ~protocol_kind
      ~parameter:"(pair string nat)"
      ~init_storage:"Pair \"the answer is: \" 42"
  in
  let transactions_test ~with_rejections =
    transaction_tests
      state
      ~client:client_0
      ~ledger_account
      ~unit_kt1_account
      ~pair_string_nat_kt1_account
      ~src:signer.key_name
      ()
      ~bake
      ~with_rejections
      ~protocol_kind
  in
  let manager_tz_delegation_tests ~with_rejections =
    manager_tz_delegation_tests
      state
      ~client:client_0
      ~ledger_key:signer.key_name
      ~ledger_account
      ~delegate_pkh:(Tezos_protocol.Account.pubkey_hash baker_0_account)
      ()
      ~bake
      ~random_contract_pkh:unit_kt1_contract_address
      ~with_rejections
      ~protocol_kind
  in
  let contracts_test ~with_rejections =
    basic_contract_operations_tests
      state
      ~client:client_0
      ~ledger_account
      ~delegate:baker_0.Tezos_client.Keyed.key_name
      ~src:signer.key_name
      ()
      ~bake
      ~with_rejections
      ~protocol_kind
  in
  let bake_command =
    Console.Prompt.unit_and_loop
      ~description:"Bake a block with the default baker."
      ["bake"]
      (fun _sexps ->
        Asynchronous_result.transform_error
          ~f:(fun e ->
            Fmt.kstr (fun s -> `Command_line s) "run-test-error: %a" pp_error e)
          (bake "Interactive"))
  in
  let run_test_command =
    Console.Prompt.unit_and_loop
      ~description:
        Fmt.(
          str
            "Run a test (%s)."
            (List.map Wallet_scenario.enum_assoc ~f:fst
            |> String.concat ~sep:"|"))
      ["rt"; "run-test"]
      (fun sexps ->
        Asynchronous_result.transform_error
          ~f:(fun e ->
            Fmt.kstr (fun s -> `Command_line s) "run-test-error: %a" pp_error e)
          (match sexps with
          | [Atom a] -> (
              let run f = f ~with_rejections:true in
              match
                List.Assoc.find ~equal:String.equal Wallet_scenario.enum_assoc a
              with
              | Some `None -> return ()
              | Some `Delegation -> run delegation_tests
              | Some `Manager -> run manager_tz_delegation_tests
              | Some `All ->
                  let* () = run delegation_tests in
                  let* () = run manager_tz_delegation_tests in
                  let* () = run batch_test in
                  let* () = run transactions_test in
                  let* () = run voting_test in
                  run contracts_test
              | Some `Batch_transactions -> run batch_test
              | Some `Transactions -> run transactions_test
              | Some `Voting -> run voting_test
              | Some `Contracts -> run contracts_test
              | None -> failf "Don't know this test: %S" a)
          | _ -> failf "Cannot understand command line"))
  in
  Interactive_test.Pauser.add_commands state [run_test_command; bake_command] ;
  let* () =
    Wallet_scenario.if_voting wallet_scenario ~yes:voting_test ~no:skipping
  in
  let* () =
    Wallet_scenario.if_batch_transactions
      wallet_scenario
      ~yes:batch_test
      ~no:skipping
  in
  let* () =
    Wallet_scenario.if_transactions
      wallet_scenario
      ~yes:transactions_test
      ~no:skipping
  in
  let* () =
    Wallet_scenario.if_contracts
      wallet_scenario
      ~yes:contracts_test
      ~no:skipping
  in
  let* () =
    Wallet_scenario.if_delegation
      wallet_scenario
      ~yes:delegation_tests
      ~no:skipping
  in
  let* () =
    Wallet_scenario.if_manager
      wallet_scenario
      ~yes:manager_tz_delegation_tests
      ~no:skipping
  in
  Interactive_test.Pauser.generic state EF.[af "Tests done."]

let cmd () =
  let open Cmdliner in
  let open Term in
  let pp_error = Test_command_line.Common_errors.pp in
  let base_state =
    Test_command_line.Command_making_state.make
      ~application_name:"Flextesa"
      ~command_name:"ledger-wallet"
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
        protocol
        wallet_scenario
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
                ~protocol_kind:protocol.kind
                ~node_exec
                ~size
                ~admin_exec
                ~base_port
                ~pp_error
                ~wallet_scenario
                ~protocol
                ~client_exec
                ~uri)))
    $ Arg.(
        required
          (pos
             0
             (some string)
             None
             (info [] ~docv:"LEDGER-URI" ~docs ~doc:"ledger:// URI")))
    $ Tezos_executable.cli_term base_state `Node "tezos"
    $ Tezos_executable.cli_term base_state `Client "tezos"
    $ Tezos_executable.cli_term base_state `Admin "tezos"
    $ Arg.(value (opt int 2 (info ["size"; "S"] ~doc:"Size of the Network")))
    $ Arg.(
        const (fun p -> `Base_port p)
        $ value
            (opt
               int
               32_000
               (info ["base-port"; "P"] ~doc:"Base port number to build upon")))
    $ Tezos_protocol.cli_term base_state
    $ Wallet_scenario.cli_term ()
    $ Test_command_line.cli_state ~name:"ledger-wallet" ()
  in
  let info =
    let doc = "Interactive test exercising the Ledger Wallet app features" in
    info ~doc "ledger-wallet"
  in
  (term, info)
