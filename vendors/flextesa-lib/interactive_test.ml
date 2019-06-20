open Internal_pervasives
open Console

module Commands = struct
  let cmdline_fail fmt = Format.kasprintf (fun s -> fail (`Command_line s)) fmt

  let no_args = function
    | [] -> return ()
    | _more -> cmdline_fail "this command expects no arguments"

  let flag f sexps = List.mem sexps (Base.Sexp.Atom f) ~equal:Base.Sexp.equal

  let unit_loop_no_args doc opts f =
    Prompt.unit_and_loop doc opts (fun sexps ->
        no_args sexps >>= fun () -> f ())

  module Sexp_options = struct
    let option_doc pattern doc = EF.(desc (haf "`%s`:" pattern) doc)

    let option_list_doc = function
      | [] -> EF.(wf "(no-options)")
      | l -> EF.(desc_list (wf "Options:") l)

    let port_number_doc _ ~default_port =
      option_doc "(port <int>)"
        EF.(wf "use port-number <int> (default: %d)" default_port)

    let port_number state ~default_port sexps =
      match
        List.find_map sexps
          ~f:
            Base.Sexp.(
              function
              | List [Atom "port"; Atom p] -> (
                try Some (`Ok (Int.of_string p))
                with _ -> Some (`Not_an_int p) )
              | List (Atom "port" :: other) -> Some (`Wrong_option other)
              | _other -> None)
      with
      | None -> return default_port
      | Some (`Ok p) -> return p
      | Some ((`Not_an_int _ | `Wrong_option _) as other) ->
          say state
            EF.(
              desc
                (shout "Error parsing port option:")
                ( match other with
                | `Not_an_int s ->
                    af "This is not an integer: %S, using default: %d" s
                      default_port
                | `Wrong_option _sexps ->
                    af "Usage (port <int>), using default: %d" default_port ))
          >>= fun () -> return default_port
  end

  let du_sh_root state =
    unit_loop_no_args
      EF.(af "Run du -sh on %s" (Paths.root state))
      ["d"; "du-root"]
      (fun () ->
        Running_processes.run_cmdf state "du -sh %s" (Paths.root state)
        >>= fun du ->
        display_errors_of_command state du
        >>= function
        | true ->
            say state
              EF.(
                desc (haf "Disk-Usage:")
                  (af "%s" (String.concat ~sep:" " du#out)))
        | false -> return ())

  let processes state =
    Prompt.unit_and_loop
      EF.(
        af "Display status of processes-manager ('all' to include non-running)")
      ["p"; "processes"]
      (fun sxp ->
        let all = flag "all" sxp in
        say state (Running_processes.ef ~all state))

  let curl_rpc state ~port ~path =
    Running_processes.run_cmdf state "curl http://localhost:%d/%s" port path
    >>= fun curl_res ->
    display_errors_of_command state curl_res ~should_output:true
    >>= fun success ->
    if not success then return None
    else
      ( try
          Ezjsonm.value_from_string (String.concat ~sep:"\n" curl_res#out)
          |> return
        with e -> cmdline_fail "Parsing JSON: %s" (Exn.to_string e) )
      >>= fun json -> return (Some json)

  let do_jq _state ~msg ~f = function
    | None -> cmdline_fail "%s: No JSON" msg
    | Some json -> (
      try return (f json)
      with e ->
        cmdline_fail "%s: failed to analyze JSON: %s from %s" msg
          (Exn.to_string e)
          (Ezjsonm.value_to_string ~minify:false json) )

  let curl_unit_display ?(jq = fun e -> e) state cmd ~default_port ~path ~doc =
    Prompt.unit_and_loop
      EF.(
        desc (af "%s" doc)
          (desc_list (af "Options:")
             [Sexp_options.port_number_doc state ~default_port]))
      cmd
      (fun sexps ->
        Sexp_options.port_number state sexps ~default_port
        >>= fun port ->
        curl_rpc state ~port ~path
        >>= fun json_opt ->
        do_jq ~msg:doc state json_opt ~f:jq
        >>= fun json ->
        say state EF.(desc (af "Curl-Node :%d" port) (ef_json doc json)))

  let curl_metadata state ~default_port =
    curl_unit_display state ["m"; "metadata"] ~default_port
      ~path:"/chains/main/blocks/head/metadata"
      ~doc:"Display `/chains/main/blocks/head/metadata`"

  let curl_level state ~default_port =
    curl_unit_display state ["l"; "level"] ~default_port
      ~path:"/chains/main/blocks/head/metadata" ~doc:"Display block level"
      ~jq:(Jqo.field ~k:"level")

  let curl_baking_rights state ~default_port =
    curl_unit_display state ["bk"; "baking-rights"] ~default_port
      ~path:"/chains/main/blocks/head/helpers/baking_rights"
      ~doc:"Display baking rights"

  let all_levels state ~nodes =
    unit_loop_no_args
      EF.(af "Get all the levels")
      ["al"; "all-levels"]
      (fun () ->
        Test_scenario.Queries.all_levels state ~nodes
        >>= fun results ->
        say state
          EF.(
            desc (af "Node-levels:")
              (list
                 (List.map results ~f:(fun (id, result) ->
                      desc (haf "%s" id)
                        ( match result with
                        | `Failed -> af "Failed"
                        | `Level i -> af "[%d]" i
                        | `Null -> af "{Null}"
                        | `Unknown s -> af "¿%s?" s ))))))

  let show_process state =
    Prompt.unit_and_loop
      EF.(af "Show more of a process (by name-prefix)")
      ["show"]
      (function
        | [Atom name] ->
            let prefix = String.lowercase name in
            Running_processes.find_process_by_id state ~f:(fun n ->
                String.is_prefix (String.lowercase n) ~prefix)
            >>= fun procs ->
            List.fold procs ~init:(return []) ~f:(fun prevm {process; lwt} ->
                prevm
                >>= fun prev ->
                let open Running_processes in
                let out = output_path state process `Stdout in
                let err = output_path state process `Stderr in
                Running_processes.run_cmdf state "tail %s" out
                >>= fun tailout ->
                Running_processes.run_cmdf state "tail %s" err
                >>= fun tailerr ->
                return
                  EF.(
                    desc_list
                      (haf "%S (%d)" process.Process.id lwt#pid)
                      [ desc (af "out: %s" out) (ocaml_string_list tailout#out)
                      ; desc (af "err: %s" err) (ocaml_string_list tailerr#out)
                      ]
                    :: prev))
            >>= fun ef -> say state EF.(list ef)
        | _other -> cmdline_fail "command expects 1 argument: name-prefix")

  let kill_all state =
    unit_loop_no_args
      EF.(af "Kill all processes.")
      ["ka"; "killall"]
      (fun () -> Running_processes.kill_all state)

  let secret_keys state ~protocol =
    unit_loop_no_args
      EF.(af "Show the protocol's “bootstrap” accounts")
      ["boa"; "bootstrap-accounts"]
      (fun () ->
        say state
          EF.(
            desc (af "Secret Keys:")
              (ocaml_list
                 (List.map (Tezos_protocol.bootstrap_accounts protocol)
                    ~f:(fun acc ->
                      let open Tezos_protocol.Account in
                      ocaml_tuple
                        [ atom (name acc)
                        ; af "Pub:%s" (pubkey acc)
                        ; af "Hash:%s" (pubkey_hash acc)
                        ; atom (private_key acc) ])))))

  let show_connections state nodes =
    unit_loop_no_args
      EF.(af "Show all node connections")
      ["ac"; "all-connections"]
      (fun () -> Helpers.dump_connections state nodes)

  let balances state ~default_port =
    Prompt.unit_and_loop
      EF.(
        desc
          (wf "Show the balances of all known accounts")
          (desc_list (wf "Options")
             [Sexp_options.port_number_doc state ~default_port]))
      ["sb"; "show-balances"]
      (fun sexps ->
        Sexp_options.port_number state sexps ~default_port
        >>= fun port ->
        curl_rpc state ~port ~path:"/chains/main/blocks/head/context/contracts"
        >>= fun json_opt ->
        do_jq state ~msg:"Getting contract list" ~f:Jqo.get_strings json_opt
        >>= fun contracts ->
        curl_rpc state ~port ~path:"/chains/main/checkpoint"
        >>= fun chkpto ->
        do_jq state chkpto ~msg:"Getting checkpoint"
          ~f:
            Jqo.(
              fun json ->
                match field ~k:"history_mode" json |> get_string with
                | "archive" -> 1
                | _ ->
                    let sp = field ~k:"save_point" json |> get_int in
                    Int.max 1 sp)
        >>= fun save_point ->
        let balance block contract =
          let path =
            sprintf "/chains/main/blocks/%s/context/contracts/%s/balance" block
              contract in
          curl_rpc state ~port ~path
          >>= fun jo ->
          do_jq state jo ~msg:"Getting balance" ~f:(fun j ->
              Jqo.get_string j |> Int.of_string) in
        List.fold contracts ~init:(return []) ~f:(fun prevm hsh ->
            prevm
            >>= fun prev ->
            balance (Int.to_string save_point) hsh
            >>= fun init ->
            balance "head" hsh
            >>= fun current -> return ((hsh, init, current) :: prev))
        >>= fun results ->
        say state
          EF.(
            desc_list
              (af "Balances from levels %d to “head” (port :%d)" save_point
                 port)
              (List.map results ~f:(fun (hsh, init, cur) ->
                   let tz i = float i /. 1_000_000. in
                   desc (haf "%s:" hsh)
                     ( if init = cur then af "%f (unchanged)" (tz cur)
                     else af "%f → %f" (tz init) (tz cur) )))))

  let arbitrary_command_on_all_clients ?make_admin
      ?(command_names = ["atc"; "all-clients"]) state ~clients =
    Prompt.unit_and_loop
      EF.(
        desc
          (wf "Run a tezos-client command on %s"
             ( match clients with
             | [] -> "NO CLIENT, so this is useless…"
             | [one] -> sprintf "the %S client." one.Tezos_client.id
             | more ->
                 sprintf "all the following clients: %s."
                   ( List.map more ~f:(fun c -> c.Tezos_client.id)
                   |> String.concat ~sep:", " ) ))
          Sexp_options.(
            let only_opt =
              option_doc "(only <name1> <name2>)"
                (wf "Restrict the clients by name") in
            ( (match clients with [_] -> [] | _ -> [only_opt])
            @
            match make_admin with
            | None -> []
            | _ -> [option_doc "(admin)" (wf "Use the admin-client instead.")]
            )
            |> option_list_doc))
      command_names
      (fun sexps ->
        let args =
          let open Base.Sexp in
          List.filter_map sexps ~f:(function Atom s -> Some s | _ -> None)
        in
        let subset_of_clients =
          let open Base.Sexp in
          List.find_map sexps ~f:(function
            | List (Atom "only" :: l) ->
                Some
                  (List.map l ~f:(function
                    | Atom a -> a
                    | other ->
                        ksprintf failwith
                          "Option `only` only accepts a list of names: %s"
                          (to_string_hum other)))
            | _ -> None)
          |> function
          | None -> clients
          | Some more ->
              List.filter clients ~f:(fun c ->
                  List.mem more c.Tezos_client.id ~equal:String.equal) in
        let use_admin =
          match make_admin with
          | None -> `Client
          | Some of_client ->
              if
                List.exists sexps
                  ~f:
                    Base.Sexp.(
                      function List [Atom "admin"] -> true | _ -> false)
              then `Admin of_client
              else `Client in
        List.fold ~init:(return []) subset_of_clients ~f:(fun prevm client ->
            prevm
            >>= fun prev ->
            Running_processes.run_cmdf state "sh -c %s"
              ( ( match use_admin with
                | `Client -> Tezos_client.client_command client ~state args
                | `Admin mkadm ->
                    Tezos_admin_client.make_command (mkadm client) state args
                )
              |> Genspio.Compile.to_one_liner |> Filename.quote )
            >>= fun res ->
            display_errors_of_command state res
            >>= function
            | true -> return ((client, String.concat ~sep:"\n" res#out) :: prev)
            | false -> return prev)
        >>= fun results ->
        let different_results =
          List.dedup_and_sort results ~compare:(fun (_, a) (_, b) ->
              String.compare a b) in
        say state
          EF.(
            desc_list (af "Done")
              [ desc (haf "Command:")
                  (ocaml_string_list
                     ( ( match use_admin with
                       | `Client -> "<client>"
                       | `Admin _ -> "<admin>" )
                     :: args ))
              ; desc (haf "Results")
                  (list
                     (List.map different_results ~f:(fun (_, res) ->
                          let clients =
                            List.filter_map results ~f:(function
                              | c, r when res = r -> Some c.Tezos_client.id
                              | _ -> None) in
                          desc
                            (haf "Client%s %s:"
                               ( if List.length subset_of_clients = 1 then ""
                               else "s" )
                               (String.concat ~sep:", " clients))
                            (markdown_verbatim res))));
              ]))

  let arbitrary_commands_for_each_client ?make_admin
      ?(make_command_names = fun i -> [sprintf "c%d" i; sprintf "client-%d" i])
      state ~clients =
    List.mapi clients ~f:(fun i c ->
        arbitrary_command_on_all_clients state ?make_admin ~clients:[c]
          ~command_names:(make_command_names i))

  let arbitrary_commands_for_each_and_all_clients ?make_admin
      ?make_individual_command_names ?all_clients_command_names state ~clients
      =
    arbitrary_command_on_all_clients state ?make_admin ~clients
      ?command_names:all_clients_command_names
    :: arbitrary_commands_for_each_client state ?make_admin ~clients
         ?make_command_names:make_individual_command_names

  let bake_command state ~clients =
    Prompt.unit_and_loop
      EF.(
        wf "Manually bake a block (with %s)."
          ( match clients with
          | [] -> "NO CLIENT, this is just wrong"
          | [one] -> one.Tezos_client.Keyed.client.id
          | m ->
              sprintf "one of %s"
                ( List.mapi m ~f:(fun ith one ->
                      sprintf "%d: %s" ith one.Tezos_client.Keyed.client.id)
                |> String.concat ~sep:", " ) ))
      ["bake"]
      (fun sexps ->
        let client =
          let open Base.Sexp in
          match sexps with
          | [] -> List.nth_exn clients 0
          | [Atom s] -> List.nth_exn clients (Int.of_string s)
          | _ -> Fmt.kstrf failwith "Wrong command line: %a" pp (List sexps)
        in
        Asynchronous_result.bind_on_error
          (Fmt.kstrf
             (Tezos_client.Keyed.bake state client)
             "Command-line baking with client %s (account: %s)"
             client.Tezos_client.Keyed.client.id
             client.Tezos_client.Keyed.key_name)
          ~f:(fun ~result:_ -> function
            | `Client_command_error (m, _) -> cmdline_fail "Error: %s" m
            | #System_error.t as e ->
                cmdline_fail "Error: %a" System_error.pp e))

  let all_defaults state ~nodes =
    let default_port = (List.hd_exn nodes).Tezos_node.rpc_port in
    [ du_sh_root state; processes state
    ; show_connections state nodes
    ; curl_level state ~default_port
    ; balances state ~default_port
    ; curl_metadata state ~default_port
    ; curl_baking_rights state ~default_port
    ; all_levels state ~nodes; show_process state; kill_all state ]
end

module Interactivity = struct
  type t = [`Full | `None | `On_error | `At_end]

  let is_interactive (state : < test_interactivity: t ; .. >) =
    state#test_interactivity = `Full

  let pause_on_error state =
    match state#test_interactivity with
    | `Full | `On_error | `At_end -> true
    | `None -> false

  let pause_on_success state =
    match state#test_interactivity with
    | `Full | `At_end -> true
    | `None | `On_error -> false

  let cli_term ?(default : t = `None) () =
    let open Cmdliner in
    Term.(
      pure (fun interactive pause_end pause_error ->
          match (interactive, pause_end, pause_error) with
          | true, _, _ -> `Full
          | false, true, _ -> `At_end
          | false, false, true -> `On_error
          | false, false, false -> `None)
      $ Arg.(
          value
          & opt bool
              ( match default with
              | `None | `On_error | `At_end -> false
              | `Full -> true )
          & info ["interactive"] ~doc:"Add all pauses with command prompts.")
      $ Arg.(
          value
          & opt bool
              ( match default with
              | `None | `On_error -> false
              | `At_end | `Full -> true )
          & info ["pause-at-end"]
              ~doc:"Add a pause with a command prompt at the end of the test.")
      $ Arg.(
          value
          & opt bool
              ( match default with
              | `None -> false
              | `At_end | `Full | `On_error -> true )
          & info ["pause-on-error"]
              ~doc:
                "Add a pause with a command prompt at the end of the test, \
                 only in case of test failure."))
end

module Pauser = struct
  type t =
    {mutable extra_commands: Prompt.item list; default_end: [`Sleep of float]}

  let make ?(default_end = `Sleep 0.5) extra_commands =
    {extra_commands; default_end}

  let commands state = state#pauser.extra_commands
  let default_end state = state#pauser.default_end

  let add_commands state cl =
    state#pauser.extra_commands <- commands state @ cl

  let generic state ?(force = false) msgs =
    let do_pause = Interactivity.is_interactive state || force in
    say state
      EF.(
        desc
          (if do_pause then haf "Pause" else haf "Not pausing")
          (list ~param:{default_list with space_before_separator= false} msgs))
    >>= fun () ->
    if do_pause then Prompt.(command state ~commands:(commands state))
    else return ()

  let run_test state f ~pp_error () =
    let finish () =
      say state EF.(af "Killing all processes.")
      >>= fun () ->
      Running_processes.kill_all state
      >>= fun () ->
      say state EF.(af "Waiting for processes to all die.")
      >>= fun () -> Running_processes.wait_all state in
    Sys.catch_break false ;
    let cond = Lwt_condition.create () in
    let catch_signals () =
      Lwt_unix.on_signal Sys.sigint (fun i ->
          Printf.eprintf
            "\nReceived signal SIGINT (%d), type `q` to quit prompts.\n\n%!" i ;
          Lwt_condition.broadcast cond `Sig_int)
      |> ignore ;
      Lwt_unix.on_signal Sys.sigterm (fun i ->
          Printf.eprintf
            "\nReceived signal SIGTERM (%d), type `q` to quit prompts.\n\n%!" i ;
          Lwt_condition.broadcast cond `Sig_term)
      |> ignore in
    let wait_on_signals () =
      System_error.catch Lwt_condition.wait cond
      >>= fun sig_name -> return (`Woken_up_by_signal sig_name) in
    Dbg.e
      EF.(wf "Running test %s on %s" state#application_name (Paths.root state)) ;
    let wrap_result f = f () >>= fun o -> return (`Successful_procedure o) in
    let run_with_signal_catching ~name procedure =
      Lwt.(
        pick
          [ ( wrap_result (fun () -> procedure ())
            >>= fun res ->
            match res.Attached_result.result with
            | Ok _ ->
                Dbg.e EF.(wf "Procedure %s ended: ok" name) ;
                return res
            | Error (`System_error (_, System_error.Exception Lwt.Canceled)) ->
                Dbg.e EF.(wf "Procedure %s was cancelled" name) ;
                System.sleep 2.
                >>= fun _ ->
                Dbg.e EF.(wf "Procedure %s slept 2." name) ;
                return res
            | Error e ->
                Dbg.e
                  EF.(wf "Procedure %s ended with error %a" name pp_error e) ;
                return res )
          ; ( wait_on_signals ()
            >>= fun res ->
            Dbg.e EF.(wf "Signal-wait %S go woken up" name) ;
            return res ) ]) in
    let last_pause_status = ref `Not_done in
    let rec protect ~name procedure =
      catch_signals () ;
      Dbg.e EF.(wf "protecting %S" name) ;
      let last_pause () =
        protect ~name:"Last-pause" (fun () ->
            generic state ~force:true
              EF.
                [ haf
                    "Last pause before the application will Kill 'Em All and \
                     Quit." ]
            >>= fun () ->
            last_pause_status := `Done ;
            return ()) in
      Asynchronous_result.bind_on_result
        ( try
            Dbg.e EF.(wf "protecting: %S in-try" name) ;
            run_with_signal_catching ~name procedure
          with e ->
            System_error.fail_fatalf
              ~attach:[("protected", `String_value name)]
              "protecting %S: ocaml-exn: %a" name Exn.pp e )
        ~f:(function
          | Ok (`Successful_procedure o) -> return (`Was_ok o)
          | Ok (`Woken_up_by_signal `Sig_term) ->
              Console.say state
                EF.(
                  desc (shout "Received SIGTERM")
                    (wf
                       "Will not pause because it's the wrong thing to do; \
                        killing everything and quitting."))
              >>= fun () -> return `Quit_with_error
          | Ok (`Woken_up_by_signal `Sig_int) ->
              Console.say state
                EF.(
                  desc (shout "Received SIGINT")
                    (wf
                       "Please the command `q` (a.k.a. `quit`) for quitting \
                        prompts."))
              >>= fun () -> return `Error_that_can_be_interactive
          | Error (`System_error (`Fatal, System_error.Exception End_of_file))
            ->
              Console.say state
                EF.(
                  desc
                    (shout "Received End-of-File (Ctrl-D?)")
                    (wf
                       "Cannot pause because interactivity broken; killing \
                        everything and quitting."))
              >>= fun () -> return `Quit_with_error
          | Error e ->
              Console.say state
                EF.(
                  desc
                    (shout "An error happened")
                    (custom (fun ppf -> pp_error ppf e)))
              >>= fun () -> return `Error_that_can_be_interactive)
      >>= fun todo_next ->
      match todo_next with
      | `Was_ok ()
        when Interactivity.pause_on_success state
             && !last_pause_status = `Not_done ->
          last_pause ()
      | `Was_ok () -> (
          finish ()
          >>= fun () ->
          match default_end state with
          | `Sleep n ->
              say state EF.(wf "Test done, sleeping %.02f seconds" n)
              >>= fun () -> System.sleep n )
      | `Error_that_can_be_interactive when Interactivity.pause_on_error state
        ->
          last_pause ()
      | `Error_that_can_be_interactive | `Quit_with_error ->
          finish () >>= fun () -> Asynchronous_result.die 4 in
    protect f ~name:"Main-function"
    >>= fun () ->
    Dbg.e EF.(wf "Finiching Interactive_test.run_test") ;
    return ()
end
