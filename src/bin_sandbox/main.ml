open Flextesa
open Internal_pervasives

module Small_utilities = struct
  let key_of_name_command () =
    let open Cmdliner in
    let open Term in
    ( ( pure (fun n ->
            let open Tezos_protocol.Account in
            let account = of_name n in
            Printf.printf
              "%s,%s,%s,%s\n%!"
              (name account)
              (pubkey account)
              (pubkey_hash account)
              (private_key account))
      $ Arg.(
          required
            (pos
               0
               (some string)
               None
               (info [] ~docv:"NAME" ~doc:"String to generate the data from.")))
      ),
      info
        "key-of-name"
        ~doc:"Make an unencrypted key-pair deterministically from a string."
        ~man:
          [ `P
              "`flextesa key-of-name hello-world` generates a key-pair of the \
               `unencrypted:..` kind and outputs it as a 4 values separated \
               by commas: `name,pub-key,pub-key-hash,private-uri` (hence \
               compatible with the `--add-bootstrap-account` option of some \
               of the test scenarios)." ] )

  let netstat_ports ~pp_error () =
    let open Cmdliner in
    let open Term in
    Test_command_line.Run_command.make
      ~pp_error
      ( pure (fun state ->
            ( state,
              fun () ->
                Test_scenario.Network.netstat_listening_ports state
                >>= fun ports ->
                let to_display =
                  List.map ports ~f:(fun (p, _) -> p)
                  |> List.sort ~compare:Int.compare
                in
                Console.sayf
                  state
                  Fmt.(
                    hvbox ~indent:2 (fun ppf () ->
                        box words ppf "Netstat listening ports:" ;
                        sp ppf () ;
                        box
                          (list
                             ~sep:(fun ppf () -> string ppf "," ; sp ppf ())
                             (fun ppf p -> fmt "%d" ppf p))
                          ppf
                          to_display)) ))
      $ Test_command_line.cli_state
          ~disable_interactivity:true
          ~name:"netstat-ports"
          () )
      (info
         "netstat-listening-ports"
         ~doc:"Like `netstat -nut | awk something-something` but glorified.")

  let all ~pp_error () = [key_of_name_command (); netstat_ports ~pp_error ()]
end

let () =
  let open Cmdliner in
  let help = Term.(ret (pure (`Help (`Auto, None))), info "help") in
  let pp_error fmt = function
    | `Scenario_error s ->
        Format.fprintf fmt "%s" s
    | #Test_scenario.Inconsistency_error.t as e ->
        Format.fprintf fmt "%a" Test_scenario.Inconsistency_error.pp e
    | #Process_result.Error.t as e ->
        Format.fprintf fmt "%a" Process_result.Error.pp e
    | #System_error.t as e ->
        Format.fprintf fmt "%a" System_error.pp e
    | `Client_command_error _ as e ->
        Tezos_client.Command_error.pp fmt e
    | `Admin_command_error _ as e ->
        Tezos_admin_client.Command_error.pp fmt e
    | `Waiting_for (msg, `Time_out) ->
        Format.fprintf fmt "WAITING-FOR “%s”: Time-out" msg
    | `Precheck_failure _ as p ->
        Helpers.System_dependencies.Error.pp fmt p
    | `Die _ ->
        ()
  in
  Term.exit
  @@ Term.eval_choice
       (help : unit Term.t * _)
       ( Small_utilities.all ~pp_error ()
       @ [ Command_daemons_protocol_change.cmd () ~pp_error;
           Command_voting.cmd () ~pp_error;
           Command_accusations.cmd () ~pp_error;
           Command_prevalidation.cmd () ~pp_error;
           Command_ledger_baking.cmd () ~pp_error;
           Command_ledger_wallet.cmd () ~pp_error;
           Flextesa.Interactive_mini_network.cmd ~pp_error () ] )
