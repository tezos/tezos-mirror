(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module Parameter = struct
  let endpoint =
    Tezos_clic.parameter (fun _ uri -> Lwt.return_ok (Uri.of_string uri))

  let signer =
    Tezos_clic.parameter (fun _ value ->
        let open Lwt_result_syntax in
        match Configuration.gcp_key_from_string_opt value with
        | None ->
            let*? secret_key = Signer.secret_key_from_hex (`Hex value) in
            return (Signer.from_secret_key secret_key)
        | Some key ->
            Signer.from_gcp_key
              {
                pool_size = 2;
                authentication_method = Gcloud_auth;
                authentication_retries = 2;
                authentication_frequency_min = 30;
                authentication_retry_backoff_sec = 5;
                authentication_timeout_sec = 5;
                gcloud_path = "gcloud";
              }
              key)

  let int =
    Tezos_clic.parameter (fun _ n ->
        Option.to_result
          ~none:[error_of_fmt "Expected an integer, got %s" n]
          (int_of_string_opt n)
        |> Lwt.return)

  let float =
    Tezos_clic.parameter (fun _ n ->
        Option.to_result
          ~none:[error_of_fmt "Expected a float, got %s" n]
          (float_of_string_opt n)
        |> Lwt.return)

  let tez =
    Tezos_clic.parameter (fun _ n ->
        let open Lwt_result_syntax in
        Lwt.catch
          (fun () ->
            match String.split_on_char '.' n with
            | [n] -> return Z.(of_int (int_of_string n) * pow (of_int 10) 18)
            | [n; m] ->
                let int_of_string = function
                  | "" -> 0
                  | other -> int_of_string other
                in
                let n = Z.(of_int (int_of_string n) * pow (of_int 10) 18) in
                let m =
                  Z.(
                    of_int (int_of_string m)
                    * pow (of_int 10) Stdlib.(18 - String.length m))
                in
                return (Z.add n m)
            | _ -> assert false)
          (fun _ -> failwith "%s is not a valid balance" n))

  let scenario =
    Tezos_clic.parameter (fun _ token ->
        let open Lwt_result_syntax in
        match String.uppercase_ascii token with
        | "XTZ" -> return `XTZ
        | "ERC20" -> return `ERC20
        | _ -> failwith "Expected <xtz | erc20>, got %s" token)
end

module Arg = struct
  let verbose =
    Tezos_clic.switch
      ~short:'v'
      ~long:"verbose"
      ~doc:"Sets logging level to debug"
      ()

  let endpoint ~default ~long ~doc =
    Tezos_clic.default_arg
      ~default
      ~long
      ~doc
      ~placeholder:"URL"
      Parameter.endpoint

  let relay_endpoint =
    endpoint
      ~default:"https://relay.ghostnet.etherlink.com"
      ~long:"relay-endpoint"
      ~doc:"Endpoint used to inject transactions and receive early confirmation"

  let rpc_endpoint =
    endpoint
      ~default:"https://node.ghostnet.etherlink.com"
      ~long:"rpc-endpoint"
      ~doc:"Endpoint used to fetch the state of the chain"

  let ws_endpoint =
    Tezos_clic.arg
      ~placeholder:"URL"
      ~long:"ws-endpoint"
      ~doc:
        "Enable websocket new heads subscription instead of the blueprint \
         follower using the provided endpoint."
      Parameter.endpoint

  let signer ~long ~short ~doc =
    Tezos_clic.arg ~doc ~long ~short ~placeholder:"SIGNER" Parameter.signer

  let max_active_eoa =
    let default = "1" in
    Tezos_clic.default_arg
      ~default
      ~long:"max-active-eoa"
      ~short:'m'
      ~placeholder:"N"
      ~doc:
        "Maximum number of Externally Owned Accounts (EOA) actively sending \
         transaction during an experiment"
      Parameter.int

  let max_transaction_batch_length =
    Tezos_clic.arg
      ~long:"max-transaction-batch-length"
      ~short:'b'
      ~placeholder:"N"
      ~doc:
        "Maximum number of transactions injected at each tick; if this option \
         is not given there is no limit"
      Parameter.int

  let spawn_interval =
    let default = "0.5" in
    Tezos_clic.default_arg
      ~default
      ~long:"spawn-interval"
      ~short:'s'
      ~placeholder:"SECONDS"
      ~doc:"Minimum time span between the activation of two EOA"
      Parameter.float

  let tick_interval =
    let default = "0.25" in
    Tezos_clic.default_arg
      ~default
      ~long:"tick-interval"
      ~short:'t'
      ~placeholder:"SECONDS"
      ~doc:
        "The time span between two ticks of the Tx queue, responsible for \
         injecting transactions to the relay endpoint"
      Parameter.float

  let base_fee_factor =
    let default = "1.0" in
    Tezos_clic.default_arg
      ~default
      ~long:"base-fee-factor"
      ~short:'f'
      ~placeholder:"FACTOR"
      ~doc:
        "The factor applied to the base fee per gas returned by the \
         `eth_estimateGas` RPC. A factor higher than 1 increases the \
         under-approximation of the balance for each account. A factor lower \
         than 1 will mostly likely to produce invalid transactions."
      Parameter.float

  let initial_balance =
    let default = "1.0" in
    Tezos_clic.default_arg
      ~default
      ~long:"initial-balance"
      ~short:'i'
      ~placeholder:"BALANCE"
      ~doc:
        "The amount of funds to transfer to EOAs for spamming the targeted \
         network (expressed in the primary unit of the targeted EVM-compatible \
         chain, e.g., tez for Etherlink, Eth for Ethereum, etc.)"
      Parameter.tez

  let controller =
    signer
      ~long:"controller"
      ~short:'c'
      ~doc:
        "The secret key (encoded in hexadecimal, with a leading 0x) to an EOA \
         with enough fund to pay for the experiment"

  let scenario =
    let default = "xtz" in
    Tezos_clic.default_arg
      ~default
      ~long:"scenario"
      ~placeholder:"xtz | erc20"
      ~doc:
        "Specifies the transfer mode: ERC20 transfers or native XTZ transfers."
      Parameter.scenario

  let elapsed_time_between_report =
    let default = "60." in
    Tezos_clic.default_arg
      ~default
      ~long:"report"
      ~short:'r'
      ~placeholder:"SECONDS"
      ~doc:"The elapsed time between two reports of measured TPS."
      Parameter.float

  let txs_salvo_eoa =
    let default = "1" in
    Tezos_clic.default_arg
      ~default
      ~long:"txs-per-salvo"
      ~placeholder:""
      ~doc:
        "The number of transactions an EOA inject before waiting for their \
         confirmation."
      Parameter.int

  let dummy_data_size =
    Tezos_clic.arg
      ~long:"dummy-data-size"
      ~placeholder:"BYTES"
      ~doc:
        "Size in bytes of dummy data to include in native XTZ transaction \
         payloads. This allows testing network capacity under load conditions \
         with data-heavy transactions. Only applicable with the `xtz` \
         scenario. If not set, no dummy data will be included in the \
         transactions."
      Parameter.int

  let retry_attempt =
    let default = "never" in
    Tezos_clic.default_arg
      ~default
      ~long:"retry-attempt"
      ~short:'a'
      ~placeholder:"never|always|INT"
      ~doc:
        "The number of time a transaction is retried when it has been dropped."
      (Tezos_clic.parameter
         Floodgate.(
           fun _ s ->
             let open Lwt_result_syntax in
             match s with
             | "always" -> return Always
             | "never" -> return Never
             | s ->
                 let*? n =
                   Option.to_result
                     ~none:
                       [
                         error_of_fmt
                           "Expected an integer, \"always\" or \"never\", got \
                            %s"
                           s;
                       ]
                     (int_of_string_opt s)
                 in
                 return (Number n)))
end

let log_config ~verbose () =
  let open Tezos_base_unix.Internal_event_unix in
  let config =
    make_with_defaults ~verbosity:(if verbose then Info else Notice) ()
  in
  init ~config ()

let require ~error_msg arg =
  let open Lwt_result_syntax in
  match arg with Some arg -> return arg | None -> failwith "%s" error_msg

let run_command =
  let open Tezos_clic in
  command
    ~desc:"Start Floodgate to spam an EVM-compatible network"
    Arg.(
      args16
        verbose
        relay_endpoint
        rpc_endpoint
        ws_endpoint
        controller
        max_active_eoa
        max_transaction_batch_length
        spawn_interval
        tick_interval
        base_fee_factor
        initial_balance
        scenario
        txs_salvo_eoa
        elapsed_time_between_report
        dummy_data_size
        retry_attempt)
    (prefixes ["run"] @@ stop)
    (fun ( verbose,
           relay_endpoint,
           rpc_endpoint,
           ws_endpoint,
           controller,
           max_active_eoa,
           max_transaction_batch_length,
           spawn_interval,
           tick_interval,
           base_fee_factor,
           initial_balance,
           scenario,
           txs_per_salvo,
           elapsed_time_between_report,
           dummy_data_size,
           retry_attempt )
         ()
       ->
      let open Lwt_result_syntax in
      let*! () = log_config ~verbose () in
      let* controller =
        require ~error_msg:"Missing argument --controller" controller
      in
      let* () =
        if Option.is_some dummy_data_size && scenario <> `XTZ then
          failwith
            "the --dummy-data-size argument is only applicable with the `xtz` \
             scenario."
        else return_unit
      in
      Floodgate.run
        ~relay_endpoint
        ~rpc_endpoint
        ~ws_endpoint
        ~controller
        ~max_active_eoa
        ~max_transaction_batch_length
        ~spawn_interval
        ~tick_interval
        ~base_fee_factor
        ~initial_balance
        ~txs_per_salvo
        ~elapsed_time_between_report
        ~scenario
        ~dummy_data_size
        ~retry_attempt)

let commands = [run_command]

let executable_name = Filename.basename Sys.executable_name

let global_options = Tezos_clic.no_options

let dispatch args =
  let open Lwt_result_syntax in
  let commands =
    Tezos_clic.add_manual
      ~executable_name
      ~global_options
      (if Unix.isatty Unix.stdout then Tezos_clic.Ansi else Tezos_clic.Plain)
      Format.std_formatter
      commands
  in
  let* (), remaining_args =
    Tezos_clic.parse_global_options global_options () args
  in
  Tezos_clic.dispatch commands () remaining_args

let handle_error = function
  | Ok _ -> ()
  | Error [Tezos_clic.Version] ->
      Format.printf "floodgate.0.0~dev\n" ;
      exit 0
  | Error [Tezos_clic.Help command] ->
      Tezos_clic.usage
        Format.std_formatter
        ~executable_name
        ~global_options
        (match command with None -> [] | Some c -> [c]) ;
      Stdlib.exit 0
  | Error errs ->
      Tezos_clic.pp_cli_errors
        Format.err_formatter
        ~executable_name
        ~global_options
        ~default:Error_monad.pp
        errs ;
      Stdlib.exit 1

let argv () = Array.to_list Sys.argv |> List.tl |> Stdlib.Option.get

let () =
  Random.self_init () ;
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stdout)
        Format.std_formatter
        Short) ;
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Tezos_base_unix.Event_loop.main_run
    ~process_name:"etherlink floodgate"
    (fun () -> Lwt_exit.wrap_and_exit (dispatch (argv ())))
  |> handle_error
