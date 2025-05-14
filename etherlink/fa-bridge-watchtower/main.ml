(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type global = {data_dir : string; verbosity : Internal_event.level}

let default_global = {data_dir = Config.default_data_dir; verbosity = Notice}

module Parameter = struct
  let endpoint =
    Tezos_clic.parameter (fun _ uri -> Lwt.return_ok (Uri.of_string uri))
end

module Secret_key = struct
  let ctxt = Efunc_core.Eth.Crypto.context ()

  let from_hex_string v =
    let open Lwt_result_syntax in
    try
      match (String.sub v 0 2, String.sub v 2 (String.length v - 2)) with
      | "0x", value ->
          let res =
            Libsecp256k1.External.Key.read_sk_exn ctxt
            @@ Hex.(to_bigstring_exn (`Hex value))
          in
          return res
      | _, _ -> raise (Invalid_argument "secret_key_from_hex_value")
    with _ ->
      fail [error_of_fmt "%s is not a valid hexa-encoded secret key" v]
end

module Arg = struct
  let verbose =
    Tezos_clic.switch
      ~short:'v'
      ~long:"verbose"
      ~doc:"Sets logging level to info"
      ()

  let debug =
    Tezos_clic.switch ~long:"debug" ~doc:"Sets logging level to debug" ()

  let endpoint ~long ~doc =
    Tezos_clic.arg ~long ~doc ~placeholder:"URL" Parameter.endpoint

  let evm_node_endpoint =
    endpoint
      ~long:"evm-node"
      ~doc:"Endpoint to reach EVM node. Websocket must be available on `/ws`."

  let data_dir =
    Tezos_clic.arg
      ~long:"data-dir"
      ~short:'d'
      ~doc:
        (Format.sprintf
           "Directory where data is stored for the FA bridge watchtower.\n\
            Defaults to `%s`."
           Config.default_data_dir)
      ~placeholder:"path"
    @@ Tezos_clic.parameter (fun _ x -> Lwt_result.return x)

  let secret_key =
    Tezos_clic.arg
      ~long:"secret-key"
      ~placeholder:"0x..."
      ~doc:"secret_key"
      ~env:"FA_BRIDGE_WATCHTOWER_SK"
      (Tezos_clic.parameter (fun _ s -> Secret_key.from_hex_string s))

  let first_block =
    Tezos_clic.arg
      ~long:"first-block"
      ~doc:
        "First block number from which to look at deposits when running for \
         the first time."
      ~placeholder:"number"
    @@ Tezos_clic.parameter (fun _ x ->
           Lwt_result.return (Ethereum_types.Qty (Z.of_string x)))
end

let log_config ~verbosity ~data_dir =
  let open Tezos_base_unix.Internal_event_unix in
  let config =
    make_with_defaults
      ~verbosity
      ~log_cfg:
        (Tezos_base_unix.Logs_simple_config.create_cfg
           ~advertise_levels:true
           ())
      ()
  in
  let uri =
    Tezos_base.Internal_event_config.make_config_uri
      ~create_dirs:true
      ~daily_logs:15
      ~level:Info
      ~format:"pp-rfc5424"
      ~chmod:0o640
      ~section_prefixes:[]
      ~advertise_levels:true
      (`Path Filename.Infix.(data_dir // "daily_logs" // "daily.log"))
  in
  let config = Tezos_base.Internal_event_config.add_uri_to_config uri config in
  init ~config ()

let global_options_args =
  let open Tezos_clic in
  let open Arg in
  map_arg
    (aggregate (args3 data_dir verbose debug))
    ~f:(fun g (data_dir, verbose, debug) ->
      let verbosity =
        if debug then Internal_event.Debug
        else if verbose then Info
        else g.verbosity
      in
      let data_dir = Option.value data_dir ~default:g.data_dir in
      Lwt_result.return {data_dir; verbosity})

let global_options = Tezos_clic.args1 global_options_args

let om_command ~desc args cmd_prefixes f =
  let open Tezos_clic in
  command
    ~desc
    (args2 global_options_args (aggregate args))
    cmd_prefixes
    (fun (global, args) -> f global args)

let run_command =
  let open Tezos_clic in
  om_command
    ~desc:"Start FA bridge watchtower"
    (args3 Arg.evm_node_endpoint Arg.secret_key Arg.first_block)
    (prefixes ["run"] @@ stop)
    (fun {data_dir; verbosity} (evm_node_endpoint, secret_key, first_block) _ ->
      let open Lwt_result_syntax in
      let* loaded_config = Config.load_file ~data_dir in
      let config = Option.value ~default:Config.default loaded_config in
      let*! () = log_config ~verbosity ~data_dir in
      let* db = Db.init ~data_dir `Read_write in
      let config = Config.patch_config config ~secret_key ~evm_node_endpoint in
      let* () =
        match config.secret_key with
        | Some _ -> return_unit
        | None ->
            failwith "secret key not provided neither in config, cli nor env"
      in
      let*! notify_ws_change =
        match config.rpc with
        | None -> Lwt.return ignore
        | Some rpc -> Rpc_server.start db config rpc
      in
      Etherlink_monitor.start db ~config ~first_block ~notify_ws_change)

let commands = [run_command]

let executable_name = Filename.basename Sys.executable_name

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
  let* global, remaining_args =
    Tezos_clic.parse_global_options global_options default_global args
  in
  Tezos_clic.dispatch commands global remaining_args

let handle_error = function
  | Ok _ -> ()
  | Error [Tezos_clic.Version] ->
      Format.printf
        "fa-bridge-watchtower.%s (%s)@."
        Tezos_version_value.Current_git_info.abbreviated_commit_hash
        Tezos_version_value.Current_git_info.committer_date ;
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
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stderr)
        Format.err_formatter
        Short) ;
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Tezos_base_unix.Event_loop.main_run (fun () ->
      Lwt_exit.wrap_and_exit (dispatch (argv ())))
  |> handle_error
