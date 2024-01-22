(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let group =
  {
    Tezos_clic.name = "octez-injector";
    title = "Commands related to the injector server";
  }

let data_dir_arg =
  let default = Configuration.default_data_dir in
  Tezos_clic.default_arg
    ~long:"data-dir"
    ~placeholder:"data-dir"
    ~doc:"The path to the injector server data directory"
    ~default
    (Client_config.string_parameter ())

let run_command =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Run the injector server"
    (args1 data_dir_arg)
    (prefixes ["run"] @@ stop)
    (fun data_dir cctxt -> Injector_daemon_http.run ~data_dir cctxt)

module Config_init = struct
  open Lwt_result_syntax
  open Tezos_clic

  let create_configuration ~data_dir ~rpc_address ~rpc_port ~block_delay ~signer
      (cctxt : Client_context.full) =
    let config =
      Configuration.make ~data_dir ~rpc_address ~rpc_port ~block_delay signer
    in
    let* () = Configuration.save config in
    let*! _ =
      cctxt#message
        "Injector server configuration written in %s"
        (Configuration.filename config)
    in
    return ()

  let address_parameter =
    param
      ~name:"signer-address"
      ~desc:"A Tezos address"
      (parameter (fun _cctxt s ->
           let open Lwt_result_syntax in
           return s))

  let command =
    command
      ~group
      ~desc:"Initialize injector server configuration"
      (args4
         (default_arg
            ~doc:"listening address or host name"
            ~short:'a'
            ~long:"address"
            ~placeholder:"host|address"
            ~default:Configuration.default_rpc_address
            (parameter (fun _ s -> return s)))
         (default_arg
            ~doc:"listening HTTP port"
            ~short:'p'
            ~long:"port"
            ~placeholder:"port number"
            ~default:(Configuration.default_rpc_port |> string_of_int)
            (parameter (fun _ s -> return s)))
         (default_arg
            ~doc:"block delay"
            ~short:'b'
            ~long:"block-delay"
            ~placeholder:"block delay"
            ~default:(Configuration.default_block_delay |> string_of_float)
            (parameter (fun _ s -> return s)))
         (default_arg
            ~doc:"data directory"
            ~short:'d'
            ~long:"data-dir"
            ~placeholder:"path"
            ~default:Configuration.default_data_dir
            (parameter (fun _ x -> Lwt.return_ok x))))
      (prefix "init-config" @@ address_parameter @@ stop)
      (fun (rpc_address, rpc_port, block_delay, data_dir) signer cctxt ->
        create_configuration
          ~data_dir
          ~rpc_address
          ~rpc_port
          ~block_delay
          ~signer
          cctxt)
end

let commands () = [Config_init.command; run_command]

let select_commands _ _ =
  let open Lwt_result_syntax in
  return (commands ())

module Daemon_config = struct
  type t = unit

  let global_options () = Tezos_clic.no_options

  let parse_config_args = Client_config.parse_config_args

  let default_chain = Client_config.default_chain

  let default_block = Client_config.default_block

  let default_base_dir = Client_config.default_base_dir

  let default_daily_logs_path = None

  let default_media_type = Daemon_config.default_media_type

  let other_registrations = None

  let clic_commands ~base_dir:_ ~config_commands:_ ~builtin_commands:_
      ~other_commands ~require_auth:_ =
    other_commands

  let logger = None
end

let () = Client_main_run.run (module Daemon_config) ~select_commands
