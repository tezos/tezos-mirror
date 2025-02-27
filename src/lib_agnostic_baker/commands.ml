(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let baker_commands (module Plugin : Protocol_plugin_sig.S) :
    Tezos_client_base.Client_context.full Tezos_clic.command list =
  let open Configuration in
  let open Tezos_clic in
  let group =
    {
      name = "delegate.baker";
      title = "Commands related to the agnostic baker daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the baker daemon."
      baker_args
      (prefixes ["run"; "with"; "local"; "node"]
      @@ param
           ~name:"node_data_path"
           ~desc:"Path to the node data directory (e.g. $HOME/.tezos-node)"
           directory_parameter
      @@ sources_param)
      (fun args local_data_dir_path sources cctxt ->
        let baking_mode = Some local_data_dir_path in
        let configuration = create_config args in
        Plugin.Baker_commands_helpers.run_baker
          ~configuration
          ~baking_mode
          ~sources
          ~cctxt);
    command
      ~group
      ~desc:"Launch the baker daemon using RPCs only."
      baker_args
      (prefixes ["run"; "remotely"] @@ sources_param)
      (fun args sources cctxt ->
        let baking_mode = None in
        let configuration = create_config args in
        Plugin.Baker_commands_helpers.run_baker
          ~configuration
          ~baking_mode
          ~sources
          ~cctxt);
    command
      ~group
      ~desc:"Launch the VDF daemon"
      (args2 pidfile_arg keep_alive_arg)
      (prefixes ["run"; "vdf"] @@ stop)
      (fun (pidfile, keep_alive) cctxt ->
        may_lock_pidfile pidfile @@ fun () ->
        Plugin.Baker_commands_helpers.run_vdf_daemon ~cctxt ~keep_alive);
  ]
