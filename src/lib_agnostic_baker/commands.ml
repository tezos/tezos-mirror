(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let run_with_local_node (module Plugin : Protocol_plugin_sig.S) args
    local_data_dir_path sources cctxt =
  let baking_mode = Some local_data_dir_path in
  let configuration = Configuration.create_config args in
  Plugin.Baker_commands_helpers.run_baker
    ~configuration
    ~baking_mode
    ~sources
    ~cctxt

let run_remotely (module Plugin : Protocol_plugin_sig.S) args sources cctxt =
  let baking_mode = None in
  let configuration = Configuration.create_config args in
  Plugin.Baker_commands_helpers.run_baker
    ~configuration
    ~baking_mode
    ~sources
    ~cctxt

let run_vdf (module Plugin : Protocol_plugin_sig.S) (pidfile, keep_alive) cctxt
    =
  Configuration.may_lock_pidfile pidfile @@ fun () ->
  Plugin.Baker_commands_helpers.run_vdf_daemon ~cctxt ~keep_alive

let run_accuser (module Plugin : Protocol_plugin_sig.S)
    (pidfile, preserved_levels, keep_alive) cctxt =
  Configuration.may_lock_pidfile pidfile @@ fun () ->
  Plugin.Accuser_commands_helpers.run ~cctxt ~preserved_levels ~keep_alive

let baker_commands ?plugin () =
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
      (match plugin with
      | Some plugin -> run_with_local_node plugin
      | None -> fun _ _ _ _ -> Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:"Launch the baker daemon using RPCs only."
      baker_args
      (prefixes ["run"; "remotely"] @@ sources_param)
      (match plugin with
      | Some plugin -> run_remotely plugin
      | None -> fun _ _ _ -> Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:"Launch the VDF daemon"
      (args2 pidfile_arg keep_alive_arg)
      (prefixes ["run"; "vdf"] @@ stop)
      (match plugin with
      | Some plugin -> run_vdf plugin
      | None -> fun _ _ -> Lwt_result_syntax.return_unit);
    command
      ~group
      ~desc:"Launch the accuser daemon"
      (args3 pidfile_arg preserved_levels_arg keep_alive_arg)
      (prefixes ["run"; "accuser"] @@ stop)
      (match plugin with
      | Some plugin -> run_accuser plugin
      | None -> fun _ _ -> Lwt_result_syntax.return_unit);
  ]
