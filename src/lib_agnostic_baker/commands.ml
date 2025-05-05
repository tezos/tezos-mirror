(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let baker_commands =
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
      (fun args data_dir sources cctxt ->
        let args = Configuration.create_config args in
        Daemon.Baker.run
          ~keep_alive:args.keep_alive
          ~command:(Daemon.Run_with_local_node {data_dir; args; sources})
          cctxt);
    command
      ~group
      ~desc:"Launch the baker daemon using RPCs only."
      baker_args
      (prefixes ["run"; "remotely"] @@ sources_param)
      (fun args sources cctxt ->
        let args = Configuration.create_config args in
        Daemon.Baker.run
          ~keep_alive:args.keep_alive
          ~command:(Daemon.Run_remotely {args; sources})
          cctxt);
    command
      ~group
      ~desc:"Launch the VDF daemon"
      (args2 pidfile_arg keep_alive_arg)
      (prefixes ["run"; "vdf"] @@ stop)
      (fun (pidfile, keep_alive) cctxt ->
        Daemon.Baker.run
          ~keep_alive
          ~command:(Daemon.Run_vdf {pidfile; keep_alive})
          cctxt);
    command
      ~group
      ~desc:"Launch the accuser daemon"
      (args3 pidfile_arg preserved_levels_arg keep_alive_arg)
      (prefixes ["run"; "accuser"] @@ stop)
      (fun (pidfile, preserved_levels, keep_alive) cctxt ->
        Daemon.Baker.run
          ~keep_alive
          ~command:(Daemon.Run_accuser {pidfile; preserved_levels; keep_alive})
          cctxt);
  ]

let accuser_commands =
  let open Configuration in
  let open Tezos_clic in
  let group =
    {
      name = "delegate.accuser";
      title = "Commands related to the agnostic accuser daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the accuser daemon"
      (args3 pidfile_arg preserved_levels_arg keep_alive_arg)
      (prefix "run" @@ stop)
      (fun (pidfile, preserved_levels, keep_alive) cctxt ->
        Daemon.Accuser.run
          ~keep_alive
          ~command:(Daemon.Run_accuser {pidfile; preserved_levels; keep_alive})
          cctxt);
  ]
