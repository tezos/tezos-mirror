(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Baking_commands

let baker_commands () : Protocol_client_context.full Tezos_clic.command list =
  let open Tezos_clic in
  let group =
    {
      Tezos_clic.name = "delegate.baker";
      title = "Commands related to the baker daemon.";
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
        let baking_mode = Local {local_data_dir_path} in
        run_baker args baking_mode sources cctxt);
    command
      ~group
      ~desc:"Launch the baker daemon using RPCs only."
      baker_args
      (prefixes ["run"; "remotely"] @@ sources_param)
      (fun args sources cctxt ->
        let baking_mode = Remote in
        run_baker args baking_mode sources cctxt);
    command
      ~group
      ~desc:"Launch the VDF daemon"
      (args2 pidfile_arg keep_alive_arg)
      (prefixes ["run"; "vdf"] @@ stop)
      (fun (pidfile, keep_alive) cctxt ->
        may_lock_pidfile pidfile @@ fun () ->
        Client_daemon.VDF.run cctxt ~chain:cctxt#chain ~keep_alive);
  ]

let map_commands () =
  List.map (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
  @@ baker_commands ()
