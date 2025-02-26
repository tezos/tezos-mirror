(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let run_baker ~configuration ~baking_mode ~sources ~cctxt =
  let args = Baker_args_parser.parse_configuration configuration in
  let baking_mode = Baker_args_parser.parse_baking_mode baking_mode in
  let cctxt = new Protocol_client_context.wrap_full cctxt in
  Baking_commands.run_baker args baking_mode sources cctxt

let sources_param =
  Tezos_clic.seq_of_param
    (Client_keys.Public_key_hash.source_param
       ~name:"baker"
       ~desc:
         "name of the delegate owning the attestation/baking right or name of \
          the consensus key signing on the delegate's behalf")

let baker_commands () :
    Tezos_client_base.Client_context.full Tezos_clic.command list =
  let open Configuration in
  let open Tezos_clic in
  let group =
    {name = "delegate.baker"; title = "Commands related to the baker daemon."}
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
        run_baker ~configuration ~baking_mode ~sources ~cctxt);
    command
      ~group
      ~desc:"Launch the baker daemon using RPCs only."
      baker_args
      (prefixes ["run"; "remotely"] @@ sources_param)
      (fun args sources cctxt ->
        let baking_mode = None in
        let configuration = create_config args in
        run_baker ~configuration ~baking_mode ~sources ~cctxt);
    command
      ~group
      ~desc:"Launch the VDF daemon"
      (args2 pidfile_arg keep_alive_arg)
      (prefixes ["run"; "vdf"] @@ stop)
      (fun (pidfile, keep_alive) cctxt ->
        let cctxt = new Protocol_client_context.wrap_full cctxt in
        may_lock_pidfile pidfile @@ fun () ->
        Client_daemon.VDF.run cctxt ~chain:cctxt#chain ~keep_alive);
  ]
