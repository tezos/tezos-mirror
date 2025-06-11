(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let default_description = "octez-node P2P"

let resolve_config_file ?data_dir ?config_file () =
  let open Lwt_result_syntax in
  let* _, config_file =
    Shared_arg.resolve_data_dir_and_config_file ?data_dir ?config_file ()
  in
  return config_file

(* Returns the port where the P2P server is listening, and the port advertised
   to peer, taking in priority the ports given with the CLI. *)
let resolve_ports config_file ?listen_addr ?advertised_net_port () =
  let open Lwt_result_syntax in
  let* port =
    let* listening_addrs =
      Option.either
        listen_addr
        config_file.Octez_node_config.Config_file.p2p.listen_addr
      |> Option.map_es Octez_node_config.Config_file.resolve_listening_addrs
    in
    match listening_addrs with
    | None | Some [] ->
        failwith "P2P server is disabled, port mapping is irrelevant"
    | Some ((_addr, port) :: _) -> return port
  in
  let advertised_net_port =
    Option.either
      advertised_net_port
      config_file.Octez_node_config.Config_file.p2p.advertised_net_port
  in
  return (port, advertised_net_port)

let patch_config ?data_dir ?config_filename config_file ?listen_addr
    ?advertised_net_port () =
  let open Lwt_result_syntax in
  let* config_file =
    Octez_node_config.Config_file.update
      ?data_dir
      ?listen_addr
      ?advertised_net_port
      config_file
  in
  let* () = Config_validation.check config_file in
  let config_filename =
    Option.value
      ~default:
        Filename.Infix.(
          config_file.data_dir // Data_version.default_config_file_name)
      config_filename
  in
  Config_file.write config_filename config_file

let map_port
    Octez_lib_upnp_args.Shared_args.
      {bind_addr; broadcast_addr; timeout; single_search_timeout}
    Octez_lib_upnp_args.Shared_args.
      {local_addr; lease_duration; description; any_net_port} ?data_dir
    ?config_file ?listen_addr ?advertised_net_port () =
  let open Lwt_result_syntax in
  let* config_file = resolve_config_file ?data_dir ?config_file () in
  let* local_port, advertised_net_port =
    resolve_ports config_file ?listen_addr ?advertised_net_port ()
  in
  let external_port = Option.value ~default:local_port advertised_net_port in
  let*! port =
    Octez_lib_upnp.Upnp.map_port
      ?bind_addr
      ?broadcast_addr
      ?timeout
      ?single_search_timeout
      ?local_addr
      ?lease_duration
      ~description:(Option.value ~default:default_description description)
      ~local_port
      ~external_port
      ~any_net_port
      ()
  in
  match port with
  | Ok (local_addr, external_port) ->
      (* Note that the external IP can be retrieved from the gateway, but it
         avoids leaking it in the logs. *)
      Format.printf
        "Redirecting <external_ip>:%d to %s:%d\n%!"
        external_port
        local_addr
        local_port ;
      let* () =
        patch_config
          config_file
          ?listen_addr
          ~advertised_net_port:external_port
          ()
      in
      return_unit
  | Error e -> fail e

let map_port_cmd search_options mapping_options ?data_dir ?config_file
    ?listen_addr ?advertised_net_port () =
  Tezos_base_unix.Event_loop.main_run
    (map_port
       search_options
       mapping_options
       ?data_dir
       ?config_file
       ?listen_addr
       ?advertised_net_port)

module Term = struct
  open Cmdliner
  open Octez_lib_upnp_args.Shared_args

  let process config_file data_dir listen_addr advertised_net_port
      search_options mapping_options =
    if advertised_net_port <> None && mapping_options.any_net_port then
      `Error (false, exclusive_parameters)
    else
      match
        map_port_cmd
          search_options
          mapping_options
          ?data_dir
          ?config_file
          ?listen_addr
          ?advertised_net_port
          ()
      with
      | Ok () -> `Ok ()
      | Error e ->
          `Error (true, Format.asprintf "%a" Error_monad.pp_print_trace e)

  let term =
    Term.(
      ret
        (const process $ Shared_arg.Term.config_file $ Shared_arg.Term.data_dir
       $ Shared_arg.Term.listen_addr $ Shared_arg.Term.advertised_net_port
       $ search_options $ mapping_options))
end

module Manpage = struct
  let command_description =
    "Port mapping through UPNP/IGD. The gateway must support UPNP and accept \
     port mapping requests, and the local firewall should accept messages from \
     it on 1900/UDP, otherwise gateway discovery will fail. Default values \
     should suffice in general setup."

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  (* Note that this command is temporary as a proof of concept, and will evolve
     into a full-fledge port mapping command. As such, the documentation is
     minimal for now as it doesn't make sense. *)
  let info = Cmdliner.Cmd.info ~doc:"Experimental" ~man "map-port"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
