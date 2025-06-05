(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type discovery_options = {
  bind_addr : string option;
  broadcast_addr : string option;
  timeout : float option;
  single_search_timeout : float option;
}

type mapping_options = {
  local_addr : string option;
  lease_duration : int option;
  description : string option;
  any_net_port : bool;
}

(* The duration depends on UPNP/IGV version:
   - If the gateway implements v1, the lease is unlimited
   - If the gateway implements v2, the lease is the maximum allocated time,
   which is 604800 seconds, i.e. one week.

   See specification at
   https://upnp.org/specs/gw/UPnP-gw-WANIPConnection-v2-Service.pdf, paragraph
   2.3.16.
*)
let default_lease_duration = 0l

let default_description = "octez-node P2P"

(* This is a ugly hack to get the IP assigned to the machine as seen by the
   gateway. It avoids having to give it manually.

   The idea is simple: open a socket to the gateway, on any port. The socket
   information will contain the IP used by the network interface to open the
   connection. Technically it should work even if the address is unreachable.

   Technically opening a socket on any distant address should work, but opening
   it to the gateway ensures the right network interface is used.
*)
let get_local_network_ip gateway_ip =
  let open Lwt_syntax in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string gateway_ip, 80) in
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Lwt.finalize
    (fun () ->
      let* () = Lwt_unix.connect sock sockaddr in
      let local_sockaddr = Lwt_unix.getsockname sock in
      match local_sockaddr with
      | Unix.ADDR_INET (addr, _) -> return_some (Unix.string_of_inet_addr addr)
      | _ -> return_none)
    (fun () -> Lwt_unix.close sock)

let handle_local_address gateway local_addr =
  let open Lwt_result_syntax in
  match local_addr with
  | Some local_addr -> return local_addr
  | None -> (
      let gateway_ip = Octez_igd_next.Igd_next_gen.gateway_ip gateway in
      let*! addr = get_local_network_ip gateway_ip in
      match addr with
      | Some a -> return a
      | None ->
          failwith
            "Cannot determine machine network address, please give it using \
             the option `--local-addr`.")

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

let choose_map_port gateway ~local_addr ~local_port ~external_port
    ~lease_duration ~description ~any_net_port =
  if any_net_port then
    Octez_igd_next.Igd_next_gen.gateway_map_any_port
      gateway
      Udp
      ~local_addr
      ~local_port
      ~lease_duration
      ~description
  else
    Octez_igd_next.Igd_next_gen.gateway_map_port
      gateway
      Udp
      ~local_addr
      ~local_port
      ~external_port
      ~lease_duration
      ~description
    |> Result.map (fun () -> external_port)

let map_port ?bind_addr ?broadcast_addr ?timeout ?single_search_timeout
    ?local_addr ?lease_duration ~description ~local_port ~external_port
    ~any_net_port () =
  let open Lwt_result_syntax in
  let gateway =
    Octez_igd_next.Igd_next_gen.search_gateway
      ~bind_addr
      ~broadcast_address:broadcast_addr
      ~timeout
      ~single_search_timeout
  in
  let*? gateway =
    match gateway with
    | Ok g -> Ok g
    | Error e -> error_with "Cannot find gateway: %s" e
  in
  let* local_addr = handle_local_address gateway local_addr in
  let*? lease_duration =
    match lease_duration with
    | Some l ->
        if l >= 1 lsl 31 then
          error_with
            "Please use a duration less than or equal to %ld seconds."
            Int32.max_int
        else Ok (Int32.of_int l)
    | None -> Ok default_lease_duration
  in
  let port =
    choose_map_port
      gateway
      ~local_addr
      ~local_port
      ~external_port
      ~lease_duration
      ~description
      ~any_net_port
  in
  match port with
  | Ok port -> return (local_addr, port)
  | Error e -> failwith "%s" e

let map_port {bind_addr; broadcast_addr; timeout; single_search_timeout}
    {local_addr; lease_duration; description; any_net_port} ?data_dir
    ?config_file ?listen_addr ?advertised_net_port () =
  let open Lwt_result_syntax in
  let* config_file = resolve_config_file ?data_dir ?config_file () in
  let* local_port, advertised_net_port =
    resolve_ports config_file ?listen_addr ?advertised_net_port ()
  in
  let external_port = Option.value ~default:local_port advertised_net_port in
  let*! port =
    map_port
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

  let docs = "UPNP OPTIONS"

  let exclusive_parameters =
    "--any-net-port and --advertised-net-port are exclusive."

  let bind_addr =
    let doc = "The URL at which this instance can be reached." in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~doc ~docv:"ADDR:PORT" ["listen-addr"])

  let broadcast_addr =
    let doc =
      "The address used for broadcasting the UPNP discovery requests."
    in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~doc ~docv:"ADDR:PORT" ["broadcast-addr"])

  let discovery_timeout =
    let doc = "Timeout until UPNP discovery stops looking for a gateway." in
    Arg.(
      value
      & opt (some float) None
      & info ~docs ~doc ~docv:"SEC" ["discovery-timeout"])

  let single_search_timeout =
    let doc =
      "Timeout until the node stops waiting for the first discovery response."
    in
    Arg.(
      value
      & opt (some float) None
      & info ~docs ~doc ~docv:"SEC" ["single-search-timeout"])

  let local_addr =
    let doc = "Local network IP address on which the redirection is done." in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~doc ~docv:"ADDR" ["local-addr"])

  let lease_duration =
    let doc =
      "Lease duration of the mapping, in seconds. Defaults to `0` which either \
       corresponds to an unlimited lease if the gateway implements UPNP/IGD \
       v1, or 604800 seconds for UPNP/IGD v2."
    in
    Arg.(
      value
      & opt (some int) None
      & info ~docs ~doc ~docv:"SEC" ["lease-duration"])

  let any_net_port =
    let doc =
      "Asks the gateway to map any available port instead, and update the \
       configuration according to the result. This option should be used in \
       priority at first invocation of the command, as it ensures an available \
       port is selected by the gateway." ^ exclusive_parameters
    in
    Arg.(value & flag & info ~docs ~doc ~docv:"SEC" ["any-net-port"])

  let description =
    let doc =
      "Name of the mapping, used by UPNP clients/routers to document the \
       redirection."
    in
    Arg.(
      value
      & opt (some string) None
      & info ~docs ~doc ~docv:"DESC" ["mapping-description"])

  let search_options =
    let open Term.Syntax in
    let+ bind_addr
    and+ broadcast_addr
    and+ discovery_timeout
    and+ single_search_timeout in
    {
      bind_addr;
      broadcast_addr;
      timeout = discovery_timeout;
      single_search_timeout;
    }

  let mapping_options =
    let open Term.Syntax in
    let+ local_addr and+ lease_duration and+ description and+ any_net_port in
    {local_addr; lease_duration; description; any_net_port}

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
