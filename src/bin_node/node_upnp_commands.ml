(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type options = {
  bind_addr : string option;
  broadcast_addr : string option;
  timeout : float option;
  single_search_timeout : float option;
}

let search_gateway {bind_addr; broadcast_addr; timeout; single_search_timeout}
    () =
  match
    Octez_igd_next.Igd_next_gen.search_gateway
      ~bind_addr
      ~broadcast_address:broadcast_addr
      ~timeout
      ~single_search_timeout
  with
  | Ok _ ->
      Format.printf "Gateway found\n%!" ;
      Lwt.return_unit
  | Error e ->
      Format.eprintf "Cannot find gateway: %s\n%!" e ;
      Lwt.return_unit

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

let search_gateway options () =
  Tezos_base_unix.Event_loop.main_run (search_gateway options)

module Term = struct
  open Cmdliner

  let docs = "UPNP OPTIONS"

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

  let options =
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

  let process options = `Ok (search_gateway options ())

  let term = Term.(ret (const process $ options))
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
  let info = Cmdliner.Cmd.info ~doc:"Experimental" ~man "search-gateway"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
