(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* The duration depends on UPNP/IGV version:
   - If the gateway implements v1, the lease is unlimited
   - If the gateway implements v2, the lease is the maximum allocated time,
   which is 604800 seconds, i.e. one week.

   See specification at
   https://upnp.org/specs/gw/UPnP-gw-WANIPConnection-v2-Service.pdf, paragraph
   2.3.16.
*)
let default_lease_duration = 0l

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
