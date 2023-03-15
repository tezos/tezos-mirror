(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let group =
  {
    Tezos_clic.name = "p2p";
    title = "Commands for monitoring and controlling p2p-layer state";
  }

let pp_connection_info ppf conn =
  P2p_connection.Info.pp (fun _ _ -> ()) ppf conn

let addr_parameter =
  let open Tezos_clic in
  param
    ~name:"address"
    ~desc:"<IPv4>:PORT or <IPV6>:PORT address (PORT defaults to 9732)."
    (parameter (fun _ x ->
         Lwt.return_ok (P2p_point.Id.of_string_exn ~default_port:9732 x)))

let p2p_peer_id_param ~name ~desc t =
  Tezos_clic.param
    ~name
    ~desc
    (Tezos_clic.parameter (fun _ str ->
         Lwt.return (P2p_peer.Id.of_b58check str)))
    t

let commands () =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  [
    command
      ~group
      ~desc:"show global network status"
      no_options
      (prefixes ["p2p"; "stat"] stop)
      (fun () (cctxt : #Client_context.full) ->
        let* stat = Shell_services.P2p.stat cctxt in
        let* conns = Shell_services.P2p.Connections.list cctxt in
        let* peers = Shell_services.P2p.Peers.list cctxt in
        let* points = Shell_services.P2p.Points.list cctxt in
        let*! () = cctxt#message "GLOBAL STATS" in
        let*! () = cctxt#message "  %a" P2p_stat.pp stat in
        let*! () = cctxt#message "CONNECTIONS" in
        let incoming, outgoing =
          List.partition (fun c -> c.P2p_connection.Info.incoming) conns
        in
        let*! () =
          List.iter_s
            (fun conn -> cctxt#message "  %a" pp_connection_info conn)
            incoming
        in
        let*! () =
          List.iter_s
            (fun conn -> cctxt#message "  %a" pp_connection_info conn)
            outgoing
        in
        let*! () = cctxt#message "KNOWN PEERS" in
        let*! () =
          List.iter_s
            (fun (p, pi) ->
              cctxt#message
                "  %a  %.0f %a %a %s"
                P2p_peer.State.pp_digram
                pi.P2p_peer.Info.state
                pi.score
                P2p_peer.Id.pp
                p
                P2p_stat.pp
                pi.stat
                (if pi.trusted then "★" else " "))
            peers
        in
        let*! () = cctxt#message "KNOWN POINTS" in
        let*! () =
          List.iter_s
            (fun (p, pi) ->
              match pi.P2p_point.Info.state with
              | Running peer_id ->
                  cctxt#message
                    "  %a  %a %a %s"
                    P2p_point.State.pp_digram
                    pi.state
                    P2p_point.Id.pp
                    p
                    P2p_peer.Id.pp
                    peer_id
                    (if pi.trusted then "★" else " ")
              | _ -> (
                  match pi.last_seen with
                  | Some (peer_id, ts) ->
                      cctxt#message
                        "  %a  %a (last seen: %a %a) %s"
                        P2p_point.State.pp_digram
                        pi.state
                        P2p_point.Id.pp
                        p
                        P2p_peer.Id.pp
                        peer_id
                        Time.System.pp_hum
                        ts
                        (if pi.trusted then "★" else " ")
                  | None ->
                      cctxt#message
                        "  %a  %a %s"
                        P2p_point.State.pp_digram
                        pi.state
                        P2p_point.Id.pp
                        p
                        (if pi.trusted then "★" else " ")))
            points
        in
        return_unit);
    command
      ~group
      ~desc:"Connect to a new point."
      no_options
      (prefixes ["connect"; "address"] @@ addr_parameter @@ stop)
      (fun () (address, port) (cctxt : #Client_context.full) ->
        let timeout = Time.System.Span.of_seconds_exn 10. in
        let*! r = P2p_services.connect cctxt ~timeout (address, port) in
        match r with
        | Ok () ->
            let*! () =
              cctxt#message
                "Connection to %a:%d established."
                P2p_addr.pp
                address
                port
            in
            return_unit
        | Error (P2p_errors.Pending_connection :: _) ->
            let*! () =
              cctxt#warning
                "Already connecting to peer %a:%d"
                P2p_addr.pp
                address
                port
            in
            return_unit
        | Error (P2p_errors.Connected :: _) ->
            let*! () =
              cctxt#warning
                "Already connected to peer %a:%d"
                P2p_addr.pp
                address
                port
            in
            return_unit
        | Error _ as e -> Lwt.return e);
    command
      ~group
      ~desc:"Kick a peer."
      no_options
      (prefixes ["kick"; "peer"]
      @@ p2p_peer_id_param ~name:"peer" ~desc:"peer network identity"
      @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
        let* () = P2p_services.Connections.kick cctxt peer in
        let*! () =
          cctxt#message "Connection to %a interrupted." P2p_peer.Id.pp peer
        in
        return_unit);
    command
      ~group
      ~desc:
        "Add an IP address and all its ports to the blacklist and kicks it. \
         Remove the address from the whitelist if it was previously in it."
      no_options
      (prefixes ["ban"; "address"] @@ addr_parameter @@ stop)
      (fun () (address, _port) (cctxt : #Client_context.full) ->
        let* _ =
          P2p_services.Points.patch cctxt (address, 0) (Some `Ban, None)
        in
        let*! () =
          cctxt#message "Address %a:* is now banned." P2p_addr.pp address
        in
        return_unit);
    command
      ~group
      ~desc:"Remove an IP address and all its ports from the blacklist."
      no_options
      (prefixes ["unban"; "address"] @@ addr_parameter @@ stop)
      (fun () (address, _port) (cctxt : #Client_context.full) ->
        let* _ =
          P2p_services.Points.patch cctxt (address, 0) (Some `Open, None)
        in
        let*! () =
          cctxt#message "Address %a:* is now unbanned." P2p_addr.pp address
        in
        return_unit);
    command
      ~group
      ~desc:
        "Add an IP address to the whitelist. Remove the address from the \
         blacklist if it was previously in it."
      no_options
      (prefixes ["trust"; "address"] @@ addr_parameter @@ stop)
      (fun () (address, port) (cctxt : #Client_context.full) ->
        let* _ =
          P2p_services.Points.patch cctxt (address, port) (Some `Trust, None)
        in
        let*! () =
          cctxt#message "Address %a:%d is now trusted." P2p_addr.pp address port
        in
        return_unit);
    command
      ~group
      ~desc:"Removes an IP address from the whitelist."
      no_options
      (prefixes ["untrust"; "address"] @@ addr_parameter @@ stop)
      (fun () (address, port) (cctxt : #Client_context.full) ->
        let* _ =
          P2p_services.Points.patch cctxt (address, port) (Some `Open, None)
        in
        let*! () =
          cctxt#message
            "Address %a:%d is now untrusted."
            P2p_addr.pp
            address
            port
        in
        return_unit);
    command
      ~group
      ~desc:"Check if an IP address is banned."
      no_options
      (prefixes ["is"; "address"; "banned"] @@ addr_parameter @@ stop)
      (fun () (address, port) (cctxt : #Client_context.full) ->
        let* banned = P2p_services.Points.banned cctxt (address, port) in
        let*! () =
          cctxt#message
            "The given ip address is %s"
            (if banned then "banned" else "not banned")
        in
        return_unit);
    command
      ~group
      ~desc:"Check if a peer ID is banned."
      no_options
      (prefixes ["is"; "peer"; "banned"]
      @@ p2p_peer_id_param ~name:"peer" ~desc:"peer network identity"
      @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
        let* banned = P2p_services.Peers.banned cctxt peer in
        let*! () =
          cctxt#message
            "The given peer ID is %s"
            (if banned then "banned" else "not banned")
        in
        return_unit);
    command
      ~group
      ~desc:
        "Add a peer ID to the blacklist and kicks it. Remove the peer ID from \
         the blacklist if was previously in it."
      no_options
      (prefixes ["ban"; "peer"]
      @@ p2p_peer_id_param ~name:"peer" ~desc:"peer network identity"
      @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
        let* _ = P2p_services.Peers.patch cctxt peer (Some `Ban) in
        let*! () =
          cctxt#message "The peer %a is now banned." P2p_peer.Id.pp_short peer
        in
        return_unit);
    command
      ~group
      ~desc:"Removes a peer ID from the blacklist."
      no_options
      (prefixes ["unban"; "peer"]
      @@ p2p_peer_id_param ~name:"peer" ~desc:"peer network identity"
      @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
        let* _ = P2p_services.Peers.patch cctxt peer (Some `Open) in
        let*! () =
          cctxt#message "The peer %a is now unbanned." P2p_peer.Id.pp_short peer
        in
        return_unit);
    command
      ~group
      ~desc:
        "Add a peer ID to the whitelist. Remove the peer ID from the blacklist \
         if it was previously in it."
      no_options
      (prefixes ["trust"; "peer"]
      @@ p2p_peer_id_param ~name:"peer" ~desc:"peer network identity"
      @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
        let* _ = P2p_services.Peers.patch cctxt peer (Some `Trust) in
        let*! () =
          cctxt#message "The peer %a is now trusted." P2p_peer.Id.pp_short peer
        in
        return_unit);
    command
      ~group
      ~desc:"Remove a peer ID from the whitelist."
      no_options
      (prefixes ["untrust"; "peer"]
      @@ p2p_peer_id_param ~name:"peer" ~desc:"peer network identity"
      @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
        let* _ = P2p_services.Peers.patch cctxt peer (Some `Open) in
        let*! () =
          cctxt#message
            "The peer %a is now untrusted."
            P2p_peer.Id.pp_short
            peer
        in
        return_unit);
    command
      ~group
      ~desc:"Clear all access control rules."
      no_options
      (prefixes ["clear"; "acls"] @@ stop)
      (fun () (cctxt : #Client_context.full) ->
        let* () = P2p_services.ACL.clear cctxt () in
        let*! () = cctxt#message "The access control rules are now cleared." in
        return_unit);
  ]
