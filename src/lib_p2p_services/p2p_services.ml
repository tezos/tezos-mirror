(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

let wait_query =
  let open Tezos_rpc.Query in
  query (fun wait ->
      object
        method wait = wait
      end)
  |+ flag "wait" (fun t -> t#wait)
  |> seal

let monitor_query =
  let open Tezos_rpc.Query in
  query (fun monitor ->
      object
        method monitor = monitor
      end)
  |+ flag "monitor" (fun t -> t#monitor)
  |> seal

let timeout_query =
  let open Tezos_rpc.Query in
  query (fun timeout ->
      object
        method timeout = timeout
      end)
  |+ field
       "timeout"
       Time.System.Span.rpc_arg
       (Time.System.Span.of_seconds_exn 10.)
       (fun t -> t#timeout)
  |> seal

module S = struct
  let self =
    Tezos_rpc.Service.get_service
      ~description:"Return the node's peer id"
      ~query:Tezos_rpc.Query.empty
      ~output:P2p_peer.Id.encoding
      Tezos_rpc.Path.(root / "network" / "self")

  let stat =
    Tezos_rpc.Service.get_service
      ~description:"Global network bandwidth statistics in B/s."
      ~query:Tezos_rpc.Query.empty
      ~output:P2p_stat.encoding
      Tezos_rpc.Path.(root / "network" / "stat")

  let events =
    Tezos_rpc.Service.get_service
      ~description:"Stream of all network events"
      ~query:Tezos_rpc.Query.empty
      ~output:P2p_connection.P2p_event.encoding
      Tezos_rpc.Path.(root / "network" / "log")

  let connect =
    Tezos_rpc.Service.put_service
      ~description:"Connect to a peer"
      ~query:timeout_query
      ~input:Data_encoding.empty
      ~output:Data_encoding.empty
      Tezos_rpc.Path.(root / "network" / "points" /: P2p_point.Id.rpc_arg)
end

open Tezos_rpc.Context

let self ctxt = make_call S.self ctxt () () ()

let stat ctxt = make_call S.stat ctxt () () ()

let events ctxt = make_streamed_call S.events ctxt () () ()

let connect ctxt ~timeout point_id =
  make_call1
    S.connect
    ctxt
    point_id
    (object
       method timeout = timeout
    end)
    ()

module Connections = struct
  type connection_info = Connection_metadata.t P2p_connection.Info.t

  let connection_info_encoding =
    P2p_connection.Info.encoding Connection_metadata.encoding

  module S = struct
    let list =
      Tezos_rpc.Service.get_service
        ~description:"List the running P2P connection."
        ~query:Tezos_rpc.Query.empty
        ~output:(Data_encoding.list connection_info_encoding)
        Tezos_rpc.Path.(root / "network" / "connections")

    let info =
      Tezos_rpc.Service.get_service
        ~query:Tezos_rpc.Query.empty
        ~output:connection_info_encoding
        ~description:
          "Details about the current P2P connection to the given peer."
        Tezos_rpc.Path.(root / "network" / "connections" /: P2p_peer.Id.rpc_arg)

    let kick =
      Tezos_rpc.Service.delete_service
        ~query:wait_query
        ~output:Data_encoding.empty
        ~description:
          "Forced close of the current P2P connection to the given peer."
        Tezos_rpc.Path.(root / "network" / "connections" /: P2p_peer.Id.rpc_arg)
  end

  let list ctxt = make_call S.list ctxt () () ()

  let info ctxt peer_id = make_call1 S.info ctxt peer_id () ()

  let kick ctxt ?(wait = false) peer_id =
    make_call1
      S.kick
      ctxt
      peer_id
      (object
         method wait = wait
      end)
      ()
end

module Points = struct
  module S = struct
    let info =
      Tezos_rpc.Service.get_service
        ~query:Tezos_rpc.Query.empty
        ~output:P2p_point.Info.encoding
        ~description:"Details about a given `IP:addr`."
        Tezos_rpc.Path.(root / "network" / "points" /: P2p_point.Id.rpc_arg)

    let patch_input_encoding =
      let open Data_encoding in
      obj2
        (opt
           "acl"
           (string_enum [("ban", `Ban); ("trust", `Trust); ("open", `Open)]))
        (opt "peer_id" P2p_peer.Id.encoding)

    let patch =
      Tezos_rpc.Service.patch_service
        ~query:Tezos_rpc.Query.empty
        ~input:patch_input_encoding
        ~output:P2p_point.Info.encoding
        ~description:
          "Change the connectivity state of a given `IP:addr`. With `{acl : \
           ban}`: blacklist the given address and remove it from the whitelist \
           if present. With `{acl: open}`: removes an address from the \
           blacklist and whitelist. With `{acl: trust}`: trust a given address \
           permanently and remove it from the blacklist if present. With \
           `{peer_id: <id>}` set the peerId of the point. Connections from \
           this address can still be closed on authentication if the peer is \
           greylisted. "
        Tezos_rpc.Path.(root / "network" / "points" /: P2p_point.Id.rpc_arg)

    let events =
      Tezos_rpc.Service.get_service
        ~query:monitor_query
        ~output:(Data_encoding.list P2p_point.Pool_event.encoding)
        ~description:"Monitor network events related to an `IP:addr`."
        Tezos_rpc.Path.(
          root / "network" / "points" /: P2p_point.Id.rpc_arg / "log")

    let list =
      let filter_query =
        let open Tezos_rpc.Query in
        query (fun filters ->
            object
              method filters = filters
            end)
        |+ multi_field "filter" P2p_point.Filter.rpc_arg (fun t -> t#filters)
        |> seal
      in
      Tezos_rpc.Service.get_service
        ~query:filter_query
        ~output:
          Data_encoding.(
            list (tup2 P2p_point.Id.encoding P2p_point.Info.encoding))
        ~description:
          "List the pool of known `IP:port` used for establishing P2P \
           connections."
        Tezos_rpc.Path.(root / "network" / "points")

    let banned =
      Tezos_rpc.Service.get_service
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.bool
        ~description:
          "Check if a given address is blacklisted or greylisted. Port \
           component is unused."
        Tezos_rpc.Path.(
          root / "network" / "points" /: P2p_point.Id.rpc_arg / "banned")
  end

  open Tezos_rpc.Context

  let info ctxt peer_id = make_call1 S.info ctxt peer_id () ()

  let events ctxt point =
    make_streamed_call
      S.events
      ctxt
      ((), point)
      (object
         method monitor = true
      end)
      ()

  let list ?(filter = []) ctxt =
    make_call
      S.list
      ctxt
      ()
      (object
         method filters = filter
      end)
      ()

  let patch ctxt peer_id input = make_call1 S.patch ctxt peer_id () input

  let banned ctxt peer_id = make_call1 S.banned ctxt peer_id () ()
end

module Peers = struct
  module S = struct
    let info =
      Tezos_rpc.Service.get_service
        ~query:Tezos_rpc.Query.empty
        ~output:
          (P2p_peer.Info.encoding
             Peer_metadata.encoding
             Connection_metadata.encoding)
        ~description:"Details about a given peer."
        Tezos_rpc.Path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg)

    let events =
      Tezos_rpc.Service.get_service
        ~query:monitor_query
        ~output:(Data_encoding.list P2p_peer.Pool_event.encoding)
        ~description:"Monitor network events related to a given peer."
        Tezos_rpc.Path.(
          root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "log")

    let list =
      let filter =
        let open Tezos_rpc.Query in
        query (fun filters ->
            object
              method filters = filters
            end)
        |+ multi_field "filter" P2p_peer.Filter.rpc_arg (fun t -> t#filters)
        |> seal
      in
      Tezos_rpc.Service.get_service
        ~query:filter
        ~output:
          Data_encoding.(
            list
              (tup2
                 P2p_peer.Id.encoding
                 (P2p_peer.Info.encoding
                    Peer_metadata.encoding
                    Connection_metadata.encoding)))
        ~description:"List the peers the node ever met."
        Tezos_rpc.Path.(root / "network" / "peers")

    let patch_input_encoding =
      let open Data_encoding in
      obj1
        (opt
           "acl"
           (string_enum [("ban", `Ban); ("trust", `Trust); ("open", `Open)]))

    let patch =
      Tezos_rpc.Service.patch_service
        ~query:Tezos_rpc.Query.empty
        ~output:
          (P2p_peer.Info.encoding
             Peer_metadata.encoding
             Connection_metadata.encoding)
        ~input:patch_input_encoding
        ~description:
          "Change the permissions of a given peer. With `{acl: ban}`: \
           blacklist the given peer and remove it from the whitelist if \
           present. With `{acl: open}`: removes the peer from the blacklist \
           and whitelist. With `{acl: trust}`: trust the given peer \
           permanently and remove it from the blacklist if present. The peer \
           cannot be blocked (but its host IP still can)."
        Tezos_rpc.Path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg)

    let banned =
      Tezos_rpc.Service.get_service
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.bool
        ~description:"Check if a given peer is blacklisted or greylisted."
        Tezos_rpc.Path.(
          root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "banned")
  end

  let info ctxt peer_id = make_call1 S.info ctxt peer_id () ()

  let events ctxt peer =
    make_streamed_call
      S.events
      ctxt
      ((), peer)
      (object
         method monitor = true
      end)
      ()

  let list ?(filter = []) ctxt =
    make_call
      S.list
      ctxt
      ()
      (object
         method filters = filter
      end)
      ()

  let patch ctxt point_id input = make_call1 S.patch ctxt point_id () input

  let banned ctxt peer_id = make_call1 S.banned ctxt peer_id () ()
end

module Full_stat = struct
  type t = {
    stat : P2p_stat.t;
    incoming_connections : Connection_metadata.t P2p_connection.Info.t list;
    outgoing_connections : Connection_metadata.t P2p_connection.Info.t list;
    peers :
      (P2p_peer.Id.t * (Peer_metadata.t, Connection_metadata.t) P2p_peer.Info.t)
      list;
    points : (P2p_point.Id.t * P2p_point.Info.t) list;
  }

  let encoding =
    let open Data_encoding in
    def
      "client_p2p_stat"
      ~description:"Stored statistics about the p2p network."
    @@ conv
         (fun {stat; incoming_connections; outgoing_connections; peers; points}
            ->
           (stat, incoming_connections, outgoing_connections, peers, points))
         (fun (stat, incoming_connections, outgoing_connections, peers, points)
            ->
           {stat; incoming_connections; outgoing_connections; peers; points})
         (obj5
            (req "stat" P2p_stat.encoding)
            (req
               "incoming_connections"
               (list
                  (P2p_connection.Info.encoding Connection_metadata.encoding)))
            (req
               "outgoing_connections"
               (list
                  (P2p_connection.Info.encoding Connection_metadata.encoding)))
            (req
               "peers"
               (list
                  (tup2
                     P2p_peer.Id.encoding
                     (P2p_peer.Info.encoding
                        Peer_metadata.encoding
                        Connection_metadata.encoding))))
            (req
               "points"
               (list (tup2 P2p_point.Id.encoding P2p_point.Info.encoding))))

  let pp_list pp ppf l =
    match l with
    | [] -> ()
    | l ->
        Format.fprintf ppf "@," ;
        Format.(pp_print_list ~pp_sep:pp_print_cut pp ppf) l

  let pp_peer ppf (p, pi) =
    Format.fprintf
      ppf
      "%a   %.0f  %a %a %s"
      P2p_peer.State.pp_digram
      pi.P2p_peer.Info.state
      pi.score
      P2p_peer.Id.pp
      p
      P2p_stat.pp
      pi.stat
      (if pi.trusted then "★" else " ")

  let pp_peers ppf peers =
    Format.fprintf
      ppf
      "@,@[<v 0>@{<bold; underline; fg_cyan>St Sc %a%a%a Tr@}%a@]"
      (Tezos_stdlib.Pretty_printing.pp_centered 30)
      "Peer Id"
      (Tezos_stdlib.Pretty_printing.pp_centered 30)
      "Upload"
      (Tezos_stdlib.Pretty_printing.pp_centered 30)
      "Download"
      (pp_list pp_peer)
      peers

  let pp_point ppf (p, pi) =
    match pi.P2p_point.Info.state with
    | Running peer_id ->
        Format.fprintf
          ppf
          "%a  %a %a %s"
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
            Format.fprintf
              ppf
              "%a  %a (last seen: %a %a) %s"
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
            Format.fprintf
              ppf
              "%a  %a %s"
              P2p_point.State.pp_digram
              pi.state
              P2p_point.Id.pp
              p
              (if pi.trusted then "★" else " "))

  let pp_connection_info ppf conn =
    P2p_connection.Info.pp (fun _ _ -> ()) ppf conn

  let to_string ~colorize
      {stat; incoming_connections; outgoing_connections; peers; points} =
    let reset =
      if colorize then
        Tezos_stdlib.Pretty_printing.add_ansi_marking Format.str_formatter
      else fun () -> ()
    in
    Format.fprintf
      Format.str_formatter
      "@[<v 2>GLOBAL STATS@,\
       %a@]@,\
       @[<v 1>CONNECTIONS@,\
       @[<v 1>INCOMING%a@]@,\
       @[<v 1>OUTGOING%a@]@]@,\
       @[<v 2>KNOWN PEERS%a@]@,\
       @[<v 2>KNOWN POINTS%a@."
      P2p_stat.pp
      stat
      (pp_list pp_connection_info)
      incoming_connections
      (pp_list pp_connection_info)
      outgoing_connections
      pp_peers
      peers
      (pp_list pp_point)
      points ;
    reset () ;
    Format.flush_str_formatter ()

  module S = struct
    let full_stat =
      Tezos_rpc.Service.get_service
        ~description:"Full network statistics."
        ~query:Tezos_rpc.Query.empty
        ~output:encoding
        Tezos_rpc.Path.(root / "network" / "full_stat")
  end

  let full_stat ctxt = make_call S.full_stat ctxt () () ()
end

module ACL = struct
  type ip_list = {ips : Ipaddr.V6.t list; not_reliable_since : Ptime.t option}

  module S = struct
    let clear_delete =
      Tezos_rpc.Service.delete_service
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.empty
        ~description:
          "Clear all greylists tables. This will unban all addresses and peers \
           automatically greylisted by the system."
        Tezos_rpc.Path.(root / "network" / "greylist")

    let get_greylisted_peers =
      Tezos_rpc.Service.get_service
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.(list P2p_peer.Id.encoding)
        ~description:"List of the last greylisted peers."
        Tezos_rpc.Path.(root / "network" / "greylist" / "peers")

    let get_greylisted_ips =
      Tezos_rpc.Service.get_service
        ~query:Tezos_rpc.Query.empty
        ~output:
          Data_encoding.(
            conv
              (fun {ips; not_reliable_since} -> (ips, not_reliable_since))
              (fun (ips, not_reliable_since) -> {ips; not_reliable_since})
              (obj2
                 (req "ips" (list P2p_addr.encoding))
                 (req
                    "not_reliable_since"
                    (Data_encoding.option Time.System.encoding))))
        ~description:
          "Returns an object that contains a list of IP and the field \
           \"not_reliable_since\".\n\
          \           If the field \"not_reliable_since\" is None then the \
           list contains the currently greylisted IP addresses.\n\
          \           If the field \"not_reliable_since\" Contains a date, \
           this means that the greylist has been overflowed and it is no more \
           possible to obtain the exact list of greylisted IPs. Since the \
           greylist of IP addresses has been design to work whatever his size, \
           there is no security issue related to this overflow.\n\
          \          Reinitialize the ACL structure by calling \"delete \
           /network/greylist\" to get back this list reliable."
        Tezos_rpc.Path.(root / "network" / "greylist" / "ips")
  end

  let clear ctxt = make_call S.clear_delete ctxt () ()

  let get_greylisted_peers ctxt = make_call S.get_greylisted_peers ctxt () () ()

  let get_greylisted_ips ctxt = make_call S.get_greylisted_ips ctxt () () ()
end
