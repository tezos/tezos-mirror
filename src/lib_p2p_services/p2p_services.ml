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
  let open RPC_query in
  query (fun wait ->
      object
        method wait = wait
      end)
  |+ flag "wait" (fun t -> t#wait)
  |> seal

let monitor_query =
  let open RPC_query in
  query (fun monitor ->
      object
        method monitor = monitor
      end)
  |+ flag "monitor" (fun t -> t#monitor)
  |> seal

let timeout_query =
  let open RPC_query in
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
    RPC_service.get_service
      ~description:"Return the node's peer id"
      ~query:RPC_query.empty
      ~output:P2p_peer.Id.encoding
      RPC_path.(root / "network" / "self")

  (* DEPRECATED: use [version] from "lib_shell_services/version_services"
     instead. *)
  let version =
    RPC_service.get_service
      ~description:"DEPRECATED: use `version` instead."
      ~query:RPC_query.empty
      ~output:Network_version.encoding
      RPC_path.(root / "network" / "version")

  (* DEPRECATED: use [version] instead. *)
  let versions =
    RPC_service.get_service
      ~description:"DEPRECATED: use `version` instead."
      ~query:RPC_query.empty
      ~output:(Data_encoding.list Network_version.encoding)
      RPC_path.(root / "network" / "versions")

  let stat =
    RPC_service.get_service
      ~description:"Global network bandwidth statistics in B/s."
      ~query:RPC_query.empty
      ~output:P2p_stat.encoding
      RPC_path.(root / "network" / "stat")

  let events =
    RPC_service.get_service
      ~description:"Stream of all network events"
      ~query:RPC_query.empty
      ~output:P2p_connection.P2p_event.encoding
      RPC_path.(root / "network" / "log")

  let connect =
    RPC_service.put_service
      ~description:"Connect to a peer"
      ~query:timeout_query
      ~input:Data_encoding.empty
      ~output:Data_encoding.empty
      RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg)
end

open RPC_context

let self ctxt = make_call S.self ctxt () () ()

let stat ctxt = make_call S.stat ctxt () () ()

let version ctxt = make_call S.version ctxt () () ()

let versions ctxt = make_call S.versions ctxt () () ()

(* DEPRECATED: use [version] instead. *)

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
      RPC_service.get_service
        ~description:"List the running P2P connection."
        ~query:RPC_query.empty
        ~output:(Data_encoding.list connection_info_encoding)
        RPC_path.(root / "network" / "connections")

    let info =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:connection_info_encoding
        ~description:
          "Details about the current P2P connection to the given peer."
        RPC_path.(root / "network" / "connections" /: P2p_peer.Id.rpc_arg)

    let kick =
      RPC_service.delete_service
        ~query:wait_query
        ~output:Data_encoding.empty
        ~description:
          "Forced close of the current P2P connection to the given peer."
        RPC_path.(root / "network" / "connections" /: P2p_peer.Id.rpc_arg)
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
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:P2p_point.Info.encoding
        ~description:"Details about a given `IP:addr`."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg)

    let patch_input_encoding =
      let open Data_encoding in
      obj2
        (opt
           "acl"
           (string_enum [("ban", `Ban); ("trust", `Trust); ("open", `Open)]))
        (opt "peer_id" P2p_peer.Id.encoding)

    let patch =
      RPC_service.patch_service
        ~query:RPC_query.empty
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
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg)

    let events =
      RPC_service.get_service
        ~query:monitor_query
        ~output:(Data_encoding.list P2p_point.Pool_event.encoding)
        ~description:"Monitor network events related to an `IP:addr`."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "log")

    let list =
      let filter_query =
        let open RPC_query in
        query (fun filters ->
            object
              method filters = filters
            end)
        |+ multi_field "filter" P2p_point.Filter.rpc_arg (fun t -> t#filters)
        |> seal
      in
      RPC_service.get_service
        ~query:filter_query
        ~output:
          Data_encoding.(
            list (tup2 P2p_point.Id.encoding P2p_point.Info.encoding))
        ~description:
          "List the pool of known `IP:port` used for establishing P2P \
           connections."
        RPC_path.(root / "network" / "points")

    let ban =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "DEPRECATED: Blacklist the given address and remove it from the \
           whitelist if present. Use PATCH `/network/point/<point_id>` \
           instead."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "ban")

    let unban =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "DEPRECATED: Remove an address from the blacklist. Use PATCH \
           `/network/point/<point_id>` instead."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "unban")

    let trust =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "DEPRECATED: Trust a given address permanently and remove it from \
           the blacklist if present. Connections from this address can still \
           be closed on authentication if the peer is greylisted. Use \
           PATCH`/network/point/<point_id>` instead."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "trust")

    let untrust =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "DEPRECATED: Remove an address from the whitelist. Use PATCH \
           `/network/point/<point_id>` instead."
        RPC_path.(
          root / "network" / "points" /: P2p_point.Id.rpc_arg / "untrust")

    let banned =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.bool
        ~description:
          "Check if a given address is blacklisted or greylisted. Port \
           component is unused."
        RPC_path.(
          root / "network" / "points" /: P2p_point.Id.rpc_arg / "banned")
  end

  open RPC_context

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
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:
          (P2p_peer.Info.encoding
             Peer_metadata.encoding
             Connection_metadata.encoding)
        ~description:"Details about a given peer."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg)

    let events =
      RPC_service.get_service
        ~query:monitor_query
        ~output:(Data_encoding.list P2p_peer.Pool_event.encoding)
        ~description:"Monitor network events related to a given peer."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "log")

    let list =
      let filter =
        let open RPC_query in
        query (fun filters ->
            object
              method filters = filters
            end)
        |+ multi_field "filter" P2p_peer.Filter.rpc_arg (fun t -> t#filters)
        |> seal
      in
      RPC_service.get_service
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
        RPC_path.(root / "network" / "peers")

    let patch_input_encoding =
      let open Data_encoding in
      obj1
        (opt
           "acl"
           (string_enum [("ban", `Ban); ("trust", `Trust); ("open", `Open)]))

    let patch =
      RPC_service.patch_service
        ~query:RPC_query.empty
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
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg)

    let ban =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "DEPRECATED: Blacklist the given peer and remove it from the \
           whitelist if present. Use PATCH `network/peers/<peer_id>` instead."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "ban")

    let unban =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "DEPRECATED: Remove the given peer from the blacklist. Use PATCH \
           `network/peers/<peer_id>` instead."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "unban")

    let trust =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "DEPRECATED: Whitelist a given peer permanently and remove it from \
           the blacklist if present. The peer cannot be blocked (but its host \
           IP still can). Use PATCH `network/peers/<peer_id>` instead."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "trust")

    let untrust =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "DEPRECATED: Remove a given peer from the whitelist. Use PATCH \
           `network/peers/<peer_id>` instead."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "untrust")

    let banned =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.bool
        ~description:"Check if a given peer is blacklisted or greylisted."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "banned")
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

module ACL = struct
  type ip_list = {ips : Ipaddr.V6.t list; not_reliable_since : Ptime.t option}

  module S = struct
    let clear =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "DEPRECATED: Clear all greylists tables. This will unban all \
           addresses and peers automatically greylisted by the system. Use \
           DELETE `/network/greylist` instead"
        RPC_path.(root / "network" / "greylist" / "clear")

    let clear_delete =
      RPC_service.delete_service
        ~query:RPC_query.empty
        ~output:Data_encoding.empty
        ~description:
          "Clear all greylists tables. This will unban all addresses and peers \
           automatically greylisted by the system."
        RPC_path.(root / "network" / "greylist")

    let get_greylisted_peers =
      RPC_service.get_service
        ~query:RPC_query.empty
        ~output:Data_encoding.(list P2p_peer.Id.encoding)
        ~description:"List of the last greylisted peers."
        RPC_path.(root / "network" / "greylist" / "peers")

    let get_greylisted_ips =
      RPC_service.get_service
        ~query:RPC_query.empty
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
        RPC_path.(root / "network" / "greylist" / "ips")
  end

  let clear ctxt = make_call S.clear_delete ctxt () ()

  let get_greylisted_peers ctxt = make_call S.get_greylisted_peers ctxt () () ()

  let get_greylisted_ips ctxt = make_call S.get_greylisted_ips ctxt () () ()
end
