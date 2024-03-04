(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
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

include Gs_interface

module Worker = struct
  module Config = Gs_interface.Worker_config
  module Default_parameters = Gs_default_parameters
  module Logging = Gs_logging
  include Gs_interface.Worker_instance
  module Validate_message_hook = Validate_message_hook
end

module Transport_layer = struct
  module Interface = Transport_layer_interface
  module Default_parameters = Transport_layer_default_parameters

  type t =
    ( Interface.p2p_message,
      Types.P2P.Metadata.Peer.t,
      Types.P2P.Metadata.Connection.t )
    P2p.t

  let create =
    let special_addresses =
      ["0.0.0.0"; "127.0.0.1"; "localhost"; "[::]"; "::1"]
    in
    fun ~network_name ~public_addr ~is_bootstrap_peer config limits ->
      let advertised_net_addr =
        if
          not
            (List.mem
               ~equal:String.equal
               (P2p_addr.to_string (fst public_addr))
               special_addresses)
        then Some (fst public_addr)
        else None
      in
      let advertised_net_port =
        (* If the public addressed was filtered, take the listening port. *)
        match advertised_net_addr with
        | None -> config.P2p.listening_port
        | Some _ -> Some (snd public_addr)
      in
      let connection_metadata =
        Types.P2P.Metadata.Connection.
          {advertised_net_addr; advertised_net_port; is_bootstrap_peer}
      in
      P2p.create
        ~config
        ~limits
        Types.P2P.Metadata.Peer.config
        (Types.P2P.Metadata.Connection.config connection_metadata)
      @@ Interface.message_config ~network_name

  let activate ?(additional_points = []) p2p =
    let open Lwt_syntax in
    let () = P2p.activate p2p in
    List.iter_s
      (fun point ->
        (* Connections to points given by the user at startup (in config file
           or via --peer options) are trusted. *)
        let* (_ : _ P2p.connection tzresult) =
          P2p.connect ~trusted:true p2p point
        in
        return_unit)
      additional_points

  let connect p2p ?timeout point =
    let open Lwt_result_syntax in
    match P2p.connect_handler p2p with
    | None -> tzfail P2p_errors.P2p_layer_disabled
    | Some connect_handler ->
        (* Connections to points explicitly given by the user are trusted. *)
        let* (_conn : _ P2p_conn.t) =
          P2p_connect_handler.connect
            ~trusted:true
            ?timeout
            connect_handler
            point
        in
        return_unit

  let disconnect_peer p2p ?wait peer_id =
    let open Lwt_syntax in
    match P2p.pool p2p with
    | None -> return_unit
    | Some pool -> (
        match P2p_pool.Connection.find_by_peer_id pool peer_id with
        | None -> return_unit
        | Some conn -> P2p_conn.disconnect ?wait ~reason:Explicit_RPC conn)

  let disconnect_point p2p ?wait point =
    let open Lwt_syntax in
    match P2p.pool p2p with
    | None -> return_unit
    | Some pool -> (
        match P2p_pool.Connection.find_by_point pool point with
        | None -> return_unit
        | Some conn -> P2p_conn.disconnect ?wait ~reason:Explicit_RPC conn)

  let get_points ?(connected = true) p2p =
    let open Lwt_result_syntax in
    match P2p.pool p2p with
    | None -> tzfail P2p_errors.P2p_layer_disabled
    | Some pool ->
        let folder =
          if connected then P2p_pool.Points.fold_connected
          else P2p_pool.Points.fold_known
        in
        folder ~init:[] ~f:(fun point _info acc -> point :: acc) pool |> return

  let get_points_info ?(connected = true) p2p =
    let open Lwt_result_syntax in
    match P2p.pool p2p with
    | None -> tzfail P2p_errors.P2p_layer_disabled
    | Some pool ->
        let folder =
          if connected then P2p_pool.Points.fold_connected
          else P2p_pool.Points.fold_known
        in
        folder
          ~init:[]
          ~f:(fun point point_info acc ->
            let info = P2p_point_state.info_of_point_info point_info in
            (point, info) :: acc)
          pool
        |> return

  let get_point_info p2p point =
    let open Lwt_result_syntax in
    match P2p.pool p2p with
    | None -> tzfail P2p_errors.P2p_layer_disabled
    | Some pool ->
        Option.map
          P2p_point_state.info_of_point_info
          (P2p_pool.Points.info pool point)
        |> return

  let get_peers ?(connected = true) p2p =
    let open Lwt_result_syntax in
    match P2p.pool p2p with
    | None -> tzfail P2p_errors.P2p_layer_disabled
    | Some pool ->
        let folder =
          if connected then P2p_pool.Peers.fold_connected
          else P2p_pool.Peers.fold_known
        in
        folder ~init:[] ~f:(fun peer _info acc -> peer :: acc) pool |> return

  let get_peers_info ?(connected = true) p2p =
    let open Lwt_result_syntax in
    match P2p.pool p2p with
    | None -> tzfail P2p_errors.P2p_layer_disabled
    | Some pool ->
        let folder =
          if connected then P2p_pool.Peers.fold_connected
          else P2p_pool.Peers.fold_known
        in
        folder
          ~init:[]
          ~f:(fun peer peer_info acc ->
            let info = P2p_pool.Peers.info_of_peer_info pool peer_info in
            (peer, info) :: acc)
          pool
        |> return

  let get_peer_info p2p peer =
    let open Lwt_result_syntax in
    match P2p.pool p2p with
    | None -> tzfail P2p_errors.P2p_layer_disabled
    | Some pool ->
        Option.map
          (P2p_pool.Peers.info_of_peer_info pool)
          (P2p_pool.Peers.info pool peer)
        |> return

  let patch_peer p2p peer acl =
    let open Lwt_result_syntax in
    match P2p.pool p2p with
    | None -> tzfail P2p_errors.P2p_layer_disabled
    | Some pool ->
        let*! () =
          match acl with
          | None -> Lwt.return_unit
          | Some `Ban ->
              (* ban *)
              P2p_pool.Peers.untrust pool peer ;
              P2p_pool.Peers.ban pool peer
          | Some `Trust ->
              (* trust *)
              P2p_pool.Peers.trust pool peer ;
              Lwt.return_unit
          | Some `Open ->
              (* unban, untrust *)
              P2p_pool.Peers.unban pool peer ;
              P2p_pool.Peers.untrust pool peer ;
              Lwt.return_unit
        in
        return
        @@ Option.map
             (P2p_pool.Peers.info_of_peer_info pool)
             (P2p_pool.Peers.info pool peer)
end

module Transport_layer_hooks = Gs_transport_connection

let version = Transport_layer_interface.version
