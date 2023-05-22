(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module Events = P2p_events.P2p_connect_handler

type config = {
  incoming_app_message_queue_size : int option;
  private_mode : bool;
  min_connections : int;
  max_connections : int;
  max_incoming_connections : int;
  incoming_message_queue_size : int option;
  outgoing_message_queue_size : int option;
  binary_chunks_size : int option;
  identity : P2p_identity.t;
  connection_timeout : Time.System.Span.t;
  authentication_timeout : Time.System.Span.t;
  reconnection_config : Point_reconnection_config.t;
  proof_of_work_target : Tezos_crypto.Crypto_box.pow_target;
  listening_port : P2p_addr.port option;
  advertised_port : P2p_addr.port option;
  disable_peer_discovery : bool;
}

type ('msg, 'peer_meta, 'conn_meta) dependencies = {
  pool_greylist_peer :
    ('msg, 'peer_meta, 'conn_meta) P2p_pool.t -> P2p_peer.Id.t -> unit;
      (** [P2p_pool.greylist_peer] *)
  peer_state_info_trusted :
    ( ('msg, 'peer_meta, 'conn_meta) P2p_conn.t,
      'peer_meta,
      'conn_meta )
    P2p_peer_state.Info.t ->
    bool;
      (** [P2p_peer_state.Info.trusted] *)
  point_state_info_trusted :
    ('msg, 'peer_meta, 'conn_meta) P2p_conn.t P2p_point_state.Info.t -> bool;
      (** [P2p_point_state.Info.trusted] *)
  fd_connect :
    P2p_fd.t ->
    Unix.sockaddr ->
    (unit, [`Unexpected_error of exn | `Connection_refused]) result Lwt.t;
      (** [P2p_fd.connect] *)
  socket_authenticate :
    canceler:Lwt_canceler.t ->
    proof_of_work_target:Tezos_crypto.Crypto_box.pow_target ->
    incoming:bool ->
    P2p_io_scheduler.connection ->
    P2p_point.Id.t ->
    ?advertised_port:int ->
    P2p_identity.t ->
    Network_version.t ->
    'conn_meta P2p_params.conn_meta_config ->
    ('conn_meta P2p_connection.Info.t
    * 'conn_meta P2p_socket.authenticated_connection)
    tzresult
    Lwt.t;
      (** [P2p_socket.authenticate] *)
  socket_accept :
    ?incoming_message_queue_size:int ->
    ?outgoing_message_queue_size:int ->
    ?binary_chunks_size:int ->
    canceler:Lwt_canceler.t ->
    'conn_meta P2p_socket.authenticated_connection ->
    'msg P2p_message.t Data_encoding.t ->
    ('msg P2p_message.t, 'conn_meta) P2p_socket.t tzresult Lwt.t;
      (** [P2p_socket.accept] *)
}

type ('msg, 'peer_meta, 'conn_meta) t = {
  config : config;
  pool : ('msg, 'peer_meta, 'conn_meta) P2p_pool.t;
  log : P2p_connection.P2p_event.t -> unit;
  triggers : P2p_trigger.t;
  io_sched : P2p_io_scheduler.t;
  announced_version : Network_version.t;
  conn_meta_config : 'conn_meta P2p_params.conn_meta_config;
  message_config : 'msg P2p_params.message_config;
  custom_p2p_versions : P2p_version.t list;
  encoding : 'msg P2p_message.t Data_encoding.t;
  incoming : Lwt_canceler.t P2p_point.Table.t;
  mutable new_connection_hook :
    (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) P2p_conn.t -> unit) list;
  mutable disconnection_hook : (P2p_peer.Id.t -> unit) list;
  answerer : 'msg P2p_answerer.t Lazy.t;
  dependencies : ('msg, 'peer_meta, 'conn_meta) dependencies;
}

let create ?(p2p_versions = P2p_version.supported) config pool message_config
    conn_meta_config io_sched triggers ~log ~answerer =
  let dependencies =
    {
      pool_greylist_peer = P2p_pool.greylist_peer;
      peer_state_info_trusted = P2p_peer_state.Info.trusted;
      point_state_info_trusted = P2p_point_state.Info.trusted;
      fd_connect = P2p_fd.connect;
      socket_authenticate = P2p_socket.authenticate;
      socket_accept = P2p_socket.accept;
    }
  in
  {
    config;
    conn_meta_config;
    message_config;
    announced_version =
      Network_version.announced
        ~chain_name:message_config.P2p_params.chain_name
        ~distributed_db_versions:
          message_config.P2p_params.distributed_db_versions
        ~p2p_versions;
    custom_p2p_versions = p2p_versions;
    incoming = P2p_point.Table.create ~random:true 53;
    io_sched;
    encoding = P2p_message.encoding message_config.P2p_params.encoding;
    triggers;
    new_connection_hook = [];
    disconnection_hook = [];
    log;
    pool;
    answerer;
    dependencies;
  }

let config t = t.config

let create_connection t p2p_conn id_point point_info peer_info
    negotiated_version =
  let open Lwt_syntax in
  let peer_id = P2p_peer_state.Info.peer_id peer_info in
  let canceler = Lwt_canceler.create () in
  let bound =
    Option.map
      (fun qs ->
        ( qs,
          fun (size, _) ->
            (Sys.word_size / 8 * 11)
            + size + Lwt_pipe.Maybe_bounded.push_overhead ))
      t.config.incoming_app_message_queue_size
  in
  let messages = Lwt_pipe.Maybe_bounded.create ?bound () in
  let greylister () =
    t.dependencies.pool_greylist_peer
      t.pool
      (P2p_peer_state.Info.peer_id peer_info)
  in
  let conn =
    P2p_conn.create
      ~conn:p2p_conn
      ~point_info
      ~peer_info
      ~messages
      ~canceler
      ~greylister
      ~callback:(Lazy.force t.answerer)
      ~disable_peer_discovery:t.config.disable_peer_discovery
      negotiated_version
  in
  let conn_meta = P2p_socket.remote_metadata p2p_conn in
  let timestamp = Time.System.now () in
  Option.iter
    (fun point_info ->
      let point = P2p_point_state.Info.point point_info in
      P2p_point_state.set_running ~timestamp point_info peer_id conn ;
      P2p_pool.Points.add_connected t.pool point point_info)
    point_info ;
  t.log (Connection_established (id_point, peer_id)) ;
  P2p_peer_state.set_running ~timestamp peer_info id_point conn conn_meta ;
  P2p_pool.Peers.add_connected t.pool peer_id peer_info ;
  P2p_trigger.broadcast_new_connection t.triggers ;
  Lwt_canceler.on_cancel canceler (fun () ->
      let* () = Events.(emit disconnected) (peer_id, id_point) in
      let timestamp = Time.System.now () in
      Option.iter
        (P2p_point_state.set_disconnected
           ~timestamp
           t.config.reconnection_config)
        point_info ;
      t.log (Disconnection peer_id) ;
      P2p_peer_state.set_disconnected ~timestamp peer_info ;
      List.iter (fun f -> f peer_id) t.disconnection_hook ;
      Option.iter
        (fun point_info -> P2p_pool.Points.remove_connected t.pool point_info)
        point_info ;
      P2p_pool.Peers.remove_connected t.pool peer_id ;
      let* () =
        if P2p_pool.active_connections t.pool < t.config.min_connections then (
          P2p_trigger.broadcast_too_few_connections t.triggers ;
          Events.(emit trigger_maintenance_too_few_connections)
            (P2p_pool.active_connections t.pool, t.config.min_connections))
        else Lwt.return_unit
      in
      Lwt_pipe.Maybe_bounded.close messages ;
      P2p_conn.close conn) ;
  List.iter (fun f -> f peer_id conn) t.new_connection_hook ;
  let* () =
    (* DISCLAIMER: A similar check is also performed in [P2p_worker] before
       running the maintenance. Thus, it is important that both conditionals
       be identical to maintain the maintainance triggering consitency.
       If this comparison needs to be updated for some reason (for example
       from a strict to a non strict one), please, consider updating also
       the [P2p_worker]. *)
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5291
       Improve invariant stability using a better encapsulation. *)
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5294
       Stop triggering the maintainance while performing a connection swap. *)
    if t.config.max_connections < P2p_pool.active_connections t.pool then (
      P2p_trigger.broadcast_too_many_connections t.triggers ;
      Events.(emit trigger_maintenance_too_many_connections)
        (P2p_pool.active_connections t.pool, t.config.max_connections))
    else Lwt.return_unit
  in
  return conn

let is_acceptable t connection_point_info peer_info incoming version =
  let open Result_syntax in
  (* Private mode only accept trusted *)
  let unexpected =
    t.config.private_mode
    && (not
          (Option.fold
             ~none:false
             ~some:t.dependencies.point_state_info_trusted
             connection_point_info))
    && not (t.dependencies.peer_state_info_trusted peer_info)
  in
  if unexpected then (
    Events.(emit__dont_wait__use_with_care peer_rejected) () ;
    tzfail P2p_errors.Private_mode)
  else
    (* checking if point is acceptable *)
    let* version =
      match connection_point_info with
      | None -> return version
      | Some connection_point_info -> (
          match P2p_point_state.get connection_point_info with
          | Accepted _ | Running _ ->
              P2p_rejection.(rejecting Already_connected)
          | Requested _ when incoming ->
              P2p_rejection.(rejecting Already_connected)
          | Requested _ | Disconnected -> return version)
    in
    (* Point is acceptable, checking if peer is. *)
    match P2p_peer_state.get peer_info with
    | Accepted _
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4679
       in some circumstances cancel and accept... *)
    | Running _ ->
        P2p_rejection.(rejecting Already_connected)
    (* All right, welcome ! *)
    | Disconnected -> return version

let may_register_my_id_point pool = function
  | [P2p_errors.Myself (addr, Some port)] ->
      P2p_pool.add_to_id_points pool (addr, port)
  | _ -> ()

(** Checking if there is an expected peer id for the connected point.
    If an id is expected,
     -  if the announced identity is not the expected one, issue a warn event and fail.
     -  if the announced identity is correct issue a notice
   *)
let check_expected_peer_id (point_info : 'a P2p_point_state.Info.t option)
    (conn_info : 'b P2p_connection.Info.t) =
  let open Lwt_result_syntax in
  match point_info with
  | None ->
      (* if no point info, nothing is expected from the point, it cannot
         even be set to trusted. *)
      return_unit
  | Some point_info -> (
      let point = P2p_point_state.Info.point point_info in
      match P2p_point_state.Info.get_expected_peer_id point_info with
      | None -> return_unit
      | Some expected_peer_id ->
          if P2p_peer.Id.(expected_peer_id <> conn_info.peer_id) then
            let*! () =
              Events.(emit authenticate_status_peer_id_incorrect)
                ("peer_id", point, expected_peer_id, conn_info.peer_id)
            in
            tzfail
              P2p_errors.(
                Identity_check_failure
                  {
                    point;
                    expected_peer_id;
                    received_peer_id = conn_info.peer_id;
                  })
          else
            let*! () =
              Events.(emit authenticate_status_peer_id_correct)
                (point, conn_info.peer_id)
            in
            return_unit)

let raw_authenticate t ?point_info canceler scheduled_conn point =
  let open Lwt_result_syntax in
  let incoming = point_info = None in
  let incoming_str = if incoming then "incoming" else "outgoing" in
  let*! () = Events.(emit authenticate_start) (point, incoming_str) in
  let* info, auth_conn =
    protect
      ~canceler
      (fun () ->
        t.dependencies.socket_authenticate
          ~canceler
          ~proof_of_work_target:t.config.proof_of_work_target
          ~incoming
          scheduled_conn
          point
          ?advertised_port:t.config.advertised_port
          t.config.identity
          t.announced_version
          t.conn_meta_config)
      ~on_error:(fun err ->
        let*! () =
          match err with
          | [Canceled] ->
              (* Currently only on time out *)
              Events.(emit authenticate) (point, incoming_str, "canceled")
          | err ->
              (* Authentication incorrect! Temp ban the offending points/peers *)
              List.iter
                (function
                  | P2p_errors.Not_enough_proof_of_work _
                  | P2p_errors.Invalid_auth | P2p_errors.Decipher_error
                  | P2p_errors.Invalid_message_size
                  | Tezos_base.Data_encoding_wrapper.Encoding_error _
                  | Tezos_base.Data_encoding_wrapper
                    .Unexpected_size_of_encoded_value
                  | P2p_errors.Decoding_error _
                  | P2p_errors.Invalid_chunks_size _ ->
                      P2p_pool.greylist_addr t.pool (fst point)
                  | _ -> ())
                err ;
              Events.(emit authenticate) (point, incoming_str, "failed")
        in
        let*! () = Events.(emit authenticate_error) (point, err) in
        may_register_my_id_point t.pool err ;
        t.log (Authentication_failed point) ;
        (if not incoming then
         let timestamp = Time.System.now () in
         Option.iter
           (P2p_point_state.set_disconnected
              ~timestamp
              t.config.reconnection_config)
           point_info) ;
        Lwt.return_error err)
  in
  (* Authentication correct! *)
  let*! () = Events.(emit authenticate_status) ("auth", point, info.peer_id) in
  let* () =
    fail_when
      (P2p_pool.Peers.banned t.pool info.peer_id)
      (P2p_errors.Peer_banned info.peer_id)
  in
  let remote_point_info =
    match info.id_point with
    | addr, Some port -> P2p_pool.register_new_point t.pool (addr, port)
    | _ -> None
  in
  let connection_point_info = Option.either point_info remote_point_info in
  (* Check if there is an expected peer id for this point. *)
  let* () = check_expected_peer_id connection_point_info info in
  let peer_info = P2p_pool.register_peer t.pool info.peer_id in
  (* [acceptable] is either Ok with a network version, or a Rejecting
     error with a motive *)
  let acceptable =
    let open Result_syntax in
    let* version =
      Network_version.select
        ~chain_name:t.message_config.chain_name
        ~distributed_db_versions:t.message_config.distributed_db_versions
        ~p2p_versions:t.custom_p2p_versions
        info.announced_version
    in
    (* we have a common version, checking if there is an available slot *)
    let* () =
      if
        (* randomly allow one additional incoming connection *)
        t.config.max_connections + Random.int 2
        > P2p_pool.active_connections t.pool
      then return_unit
      else P2p_rejection.(rejecting Too_many_connections)
    in
    (* we have a slot, checking if point and peer are acceptable *)
    is_acceptable t connection_point_info peer_info incoming version
  in
  Option.iter
    (fun point_info ->
      (* set the point to private or not, depending on the [info] gathered
           during authentication *)
      P2p_point_state.set_private point_info info.private_node)
    connection_point_info ;
  match acceptable with
  | Error
      (P2p_rejection.Rejecting
         {
           motive =
             ( Too_many_connections | Unknown_chain_name
             | Deprecated_p2p_version | Deprecated_distributed_db_version
             | Already_connected ) as motive;
         }
      :: _) -> (
      (* non-acceptable point, nack-ing it. *)
      t.log (Rejecting_request (point, info.id_point, info.peer_id)) ;
      let*! () =
        Events.(emit authenticate_status ("nack", point, info.peer_id))
      in
      let*! nack_point_list =
        if t.config.disable_peer_discovery then Lwt.return []
        else
          (* Never send more than 100 points, you would be greylisted *)
          P2p_pool.list_known_points ~ignore_private:true ~size:50 t.pool
      in
      let*! () = P2p_socket.nack auth_conn motive nack_point_list in
      let () =
        if not incoming then
          let timestamp = Time.System.now () in
          Option.iter
            (P2p_point_state.set_disconnected
               ~timestamp
               ~requested:true
               t.config.reconnection_config)
            point_info
      in
      match motive with
      | Unknown_chain_name | Deprecated_distributed_db_version
      | Deprecated_p2p_version ->
          let*! () =
            Events.(emit authenticate_reject_protocol_mismatch)
              ( point,
                info.peer_id,
                t.message_config.chain_name,
                info.announced_version.chain_name,
                t.message_config.distributed_db_versions,
                info.announced_version.distributed_db_version,
                t.custom_p2p_versions,
                info.announced_version.p2p_version )
          in
          tzfail
            (P2p_errors.Rejected_no_common_protocol
               {announced = info.announced_version})
      | _ -> tzfail (P2p_errors.Rejected {peer = info.peer_id; motive}))
  | Error errs as err ->
      let*! () =
        Events.(emit authenticate_status) ("reject", point, info.peer_id)
      in
      let*! () = Events.(emit authenticate_error) (point, errs) in
      Lwt.return err
  | Ok version ->
      t.log (Accepting_request (point, info.id_point, info.peer_id)) ;
      let timestamp = Time.System.now () in
      Option.iter
        (fun point_info ->
          P2p_point_state.set_accepted
            ~timestamp
            point_info
            info.peer_id
            canceler)
        connection_point_info ;
      P2p_peer_state.set_accepted ~timestamp peer_info info.id_point canceler ;
      let*! () =
        Events.(emit authenticate_status) ("accept", point, info.peer_id)
      in
      let* conn =
        protect
          ~canceler
          (fun () ->
            let* conn =
              t.dependencies.socket_accept
                ?incoming_message_queue_size:
                  t.config.incoming_message_queue_size
                ?outgoing_message_queue_size:
                  t.config.outgoing_message_queue_size
                ?binary_chunks_size:t.config.binary_chunks_size
                ~canceler
                auth_conn
                t.encoding
            in
            let*! () =
              Events.(emit authenticate_status)
                ("connected", point, info.peer_id)
            in
            return conn)
          ~on_error:(fun err ->
            if incoming then
              t.log
                (Request_rejected (point, Some (info.id_point, info.peer_id))) ;
            let*! () =
              match err with
              | P2p_errors.Rejected_by_nack
                  {alternative_points = Some points; motive}
                :: _ ->
                  let*! () =
                    Events.(emit connection_rejected_by_peers)
                      (point, info.peer_id, motive, points)
                  in
                  if t.config.disable_peer_discovery then Lwt.return_unit
                  else
                    P2p_pool.register_list_of_new_points
                      ~medium:"Nack"
                      ~source:info.peer_id
                      t.pool
                      points
              | _ -> Events.(emit connection_error) (point, err)
            in
            let*! () =
              Events.(emit authenticate_status) ("rejected", point, info.peer_id)
            in
            let timestamp = Time.System.now () in
            Option.iter
              (P2p_point_state.set_disconnected
                 ~timestamp
                 t.config.reconnection_config)
              connection_point_info ;
            P2p_peer_state.set_disconnected ~timestamp peer_info ;
            List.iter (fun f -> f info.peer_id) t.disconnection_hook ;
            Lwt.return_error err)
      in
      let id_point =
        match
          (info.id_point, Option.map P2p_point_state.Info.point point_info)
        with
        | (addr, _), Some (_, port) -> (addr, Some port)
        | id_point, None -> id_point
      in
      let*! conn =
        create_connection
          t
          conn
          id_point
          connection_point_info
          peer_info
          version
      in
      let peer_id = P2p_peer_state.Info.peer_id peer_info in
      let*! () =
        Events.(emit new_connection) (fst id_point, snd id_point, peer_id)
      in
      return conn

let authenticate t ?point_info canceler fd point =
  let open Lwt_result_syntax in
  let scheduled_conn = P2p_io_scheduler.register t.io_sched fd in
  let*! r = raw_authenticate t ?point_info canceler scheduled_conn point in
  match r with
  | Ok connection -> return connection
  | Error
      (P2p_errors.Rejected {motive = P2p_rejection.Unknown_chain_name; _} :: _)
    as err ->
      (* We don't register point that belong to another network.
         They are useless, and we don't want to advertize them.
         They are not greylisted as their might be node from our
         network on the same IP.
      *)
      P2p_pool.unregister_point t.pool point ;
      let* () = P2p_io_scheduler.close scheduled_conn in
      Lwt.return err
  | Error _ as err ->
      let* () = P2p_io_scheduler.close scheduled_conn in
      Lwt.return err

let accept t fd point =
  let open Lwt_syntax in
  t.log (Incoming_connection point) ;
  if
    t.config.max_incoming_connections <= P2p_point.Table.length t.incoming
    (* silently ignore banned points *)
    || P2p_pool.Points.banned t.pool point
  then
    Error_monad.dont_wait
      (fun () -> P2p_fd.close fd)
      (fun (`Unexpected_error ex) ->
        Format.eprintf "Uncaught error: %s\n%!" (Printexc.to_string ex))
      (fun exc ->
        Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc))
  else
    let canceler = Lwt_canceler.create () in
    P2p_point.Table.add t.incoming point canceler ;
    Lwt.dont_wait
      (fun () ->
        let* _ =
          with_timeout
            ~canceler
            (Systime_os.sleep t.config.authentication_timeout)
            (fun canceler -> authenticate t canceler fd point)
        in
        P2p_point.Table.remove t.incoming point ;
        Lwt.return_unit)
      (fun exc ->
        P2p_point.Table.remove t.incoming point ;
        P2p_pool.greylist_addr t.pool (fst point) ;
        Format.eprintf
          "Uncaught exception on incoming connection from %a: %s\n%!"
          P2p_point.Id.pp
          point
          (Printexc.to_string exc))

let fail_unless_disconnected_point point_info =
  let open Lwt_result_syntax in
  match P2p_point_state.get point_info with
  | Disconnected -> return_unit
  | Requested _ | Accepted _ -> tzfail P2p_errors.Pending_connection
  | Running _ -> tzfail P2p_errors.Connected

let connect ?timeout t point =
  let open Lwt_result_syntax in
  let* () =
    fail_when
      (P2p_pool.Points.banned t.pool point)
      (P2p_errors.Point_banned point)
  in
  let timeout = Option.value ~default:t.config.connection_timeout timeout in
  let* () =
    fail_unless
      (P2p_pool.active_connections t.pool <= t.config.max_connections)
      P2p_errors.Too_many_connections
  in
  let canceler = Lwt_canceler.create () in
  with_timeout ~canceler (Systime_os.sleep timeout) (fun canceler ->
      let point_info = P2p_pool.register_point t.pool point in
      let ((addr, port) as point) = P2p_point_state.Info.point point_info in
      let* () =
        fail_unless
          ((not t.config.private_mode)
          || t.dependencies.point_state_info_trusted point_info)
          P2p_errors.Private_mode
      in
      let* () = fail_unless_disconnected_point point_info in
      let timestamp = Time.System.now () in
      P2p_point_state.set_requested ~timestamp point_info canceler ;
      let*! fd = P2p_fd.socket PF_INET6 SOCK_STREAM 0 in
      let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
      let*! () = Events.(emit connect_status) ("start", point) in
      let* () =
        protect ~canceler (fun () ->
            t.log (Outgoing_connection point) ;
            let*! r = t.dependencies.fd_connect fd uaddr in
            Result.map_error_es
              (fun err ->
                let timestamp = Time.System.now () in
                P2p_point_state.set_disconnected
                  ~timestamp
                  t.config.reconnection_config
                  point_info ;
                let*! close_res = P2p_fd.close fd in
                let*! () =
                  match close_res with
                  | Ok () -> Lwt.return_unit
                  | Error (`Unexpected_error ex) ->
                      Events.(emit connect_close_error)
                        (point, lazy (Printexc.to_string ex))
                in
                match err with
                | `Unexpected_error ex ->
                    let*! () =
                      Events.(emit connect_error)
                        (point, lazy (Printexc.to_string ex))
                    in
                    Lwt.return_error (TzTrace.make (error_of_exn ex))
                | `Connection_refused ->
                    let*! () =
                      Events.(emit connect_error)
                        (point, lazy "connection_refused")
                    in
                    tzfail P2p_errors.Connection_refused)
              r)
      in
      let*! () = Events.(emit connect_status) ("authenticate", point) in
      authenticate t ~point_info canceler fd point)

let stat t = P2p_io_scheduler.global_stat t.io_sched

let on_new_connection t f = t.new_connection_hook <- f :: t.new_connection_hook

let on_disconnection t f = t.disconnection_hook <- f :: t.disconnection_hook

let destroy t =
  P2p_point.Table.iter_p
    (fun _point canceler -> Error_monad.cancel_with_exceptions canceler)
    t.incoming

module Internal_for_tests = struct
  type nonrec ('msg, 'peer_meta, 'conn_meta) dependencies =
        ('msg, 'peer_meta, 'conn_meta) dependencies = {
    pool_greylist_peer :
      ('msg, 'peer_meta, 'conn_meta) P2p_pool.t -> P2p_peer.Id.t -> unit;
    peer_state_info_trusted :
      ( ('msg, 'peer_meta, 'conn_meta) P2p_conn.t,
        'peer_meta,
        'conn_meta )
      P2p_peer_state.Info.t ->
      bool;
    point_state_info_trusted :
      ('msg, 'peer_meta, 'conn_meta) P2p_conn.t P2p_point_state.Info.t -> bool;
    fd_connect :
      P2p_fd.t ->
      Unix.sockaddr ->
      (unit, [`Unexpected_error of exn | `Connection_refused]) result Lwt.t;
    socket_authenticate :
      canceler:Lwt_canceler.t ->
      proof_of_work_target:Tezos_crypto.Crypto_box.pow_target ->
      incoming:bool ->
      P2p_io_scheduler.connection ->
      P2p_point.Id.t ->
      ?advertised_port:int ->
      P2p_identity.t ->
      Network_version.t ->
      'conn_meta P2p_params.conn_meta_config ->
      ('conn_meta P2p_connection.Info.t
      * 'conn_meta P2p_socket.authenticated_connection)
      tzresult
      Lwt.t;
    socket_accept :
      ?incoming_message_queue_size:int ->
      ?outgoing_message_queue_size:int ->
      ?binary_chunks_size:int ->
      canceler:Lwt_canceler.t ->
      'conn_meta P2p_socket.authenticated_connection ->
      'msg P2p_message.t Data_encoding.t ->
      ('msg P2p_message.t, 'conn_meta) P2p_socket.t tzresult Lwt.t;
  }

  let mock_dependencies default_metadata =
    {
      pool_greylist_peer = (fun _ _ -> ());
      peer_state_info_trusted = (fun _ -> true);
      point_state_info_trusted = (fun _ -> true);
      fd_connect = (fun _ _ -> Lwt.return_ok ());
      socket_authenticate =
        (fun ~canceler:_
             ~proof_of_work_target:_
             ~incoming:_
             _
             _
             ?advertised_port:_
             _
             _
             _ ->
          let connection_info =
            P2p_connection.Internal_for_tests.Info.mock default_metadata
          in
          let authenticated_connection =
            P2p_socket.Internal_for_tests.mock_authenticated_connection
              default_metadata
          in
          Lwt.return_ok (connection_info, authenticated_connection));
      socket_accept =
        (fun ?incoming_message_queue_size:_
             ?outgoing_message_queue_size:_
             ?binary_chunks_size:_
             ~canceler:_
             authenticated_connection
             _encoding ->
          Lwt.return_ok
            (P2p_socket.Internal_for_tests.mock authenticated_connection));
    }

  let dumb_config : config =
    let incoming_app_message_queue_size = None in
    let private_mode = true in
    let min_connections = 10 in
    let max_connections = 10 in
    let max_incoming_connections = 20 in
    let incoming_message_queue_size = None in
    let outgoing_message_queue_size = None in
    let binary_chunks_size = None in
    let identity = P2p_identity.generate_with_pow_target_0 () in
    let connection_timeout = Time.System.Span.of_seconds_exn 10. in
    let authentication_timeout = Time.System.Span.of_seconds_exn 5. in
    let reconnection_config = Point_reconnection_config.default in
    let proof_of_work_target = Tezos_crypto.Crypto_box.make_pow_target 0. in
    let listening_port = Some 9732 in
    let advertised_port = None in
    let disable_peer_discovery = false in
    {
      incoming_app_message_queue_size;
      private_mode;
      min_connections;
      max_connections;
      max_incoming_connections;
      incoming_message_queue_size;
      outgoing_message_queue_size;
      binary_chunks_size;
      identity;
      connection_timeout;
      authentication_timeout;
      reconnection_config;
      proof_of_work_target;
      listening_port;
      advertised_port;
      disable_peer_discovery;
    }

  (** An encoding that typechecks for all types, but fails at runtime. This is a placeholder as most tests never go through
      this encoding. Any test that requires it should provide a valid encoding in whatever mock needs it directly.

      The use of [assert false] rather than raising an error is to let developers have the stack trace, in case of test failure. *)
  let make_crashing_encoding () : 'a Data_encoding.t =
    Data_encoding.conv
      (fun _ -> assert false)
      (fun _ -> assert false)
      Data_encoding.unit

  let conn_meta_config_default () =
    P2p_params.
      {
        conn_meta_encoding = make_crashing_encoding ();
        conn_meta_value = (fun () -> assert false);
        private_node = (fun _ -> false);
      }

  let message_config_default () =
    P2p_params.
      {
        encoding = [];
        chain_name = Distributed_db_version.Name.of_string "";
        distributed_db_versions = [Distributed_db_version.zero];
      }

  let create ?(config = dumb_config) ?(log = fun _ -> ())
      ?(triggers = P2p_trigger.create ())
      ?(io_sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) ())
      ?(announced_version = Network_version.Internal_for_tests.mock ())
      ?(conn_meta_config = conn_meta_config_default ())
      ?(message_config = message_config_default ())
      ?(custom_p2p_versions = [P2p_version.zero])
      ?(encoding = make_crashing_encoding ())
      ?(incoming = P2p_point.Table.create ~random:true 53)
      ?(new_connection_hook = []) ?(disconnection_hook = [])
      ?(answerer = lazy (P2p_protocol.create_private ())) pool dependencies :
      ('msg, 'peer_meta, 'conn_meta) t =
    let pool =
      match pool with
      | `Pool pool -> pool
      | `Make_default_pool default_peer_meta ->
          P2p_pool.Internal_for_tests.create
            (make_crashing_encoding ())
            default_peer_meta
    in
    let dependencies =
      match dependencies with
      | `Dependencies dependencies -> dependencies
      | `Make_default_dependencies default_conn_meta ->
          mock_dependencies default_conn_meta
    in
    {
      config;
      pool;
      log;
      triggers;
      io_sched;
      announced_version;
      conn_meta_config;
      message_config;
      custom_p2p_versions;
      encoding;
      incoming;
      new_connection_hook;
      disconnection_hook;
      answerer;
      dependencies;
    }
end
