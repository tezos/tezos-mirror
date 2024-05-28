(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module Events = P2p_events.P2p

type config = {
  listening_port : P2p_addr.port option;
  listening_addr : P2p_addr.t option;
  advertised_port : P2p_addr.port option;
  discovery_port : P2p_addr.port option;
  discovery_addr : Ipaddr.V4.t option;
  trusted_points : (P2p_point.Id.t * P2p_peer.Id.t option) list;
  peers_file : string;
  private_mode : bool;
  identity : P2p_identity.t;
  proof_of_work_target : Tezos_crypto.Crypto_box.pow_target;
  trust_discovered_peers : bool;
  reconnection_config : Point_reconnection_config.t;
  disable_peer_discovery : bool;
}

let create_scheduler limits =
  let open P2p_limits in
  let max_upload_speed = Option.map (( * ) 1024) limits.max_upload_speed in
  let max_download_speed = Option.map (( * ) 1024) limits.max_download_speed in
  P2p_io_scheduler.create
    ~read_buffer_size:limits.read_buffer_size
    ?max_upload_speed
    ?max_download_speed
    ?read_queue_size:limits.read_queue_size
    ?write_queue_size:limits.write_queue_size
    ()

let create_connection_pool config limits meta_cfg log triggers =
  let open P2p_limits in
  let pool_cfg =
    {
      P2p_pool.identity = config.identity;
      trusted_points = config.trusted_points;
      peers_file = config.peers_file;
      private_mode = config.private_mode;
      max_known_points = limits.max_known_points;
      max_known_peer_ids = limits.max_known_peer_ids;
      peer_greylist_size = limits.peer_greylist_size;
      ip_greylist_size_in_kilobytes = limits.ip_greylist_size_in_kilobytes;
      ip_greylist_cleanup_delay = limits.ip_greylist_cleanup_delay;
    }
  in
  P2p_pool.create pool_cfg meta_cfg ~log triggers

let create_connect_handler config limits pool msg_cfg conn_meta_cfg io_sched
    triggers log answerer =
  let open P2p_limits in
  let connect_handler_cfg =
    {
      P2p_connect_handler.identity = config.identity;
      proof_of_work_target = config.proof_of_work_target;
      listening_port = config.listening_port;
      advertised_port = config.advertised_port;
      private_mode = config.private_mode;
      reconnection_config = config.reconnection_config;
      min_connections = limits.min_connections;
      max_connections = limits.max_connections;
      max_incoming_connections = limits.max_incoming_connections;
      connection_timeout = limits.connection_timeout;
      authentication_timeout = limits.authentication_timeout;
      incoming_app_message_queue_size = limits.incoming_app_message_queue_size;
      incoming_message_queue_size = limits.incoming_message_queue_size;
      outgoing_message_queue_size = limits.outgoing_message_queue_size;
      binary_chunks_size = limits.binary_chunks_size;
      disable_peer_discovery = config.disable_peer_discovery;
    }
  in
  P2p_connect_handler.create
    connect_handler_cfg
    pool
    msg_cfg
    conn_meta_cfg
    io_sched
    triggers
    ~log
    ~answerer

let may_create_discovery_worker _limits config pool =
  match
    (config.listening_port, config.discovery_port, config.discovery_addr)
  with
  | Some listening_port, Some discovery_port, Some discovery_addr ->
      Some
        (P2p_discovery.create
           pool
           config.identity.peer_id
           ~listening_port
           ~discovery_port
           ~discovery_addr
           ~trust_discovered_peers:config.trust_discovered_peers)
  | _, _, _ -> None

let create_maintenance_worker limits pool connect_handler config triggers log =
  let open P2p_limits in
  let open Lwt_syntax in
  match limits.maintenance_idle_time with
  | None -> return_none
  | Some maintenance_idle_time ->
      let maintenance_config =
        {
          P2p_maintenance.maintenance_idle_time;
          private_mode = config.private_mode;
          min_connections = limits.min_connections;
          max_connections = limits.max_connections;
          expected_connections = limits.expected_connections;
          time_between_looking_for_peers =
            Ptime.Span.of_int_s 5
            (* Empirical value. Enough to observe changes in the network,
               and not too long to discover new peers quickly. *)
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/1655
               Check whether the value is optimal or not through integration tests
            *);
        }
      in
      let discovery = may_create_discovery_worker limits config pool in
      return_some
        (P2p_maintenance.create
           ?discovery
           maintenance_config
           pool
           connect_handler
           triggers
           ~log)

let may_create_welcome_worker config limits connect_handler =
  config.listening_port
  |> Option.map_es (fun port ->
         P2p_welcome.create
           ~backlog:limits.P2p_limits.backlog
           connect_handler
           ?addr:config.listening_addr
           port)

type ('msg, 'peer_meta, 'conn_meta) connection =
  ('msg, 'peer_meta, 'conn_meta) P2p_conn.t

module Real = struct
  type ('msg, 'peer_meta, 'conn_meta) net = {
    config : config;
    limits : P2p_limits.t;
    io_sched : P2p_io_scheduler.t;
    pool : ('msg, 'peer_meta, 'conn_meta) P2p_pool.t;
    connect_handler : ('msg, 'peer_meta, 'conn_meta) P2p_connect_handler.t;
    maintenance : ('msg, 'peer_meta, 'conn_meta) P2p_maintenance.t option;
    welcome : P2p_welcome.t option;
    watcher : P2p_connection.P2p_event.t Lwt_watcher.input;
    triggers : P2p_trigger.t;
    received_msg_hook :
      ('msg, 'peer_meta, 'conn_meta) connection -> 'msg -> unit;
    sent_msg_hook : ('msg, 'peer_meta, 'conn_meta) connection -> 'msg -> unit;
    broadcasted_msg_hook :
      ('msg, 'peer_meta, 'conn_meta) connection P2p_peer.Table.t ->
      ?except:(('msg, 'peer_meta, 'conn_meta) connection -> bool) ->
      ?alt:(('msg, 'peer_meta, 'conn_meta) connection -> bool) * 'msg ->
      'msg ->
      unit;
  }

  let create ~config ~limits ?received_msg_hook ?sent_msg_hook
      ?broadcasted_msg_hook meta_cfg msg_cfg conn_meta_cfg =
    let open Lwt_result_syntax in
    let io_sched = create_scheduler limits in
    let watcher = Lwt_watcher.create_input () in
    let log event = Lwt_watcher.notify watcher event in
    let triggers = P2p_trigger.create () in
    let*! pool = create_connection_pool config limits meta_cfg log triggers in
    (* There is a mutual recursion between an answerer and connect_handler,
       for the default answerer. Because of the swap request mechanism, the
       default answerer needs to initiate new connections using the
       [P2p_connect_handler.connect] callback. *)
    let rec answerer =
      lazy
        (if config.private_mode then P2p_protocol.create_private ()
        else
          let connect =
            P2p_connect_handler.connect (Lazy.force connect_handler)
          in
          let proto_conf =
            {
              P2p_protocol.swap_linger = limits.P2p_limits.swap_linger;
              pool;
              log;
              connect;
              latest_accepted_swap = Ptime.epoch;
              latest_successful_swap = Ptime.epoch;
            }
          in
          P2p_protocol.create_default proto_conf)
    and connect_handler =
      lazy
        (create_connect_handler
           config
           limits
           pool
           msg_cfg
           conn_meta_cfg
           io_sched
           triggers
           log
           answerer)
    in
    let connect_handler = Lazy.force connect_handler in
    let*! maintenance =
      create_maintenance_worker limits pool connect_handler config triggers log
    in
    let* welcome = may_create_welcome_worker config limits connect_handler in
    P2p_metrics_collectors.collect pool io_sched ;
    return
      {
        config;
        limits;
        io_sched;
        pool;
        connect_handler;
        maintenance;
        welcome;
        watcher;
        triggers;
        received_msg_hook =
          Option.value ~default:(fun _ _ -> ()) received_msg_hook;
        sent_msg_hook = Option.value ~default:(fun _ _ -> ()) sent_msg_hook;
        broadcasted_msg_hook =
          Option.value
            ~default:(fun _ ?except:_ ?alt:_ _ -> ())
            broadcasted_msg_hook;
      }

  let peer_id {config; _} = config.identity.peer_id

  let maintain {maintenance; _} () =
    let open Lwt_result_syntax in
    match maintenance with
    | Some maintenance ->
        let*! () = P2p_maintenance.maintain maintenance in
        return_unit
    | None -> tzfail P2p_errors.Maintenance_disabled

  let activate t () =
    Events.(emit__dont_wait__use_with_care activate_network)
      t.config.identity.peer_id ;
    (match t.welcome with None -> () | Some w -> P2p_welcome.activate w) ;
    match t.maintenance with
    | Some maintenance -> P2p_maintenance.activate maintenance
    | None -> ()

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4597
     Implement [roll] function. *)
  let roll _net () = Lwt.return_unit

  (* returns when all workers have shut down in the opposite
     creation order. *)
  let shutdown net () =
    let open Lwt_syntax in
    let* () = Events.(emit shutdown_welcome_worker) () in
    let* () = Option.iter_s P2p_welcome.shutdown net.welcome in
    let* () = Events.(emit shutdown_maintenance_worker) () in
    let* () =
      Option.iter_s
        (fun maintenance -> P2p_maintenance.shutdown maintenance)
        net.maintenance
    in
    let* () = Events.(emit shutdown_connection_pool) () in
    let* () = P2p_pool.destroy net.pool in
    let* () = Events.(emit shutdown_connection_handler) () in
    let* () = P2p_connect_handler.destroy net.connect_handler in
    let* () = Events.(emit shutdown_scheduler) () in
    P2p_io_scheduler.shutdown ~timeout:3.0 net.io_sched

  let connections {pool; _} () =
    P2p_pool.Connection.fold pool ~init:[] ~f:(fun _peer_id c acc -> c :: acc)

  let find_connection_by_peer_id {pool; _} peer_id =
    P2p_pool.Connection.find_by_peer_id pool peer_id

  let find_connection_by_point {pool; _} point =
    P2p_pool.Connection.find_by_point pool point

  let disconnect ?wait ~reason = P2p_conn.disconnect ?wait ~reason:(User reason)

  let connection_info _net conn = P2p_conn.info conn

  let connection_local_metadata _net conn = P2p_conn.local_metadata conn

  let connection_remote_metadata _net conn = P2p_conn.remote_metadata conn

  let connection_stat _net conn = P2p_conn.stat conn

  let global_stat {connect_handler; _} () =
    P2p_connect_handler.stat connect_handler

  let set_peer_metadata {pool; _} conn meta =
    P2p_pool.Peers.set_peer_metadata pool conn meta

  let get_peer_metadata {pool; _} conn =
    P2p_pool.Peers.get_peer_metadata pool conn

  let connect ?trusted ?expected_peer_id ?timeout net point =
    P2p_connect_handler.connect
      ?trusted
      ?expected_peer_id
      ?timeout
      net.connect_handler
      point

  let recv net conn =
    let open Lwt_syntax in
    let* msg = P2p_conn.read conn in
    let peer_id = (P2p_conn.info conn).peer_id in
    let* () =
      match msg with
      | Ok msg ->
          let* () = Events.(emit message_read) peer_id in
          net.received_msg_hook conn msg ;
          return_unit
      | Error _ -> Events.(emit message_read_error) peer_id
    in
    return msg

  let rec recv_any net () =
    let open Lwt_syntax in
    let pipes =
      P2p_pool.Connection.fold net.pool ~init:[] ~f:(fun _peer_id conn acc ->
          (let* r = P2p_conn.is_readable conn in
           match r with
           | Ok () -> Lwt.return_some conn
           | Error _ -> Lwt_utils.never_ending ())
          :: acc)
    in
    let new_connection =
      let* () = P2p_trigger.wait_new_connection net.triggers in
      Lwt.return_none
    in
    let* o = Lwt.pick (new_connection :: pipes) in
    match o with
    | None -> recv_any net ()
    | Some conn -> (
        let* r = recv net conn in
        match r with
        | Ok msg ->
            net.received_msg_hook conn msg ;
            Lwt.return (conn, msg)
        | Error _ ->
            let* () = Lwt.pause () in
            recv_any net ())

  let send net conn m =
    let open Lwt_result_syntax in
    let*! r = P2p_conn.write conn m in
    let*! () =
      match r with
      | Ok () ->
          let peer_id = (P2p_conn.info conn).peer_id in
          let*! () = Events.(emit message_sent) peer_id in
          net.sent_msg_hook conn m ;
          Lwt.return_unit
      | Error trace ->
          Events.(emit sending_message_error)
            ((P2p_conn.info conn).peer_id, trace)
    in
    Lwt.return r

  let try_send net conn m =
    match P2p_conn.write_now conn m with
    | Ok v ->
        Events.(emit__dont_wait__use_with_care message_trysent)
          ((P2p_conn.info conn).peer_id, v) ;
        if v then net.sent_msg_hook conn m ;
        v
    | Error err ->
        Events.(emit__dont_wait__use_with_care trysending_message_error)
          ((P2p_conn.info conn).peer_id, err) ;
        false

  (* Memoization of broadcast encoded [msg] inside a buffer [buf]. *)
  (* For generalisation purposes, each connection has a `writer` that
     defines a specific encoding for messages. Currently we use the
     same encoding for every connection. It makes this simple
     memoization possible but will need modifications if connections
     have specialised encodings. *)
  let broadcast_encode conn buff msg =
    let open Result_syntax in
    match !buff with
    | None ->
        let* encoded_msg = P2p_conn.encode conn msg in
        buff := Some encoded_msg ;
        return encoded_msg
    | Some em -> return em

  let send_conn ?alt conn buf alt_buf msg =
    let open Result_syntax in
    (* Silently discards Error P2p_errors.Connection_closed in case
                  the pipe is closed. Shouldn't happen because
       - no race conditions (no Lwt)
       - the peer state is Running.

       Also ignore if the message is dropped instead of being added
       to the write queue. *)
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4205
       Ensure sent messages are actually sent.
    *)
    ignore
    @@ let* encoded_msg =
         match alt with
         | None -> broadcast_encode conn buf msg
         | Some (if_conn, then_msg) ->
             if if_conn conn then broadcast_encode conn alt_buf then_msg
             else broadcast_encode conn buf msg
       in
       P2p_conn.write_encoded_now
         conn
         (P2p_socket.copy_encoded_message encoded_msg)

  let raw_broadcast connections ?except ?alt msg =
    let buf = ref None in
    let alt_buf = ref None in
    let send conn = send_conn ?alt conn buf alt_buf msg in
    P2p_peer.Table.iter
      (fun _peer_id conn ->
        match except with
        | None -> send conn
        | Some f when not (f conn) -> send conn
        | _ -> ())
      connections ;
    Events.(emit__dont_wait__use_with_care broadcast) ()

  let broadcast net connections ?except ?alt msg =
    raw_broadcast connections ?except ?alt msg ;
    net.broadcasted_msg_hook connections ?except ?alt msg

  let fold_connections {pool; _} ~init ~f =
    P2p_pool.Connection.fold pool ~init ~f

  let iter_connections {pool; _} f =
    P2p_pool.Connection.fold pool ~init:() ~f:(fun gid conn () -> f gid conn)

  let on_new_connection {connect_handler; _} f =
    P2p_connect_handler.on_new_connection connect_handler f

  let on_disconnection {connect_handler; _} f =
    P2p_connect_handler.on_disconnection connect_handler f

  let negotiated_version _ conn = P2p_conn.negotiated_version conn
end

module Fake = struct
  let id = P2p_identity.generate_with_pow_target_0 ()

  let empty_stat =
    {
      P2p_stat.total_sent = 0L;
      total_recv = 0L;
      current_inflow = 0;
      current_outflow = 0;
    }

  let connection_info announced_version faked_metadata =
    {
      P2p_connection.Info.incoming = false;
      peer_id = id.peer_id;
      id_point = (Ipaddr.V6.unspecified, None);
      remote_socket_port = 0;
      announced_version;
      local_metadata = faked_metadata;
      remote_metadata = faked_metadata;
      private_node = false;
    }
end

type ('msg, 'peer_meta, 'conn_meta) t = {
  announced_version : Network_version.t;
  peer_id : P2p_peer.Id.t;
  maintain : unit -> unit tzresult Lwt.t;
  roll : unit -> unit Lwt.t;
  shutdown : unit -> unit Lwt.t;
  connections : unit -> ('msg, 'peer_meta, 'conn_meta) connection list;
  find_connection_by_peer_id :
    P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection option;
  find_connection_by_point :
    P2p_point.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection option;
  disconnect :
    ?wait:bool ->
    reason:string ->
    ('msg, 'peer_meta, 'conn_meta) connection ->
    unit Lwt.t;
  connection_info :
    ('msg, 'peer_meta, 'conn_meta) connection ->
    'conn_meta P2p_connection.Info.t;
  connection_local_metadata :
    ('msg, 'peer_meta, 'conn_meta) connection -> 'conn_meta;
  connection_remote_metadata :
    ('msg, 'peer_meta, 'conn_meta) connection -> 'conn_meta;
  connection_stat : ('msg, 'peer_meta, 'conn_meta) connection -> P2p_stat.t;
  global_stat : unit -> P2p_stat.t;
  get_peer_metadata : P2p_peer.Id.t -> 'peer_meta;
  set_peer_metadata : P2p_peer.Id.t -> 'peer_meta -> unit;
  connect :
    ?trusted:bool ->
    ?expected_peer_id:P2p_peer.Id.t ->
    ?timeout:Ptime.span ->
    P2p_point.Id.t ->
    ('msg, 'peer_meta, 'conn_meta) connection tzresult Lwt.t;
  recv : ('msg, 'peer_meta, 'conn_meta) connection -> 'msg tzresult Lwt.t;
  recv_any : unit -> (('msg, 'peer_meta, 'conn_meta) connection * 'msg) Lwt.t;
  send :
    ('msg, 'peer_meta, 'conn_meta) connection -> 'msg -> unit tzresult Lwt.t;
  try_send : ('msg, 'peer_meta, 'conn_meta) connection -> 'msg -> bool;
  broadcast :
    ('msg, 'peer_meta, 'conn_meta) connection P2p_peer.Table.t ->
    ?except:(('msg, 'peer_meta, 'conn_meta) connection -> bool) ->
    ?alt:(('msg, 'peer_meta, 'conn_meta) connection -> bool) * 'msg ->
    'msg ->
    unit;
  pool : ('msg, 'peer_meta, 'conn_meta) P2p_pool.t option;
  connect_handler : ('msg, 'peer_meta, 'conn_meta) P2p_connect_handler.t option;
  fold_connections :
    'a.
    init:'a ->
    f:(P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> 'a -> 'a) ->
    'a;
  iter_connections :
    (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) -> unit;
  on_new_connection :
    (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) -> unit;
  on_disconnection : (P2p_peer.Id.t -> unit) -> unit;
  negotiated_version :
    ('msg, 'peer_meta, 'conn_meta) connection -> Network_version.t;
  activate : unit -> unit;
  watcher : P2p_connection.P2p_event.t Lwt_watcher.input;
}

type ('msg, 'peer_meta, 'conn_meta) net = ('msg, 'peer_meta, 'conn_meta) t

let announced_version net = net.announced_version

let pool net = net.pool

let connect_handler net = net.connect_handler

let check_limits =
  let open Result_syntax in
  let open P2p_limits in
  let fail_1 v orig =
    if not (Ptime.Span.compare v Ptime.Span.zero <= 0) then return_unit
    else
      Error_monad.error_with
        "value of option %S cannot be negative or null@."
        orig
  in
  let fail_2 v orig =
    if not (v < 0) then return_unit
    else Error_monad.error_with "value of option %S cannot be negative@." orig
  in
  fun c ->
    let* () = fail_1 c.authentication_timeout "authentication-timeout" in
    let* () = fail_2 c.min_connections "min-connections" in
    let* () = fail_2 c.expected_connections "expected-connections" in
    let* () = fail_2 c.max_connections "max-connections" in
    let* () = fail_2 c.max_incoming_connections "max-incoming-connections" in
    let* () = fail_2 c.read_buffer_size "read-buffer-size" in
    let* () =
      match c.swap_linger with
      | Some swap_linger -> fail_1 swap_linger "swap-linger"
      | None -> return_unit
    in
    let* () =
      match c.binary_chunks_size with
      | None -> return_unit
      | Some size -> P2p_socket.check_binary_chunks_size size
    in
    return_unit

let create ~config ~limits ?received_msg_hook ?sent_msg_hook
    ?broadcasted_msg_hook peer_cfg conn_cfg msg_cfg =
  let open Lwt_result_syntax in
  let*? () = check_limits limits in
  let* net =
    Real.create
      ~config
      ~limits
      ?received_msg_hook
      ?sent_msg_hook
      ?broadcasted_msg_hook
      peer_cfg
      msg_cfg
      conn_cfg
  in
  return
    {
      announced_version =
        Network_version.announced
          ~chain_name:msg_cfg.chain_name
          ~distributed_db_versions:msg_cfg.distributed_db_versions
          ~p2p_versions:P2p_version.supported;
      peer_id = Real.peer_id net;
      maintain = Real.maintain net;
      roll = Real.roll net;
      shutdown = Real.shutdown net;
      connections = Real.connections net;
      find_connection_by_peer_id = Real.find_connection_by_peer_id net;
      find_connection_by_point = Real.find_connection_by_point net;
      disconnect = Real.disconnect;
      connection_info = Real.connection_info net;
      connection_local_metadata = Real.connection_local_metadata net;
      connection_remote_metadata = Real.connection_remote_metadata net;
      connection_stat = Real.connection_stat net;
      global_stat = Real.global_stat net;
      get_peer_metadata = Real.get_peer_metadata net;
      set_peer_metadata = Real.set_peer_metadata net;
      connect =
        (fun ?trusted ?expected_peer_id ?timeout ->
          Real.connect ?trusted ?expected_peer_id ?timeout net);
      recv = Real.recv net;
      recv_any = Real.recv_any net;
      send = Real.send net;
      try_send = Real.try_send net;
      broadcast = Real.broadcast net;
      pool = Some net.pool;
      connect_handler = Some net.connect_handler;
      fold_connections = (fun ~init ~f -> Real.fold_connections net ~init ~f);
      iter_connections = Real.iter_connections net;
      on_new_connection = Real.on_new_connection net;
      on_disconnection = Real.on_disconnection net;
      negotiated_version = Real.negotiated_version net;
      activate = Real.activate net;
      watcher = net.Real.watcher;
    }

let activate t =
  Events.(emit__dont_wait__use_with_care activate_layer) () ;
  t.activate ()

let faked_network (msg_cfg : 'msg P2p_params.message_config) peer_cfg
    faked_metadata =
  let announced_version =
    Network_version.announced
      ~chain_name:msg_cfg.chain_name
      ~distributed_db_versions:msg_cfg.distributed_db_versions
      ~p2p_versions:P2p_version.supported
  in
  {
    announced_version;
    peer_id = Fake.id.peer_id;
    maintain = Lwt_result_syntax.return;
    roll = Lwt.return;
    shutdown = Lwt.return;
    connections = (fun () -> []);
    find_connection_by_peer_id = (fun _ -> None);
    find_connection_by_point = (fun _ -> None);
    disconnect = (fun ?wait:_ ~reason:_ _ -> Lwt.return_unit);
    connection_info =
      (fun _ -> Fake.connection_info announced_version faked_metadata);
    connection_local_metadata = (fun _ -> faked_metadata);
    connection_remote_metadata = (fun _ -> faked_metadata);
    connection_stat = (fun _ -> Fake.empty_stat);
    global_stat = (fun () -> Fake.empty_stat);
    get_peer_metadata = (fun _ -> peer_cfg.P2p_params.peer_meta_initial ());
    set_peer_metadata = (fun _ _ -> ());
    connect =
      (fun ?trusted:_ ?expected_peer_id:_ ?timeout:_ _ ->
        Lwt_result_syntax.tzfail P2p_errors.Connection_failed);
    recv = (fun _ -> Lwt_utils.never_ending ());
    recv_any = (fun () -> Lwt_utils.never_ending ());
    send = (fun _ _ -> Lwt_result_syntax.tzfail P2p_errors.Connection_closed);
    try_send = (fun _ _ -> false);
    broadcast = (fun _ ?except:_ ?alt:_ _ -> ());
    fold_connections = (fun ~init ~f:_ -> init);
    iter_connections = (fun _f -> ());
    on_new_connection = (fun _f -> ());
    on_disconnection = (fun _f -> ());
    negotiated_version = (fun _ -> announced_version);
    pool = None;
    connect_handler = None;
    activate = (fun _ -> ());
    watcher = Lwt_watcher.create_input ();
  }

let peer_id net = net.peer_id

let maintain net = net.maintain ()

let roll net = net.roll ()

let shutdown net = net.shutdown ()

let connections net = net.connections ()

let disconnect net = net.disconnect

let find_connection_by_peer_id net = net.find_connection_by_peer_id

let find_connection_by_point net = net.find_connection_by_point

let connection_info net = net.connection_info

let connection_local_metadata net = net.connection_local_metadata

let connection_remote_metadata net = net.connection_remote_metadata

let connection_stat net = net.connection_stat

let global_stat net = net.global_stat ()

let get_peer_metadata net = net.get_peer_metadata

let set_peer_metadata net = net.set_peer_metadata

let connect net = net.connect

let recv net = net.recv

let recv_any net = net.recv_any ()

let send net = net.send

let try_send net = net.try_send

let broadcast net connections ?except ?alt =
  net.broadcast connections ?except ?alt

let fold_connections net = net.fold_connections

let iter_connections net = net.iter_connections

let on_new_connection net = net.on_new_connection

let on_disconnection net = net.on_disconnection

let greylist_addr net addr =
  Option.iter (fun pool -> P2p_pool.greylist_addr pool addr) net.pool

let greylist_peer net peer_id =
  Option.iter (fun pool -> P2p_pool.greylist_peer pool peer_id) net.pool

let watcher net = Lwt_watcher.create_stream net.watcher

let negotiated_version net = net.negotiated_version

module Internal_for_tests = struct
  let raw_broadcast (connections : ('a, 'b, 'c) P2p_conn.t P2p_peer.Table.t)
      ?except ?alt =
    Real.raw_broadcast connections ?except ?alt
end
