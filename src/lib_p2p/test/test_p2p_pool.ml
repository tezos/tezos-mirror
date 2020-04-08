(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing of the Pool

    Each test launches nodes in separate process, each node has its own pool and is given a function to be executed.

    [Simple] test: each node pings all other nodes.

    [Random] test: each node pings all other nodes but at random points in time.

    [Garbled] test: each node write garbled data to all peers, then it checks that connections are closed.

    [Overcrowded] tests:
    both test [run] and [run_mixed_versions] creates one target node which
    knows all the other points  and has max_incoming_connections set
    to zero,
    all other clients know only the target and try to connect to it.

    In [run], each client use p2p v.1 and check that they eventually
    know all other nodes thanks to the node reply.

    In [run_mixed_versions], half the clients do as in run and half of
    the clients use p2p v.0 and check that they didn't receive new
    nodes (meaning that [target] actually sent a Nack_v_0.

*)

include Internal_event.Legacy_logging.Make (struct
  let name = "test.p2p.connection-pool"
end)

type message = Ping

let msg_config : message P2p_params.message_config =
  {
    encoding =
      [ P2p_params.Encoding
          {
            tag = 0x10;
            title = "Ping";
            encoding = Data_encoding.empty;
            wrap = (function () -> Ping);
            unwrap = (function Ping -> Some ());
            max_length = None;
          } ];
    chain_name = Distributed_db_version.Name.of_string "SANDBOXED_TEZOS";
    distributed_db_versions = [Distributed_db_version.zero];
  }

type metadata = unit

let peer_meta_config : metadata P2p_params.peer_meta_config =
  {
    peer_meta_encoding = Data_encoding.empty;
    peer_meta_initial = (fun _ -> ());
    score = (fun () -> 0.);
  }

let conn_meta_config : metadata P2p_params.conn_meta_config =
  {
    conn_meta_encoding = Data_encoding.empty;
    conn_meta_value = (fun () -> ());
    private_node = (fun _ -> false);
  }

let sync iteration ch =
  incr iteration ;
  lwt_debug "Sync iteration %i" !iteration
  >>= fun () ->
  Process.Channel.push ch ()
  >>=? fun () -> Process.Channel.pop ch >>=? fun () -> return_unit

(** Syncing everyone until one node fails to sync  *)
let rec sync_nodes nodes =
  iter_p (fun {Process.channel; _} -> Process.Channel.pop channel) nodes
  >>=? fun () ->
  iter_p (fun {Process.channel; _} -> Process.Channel.push channel ()) nodes
  >>=? fun () -> sync_nodes nodes

let sync_nodes nodes =
  sync_nodes nodes
  >>= function
  | Ok () | Error (Exn End_of_file :: _) ->
      return_unit
  | Error _ as err ->
      Lwt.return err

let detach_node ?(prefix = "") ?timeout ?(min_connections : int option)
    ?max_connections ?max_incoming_connections ?p2p_versions
    ?(msg_config = msg_config) f trusted_points all_points addr port =
  let trusted_points =
    List.filter
      (fun p -> not (P2p_point.Id.equal (addr, port) p))
      trusted_points
  in
  let proof_of_work_target = Crypto_box.make_target 0. in
  let identity = P2p_identity.generate proof_of_work_target in
  let private_mode = false in
  let nb_points = List.length trusted_points in
  let unopt = Option.unopt ~default:nb_points in
  let connect_handler_cfg =
    P2p_connect_handler.
      {
        identity;
        proof_of_work_target;
        listening_port = Some port;
        private_mode;
        greylisting_config = P2p_point_state.Info.default_greylisting_config;
        min_connections = unopt min_connections;
        max_connections = unopt max_connections;
        max_incoming_connections = unopt max_incoming_connections;
        connection_timeout = Time.System.Span.of_seconds_exn 10.;
        authentication_timeout = Time.System.Span.of_seconds_exn 2.;
        incoming_app_message_queue_size = None;
        incoming_message_queue_size = None;
        outgoing_message_queue_size = None;
        binary_chunks_size = None;
      }
  in
  let pool_config =
    P2p_pool.
      {
        identity;
        trusted_points;
        peers_file = "/dev/null";
        private_mode;
        max_known_points = None;
        max_known_peer_ids = None;
      }
  in
  (* swap_linger = Time.System.Span.of_seconds_exn 0. ; *)
  Process.detach
    ~prefix:
      (Format.asprintf "%s%a: " prefix P2p_peer.Id.pp_short identity.peer_id)
    (fun channel ->
      let canceler = Lwt_canceler.create () in
      let timer ti =
        Lwt_unix.sleep ti >>= fun () -> lwt_debug "Process timeout"
      in
      with_timeout
        ~canceler
        (Option.unopt_map ~f:timer ~default:(Lwt_utils.never_ending ()) timeout)
        (fun _canceler ->
          let iteration = ref 0 in
          let sched =
            P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) ()
          in
          let triggers = P2p_trigger.create () in
          let watcher = Lwt_watcher.create_input () in
          let stream = Lwt_watcher.create_stream watcher in
          let log event = Lwt_watcher.notify watcher event in
          P2p_pool.create pool_config peer_meta_config ~log triggers
          >>= fun pool ->
          let answerer = lazy (P2p_protocol.create_private ()) in
          let connect_handler =
            P2p_connect_handler.create
              ?p2p_versions
              connect_handler_cfg
              pool
              msg_config
              conn_meta_config
              sched
              triggers
              ~log
              ~answerer
          in
          Lwt_list.map_p
            (fun point ->
              P2p_pool.Points.info pool point
              |> Option.iter ~f:(fun info ->
                     P2p_point_state.set_private info false) ;
              Lwt.return_unit)
            trusted_points
          >>= fun _ ->
          P2p_welcome.create ~backlog:10 connect_handler ~addr port
          >>= fun welcome ->
          P2p_welcome.activate welcome ;
          lwt_log_info "Node ready (port: %d)@." port
          >>= fun () ->
          sync iteration channel
          >>=? fun () ->
          (* Sync interation 1 *)
          f
            iteration
            channel
            stream
            connect_handler
            pool
            trusted_points
            all_points
          >>=? fun () ->
          lwt_log_info "Shutting down...@."
          >>= fun () ->
          P2p_welcome.shutdown welcome
          >>= fun () ->
          P2p_pool.destroy pool
          >>= fun () ->
          P2p_io_scheduler.shutdown sched
          >>= fun () -> lwt_log_info "Bye.@." >>= fun () -> return_unit))

let detach_nodes ?prefix ?timeout ?min_connections ?max_connections
    ?max_incoming_connections ?p2p_versions ?msg_config run_node
    ?(trusted = fun _ points -> points) points =
  let clients = List.length points in
  Lwt_list.map_p
    (fun n ->
      let prefix = Option.map prefix ~f:(fun f -> f n) in
      let p2p_versions = Option.map p2p_versions ~f:(fun f -> f n) in
      let msg_config = Option.map msg_config ~f:(fun f -> f n) in
      let min_connections = Option.map min_connections ~f:(fun f -> f n) in
      let max_connections = Option.map max_connections ~f:(fun f -> f n) in
      let max_incoming_connections =
        Option.map max_incoming_connections ~f:(fun f -> f n)
      in
      let ((addr, port), other_points) = List.select n points in
      detach_node
        ?prefix
        ?p2p_versions
        ?timeout
        ?min_connections
        ?max_connections
        ?max_incoming_connections
        ?msg_config
        (run_node n)
        (trusted n points)
        other_points
        addr
        port)
    (0 -- (clients - 1))
  >>= fun nodes ->
  Lwt.ignore_result (sync_nodes nodes) ;
  Process.wait_all nodes

type error += Connect | Write | Read

module Simple = struct
  let rec connect ~timeout connect_handler pool point =
    lwt_log_info "Connect to %a" P2p_point.Id.pp point
    >>= fun () ->
    P2p_connect_handler.connect connect_handler point ~timeout
    >>= function
    | Error (Tezos_p2p_services.P2p_errors.Connected :: _) -> (
      match P2p_pool.Connection.find_by_point pool point with
      | Some conn ->
          return conn
      | None ->
          failwith "Woops..." )
    | Error
        (( ( Tezos_p2p_services.P2p_errors.Connection_refused
           | Tezos_p2p_services.P2p_errors.Pending_connection
           | Tezos_p2p_services.P2p_errors.Rejected_socket_connection
           | Tezos_p2p_services.P2p_errors.Rejected_by_nack _
           | Canceled
           | Timeout
           | Tezos_p2p_services.P2p_errors.Rejected _ ) as head_err )
        :: _) ->
        lwt_log_info
          "Connection to %a failed (%a)@."
          P2p_point.Id.pp
          point
          (fun ppf err ->
            match err with
            | Tezos_p2p_services.P2p_errors.Connection_refused ->
                Format.fprintf ppf "connection refused"
            | Tezos_p2p_services.P2p_errors.Pending_connection ->
                Format.fprintf ppf "pending connection"
            | Tezos_p2p_services.P2p_errors.Rejected_socket_connection ->
                Format.fprintf ppf "rejected"
            | Tezos_p2p_services.P2p_errors.Rejected_by_nack
                {alternative_points = Some alternative_points; _} ->
                Format.fprintf
                  ppf
                  "rejected (nack_v1, peer list: @[<h>%a@])"
                  P2p_point.Id.pp_list
                  alternative_points
            | Tezos_p2p_services.P2p_errors.Rejected_by_nack
                {alternative_points = None; _} ->
                Format.fprintf ppf "rejected (nack_v0)"
            | Canceled ->
                Format.fprintf ppf "canceled"
            | Timeout ->
                Format.fprintf ppf "timeout"
            | Tezos_p2p_services.P2p_errors.Rejected {peer; motive} ->
                Format.fprintf
                  ppf
                  "rejected (%a) motive:%a"
                  P2p_peer.Id.pp
                  peer
                  P2p_rejection.pp
                  motive
            | _ ->
                assert false)
          head_err
        >>= fun () ->
        Lwt_unix.sleep (0.5 +. Random.float 2.)
        >>= fun () -> connect ~timeout connect_handler pool point
    | (Ok _ | Error _) as res ->
        Lwt.return res

  let connect_all ~timeout connect_handler pool points =
    map_p (connect ~timeout connect_handler pool) points

  let write_all conns msg =
    iter_p (fun conn -> trace Write @@ P2p_conn.write_sync conn msg) conns

  let read_all conns =
    iter_p
      (fun conn ->
        trace Read @@ P2p_conn.read conn >>=? fun Ping -> return_unit)
      conns

  let close_all conns = Lwt_list.iter_p P2p_conn.disconnect conns

  let node iteration channel _stream connect_handler pool points _ =
    connect_all
      ~timeout:(Time.System.Span.of_seconds_exn 2.)
      connect_handler
      pool
      points
    >>=? fun conns ->
    lwt_log_info "Bootstrap OK@."
    >>= fun () ->
    sync iteration channel
    >>=? fun () ->
    write_all conns Ping
    >>=? fun () ->
    lwt_log_info "Sent all messages.@."
    >>= fun () ->
    sync iteration channel
    >>=? fun () ->
    read_all conns
    >>=? fun () ->
    lwt_log_info "Read all messages.@."
    >>= fun () ->
    sync iteration channel
    >>=? fun () ->
    close_all conns
    >>= fun () ->
    lwt_log_info "All connections successfully closed.@."
    >>= fun () -> return_unit

  let run points = detach_nodes (fun _ -> node) points
end

module Random_connections = struct
  let rec connect_random connect_handler pool total rem point n =
    Lwt_unix.sleep (0.2 +. Random.float 1.0)
    >>= fun () ->
    trace Connect
    @@ Simple.connect
         ~timeout:(Time.System.Span.of_seconds_exn 2.)
         connect_handler
         pool
         point
    >>=? fun conn ->
    trace Write @@ P2p_conn.write conn Ping
    >>= fun _ ->
    trace Read @@ P2p_conn.read conn
    >>=? fun Ping ->
    Lwt_unix.sleep (0.2 +. Random.float 1.0)
    >>= fun () ->
    P2p_conn.disconnect conn
    >>= fun () ->
    ( decr rem ;
      if !rem mod total = 0 then lwt_log_info "Remaining: %d.@." (!rem / total)
      else Lwt.return_unit )
    >>= fun () ->
    if n > 1 then connect_random connect_handler pool total rem point (pred n)
    else return_unit

  let connect_random_all connect_handler pool points n =
    let total = List.length points in
    let rem = ref (n * total) in
    iter_p
      (fun point -> connect_random connect_handler pool total rem point n)
      points

  let node repeat _channel _stream connect_handler pool points _ =
    lwt_log_info "Begin random connections.@."
    >>= fun () ->
    connect_random_all connect_handler pool points repeat
    >>=? fun () ->
    lwt_log_info "Random connections OK.@." >>= fun () -> return_unit

  let run points repeat = detach_nodes (fun _ _ -> node repeat) points
end

module Garbled = struct
  let is_connection_closed = function
    | Error
        ((Write | Read)
        :: Tezos_p2p_services.P2p_errors.Connection_closed :: _) ->
        true
    | Ok _ ->
        false
    | Error err ->
        log_info "Unexpected error: %a@." pp_print_error err ;
        false

  let write_bad_all conns =
    let bad_msg = Bytes.of_string (String.make 16 '\000') in
    iter_p
      (fun conn -> trace Write @@ P2p_conn.raw_write_sync conn bad_msg)
      conns

  let node iteration ch _stream connect_handler pool points _ =
    Simple.connect_all
      ~timeout:(Time.System.Span.of_seconds_exn 2.)
      connect_handler
      pool
      points
    >>=? fun conns ->
    sync iteration ch
    >>=? fun () ->
    write_bad_all conns
    >>=? (fun () -> Simple.read_all conns)
    >>= fun err -> _assert (is_connection_closed err) __LOC__ ""

  let run points = detach_nodes (fun _ -> node) points
end

module Overcrowded = struct
  type error += Advertisement_failure of P2p_point.Id.t list

  let () =
    register_error_kind
      `Permanent
      ~id:"test_p2p_pool.Overcrowded.advertisement_failure"
      ~title:"Advertisement Failure"
      ~description:"The given list of points should be known, but are not."
      ~pp:(fun ppf lst ->
        Format.fprintf
          ppf
          "The given list of points should be known, but are not : %a"
          P2p_point.Id.pp_list
          lst)
      Data_encoding.(obj1 (req "value" (list P2p_point.Id.encoding)))
      (function Advertisement_failure l -> Some l | _ -> None)
      (fun l -> Advertisement_failure l)

  let rec connect ?iter_count ~timeout connect_handler pool point =
    lwt_log_info
      "Connect%a to %a@."
      (Option.pp ~default:"" (fun ppf ->
           Format.pp_print_string ppf " to peer " ;
           Format.pp_print_int ppf))
      iter_count
      P2p_point.Id.pp
      point
    >>= fun () ->
    P2p_connect_handler.connect connect_handler point ~timeout
    >>= function
    | Error [Tezos_p2p_services.P2p_errors.Connected] -> (
      match P2p_pool.Connection.find_by_point pool point with
      | Some conn ->
          return conn
      | None ->
          failwith "Woops..." )
    | Error
        [ ( ( Tezos_p2p_services.P2p_errors.Connection_refused
            | Tezos_p2p_services.P2p_errors.Pending_connection
            | Tezos_p2p_services.P2p_errors.Rejected_socket_connection
            | Canceled
            | Timeout
            | Tezos_p2p_services.P2p_errors.Rejected _ ) as err ) ] ->
        lwt_log_info
          "Connection to%a %a failed (%a)@."
          (Option.pp ~default:"" (fun ppf ->
               Format.pp_print_string ppf " peer " ;
               Format.pp_print_int ppf))
          iter_count
          P2p_point.Id.pp
          point
          (fun ppf err ->
            match err with
            | Tezos_p2p_services.P2p_errors.Connection_refused ->
                Format.fprintf ppf "connection refused"
            | Tezos_p2p_services.P2p_errors.Pending_connection ->
                Format.fprintf ppf "pending connection"
            | Tezos_p2p_services.P2p_errors.Rejected_socket_connection ->
                Format.fprintf ppf "rejected"
            | Canceled ->
                Format.fprintf ppf "canceled"
            | Timeout ->
                Format.fprintf ppf "timeout"
            | Tezos_p2p_services.P2p_errors.Rejected {peer; motive} ->
                Format.fprintf
                  ppf
                  "rejected (%a) motive:%a"
                  P2p_peer.Id.pp
                  peer
                  P2p_rejection.pp
                  motive
            | _ ->
                assert false)
          err
        >>= fun () ->
        Lwt_unix.sleep (0.5 +. Random.float 2.)
        >>= fun () -> connect ~timeout connect_handler pool point
    | (Ok _ | Error _) as res ->
        Lwt.return res

  (** Node code of nodes that will connect to the target,
      and either get a list of pairs or have an established connection.
  *)
  let client_connect connect_handler pool legacy trusted_points all_points =
    debug
      "@[<v 2>client connects to %a in the universe @[%a@]@]@."
      P2p_point.Id.pp_list
      trusted_points
      P2p_point.Id.pp_list
      all_points ;
    let port =
      Option.unopt
        ~default:0
        (P2p_connect_handler.config connect_handler).listening_port
    in
    let target = List.hd trusted_points in
    connect
      ~iter_count:0
      ~timeout:(Time.System.Span.of_seconds_exn 2.)
      connect_handler
      pool
      target
    >>= function
    | Ok conn ->
        lwt_log_info
          "Not good: connection accepted while it should be rejected (local: \
           %d, remote: %d).@."
          port
          (snd target)
        >>= fun () -> P2p_conn.disconnect conn >>= fun () -> return_unit
    | Error
        [ Tezos_p2p_services.P2p_errors.Rejected_by_nack
            {alternative_points = None; _} ] as err ->
        if legacy then
          lwt_log_info
            "Good: client is rejected without point list (local: %d, remote: \
             %d)@."
            port
            (snd target)
          >>= fun () -> return_unit
        else
          lwt_log_info
            "Not good: client is rejected without point list (local: %d, \
             remote: %d)@."
            port
            (snd target)
          >>= fun () -> Lwt.return err
    | Error
        [ Tezos_p2p_services.P2p_errors.Rejected_by_nack
            {alternative_points = Some alternative_points; _} ] ->
        lwt_log_info
          "Good: client is rejected with point list (local: %d, remote: %d) \
           @[%a@]@."
          port
          (snd target)
          P2p_point.Id.pp_list
          alternative_points
        >>= fun () -> return_unit
    | Error _ as res ->
        Lwt.return res

  let client_knowledge pool all_points =
    let (unknowns, known) =
      P2p_pool.Points.fold_known
        pool
        ~init:(all_points, [])
        ~f:(fun id _ (unknown_points, known) ->
          let unknown_points =
            List.filter
              (fun unk_id -> not (P2p_point.Id.equal id unk_id))
              unknown_points
          in
          (unknown_points, id :: known))
    in
    (unknowns, known)

  let client_check pool all_points legacy =
    let (unknowns, _known) = client_knowledge pool all_points in
    let advert_succeed = unknowns = [] in
    if legacy || advert_succeed then
      log_info
        "Good: Advertisement%s worked as intended.@."
        (if legacy then " legacy" else "")
    else
      log_info
        "@[<v 2>Not Good: advertisement failure. legacy %b. unkowns :  @[%a@]\n\
         \t knowns : @[%a@].@."
        legacy
        P2p_point.Id.pp_list
        unknowns
        P2p_point.Id.pp_list
        _known ;
    fail_unless
      (if legacy then not advert_succeed else advert_succeed)
      (Advertisement_failure unknowns)

  let client legacy iteration channel _stream connect_handler pool
      trusted_points all_points =
    if List.length all_points > 50 then (
      log_error
        "This test only works for less clients than the advertisement list \
         length (50)@." ;
      assert false ) ;
    (*   *)
    (* first connection: let advertise us as public nodes *)
    client_connect connect_handler pool legacy trusted_points all_points
    >>=? fun () ->
    sync iteration channel
    >>=? fun () ->
    (* sync 2 *)
    client_connect connect_handler pool legacy trusted_points all_points
    >>=? fun () ->
    sync iteration channel
    >>=? fun () ->
    (* sync 3 *)
    client_check pool all_points legacy
    >>=? fun () ->
    sync iteration channel
    >>=? fun () ->
    (* sync 4 *)
    lwt_log_info "client closing.@." >>= fun () -> return_unit

  (** Code of the target that should be overcrowded by all the clients. *)
  let target iteration channel stream connect_handler pool trusted_points
      all_points =
    let _trusted = trusted_points
    and _all_points = all_points
    and _pool = pool
    and _connect_handler = connect_handler in
    let unknowns_knowns () =
      P2p_pool.Points.fold_known
        _pool
        ~init:(all_points, [])
        ~f:(fun id _ (unknown_points, knowns) ->
          let unknown_points =
            List.filter
              (fun unk_id -> not (P2p_point.Id.equal id unk_id))
              unknown_points
          in
          (unknown_points, id :: knowns))
    in
    let (unknowns, knowns) = unknowns_knowns () in
    let (watcher, stopper) = stream in
    lwt_debug "trusted : %a" P2p_point.Id.pp_list trusted_points
    >>= fun () ->
    lwt_debug "unknown : %a" P2p_point.Id.pp_list unknowns
    >>= fun () ->
    lwt_debug "known : %a" P2p_point.Id.pp_list knowns
    >>= fun () ->
    let _pool_watcher =
      Lwt.return_unit
      >>= fun () ->
      Lwt_stream.iter
        (debug "p2p event %a" P2p_connection.P2p_event.pp)
        watcher
    in
    lwt_log_info "Target waiting@."
    >>= fun () ->
    sync iteration channel
    >>=? fun () ->
    (* sync 2 *)
    sync iteration channel
    >>=? fun () ->
    (* sync 3 *)
    sync iteration channel
    >>=? fun () ->
    (* sync 4 *)
    Lwt_watcher.shutdown stopper ;
    lwt_log_info "Target closing.@." >>= fun () -> return_unit

  let node i = if i = 0 then target else client false

  let node_mixed i = if i = 0 then target else client (i mod 2 = 1)

  let trusted i points = if i = 0 then points else [List.hd points]

  (** Running the target and the clients.
      All clients should have their pool populated with the list of points. *)
  let run points =
    (* setting connections to -1 ensure there will be no random
       acceptance of connections *)
    let prefix = function 0 -> "Target_" | _ -> "Client_"
    and min_connections = function 0 -> -1 | _ -> 1
    and max_connections = function 0 -> -1 | _ -> 1
    and max_incoming_connections = function _ -> List.length points in
    detach_nodes
      ~prefix
      ~timeout:10.
      ~min_connections
      ~max_connections
      ~max_incoming_connections
      node
      points
      ~trusted

  let run_mixed_versions points =
    let prefix = function
      | 0 ->
          "Target_"
      | i ->
          if i mod 2 = 1 then "Client_v0_" else "Client_v1_"
    and min_connections = function 0 -> -1 | _ -> 1
    and max_connections = function 0 -> -1 | _ -> 1
    and max_incoming_connections = function _ -> List.length points
    and p2p_versions i =
      if i mod 2 = 1 then [P2p_version.zero]
      else [P2p_version.zero; P2p_version.one]
    in
    detach_nodes
      ~prefix
      ~p2p_versions
      ~timeout:10.
      ~min_connections
      ~max_connections
      ~max_incoming_connections
      node_mixed
      points
      ~trusted
end

module No_common_network = struct
  let rec connect ?iter_count ~timeout connect_handler pool point =
    lwt_log_info
      "Connect%a to @[%a@]@."
      (Option.pp ~default:"" (fun ppf ->
           Format.pp_print_string ppf " to peer " ;
           Format.pp_print_int ppf))
      iter_count
      P2p_point.Id.pp
      point
    >>= fun () ->
    P2p_connect_handler.connect connect_handler point ~timeout
    >>= function
    | Error [Tezos_p2p_services.P2p_errors.Connected] -> (
      match P2p_pool.Connection.find_by_point pool point with
      | Some conn ->
          return conn
      | None ->
          failwith "Woops..." )
    | Error
        [ ( ( Tezos_p2p_services.P2p_errors.Connection_refused
            | Tezos_p2p_services.P2p_errors.Pending_connection
            | Tezos_p2p_services.P2p_errors.Rejected_socket_connection
            | Canceled
            | Timeout
            | Tezos_p2p_services.P2p_errors.Rejected _ ) as err ) ] ->
        lwt_log_info
          "Connection to%a %a failed (%a)@."
          (Option.pp ~default:"" (fun ppf ->
               Format.pp_print_string ppf " peer " ;
               Format.pp_print_int ppf))
          iter_count
          P2p_point.Id.pp
          point
          (fun ppf err ->
            match err with
            | Tezos_p2p_services.P2p_errors.Connection_refused ->
                Format.fprintf ppf "connection refused"
            | Tezos_p2p_services.P2p_errors.Pending_connection ->
                Format.fprintf ppf "pending connection"
            | Tezos_p2p_services.P2p_errors.Rejected_socket_connection ->
                Format.fprintf ppf "rejected"
            | Canceled ->
                Format.fprintf ppf "canceled"
            | Timeout ->
                Format.fprintf ppf "timeout"
            | Tezos_p2p_services.P2p_errors.Rejected {peer; motive} ->
                Format.fprintf
                  ppf
                  "rejected (%a) motive:%a"
                  P2p_peer.Id.pp
                  peer
                  P2p_rejection.pp
                  motive
            | _ ->
                assert false)
          err
        >>= fun () ->
        Lwt_unix.sleep (0.5 +. Random.float 2.)
        >>= fun () -> connect ~timeout connect_handler pool point
    | (Ok _ | Error _) as res ->
        Lwt.return res

  (** Node code of nodes that will connect to the target,
      and either get a list of pairs or have an established connection.
  *)
  let client_connect connect_handler pool trusted_points all_points =
    debug
      "@[<v 2>client connects to %a in the universe @[%a@]@]@."
      P2p_point.Id.pp_list
      trusted_points
      P2p_point.Id.pp_list
      all_points ;
    connect
      ~iter_count:0
      ~timeout:(Time.System.Span.of_seconds_exn 2.)
      connect_handler
      pool
      (List.hd trusted_points)
    >>= function
    | Ok conn ->
        lwt_log_info
          "Not good: connection accepted while it should be rejected.@."
        >>= fun () -> P2p_conn.disconnect conn >>= fun () -> return_unit
    | Error
        [Tezos_p2p_services.P2p_errors.Rejected_no_common_protocol {announced}]
      ->
        lwt_log_info
          "Good: Connection cannot be established,no common network with \
           @[%a@].@."
          Network_version.pp
          announced
        >>= fun () -> return_unit
    | Error _ as res ->
        Lwt.return res

  let client iteration channel _stream connect_handler pool trusted_points
      all_points =
    client_connect connect_handler pool trusted_points all_points
    >>=? fun () ->
    sync iteration channel
    >>=? fun () ->
    (* sync 2 *)
    lwt_log_info "client closing.@." >>= fun () -> return_unit

  (** Code of the target that should be overcrowded by all the clients. *)
  let target iteration channel _stream connect_handler pool trusted_points
      all_points =
    let _trusted = trusted_points
    and _all_points = all_points
    and _pool = pool
    and _connect_handler = connect_handler in
    lwt_log_info "Target waiting.@."
    >>= fun () ->
    sync iteration channel
    >>=? fun () ->
    (* sync 2 *)
    lwt_log_info "Target closing.@." >>= fun () -> return_unit

  let node i = if i = 0 then target else client

  let trusted i points = if i = 0 then points else [List.hd points]

  (** Running the target and the clients.
      All clients should have their pool populated with the list of points.

  *)
  let run points =
    let point_count = List.length points in
    let min_connections = function _ -> 0
    and max_connections = function 0 -> point_count | _ -> 1
    and max_incoming_connections = function _ -> point_count
    and p2p_versions = function
      | 0 ->
          [P2p_version.zero]
      | _ ->
          [P2p_version.one]
    in
    detach_nodes
      ~timeout:10.
      ~min_connections
      ~max_connections
      ~max_incoming_connections
      ~p2p_versions
      node
      points
      ~trusted
end

let () = Random.self_init ()

let addr = ref Ipaddr.V6.localhost

let port = ref (1024 + Random.int 8192)

let clients = ref 10

let repeat_connections = ref 5

let log_config = ref None

let spec =
  Arg.
    [ ("--port", Int (fun p -> port := p), " Listening port of the first peer.");
      ( "--addr",
        String (fun p -> addr := Ipaddr.V6.of_string_exn p),
        " Listening addr" );
      ("--clients", Set_int clients, " Number of concurrent clients.");
      ( "--repeat",
        Set_int repeat_connections,
        " Number of connections/disconnections." );
      ( "-v",
        Unit
          (fun () ->
            log_config :=
              Some
                (Lwt_log_sink_unix.create_cfg
                   ~rules:
                     "test.p2p.connection-pool -> info; p2p.connection-pool \
                      -> info"
                   ())),
        " Log up to info msgs" );
      ( "-vv",
        Unit
          (fun () ->
            log_config :=
              Some
                (Lwt_log_sink_unix.create_cfg
                   ~rules:
                     "test.p2p.connection-pool -> debug; p2p.connection-pool \
                      -> debug"
                   ())),
        " Log up to debug msgs" );
      ( "-vvv",
        Unit
          (fun () ->
            log_config :=
              Some
                (Lwt_log_sink_unix.create_cfg
                   ~rules:
                     "test.p2p.connection-pool -> debug;p2p.connection-pool \
                      -> debug;p2p.connection -> debug"
                   ())),
        " Log up to debug msgs, socket included" ) ]

let init_logs = lazy (Internal_event_unix.init ?lwt_log_sink:!log_config ())

let wrap n f =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      Lazy.force init_logs
      >>= fun () ->
      f ()
      >>= function
      | Ok () ->
          Lwt.return_unit
      | Error error ->
          Format.kasprintf Stdlib.failwith "%a" pp_print_error error)

let main () =
  let anon_fun _num_peers = raise (Arg.Bad "No anonymous argument.") in
  let usage_msg = "Usage: %s <num_peers>.\nArguments are:" in
  Arg.parse spec anon_fun usage_msg ;
  let ports = !port -- (!port + !clients - 1) in
  let points = List.map (fun port -> (!addr, port)) ports in
  Alcotest.run
    ~argv:[|""|]
    "tezos-p2p"
    [ ( "p2p-connection-pool",
        [ wrap "simple" (fun _ -> Simple.run points);
          wrap "random" (fun _ ->
              Random_connections.run points !repeat_connections);
          wrap "garbled" (fun _ -> Garbled.run points);
          wrap "overcrowded" (fun _ -> Overcrowded.run points);
          wrap "overcrowded-mixed" (fun _ ->
              Overcrowded.run_mixed_versions points);
          wrap "no-common-network-protocol" (fun _ ->
              No_common_network.run points) ] ) ]

let () =
  Sys.catch_break true ;
  try main () with _ -> ()
