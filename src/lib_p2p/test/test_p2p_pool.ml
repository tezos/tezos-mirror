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

include Internal_event.Legacy_logging.Make (struct
  let name = "test.p2p.connection-pool"
end)

type message = Ping

let msg_config : message P2p_connect_handler.message_config =
  {
    encoding =
      [ P2p_message.Encoding
          {
            tag = 0x10;
            title = "Ping";
            encoding = Data_encoding.empty;
            wrap = (function () -> Ping);
            unwrap = (function Ping -> Some ());
            max_length = None;
          } ];
    chain_name = Distributed_db_version.sandboxed_chain_name;
    distributed_db_versions = [Distributed_db_version.zero];
  }

type metadata = unit

let peer_meta_config : metadata P2p_pool.peer_meta_config =
  {
    peer_meta_encoding = Data_encoding.empty;
    peer_meta_initial = (fun _ -> ());
    score = (fun () -> 0.);
  }

let conn_meta_config : metadata P2p_socket.metadata_config =
  {
    conn_meta_encoding = Data_encoding.empty;
    conn_meta_value = (fun _ -> ());
    private_node = (fun _ -> false);
  }

let sync ch =
  Process.Channel.push ch ()
  >>=? fun () -> Process.Channel.pop ch >>=? fun () -> return_unit

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

let detach_node f points n =
  let ((addr, port), points) = List.select n points in
  let proof_of_work_target = Crypto_box.make_target 0. in
  let identity = P2p_identity.generate proof_of_work_target in
  let private_mode = true in
  let nb_points = List.length points in
  let connect_handler_cfg =
    P2p_connect_handler.
      {
        identity;
        proof_of_work_target;
        listening_port = Some port;
        private_mode;
        greylisting_config = P2p_point_state.Info.default_greylisting_config;
        min_connections = nb_points;
        max_connections = nb_points;
        max_incoming_connections = nb_points;
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
        trusted_points = points;
        peers_file = "/dev/null";
        private_mode;
        max_known_points = None;
        max_known_peer_ids = None;
      }
  in
  (* swap_linger = Time.System.Span.of_seconds_exn 0. ; *)
  Process.detach
    ~prefix:(Format.asprintf "%a: " P2p_peer.Id.pp_short identity.peer_id)
    (fun channel ->
      let sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
      let triggers = P2p_trigger.create () in
      let log _ = () in
      P2p_pool.create pool_config peer_meta_config ~log triggers
      >>= fun pool ->
      let answerer = lazy (P2p_protocol.create_private ()) in
      let connect_handler =
        P2p_connect_handler.create
          connect_handler_cfg
          pool
          msg_config
          conn_meta_config
          sched
          triggers
          ~log
          ~answerer
      in
      P2p_welcome.create ~backlog:10 connect_handler ~addr port
      >>= fun welcome ->
      P2p_welcome.activate welcome ;
      lwt_log_info "Node ready (port: %d)" port
      >>= fun () ->
      sync channel
      >>=? fun () ->
      f channel connect_handler pool points
      >>=? fun () ->
      lwt_log_info "Shutting down..."
      >>= fun () ->
      P2p_welcome.shutdown welcome
      >>= fun () ->
      P2p_pool.destroy pool
      >>= fun () ->
      P2p_connect_handler.destroy connect_handler
      >>= fun () ->
      P2p_io_scheduler.shutdown sched
      >>= fun () -> lwt_log_info "Bye." >>= fun () -> return_unit)

let detach_nodes run_node points =
  let clients = List.length points in
  Lwt_list.map_p (detach_node run_node points) (0 -- (clients - 1))
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
    | Error (P2p_errors.Connected :: _) -> (
      match P2p_pool.Connection.find_by_point pool point with
      | Some conn ->
          return conn
      | None ->
          failwith "Woops..." )
    | Error
        (( ( P2p_errors.Connection_refused
           | P2p_errors.Pending_connection
           | P2p_errors.Rejected_socket_connection
           | Canceled
           | Timeout
           | P2p_errors.Rejected _ ) as head_err )
        :: _) ->
        lwt_log_info
          "Connection to %a failed (%a)"
          P2p_point.Id.pp
          point
          (fun ppf err ->
            match err with
            | P2p_errors.Connection_refused ->
                Format.fprintf ppf "connection refused"
            | P2p_errors.Pending_connection ->
                Format.fprintf ppf "pending connection"
            | P2p_errors.Rejected_socket_connection ->
                Format.fprintf ppf "rejected"
            | Canceled ->
                Format.fprintf ppf "canceled"
            | Timeout ->
                Format.fprintf ppf "timeout"
            | P2p_errors.Rejected peer ->
                Format.fprintf ppf "rejected (%a)" P2p_peer.Id.pp peer
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

  let node channel connect_handler pool points =
    connect_all
      ~timeout:(Time.System.Span.of_seconds_exn 2.)
      connect_handler
      pool
      points
    >>=? fun conns ->
    lwt_log_info "Bootstrap OK"
    >>= fun () ->
    sync channel
    >>=? fun () ->
    write_all conns Ping
    >>=? fun () ->
    lwt_log_info "Sent all messages."
    >>= fun () ->
    sync channel
    >>=? fun () ->
    read_all conns
    >>=? fun () ->
    lwt_log_info "Read all messages."
    >>= fun () ->
    sync channel
    >>=? fun () ->
    close_all conns
    >>= fun () ->
    lwt_log_info "All connections successfully closed."
    >>= fun () -> return_unit

  let run points = detach_nodes node points
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
      if !rem mod total = 0 then lwt_log_info "Remaining: %d." (!rem / total)
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

  let node repeat _channel connect_handler pool points =
    lwt_log_info "Begin random connections."
    >>= fun () ->
    connect_random_all connect_handler pool points repeat
    >>=? fun () ->
    lwt_log_info "Random connections OK." >>= fun () -> return_unit

  let run points repeat = detach_nodes (node repeat) points
end

module Garbled = struct
  let is_connection_closed = function
    | Error ((Write | Read) :: P2p_errors.Connection_closed :: _) ->
        true
    | Ok _ ->
        false
    | Error err ->
        log_info "Unexpected error: %a" pp_print_error err ;
        false

  let write_bad_all conns =
    let bad_msg = Bytes.of_string (String.make 16 '\000') in
    iter_p
      (fun conn -> trace Write @@ P2p_conn.raw_write_sync conn bad_msg)
      conns

  let node ch connect_handler pool points =
    Simple.connect_all
      ~timeout:(Time.System.Span.of_seconds_exn 2.)
      connect_handler
      pool
      points
    >>=? fun conns ->
    sync ch
    >>=? fun () ->
    write_bad_all conns
    >>=? (fun () -> Simple.read_all conns)
    >>= fun err -> _assert (is_connection_closed err) __LOC__ ""

  let run points = detach_nodes node points
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
        " Log up to debug msgs" ) ]

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
          Format.kasprintf Pervasives.failwith "%a" pp_print_error error)

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
          wrap "garbled" (fun _ -> Garbled.run points) ] ) ]

let () =
  Sys.catch_break true ;
  try main () with _ -> ()
