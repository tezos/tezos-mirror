module type TEST = sig
  val name : string

  val run : unit -> unit tzresult Lwt.t
end

let canceler = Lwt_canceler.create () (* unused *)

module Authentication = struct
  open P2p_test_utils

  let name = "authentication"

  let expected_section =
    Internal_event.Section.make_sanitized ["p2p"; "connection"]

  let auth_event =
    Mock_sink.Pattern.
      {
        level = Some Internal_event.Debug;
        section =
          Some (Some (Internal_event.Section.make_sanitized ["p2p"; "socket"]));
        name = "sending_authentication";
      }

  let close_event =
    Mock_sink.Pattern.
      {
        level = Some Internal_event.Debug;
        section =
          Some (Some (Internal_event.Section.make_sanitized ["p2p"; "socket"]));
        name = "connection_closed";
      }

  let encoding =
    let open Data_encoding in
    dynamic_size @@ option @@ string

  let server _ch sched socket =
    let open Lwt_result_syntax in
    let* _info, auth_fd = accept sched socket in
    let* conn = P2p_socket.accept ~canceler auth_fd encoding in
    let*! () = P2p_socket.close conn in
    Mock_sink.assert_has_event
      ~strict:false
      "authentication should be sent"
      auth_event ;
    Mock_sink.assert_has_event
      ~strict:false
      "connection should be closed"
      close_event ;
    return_unit

  let client _ch sched addr port =
    let open Lwt_result_syntax in
    let*! id2 in
    let* _, auth_fd = connect sched addr port id2 in
    let* conn = P2p_socket.accept ~canceler auth_fd encoding in
    let*! () = P2p_socket.close conn in
    Mock_sink.assert_has_event
      ~strict:false
      "authentication should be sent"
      auth_event ;
    Mock_sink.assert_has_event
      ~strict:false
      "connection should be closed"
      close_event ;
    return_unit

  let run _dir = run_nodes client server
end

module Nack = struct
  open P2p_test_utils

  let name = "nack"

  let expected_section =
    Internal_event.Section.make_sanitized ["p2p"; "connection"]

  let nack_event =
    Mock_sink.Pattern.
      {
        level = Some Internal_event.Debug;
        section =
          Some (Some (Internal_event.Section.make_sanitized ["p2p"; "socket"]));
        name = "nack_point_no_point";
      }

  let server ch sched socket =
    let open Lwt_result_syntax in
    let* _info, auth_fd = accept sched socket in
    let*! () = P2p_socket.nack auth_fd P2p_rejection.No_motive [] in
    Mock_sink.assert_has_event
      ~strict:false
      "nack point without list should have happened"
      nack_event ;
    sync ch

  let client ch sched addr port =
    let open Lwt_result_syntax in
    let*! id2 in
    let* _, auth_fd = connect sched addr port id2 in
    let*! _conn = P2p_socket.accept ~canceler auth_fd Data_encoding.bytes in
    sync ch

  let run _ = run_nodes client server
end

module Read_and_write = struct
  open P2p_test_utils

  let name = "read_and_write"

  let expected_section = Internal_event.Section.make_sanitized ["p2p"; "socket"]

  let read_event =
    Mock_sink.Pattern.
      {
        name = "socket_read";
        level = Some Debug;
        section = Some (Some expected_section);
      }

  let write_event =
    Mock_sink.Pattern.
      {
        name = "socket_write";
        level = Some Debug;
        section = Some (Some expected_section);
      }

  let message_event =
    Mock_sink.Pattern.
      {
        name = "socket_send_message";
        level = Some Debug;
        section = Some (Some expected_section);
      }

  let server ch sched socket =
    let open Lwt_result_syntax in
    let* _info, auth_fd = accept sched socket in
    let* conn = P2p_socket.accept ~canceler auth_fd Data_encoding.bytes in
    let* () =
      P2p_socket.write_sync conn @@ Bytes.of_string "a polite greeting"
    in
    let* _msg_size, _msg = P2p_socket.read conn in
    let* () = sync ch in
    let*! () = P2p_socket.close conn in
    Mock_sink.assert_has_event
      ~strict:false
      "socket should have been read"
      read_event ;
    Mock_sink.assert_has_event
      ~strict:false
      "socket should have been written to"
      write_event ;
    Mock_sink.assert_has_event
      ~strict:false
      "message should have been sent"
      message_event ;
    return_unit

  let client ch sched addr port =
    let open Lwt_result_syntax in
    let*! id2 in
    let* _, auth_fd = connect sched addr port id2 in
    let* conn = P2p_socket.accept ~canceler auth_fd Data_encoding.bytes in
    let* () =
      P2p_socket.write_sync conn @@ Bytes.of_string "a polite request"
    in
    let* _msg_size, _msg = P2p_socket.read conn in
    let* () = sync ch in
    let*! _stat = P2p_socket.close conn in
    Mock_sink.assert_has_event
      ~strict:false
      "socket should have been read"
      read_event ;
    Mock_sink.assert_has_event
      ~strict:false
      "socket should have been written to"
      write_event ;
    Mock_sink.assert_has_event
      ~strict:false
      "message should have been sent"
      message_event ;
    return_unit

  let run _dir = run_nodes client server
end

module P2p_net = struct
  let name = "activate_network"

  let conf identity =
    P2p.
      {
        listening_port = None;
        listening_addr = None;
        advertised_port = None;
        discovery_port = None;
        discovery_addr = None;
        trusted_points = [];
        peers_file = Filename.temp_file "test_p2p_logging" "peers";
        private_mode = true;
        identity;
        proof_of_work_target = P2p_test_utils.proof_of_work_target;
        trust_discovered_peers = false;
        reconnection_config = Point_reconnection_config.default;
        disable_peer_discovery = false;
      }

  let p2p_conf =
    P2p_params.
      {
        encoding =
          [
            Encoding
              {
                tag = 0;
                title = "fake-net";
                encoding = Data_encoding.string;
                wrap = Fun.id;
                unwrap = Option.some;
                max_length = Some 255;
              };
          ];
        chain_name = Distributed_db_version.Name.of_string "test-chain";
        distributed_db_versions = [Distributed_db_version.one];
      }

  let limits =
    P2p_limits.
      {
        connection_timeout = Ptime.Span.zero;
        authentication_timeout = Ptime.Span.of_int_s 60;
        (* can't be 0 *)
        greylist_timeout = Ptime.Span.zero;
        maintenance_idle_time = Some Ptime.Span.zero;
        min_connections = 0;
        expected_connections = 0;
        max_connections = 0;
        backlog = 0;
        max_incoming_connections = 0;
        max_download_speed = None;
        max_upload_speed = None;
        read_buffer_size = 255;
        read_queue_size = None;
        write_queue_size = None;
        incoming_app_message_queue_size = None;
        incoming_message_queue_size = None;
        outgoing_message_queue_size = None;
        max_known_peer_ids = None;
        max_known_points = None;
        peer_greylist_size = 1;
        ip_greylist_size_in_kilobytes = 64;
        ip_greylist_cleanup_delay = Ptime.Span.of_int_s 10;
        swap_linger = Ptime.Span.of_int_s 10;
        (* can't be 0 *)
        binary_chunks_size = None;
      }

  let peer_conf =
    P2p_params.
      {
        peer_meta_encoding = Data_encoding.string;
        peer_meta_initial = Fun.const "127.0.0.0";
        score = Fun.const 0.0;
      }

  let conn =
    P2p_params.
      {
        conn_meta_encoding = Data_encoding.unit;
        conn_meta_value = Fun.const ();
        private_node = Fun.const true;
      }

  let log_pattern ?(level = Internal_event.Notice) name =
    Mock_sink.Pattern.
      {
        name;
        level = Some level;
        section = Some (Some (Internal_event.Section.make_sanitized ["p2p"]));
      }

  let run () =
    let open Lwt_result_syntax in
    let*! identity = P2p_test_utils.id1 in
    let* net =
      P2p.create ~config:(conf identity) ~limits peer_conf conn p2p_conf
    in
    P2p.activate net ;
    let*! () = P2p.shutdown net in
    Mock_sink.assert_has_event
      ~strict:false
      "layer should have been activated"
      (log_pattern ~level:Info "activate_layer") ;
    Mock_sink.assert_has_event
      ~strict:false
      "network should have been activated"
      (log_pattern ~level:Info "activate_network") ;
    Mock_sink.assert_has_event
      ~strict:false
      "welcome worker should have been shut down"
      (log_pattern "shutdown_welcome_worker") ;
    Mock_sink.assert_has_event
      ~strict:false
      "maintenance worker should have been shut down"
      (log_pattern "shutdown_maintenance_worker") ;
    Mock_sink.assert_has_event
      ~strict:false
      "connection pool should have been shut down"
      (log_pattern "shutdown_connection_pool") ;
    Mock_sink.assert_has_event
      ~strict:false
      "connection handler should have been shut down"
      (log_pattern "shutdown_connection_handler") ;
    Mock_sink.assert_has_event
      ~strict:false
      "scheduler should have been shut down"
      (log_pattern "shutdown_scheduler") ;
    Lwt_result.return ()
end

let lwt_log_sink = Lwt_log_sink_unix.create_cfg ~rules:"* -> debug" ()

let testcase (module T : TEST) =
  Alcotest_lwt.test_case T.name `Quick (fun _switch () ->
      let open Lwt_syntax in
      let* () = Tezos_base_unix.Internal_event_unix.init ~lwt_log_sink () in
      Tztest.with_empty_mock_sink (fun () ->
          let* r = T.run () in
          match r with
          | Ok () -> return_unit
          | Error error ->
              Format.kasprintf Stdlib.failwith "%a" pp_print_trace error))

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       ~argv:[|""|]
       "tezos-p2p"
       [
         ( "p2p-logging",
           [
             testcase (module Authentication);
             testcase (module Nack);
             testcase (module Read_and_write);
             testcase (module P2p_net);
           ] );
       ]
