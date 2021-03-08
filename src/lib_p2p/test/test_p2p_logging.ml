open Test_services

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
    accept sched socket
    >>=? fun (_info, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd encoding
    >>=? fun conn ->
    P2p_socket.close conn
    >>= fun _stat ->
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
    id2
    >>= fun id2 ->
    connect sched addr port id2
    >>=? fun (_, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd encoding
    >>=? fun conn ->
    P2p_socket.close conn
    >>= fun _stat ->
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
    accept sched socket
    >>=? fun (_info, auth_fd) ->
    P2p_socket.nack auth_fd P2p_rejection.No_motive []
    >>= fun () ->
    Mock_sink.assert_has_event
      ~strict:false
      "nack point without list should have happened"
      nack_event ;
    sync ch

  let client ch sched addr port =
    id2
    >>= fun id2 ->
    connect sched addr port id2
    >>=? fun (_, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd Data_encoding.bytes
    >>= fun _conn -> sync ch

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
    accept sched socket
    >>=? fun (_info, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd Data_encoding.bytes
    >>=? fun conn ->
    P2p_socket.write_sync conn @@ Bytes.of_string "a polite greeting"
    >>=? fun () ->
    P2p_socket.read conn
    >>=? fun (_msg_size, _msg) ->
    sync ch
    >>=? fun () ->
    P2p_socket.close conn
    >>= fun _stat ->
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
    id2
    >>= fun id2 ->
    connect sched addr port id2
    >>=? fun (_, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd Data_encoding.bytes
    >>=? fun conn ->
    P2p_socket.write_sync conn @@ Bytes.of_string "a polite request"
    >>=? fun () ->
    P2p_socket.read conn
    >>=? fun (_msg_size, _msg) ->
    sync ch
    >>=? fun () ->
    P2p_socket.close conn
    >>= fun _stat ->
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

let lwt_log_sink = Lwt_log_sink_unix.create_cfg ~rules:"* -> debug" ()

let testcase (module T : TEST) =
  Alcotest_lwt.test_case T.name `Quick (fun _switch () ->
      Internal_event_unix.init ~lwt_log_sink ()
      >>= fun () ->
      Test_services.with_empty_mock_sink (fun () ->
          T.run ()
          >>= function
          | Ok () ->
              Lwt.return_unit
          | Error error ->
              Format.kasprintf Stdlib.failwith "%a" pp_print_error error))

let main () =
  Alcotest_lwt.run
    ~argv:[|""|]
    "tezos-p2p"
    [ ( "p2p-logging.",
        List.map
          testcase
          [(module Authentication); (module Nack); (module Read_and_write)] )
    ]

let () =
  Sys.catch_break true ;
  Lwt_main.run @@ Lwt.catch main (fun _ -> Lwt.return_unit)
