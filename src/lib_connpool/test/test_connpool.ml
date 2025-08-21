(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Connection pool
    Invocation:   dune exec src/lib_connpool/test/test_connpool.exe
    Subject:      Tests for the connection pool
*)

module Server = struct
  let close_conn = function
    | Conduit_lwt_unix.TCP {fd; _}, _ ->
        Lwt_unix.shutdown fd SHUTDOWN_ALL ;
        Lwt_unix.close fd
    | _ -> Lwt.return_unit

  let make ~sw ?(port = Port.fresh ()) ?conn_closed callback =
    let stop, r = Lwt.task () in
    let server =
      Cohttp_lwt_unix.Server.make
        ?conn_closed
        ~callback:(fun conn req body ->
          let open Lwt.Syntax in
          (* Cohttp_connpool_lwt_unix does not seem to close its persistent
             connections when the server is stopped... let's do that manually.
          *)
          if Lwt.state stop <> Sleep then
            let* () = close_conn conn in
            assert false
          else callback conn req body)
        ()
    in
    let endpoint = Uri.make ~scheme:"http" ~host:"localhost" ~port () in
    let server =
      Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port)) ~stop server
    in
    Lwt_switch.add_hook (Some sw) (fun () ->
        Lwt.wakeup r () ;
        server) ;
    Lwt.return endpoint
end

let register_test ~title k =
  Test.register ~__FILE__ ~title ~tags:["connection_pool"] @@ fun () ->
  Lwt_switch.with_switch k

let warm_established_reusable_connections () =
  register_test ~title:"warm established reusable connections"
  @@ fun server_sw ->
  let open Lwt.Syntax in
  let performed_requests = ref 0 in
  let closed_connections = ref 0 in
  let* endpoint =
    Server.make
      ~sw:server_sw
      ~conn_closed:(fun _conn -> incr closed_connections)
      (fun _conn _ _ ->
        incr performed_requests ;
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"/" ())
  in

  Log.info "Endpoint %a" Uri.pp_hum endpoint ;

  let t = Octez_connpool.make ~n:2 endpoint in
  let* () = Octez_connpool.warm t in

  Check.(
    (!performed_requests = 0)
      int
      ~error_msg:
        "[warm] should not perform any request, but the server claims it have \
         received %L.") ;

  let* _ = Octez_connpool.get t "/" in

  Check.(
    (!performed_requests = 1)
      int
      ~error_msg:
        "the server should have performed %R request, but claims it have \
         received %L.") ;
  Check.(
    (!closed_connections = 0)
      int
      ~error_msg:
        "the client should have kept the connection open, but the server \
         claims it was closed.") ;

  Lwt.return_unit

let connection_close_prevents_reuse () =
  register_test ~title:"connection_close_prevents_reuse" @@ fun sw ->
  let open Lwt.Syntax in
  let performed_requests = ref 0 in
  let connection_closed_witness, u = Lwt.task () in
  let* endpoint =
    Server.make
      ~sw
      ~conn_closed:(fun _conn -> Lwt.wakeup u ())
      (fun _conn _ _ ->
        incr performed_requests ;
        Cohttp_lwt_unix.Server.respond_string
          ~headers:(Cohttp.Header.of_list [("connection", "close")])
          ~status:`OK
          ~body:"/"
          ())
  in
  let t = Octez_connpool.make ~n:1 endpoint in

  let* _ = Octez_connpool.get t "/" in

  Check.(
    (!performed_requests = 1)
      int
      ~error_msg:"Server performed %L requests but we expected %R") ;

  let* () =
    Lwt.pick
      [
        connection_closed_witness;
        (let* () = Lwt_unix.sleep 3. in
         Test.fail "Closing the connection takes too much time");
      ]
  in

  Lwt.return_unit

let server_restarts_gracefully_handled () =
  register_test ~title:"server restarts are gracefully handled by the pool"
  @@ fun _sw ->
  let open Lwt.Syntax in
  let performed_requests = ref 0 in

  let port = Port.fresh () in

  let* t =
    Lwt_switch.with_switch @@ fun sw ->
    let* endpoint =
      Server.make ~sw ~port (fun _conn _ _ ->
          incr performed_requests ;
          Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"answered" ())
    in
    let t = Octez_connpool.make ~n:2 endpoint in
    let+ () = Octez_connpool.warm t in
    t
  in
  Log.info
    "Trying to make a request that should fail because the server is down" ;
  (* The server was closed by the switch, this should fail *)
  let* request_result = Octez_connpool.get t "/" in

  (match request_result with
  | Ok _ -> Test.fail "Should have failed"
  | Error [Octez_connpool.Cannot_perform_http_request] ->
      Log.info "Failed with the expected error"
  | Error tz ->
      Log.info
        "Failed but not with the expected error %a"
        Error_monad.pp_print_trace
        tz) ;

  (* We restart it *)
  Lwt_switch.with_switch @@ fun sw ->
  let* _endpoint =
    Server.make ~sw ~port (fun _conn _ _ ->
        incr performed_requests ;
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"/" ())
  in

  (* and try to make a request *)
  let* request_result = Octez_connpool.get t "/" in
  let resp =
    match request_result with
    | Ok (resp, _) -> resp
    | Error tztrace ->
        Test.fail "Request failed with %a" Error_monad.pp_print_trace tztrace
  in
  assert (Cohttp.Response.status resp = `OK) ;

  Check.(
    (!performed_requests = 1)
      int
      ~error_msg:"the server performed %L requests, but expected %R") ;
  Lwt.return_unit

let () =
  warm_established_reusable_connections () ;
  connection_close_prevents_reuse () ;
  server_restarts_gracefully_handled ()
