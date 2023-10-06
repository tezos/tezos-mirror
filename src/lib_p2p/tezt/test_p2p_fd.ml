(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    P2P
    Invocation:   dune exec src/lib_p2p/tezt/main.exe -- --file test_p2p_fd.ml
    Subject:      Unit tests for the [P2p_fd] module.
*)

open P2p_test_utils

let test_connect () =
  let open Lwt_result_syntax in
  let server _ch listening_fd =
    let*! r = P2p_fd.accept listening_fd in
    match r with
    | Error (`Socket_error ex | `System_error ex | `Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok (_fd, _sockaddr) -> return_unit
  in

  let client _ch addr port =
    let*! fd = P2p_fd.socket () in
    let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    let*! r = P2p_fd.connect fd uaddr in
    match r with
    | Ok () -> return_unit
    | Error `Connection_failed -> tzfail P2p_errors.Connection_failed
    | Error (`Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
  in

  Test.register
    ~__FILE__
    ~title:"p2p fd connect"
    ~tags:
      [
        "p2p";
        "fd";
        (* FIXME: https://gitlab.com/nomadic-labs/tezt/-/issues/13
           Fatal error: exception Bad file descriptor` errors
        *)
        Tag.flaky;
      ]
  @@ fun () ->
  let*! r = run_nodes_fd ~addr:Node.default_ipv6_addr client server in
  match r with
  | Ok () -> unit
  | Error e ->
      Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

let test_read_write () =
  let open Lwt_result_syntax in
  let server data _ch listening_fd =
    let*! r = P2p_fd.accept listening_fd in
    match r with
    | Error (`Socket_error ex | `System_error ex | `Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok (fd, _sockaddr) -> (
        let*! r = P2p_fd.write fd data in
        match r with
        | Error `Connection_locally_closed ->
            Error_monad.failwith "Connection_locally_closed"
        | Error `Connection_closed_by_peer ->
            Error_monad.failwith "Connection_closed_by_peer"
        | Error (`Connection_lost ex | `Unexpected_error ex) ->
            Lwt.return_error (TzTrace.make (error_of_exn ex))
        | Ok () -> return_unit)
  in

  let client data _ch addr port =
    let*! fd = P2p_fd.socket () in
    let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    let*! r = P2p_fd.connect fd uaddr in
    match r with
    | Error `Connection_failed -> tzfail P2p_errors.Connection_failed
    | Error (`Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok () -> (
        let data_length = Bytes.length data in
        let recv_data = Bytes.create data_length in
        let*! r = P2p_fd.read fd recv_data 0 data_length in
        match r with
        | Error `Connection_locally_closed ->
            Error_monad.failwith "Connection_locally_closed"
        | Error `Connection_closed_by_peer ->
            Error_monad.failwith "Connection_closed_by_peer"
        | Error (`Connection_lost ex | `Unexpected_error ex) ->
            Lwt.return_error (TzTrace.make (error_of_exn ex))
        | Ok l when l = data_length ->
            if Bytes.compare data recv_data <> 0 then
              Test.fail "data sent and recieved are different" ;
            return_unit
        | Ok nread ->
            Test.fail
              "returned number of bytes read is %d but was expected to be %d"
              nread
              (Bytes.length data))
  in

  Test.register
    ~__FILE__
    ~title:"p2p fd read write"
    ~tags:
      [
        "p2p";
        "fd";
        "read";
        "write";
        (* FIXME: https://gitlab.com/nomadic-labs/tezt/-/issues/13
           Fatal error: exception Bad file descriptor` errors
        *)
        Tag.flaky;
      ]
  @@ fun () ->
  let data = Bytes.of_string "test" in
  let*! r =
    run_nodes_fd ~addr:Node.default_ipv6_addr (client data) (server data)
  in
  match r with
  | Ok () -> unit
  | Error e ->
      Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

let test_closed_by_peer_read_outgoing () =
  let open Lwt_result_syntax in
  let server ch listening_fd =
    let*! r = P2p_fd.accept listening_fd in
    match r with
    | Error (`Socket_error ex | `System_error ex | `Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok (fd, _sockaddr) ->
        let*! () = P2p_fd.close ~reason:(User "server explicit close") fd in
        let* () = sync ch in
        return_unit
  in

  let client ch addr port =
    let*! fd = P2p_fd.socket () in
    let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    let*! r = P2p_fd.connect fd uaddr in
    match r with
    | Error `Connection_failed -> tzfail P2p_errors.Connection_failed
    | Error (`Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok () -> (
        let* () = sync ch in
        let data_length = 10 in
        let recv_data = Bytes.create data_length in
        let*! r = P2p_fd.read fd recv_data 0 data_length in
        match r with
        | Error `Connection_locally_closed ->
            Error_monad.failwith "Connection_locally_closed"
        | Error `Connection_closed_by_peer -> return_unit
        | Error (`Connection_lost ex | `Unexpected_error ex) ->
            Lwt.return_error (TzTrace.make (error_of_exn ex))
        | Ok nread -> Test.fail "%d bytes read on a closed socket" nread)
  in

  Test.register
    ~__FILE__
    ~title:"p2p fd closed by peer read outgoing"
    ~tags:
      [
        "p2p";
        "fd";
        "closed_by_peer";
        "read";
        (* FIXME: https://gitlab.com/nomadic-labs/tezt/-/issues/13
           Fatal error: exception Bad file descriptor` errors
        *)
        Tag.flaky;
      ]
  @@ fun () ->
  let*! r = run_nodes_fd ~addr:Node.default_ipv6_addr client server in
  match r with
  | Ok () -> unit
  | Error e ->
      Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

let test_closed_by_peer_read_incoming () =
  let open Lwt_result_syntax in
  let server ch listening_fd =
    let*! r = P2p_fd.accept listening_fd in
    match r with
    | Error (`Socket_error ex | `System_error ex | `Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok (fd, _sockaddr) -> (
        let* () = sync ch in
        let data_length = 10 in
        let recv_data = Bytes.create data_length in
        let*! r = P2p_fd.read fd recv_data 0 data_length in
        match r with
        | Error `Connection_locally_closed ->
            Error_monad.failwith "Connection_locally_closed"
        | Error `Connection_closed_by_peer -> return_unit
        | Error (`Connection_lost ex | `Unexpected_error ex) ->
            Lwt.return_error (TzTrace.make (error_of_exn ex))
        | Ok nread -> Test.fail "%d bytes read on a closed socket" nread)
  in

  let client ch addr port =
    let*! fd = P2p_fd.socket () in
    let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    let*! r = P2p_fd.connect fd uaddr in
    match r with
    | Error `Connection_failed -> tzfail P2p_errors.Connection_failed
    | Error (`Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok () ->
        let*! () = P2p_fd.close ~reason:(User "client explicit close") fd in
        let* () = sync ch in
        return_unit
  in

  Test.register
    ~__FILE__
    ~title:"p2p fd closed by peer read incoming"
    ~tags:
      [
        "p2p";
        "fd";
        "closed_by_peer";
        "read";
        (* FIXME: https://gitlab.com/nomadic-labs/tezt/-/issues/13
           Fatal error: exception Bad file descriptor` errors
        *)
        Tag.flaky;
      ]
  @@ fun () ->
  let*! r = run_nodes_fd ~addr:Node.default_ipv6_addr client server in
  match r with
  | Ok () -> unit
  | Error e ->
      Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

let test_locally_closed_read_outgoing () =
  let open Lwt_result_syntax in
  let server _ch listening_fd =
    let*! r = P2p_fd.accept listening_fd in
    match r with
    | Error (`Socket_error ex | `System_error ex | `Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok (_fd, _sockaddr) -> return_unit
  in

  let client _ch addr port =
    let*! fd = P2p_fd.socket () in
    let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    let*! r = P2p_fd.connect fd uaddr in
    match r with
    | Error `Connection_failed -> tzfail P2p_errors.Connection_failed
    | Error (`Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok () -> (
        let*! () = P2p_fd.close ~reason:(User "client explicit close") fd in
        let data_length = 10 in
        let recv_data = Bytes.create data_length in
        let*! r = P2p_fd.read fd recv_data 0 data_length in
        match r with
        | Error `Connection_locally_closed -> return_unit
        | Error `Connection_closed_by_peer ->
            Error_monad.failwith "Connection_closed_by_peer"
        | Error (`Connection_lost ex | `Unexpected_error ex) ->
            Lwt.return_error (TzTrace.make (error_of_exn ex))
        | Ok nread -> Test.fail "%d bytes read on a closed socket" nread)
  in

  Test.register
    ~__FILE__
    ~title:"p2p fd locally closed read outgoing"
    ~tags:
      [
        "p2p";
        "fd";
        "locally_closed";
        "read";
        (* FIXME: https://gitlab.com/nomadic-labs/tezt/-/issues/13
           Fatal error: exception Bad file descriptor` errors
        *)
        Tag.flaky;
      ]
  @@ fun () ->
  let*! r = run_nodes_fd ~addr:Node.default_ipv6_addr client server in
  match r with
  | Ok () -> unit
  | Error e ->
      Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

let test_locally_closed_read_incoming () =
  let open Lwt_result_syntax in
  let server _ch listening_fd =
    let*! r = P2p_fd.accept listening_fd in
    match r with
    | Error (`Socket_error ex | `System_error ex | `Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok (fd, _sockaddr) -> (
        let*! () = P2p_fd.close ~reason:(User "server explicit close") fd in
        let data_length = 10 in
        let recv_data = Bytes.create data_length in
        let*! r = P2p_fd.read fd recv_data 0 data_length in
        match r with
        | Error `Connection_locally_closed -> return_unit
        | Error `Connection_closed_by_peer ->
            Error_monad.failwith "Connection_closed_by_peer"
        | Error (`Connection_lost ex | `Unexpected_error ex) ->
            Lwt.return_error (TzTrace.make (error_of_exn ex))
        | Ok nread -> Test.fail "%d bytes read on a closed socket" nread)
  in

  let client _ch addr port =
    let*! fd = P2p_fd.socket () in
    let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    let*! r = P2p_fd.connect fd uaddr in
    match r with
    | Error `Connection_failed -> tzfail P2p_errors.Connection_failed
    | Error (`Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok () -> return_unit
  in

  Test.register
    ~__FILE__
    ~title:"p2p fd locally closed read incoming"
    ~tags:
      [
        "p2p";
        "fd";
        "locally_closed";
        "read";
        (* FIXME: https://gitlab.com/nomadic-labs/tezt/-/issues/13
           Fatal error: exception Bad file descriptor` errors
        *)
        Tag.flaky;
      ]
  @@ fun () ->
  let*! r = run_nodes_fd ~addr:Node.default_ipv6_addr client server in
  match r with
  | Ok () -> unit
  | Error e ->
      Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

let test_locally_closed_write_outgoing () =
  let open Lwt_result_syntax in
  let server _ch listening_fd =
    let*! r = P2p_fd.accept listening_fd in
    match r with
    | Error (`Socket_error ex | `System_error ex | `Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok (_fd, _sockaddr) -> return_unit
  in

  let client _ch addr port =
    let*! fd = P2p_fd.socket () in
    let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    let*! r = P2p_fd.connect fd uaddr in
    match r with
    | Error `Connection_failed -> tzfail P2p_errors.Connection_failed
    | Error (`Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok () -> (
        let*! () = P2p_fd.close ~reason:(User "client explicit close") fd in
        let data = Bytes.of_string "test" in
        let*! r = P2p_fd.write fd data in
        match r with
        | Error `Connection_locally_closed -> return_unit
        | Error `Connection_closed_by_peer ->
            Error_monad.failwith "Connection_closed_by_peer"
        | Error (`Connection_lost ex | `Unexpected_error ex) ->
            Lwt.return_error (TzTrace.make (error_of_exn ex))
        | Ok () -> Test.fail "data writen on a closed socket")
  in

  Test.register
    ~__FILE__
    ~title:"p2p fd locally closed write outgoing"
    ~tags:
      [
        "p2p";
        "fd";
        "locally_closed";
        "write";
        (* FIXME: https://gitlab.com/nomadic-labs/tezt/-/issues/13
           Fatal error: exception Bad file descriptor` errors
        *)
        Tag.flaky;
      ]
  @@ fun () ->
  let*! r = run_nodes_fd ~addr:Node.default_ipv6_addr client server in
  match r with
  | Ok () -> unit
  | Error e ->
      Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

let test_locally_closed_write_incoming () =
  let open Lwt_result_syntax in
  let server _ch listening_fd =
    let*! r = P2p_fd.accept listening_fd in
    match r with
    | Error (`Socket_error ex | `System_error ex | `Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok (fd, _sockaddr) -> (
        let*! () = P2p_fd.close ~reason:(User "server explicit close") fd in
        let data = Bytes.of_string "test" in
        let*! r = P2p_fd.write fd data in
        match r with
        | Error `Connection_locally_closed -> return_unit
        | Error `Connection_closed_by_peer ->
            Error_monad.failwith "Connection_closed_by_peer"
        | Error (`Connection_lost ex | `Unexpected_error ex) ->
            Lwt.return_error (TzTrace.make (error_of_exn ex))
        | Ok () -> Test.fail "data writen on a closed socket")
  in

  let client _ch addr port =
    let*! fd = P2p_fd.socket () in
    let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
    let*! r = P2p_fd.connect fd uaddr in
    match r with
    | Error `Connection_failed -> tzfail P2p_errors.Connection_failed
    | Error (`Unexpected_error ex) ->
        Lwt.return_error (TzTrace.make (error_of_exn ex))
    | Ok () -> return_unit
  in

  Test.register
    ~__FILE__
    ~title:"p2p fd locally closed write incoming"
    ~tags:
      [
        "p2p";
        "fd";
        "locally_closed";
        "write";
        (* FIXME: https://gitlab.com/nomadic-labs/tezt/-/issues/13
           Fatal error: exception Bad file descriptor` errors
        *)
        Tag.flaky;
      ]
  @@ fun () ->
  let*! r = run_nodes_fd ~addr:Node.default_ipv6_addr client server in
  match r with
  | Ok () -> unit
  | Error e ->
      Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

let () =
  test_connect () ;
  test_read_write () ;
  test_closed_by_peer_read_outgoing () ;
  test_closed_by_peer_read_incoming () ;
  test_locally_closed_read_outgoing () ;
  test_locally_closed_read_incoming () ;
  test_locally_closed_write_outgoing () ;
  test_locally_closed_write_incoming ()
