(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Invocation:   dune exec src/lib_p2p/test/test_p2p_io_scheduler.exe
    Dependencies: src/lib_p2p/test/process.ml
    Subject:      On I/O scheduling of client-server connections.
*)

exception Error of error list

let rec listen ?port addr =
  let open Lwt_result_syntax in
  let tentative_port = match port with None -> 0 | Some port -> port in
  let*! sock =
    P2p_fd.create_listening_socket ~backlog:50 ~addr tentative_port
  in
  match sock with
  | Ok sock -> return (sock, tentative_port)
  | Error
      (P2p_fd.Failed_to_open_listening_socket
         {reason = Unix.EADDRINUSE | Unix.EADDRNOTAVAIL; _}
      :: _) ->
      listen ?port addr
  | Error err -> Lwt.return_error err

let accept main_socket =
  let open Lwt_result_syntax in
  let*! r = P2p_fd.accept main_socket in
  match r with
  | Error e -> tzfail (P2p_fd.accept_error_to_tzerror e)
  | Ok (fd, _) -> return fd

let rec accept_n main_socket n =
  let open Lwt_result_syntax in
  if n <= 0 then return_nil
  else
    let* acc = accept_n main_socket (n - 1) in
    let* conn = accept main_socket in
    return (conn :: acc)

let connect addr port =
  let open Lwt_syntax in
  let* fd = P2p_fd.socket () in
  let fd =
    match fd with
    | Ok fd -> fd
    | Error _ ->
        assert
          false (* [P2p_fd.socket] cannot fail when not given a fd_semaphore. *)
  in
  let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
  let* r = P2p_fd.connect fd uaddr in
  match r with
  | Error _ -> Lwt.fail Alcotest.Test_error
  | Ok () -> Lwt.return fd

let simple_msgs =
  [|
    Bytes.create (1 lsl 6);
    Bytes.create (1 lsl 7);
    Bytes.create (1 lsl 8);
    Bytes.create (1 lsl 9);
    Bytes.create (1 lsl 10);
    Bytes.create (1 lsl 11);
    Bytes.create (1 lsl 12);
    Bytes.create (1 lsl 13);
    Bytes.create (1 lsl 14);
    Bytes.create (1 lsl 15);
    Bytes.create (1 lsl 16);
  |]

let nb_simple_msgs = Array.length simple_msgs

let receive conn =
  let open Lwt_syntax in
  let buf = Bytes.create (1 lsl 16) in
  let rec loop () =
    let open P2p_buffer_reader in
    let* r = read conn (mk_buffer_safe buf) in
    match r with
    | Ok _ -> loop ()
    | Error (Tezos_p2p_services.P2p_errors.Connection_closed :: _) ->
        return_unit
    | Error err -> Lwt.fail (Error err)
  in
  loop ()

let server ?(display_client_stat = true) ?max_download_speed ?read_queue_size
    ~read_buffer_size main_socket n =
  let open Lwt_result_syntax in
  let* sched =
    P2p_io_scheduler.create
      ?max_download_speed
      ?read_queue_size
      ~read_buffer_size
      ()
  in
  Moving_average.on_update (P2p_io_scheduler.ma_state sched) (fun () ->
      Tezt.Log.debug "Stat: %a" P2p_stat.pp (P2p_io_scheduler.global_stat sched) ;
      if display_client_stat then
        P2p_io_scheduler.iter_connection sched (fun conn ->
            Tezt.Log.debug
              " client(%d) %a"
              (P2p_io_scheduler.id conn)
              P2p_stat.pp
              (P2p_io_scheduler.stat conn))) ;
  (* Accept and read message until the connection is closed. *)
  let* conns = accept_n main_socket n in
  let conns = List.map (P2p_io_scheduler.register sched) conns in
  let*! () =
    List.iter_p receive (List.map P2p_io_scheduler.to_readable conns)
  in
  let*! r =
    List.iter_ep
      (P2p_io_scheduler.close ~reason:(User "shutdown from server"))
      conns
  in
  match r with
  | Ok () ->
      Tezt.Log.debug "OK %a" P2p_stat.pp (P2p_io_scheduler.global_stat sched) ;
      return ()
  | Error _ -> Lwt.fail Alcotest.Test_error

let max_size ?max_upload_speed () =
  match max_upload_speed with
  | None -> nb_simple_msgs
  | Some max_upload_speed ->
      let rec loop n =
        if n <= 1 then 1
        else if Bytes.length simple_msgs.(n - 1) <= max_upload_speed then n
        else loop (n - 1)
      in
      loop nb_simple_msgs

let rec send conn nb_simple_msgs =
  let open Lwt_syntax in
  let* () = Lwt.pause () in
  let msg = simple_msgs.(Random.int nb_simple_msgs) in
  let* r = P2p_io_scheduler.write conn msg in
  match r with
  | Error err -> Lwt.fail (Error err)
  | Ok () -> send conn nb_simple_msgs

let client ?max_upload_speed ?write_queue_size addr port time _n =
  let open Lwt_syntax in
  let* sched =
    P2p_io_scheduler.create
      ?max_upload_speed
      ?write_queue_size
      ~read_buffer_size:(1 lsl 12)
      ()
  in
  let* sched =
    match sched with
    | Error err -> Lwt.fail (Error err)
    | Ok sched -> return sched
  in
  let* conn = connect addr port in
  let conn = P2p_io_scheduler.register sched conn in
  let _nb_simple_msgs = max_size ?max_upload_speed () in
  let* () =
    Lwt.pick
      [
        send conn nb_simple_msgs;
        (let* () = Lwt_unix.sleep time in
         return_unit);
      ]
  in
  let* r = P2p_io_scheduler.close ~reason:(User "shutdown from client") conn in
  match r with
  | Error err -> Lwt.fail (Error err)
  | Ok () ->
      let stat = P2p_io_scheduler.stat conn in
      Tezt.Log.debug "Client OK %a" P2p_stat.pp stat ;
      return_ok ()

(** Listens to address [addr] on port [port] to open a socket [main_socket].
    Spawns a server on it, and [n] clients connecting to the server. Then,
    the server will close all connections.
*)
let run ?display_client_stat ?max_download_speed ?max_upload_speed
    ~read_buffer_size ?read_queue_size ?write_queue_size addr port time n =
  let open Lwt_result_syntax in
  let*! () = Tezos_base_unix.Internal_event_unix.init () in
  let* main_socket, port = listen ?port addr in
  let* server_node =
    Process.detach
      ~prefix:"server: "
      (fun (_ : (unit, unit) Process.Channel.t) ->
        server
          ?display_client_stat
          ?max_download_speed
          ~read_buffer_size
          ?read_queue_size
          main_socket
          n)
  in

  let client n =
    let prefix = Printf.sprintf "client(%d): " n in
    Process.detach ~prefix (fun (_ : (unit, unit) Process.Channel.t) ->
        let* () =
          Lwt.catch
            (fun () ->
              let*! () = Lwt_unix.close main_socket in
              return_unit)
            (function
              (* the connection was already closed *)
              | Unix.Unix_error (EBADF, _, _) -> return_unit
              | err -> Lwt.reraise err)
        in
        client ?max_upload_speed ?write_queue_size addr port time n)
  in
  let* client_nodes = List.map_es client (1 -- n) in
  Process.wait_all (server_node :: client_nodes)

let () = Random.self_init ()

let init_logs = lazy (Tezos_base_unix.Internal_event_unix.init ())

let wrap n f =
  Alcotest_lwt.test_case n `Quick (fun _lwt_switch () ->
      let open Lwt_syntax in
      let* () = Lazy.force init_logs in
      let* r = f () in
      match r with
      | Ok () -> return_unit
      | Error error ->
          Format.kasprintf Stdlib.failwith "%a" pp_print_trace error)

let () =
  let addr = Node.default_ipv6_addr in
  let port = Some (Tezt_tezos.Port.fresh ()) in
  let max_download_speed = 1048576 in
  let max_upload_speed = 262144 in
  (* 1 << 14 = 16kB *)
  let read_buffer_size = 1 lsl 14 in
  let read_queue_size = 1 lsl 14 in
  let write_queue_size = 1 lsl 14 in
  let delay = 5. in
  let clients = 8 in
  Lwt_main.run
  @@ Alcotest_lwt.run
       ~__FILE__
       "tezos-p2p"
       [
         ( "p2p.io-scheduler",
           [
             wrap "trivial-quota" (fun () ->
                 run
                   ~max_download_speed
                   ~max_upload_speed
                   ~read_buffer_size
                   ~read_queue_size
                   ~write_queue_size
                   addr
                   port
                   delay
                   clients);
           ] );
       ]

let () = Tezt.Test.run ()
