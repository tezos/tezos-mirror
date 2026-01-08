(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** This module provides functions used for tests. *)

type error += Timeout_exceed of string

let () =
  register_error_kind
    `Permanent
    ~id:"test_p2p.utils.timeout_exceed"
    ~title:"Timeout exceed"
    ~description:"The timeout used to wait connections has been exceed."
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "The timeout has been exceed : %s." msg)
    Data_encoding.(obj1 (req "msg" Data_encoding.string))
    (function Timeout_exceed msg -> Some msg | _ -> None)
    (fun msg -> Timeout_exceed msg)

let connect_all ?timeout connect_handler points =
  List.map_ep
    (fun point -> P2p_connect_handler.connect ?timeout connect_handler point)
    points

type 'a timeout_t = {time : float; msg : 'a -> string}

(* [wait_pred] wait until [pred] [arg] is true or [timeout] is exceed. If
     [timeout] is exceed an error is raised. There is a Lwt cooperation
     point. *)
let wait_pred ?timeout ~pred ~arg () =
  let open Lwt_result_syntax in
  let rec inner_wait_pred () =
    let*! () = Lwt.pause () in
    if not (pred arg) then inner_wait_pred () else return_unit
  in
  match timeout with
  | None -> inner_wait_pred ()
  | Some timeout ->
      Lwt.pick
        [
          (let*! () = Lwt_unix.sleep timeout.time in
           tzfail (Timeout_exceed (timeout.msg arg)));
          inner_wait_pred ();
        ]

let wait_pred_s ?timeout ~pred ~arg () =
  let open Lwt_result_syntax in
  let rec inner_wait_pred () =
    let*! () = Lwt.pause () in
    let*! cond = pred arg in
    if not cond then inner_wait_pred () else return_unit
  in
  match timeout with
  | None -> inner_wait_pred ()
  | Some timeout ->
      Lwt.pick
        [
          (let*! () = Lwt_unix.sleep timeout.time in
           tzfail (Timeout_exceed (timeout.msg arg)));
          inner_wait_pred ();
        ]

let wait_conns ?timeout ~pool n =
  let timeout =
    Option.map
      (fun timeout ->
        {
          time = timeout;
          msg =
            (fun (pool, n) ->
              Format.asprintf
                "in wait_conns with n = %d and conns = %d"
                n
                (P2p_pool.active_connections pool));
        })
      timeout
  in
  wait_pred
    ?timeout
    ~pred:(fun (pool, n) -> P2p_pool.active_connections pool = n)
    ~arg:(pool, n)
    ()

let close_active_conns pool =
  P2p_pool.Connection.fold
    ~init:Lwt.return_unit
    ~f:(fun _ conn _ ->
      P2p_conn.disconnect ~reason:(User "closed by test") ~wait:true conn)
    pool

let canceler = Lwt_canceler.create () (* unused *)

let proof_of_work_target = Tezos_crypto.Crypto_box.make_pow_target 1.

let id1 = P2p_identity.generate proof_of_work_target

let id2 = P2p_identity.generate proof_of_work_target

let version =
  {
    Network_version.chain_name =
      Distributed_db_version.Name.of_string "SANDBOXED_TEZOS";
    distributed_db_version = Distributed_db_version.one;
    p2p_version = P2p_version.zero;
  }

let conn_meta_config : unit P2p_params.conn_meta_config =
  {
    conn_meta_encoding = Data_encoding.empty;
    conn_meta_value = (fun () -> ());
    private_node = (fun _ -> false);
  }

let glob_port = ref None

let listen ?port addr =
  let open Lwt_result_syntax in
  (* If [port] is [0], a fresh port is used. *)
  let port_or_gen = Option.value port ~default:0 in
  let* sock = P2p_fd.create_listening_socket ~backlog:1 ~addr port_or_gen in
  let port =
    match port with
    | Some port -> port
    | None -> (
        (* if [port] was [0], we get the port generated. *)
        let addr = Lwt_unix.getsockname sock in
        match addr with ADDR_INET (_, port) -> port | _ -> assert false)
  in
  return (sock, port)

let rec sync_nodes nodes =
  let open Lwt_result_syntax in
  let* () = List.iter_ep (fun p -> Process.receive p) nodes in
  let* () = List.iter_ep (fun p -> Process.send p ()) nodes in
  sync_nodes nodes

let sync_nodes nodes =
  let open Lwt_result_syntax in
  let*! r = sync_nodes nodes in
  match r with
  | Ok () | Error (Exn End_of_file :: _) -> return_unit
  | Error _ as err -> Lwt.return err

(* addr here is localhost as we run different nodes
   on the same addr but different ports *)
let run_nodes ~addr ?port client server =
  let open Lwt_result_syntax in
  let p =
    match port with
    | None ->
        glob_port := None ;
        None
    | Some p ->
        glob_port := Some (p + 1) ;
        Some p
  in
  let* main_socket, p = listen ?port:p addr in
  let* server_node =
    Process.detach ~prefix:"server: " (fun channel ->
        let* sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
        Lwt.finalize
          (fun () ->
            let* () = server channel sched main_socket in
            let*! () = P2p_io_scheduler.shutdown sched in
            return_unit)
          (fun () ->
            let*! r = Lwt_utils_unix.safe_close main_socket in
            match r with
            | Error trace ->
                Format.eprintf "Uncaught error: %a\n%!" pp_print_trace trace ;
                Lwt.return_unit
            | Ok () -> Lwt.return_unit))
  in
  let* client_node =
    Process.detach ~prefix:"client: " (fun channel ->
        let*! () =
          let*! r = Lwt_utils_unix.safe_close main_socket in
          match r with
          | Error trace ->
              Format.eprintf "Uncaught error: %a\n%!" pp_print_trace trace ;
              Lwt.return_unit
          | Ok () -> Lwt.return_unit
        in
        let* sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
        let* () = client channel sched addr p in
        let*! () = P2p_io_scheduler.shutdown sched in
        return_unit)
  in
  let nodes = [server_node; client_node] in
  Lwt.ignore_result (sync_nodes nodes) ;
  let* () = Process.wait_all nodes in
  let*! _ = Lwt_utils_unix.safe_close main_socket in
  return_unit

(* for fd tests *)
let run_nodes_fd ~addr ?port client server =
  let open Lwt_result_syntax in
  let p =
    match port with
    | None ->
        glob_port := None ;
        None
    | Some p ->
        glob_port := Some (p + 1) ;
        Some p
  in
  let* main_socket, p = listen ?port:p addr in
  let* server_node =
    Process.detach ~prefix:"server: " (fun channel ->
        Lwt.finalize
          (fun () ->
            let* () = server channel main_socket in
            return_unit)
          (fun () ->
            let*! r = Lwt_utils_unix.safe_close main_socket in
            match r with
            | Error trace ->
                Format.eprintf "Uncaught error: %a\n%!" pp_print_trace trace ;
                Lwt.return_unit
            | Ok () -> Lwt.return_unit))
  in
  let* client_node =
    Process.detach ~prefix:"client: " (fun channel ->
        let*! () =
          let*! r = Lwt_utils_unix.safe_close main_socket in
          match r with
          | Error trace ->
              Format.eprintf "Uncaught error: %a\n%!" pp_print_trace trace ;
              Lwt.return_unit
          | Ok () -> Lwt.return_unit
        in
        let* () = client channel addr p in
        return_unit)
  in
  let nodes = [server_node; client_node] in
  Lwt.ignore_result (sync_nodes nodes) ;
  let* () = Process.wait_all nodes in
  let*! _ = Lwt_utils_unix.safe_close main_socket in
  return_unit

let raw_accept sched main_socket =
  let open Lwt_syntax in
  let* r = P2p_fd.accept main_socket in
  match r with
  | Error e -> Lwt_result_syntax.tzfail (P2p_fd.accept_error_to_tzerror e)
  | Ok (fd, sockaddr) ->
      let fd = P2p_io_scheduler.register sched fd in
      let point =
        match sockaddr with
        | Lwt_unix.ADDR_UNIX _ -> assert false
        | Lwt_unix.ADDR_INET (addr, port) ->
            (Ipaddr_unix.V6.of_inet_addr_exn addr, port)
      in
      Lwt.return_ok (fd, point)

(** [accept ?id ?proof_of_work_target sched main_socket] connect
   and performs [P2p_socket.authenticate] with the given
   [proof_of_work_target].  *)
let accept ?(id = id1) ?(proof_of_work_target = proof_of_work_target) sched
    main_socket =
  let open Lwt_syntax in
  let* r = raw_accept sched main_socket in
  let* id1 = id in
  match r with
  | Error e -> Lwt_result.fail e
  | Ok (fd, point) ->
      P2p_socket.authenticate
        ~canceler
        ~proof_of_work_target
        ~incoming:true
        fd
        point
        id1
        version
        conn_meta_config

let raw_connect sched addr port =
  let open Lwt_result_syntax in
  let* fd = Lwt_result.map_error Either.right (P2p_fd.socket ()) in
  let uaddr = Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
  let* () = Lwt_result.map_error Either.left (P2p_fd.connect fd uaddr) in
  let fd = P2p_io_scheduler.register sched fd in
  return fd

exception Cant_connect

(** [connect ?proof_of_work_target sched addr port] connect
   and performs [P2p_socket.authenticate] with the given
   [proof_of_work_target]. *)
let connect ?(proof_of_work_target = proof_of_work_target) sched addr port id =
  let open Lwt_result_syntax in
  let*! r = raw_connect sched addr port in
  match r with
  | Error
      (Left
         ( `Network_unreachable | `Connection_unreachable | `Connection_refused
         | `Connection_canceled | `Unexpected_error _ )) ->
      Lwt.fail Cant_connect
  | Error (Right e) -> Lwt_result.fail e
  | Ok fd ->
      P2p_socket.authenticate
        ~canceler
        ~proof_of_work_target
        ~incoming:false
        fd
        (addr, port)
        id
        version
        conn_meta_config

let sync ch =
  let open Lwt_result_syntax in
  let* () = Process.Channel.push ch () in
  let* () = Process.Channel.pop ch in
  return_unit
