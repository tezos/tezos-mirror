(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2023 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
    Invocation:   dune exec src/lib_p2p/tezt/main.exe
    Subject:      Sockets and client-server communications.
*)

open P2p_test_utils

include Internal_event.Legacy_logging.Make (struct
  let name = "test.p2p.connection"
end)

let p2p_peer_id_typ = Check.comparable P2p_peer.Id.pp P2p_peer.Id.compare

(** [connect ?target_id ?proof_of_work_target sched addr port] connect
    and performs [P2p_socket.authenticate] with the given
    [proof_of_work_target] (also checking that the remote point is the
    expected [target_id]). *)
let connect ?proof_of_work_target ?(target_id = id1) sched addr port id =
  let open Lwt_result_syntax in
  let* info, auth_fd =
    P2p_test_utils.connect ?proof_of_work_target sched addr port id
  in
  let*! id1 = target_id in
  Check.is_false info.incoming ~error_msg:"Message should be outgoing" ;
  Check.(info.peer_id = id1.peer_id)
    p2p_peer_id_typ
    ~error_msg:"expected value = %R, got %L" ;
  return auth_fd

(** Spawns a client and a server. The client connects to the server
    using identity [id2], this identity is checked on server-side. The
    server sends a Nack message with no rejection motive. The client
    asserts that its connection has been rejected by Nack.
*)
let nack_test () =
  Test.register
    ~__FILE__
    ~title:"p2p socket nack"
    ~tags:["p2p"; "socket"; "nack"]
  @@ fun () ->
  let open Lwt_result_syntax in
  let encoding = Data_encoding.bytes in

  let is_rejected = function
    | Error (Tezos_p2p_services.P2p_errors.Rejected_by_nack _ :: _) -> true
    | Ok _ -> false
    | Error err ->
        log_notice "Error: %a" pp_print_trace err ;
        false
  in

  let server ch sched socket =
    let* info, auth_fd = accept sched socket in
    Check.is_true info.incoming ~error_msg:"Message should be incomming." ;
    let*! id2 in
    Check.(info.peer_id = id2.peer_id)
      p2p_peer_id_typ
      ~error_msg:"Expected value = %R, got %L" ;
    let*! () = P2p_socket.nack auth_fd P2p_rejection.No_motive [] in
    sync ch
  in

  let client ch sched addr port =
    let*! id2 in
    let* auth_fd = connect sched addr port id2 in
    let*! conn = P2p_socket.accept ~canceler auth_fd encoding in
    Check.is_true (is_rejected conn) ~error_msg:"Connection should be rejected" ;
    sync ch
  in

  let*! r = run_nodes ~addr:Node.default_ipv6_addr client server in
  match r with
  | Ok () -> unit
  | Error e ->
      Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

module Self_identification = struct
  (* Test where node A tries to connect to node B using the peer id of B
     instead of its own peer id. *)
  let self_id () =
    Test.register
      ~__FILE__
      ~title:"p2p socket self peer id"
      ~tags:["p2p"; "socket"; "self_identification"]
    @@ fun () ->
    let server _ch sched socket =
      let open Lwt_result_syntax in
      let*! r = accept ~id:id1 sched socket in
      match r with
      (* There are 2 acceptable ways to fail. *)
      (* The proof of work may look correct enough to check its power. In this
         case the error could be Not_enough_proof_of_work before a
         Decipher_error occurs. *)
      | Error (P2p_errors.Decipher_error :: _)
      | Error (P2p_errors.Not_enough_proof_of_work _ :: _) ->
          return_unit
      | Error (P2p_errors.Myself _ :: _) ->
          Test.fail ~__LOC__ "Unexpected detection of self connection"
      | Ok _ -> Test.fail ~__LOC__ "Unexpected success of authentication"
      | Error err ->
          Test.fail
            ~__LOC__
            "Unexpected error %a"
            Error_monad.pp_print_top_error_of_trace
            err
    in

    let client _ch sched addr port =
      let open Lwt_result_syntax in
      let*! id_server = id1 in
      let*! id_client = id2 in
      (* The server's peer id is used instead of the client one. *)
      let id_self_peer_id =
        {id_client with public_key = id_server.public_key}
      in
      let*! _ = P2p_test_utils.connect sched addr port id_self_peer_id in
      return_unit
    in

    let* r = run_nodes ~addr:Node.default_ipv6_addr client server in
    match r with
    | Ok () -> unit
    | Error e ->
        Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

  (* Test where node A tries to connect to node B reusing Connection_message of
     B. *)
  let self_connection_message () =
    Test.register
      ~__FILE__
      ~title:"p2p socket self connection message"
      ~tags:["p2p"; "socket"; "self_identification"]
    @@ fun () ->
    let server _ch sched socket =
      let open Lwt_result_syntax in
      (* The first connection permits the client to get the connection message
         of the server. *)
      let*! _r = accept sched socket in
      (* During second connection handshake, the client will send back the
         server connection message. *)
      let*! r = accept sched socket in
      match r with
      (* The handshake should fail because the Metadata message is not ciphered
         correctly. *)
      | Error (P2p_errors.Decipher_error :: _) -> return_unit
      | Error (P2p_errors.Myself _ :: _) ->
          Test.fail ~__LOC__ "Unexpected detection of self connection"
      | Ok _ -> Test.fail ~__LOC__ "Unexpected success of authentication"
      | Error err ->
          Test.fail
            ~__LOC__
            "Unexpected error %a"
            Error_monad.pp_print_top_error_of_trace
            err
    in

    let client _ch sched addr port =
      let open Lwt_result_syntax in
      let*! id_client = id2 in
      let pp_connect_error ppf e =
        match e with
        | `Connection_refused ->
            Format.pp_print_string ppf "Connection Connection_refused"
        | `Unexpected_error e ->
            Format.pp_print_string ppf (Printexc.to_string e)
      in
      let*! conn = P2p_test_utils.raw_connect sched addr port in
      match conn with
      | Error e -> Test.fail "First connection failed: %a" pp_connect_error e
      | Ok conn -> (
          let canceler = Lwt_canceler.create () in
          (* During the first connection, the client get the connection message
             from the server. *)
          let* server_connection_msg, _ =
            P2p_socket.Internal_for_tests.Connection_message.read
              ~canceler
              (P2p_io_scheduler.to_readable conn)
          in
          let* () = P2p_io_scheduler.close conn in
          (* During the second connection, the client will send the server's
             connection message instead of its own. *)
          let*! conn = P2p_test_utils.raw_connect sched addr port in
          match conn with
          | Error e ->
              Test.fail "Second connection failed: %a" pp_connect_error e
          | Ok conn ->
              let canceler = Lwt_canceler.create () in
              let* sent_msg =
                P2p_socket.Internal_for_tests.Connection_message.write
                  ~canceler
                  conn
                  server_connection_msg
              in
              let* _, recv_msg =
                P2p_socket.Internal_for_tests.Connection_message.read
                  ~canceler
                  (P2p_io_scheduler.to_readable conn)
              in
              (* The client does not have the private key of the server. He
                 tries with its own private key. *)
              let cryptobox_data =
                P2p_socket.Internal_for_tests.Crypto.create_data
                  ~incoming:false
                  ~recv_msg
                  ~sent_msg
                  ~sk:id_client.secret_key
                  ~pk:
                    (P2p_socket.Internal_for_tests.Connection_message
                     .get_public_key
                       server_connection_msg)
              in
              let metadata =
                P2p_test_utils.conn_meta_config.conn_meta_value ()
              in
              (* By sending a message without having the private key
                 corresponding to the connection message the client sent, it
                 will no longer be able to progress in the handshake. *)
              P2p_socket.Internal_for_tests.Metadata.write
                ~canceler
                P2p_test_utils.conn_meta_config
                conn
                cryptobox_data
                metadata)
    in

    let* r = run_nodes ~addr:Node.default_ipv6_addr client server in
    match r with
    | Ok () -> unit
    | Error e ->
        Test.fail "Errors in fork processes: %a." Error_monad.pp_print_trace e

  let tests () =
    self_id () ;
    self_connection_message ()
end

let () =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4882
     Use ipv4 localhost by default.
     Setting [addr] to ipv4 localhost is mandatory to
     perform these tests on the CI. As your tests may be
     successful using ipv6 on your machine, this will
     not work on the CI as ipv6 network interface is
     disabled on CI's executors. *)
  nack_test () ;
  Self_identification.tests ()
