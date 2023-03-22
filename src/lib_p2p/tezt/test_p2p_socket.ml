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
  let encoding = Data_encoding.bytes in

  let is_rejected = function
    | Error (Tezos_p2p_services.P2p_errors.Rejected_by_nack _ :: _) -> true
    | Ok _ -> false
    | Error err ->
        log_notice "Error: %a" pp_print_trace err ;
        false
  in

  let server ch sched socket =
    let open Lwt_result_syntax in
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
    let open Lwt_result_syntax in
    let*! id2 in
    let* auth_fd = connect sched addr port id2 in
    let*! conn = P2p_socket.accept ~canceler auth_fd encoding in
    Check.is_true (is_rejected conn) ~error_msg:"Connection should be rejected" ;
    sync ch
  in

  let* _ = run_nodes ~addr:Node.default_ipv6_addr client server in
  unit

let () =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4882
     Use ipv4 localhost by default.
     Setting [addr] to ipv4 localhost is mandatory to
     perform these tests on the CI. As your tests may be
     successful using ipv6 on your machine, this will
     not work on the CI as ipv6 network interface is
     disabled on CI's executors. *)
  nack_test ()
