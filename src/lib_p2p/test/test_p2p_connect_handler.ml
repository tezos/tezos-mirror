(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Invocation:   dune build @src/lib_p2p/test/runtest_p2p_connect_handler
    Subject:      Test that P2p_connect_handler is well-behaved
*)

let tztest = Tezos_base_test_helpers.Tztest.tztest

let dependencies :
    (unit, unit, unit) P2p_connect_handler.Internal_for_tests.dependencies =
  P2p_connect_handler.Internal_for_tests.mock_dependencies ()

let point_id : P2p_point.Id.t = (P2p_addr.of_string_exn "0.0.0.0", 0)

let test_connect_destroy =
  tztest "Connect then destroy" `Quick @@ fun () ->
  let t = P2p_connect_handler.Internal_for_tests.create ~dependencies () () in
  P2p_connect_handler.connect t point_id >>=? fun _conn ->
  P2p_connect_handler.destroy t >|= ok

(** Check that we don't remove the accepted connection too early (before the authentication succeeds or fails),
   otherwise there is a risk of too many open file descriptors.

   The trick is to have an inner function (like [socket_accept]) hang to avoid a race condition in the test
   between [accept] and the assertion. *)
let test_correct_incoming_connection_number =
  tztest "Don't remove incoming connections too early" `Quick @@ fun () ->
  let config =
    {
      P2p_connect_handler.Internal_for_tests.dumb_config with
      authentication_timeout =
        Ptime.Span.of_int_s 5 (* Don't wait too long when the test fails *);
    }
  in
  let socket_accept_hanging_forever ?incoming_message_queue_size:_
      ?outgoing_message_queue_size:_ ?binary_chunks_size:_ ~canceler:_ _ _ =
    let rec loop () = Lwt_unix.sleep 1. >>= fun () -> loop () in
    loop ()
  in
  let dependencies =
    {dependencies with socket_accept = socket_accept_hanging_forever}
  in
  let incoming = P2p_point.Table.create 0 in
  let t =
    P2p_connect_handler.Internal_for_tests.create
      ~config
      ~dependencies
      ~incoming
      ()
      ()
  in
  P2p_fd.socket PF_INET6 SOCK_STREAM 0 >>= fun fd ->
  Alcotest.(
    check'
      int
      ~msg:"Before accepting, no incoming connection"
      ~expected:0
      ~actual:(P2p_point.Table.length incoming)) ;
  P2p_connect_handler.accept t fd point_id ;
  Alcotest.(
    check'
      int
      ~msg:"After first accept, 1 incoming connection"
      ~expected:1
      ~actual:(P2p_point.Table.length incoming)) ;
  return_unit

let tests = [test_connect_destroy; test_correct_incoming_connection_number]

let () =
  Alcotest_lwt.run "P2p_connect_handler" [("P2p_connect_handler", tests)]
  |> Lwt_main.run
