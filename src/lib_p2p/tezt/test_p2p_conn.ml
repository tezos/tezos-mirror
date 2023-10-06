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
    Invocation:   dune exec src/lib_p2p/tezt/main.exe -- --file test_p2p_conn.ml
    Subject:      Unit tests for the [P2p_conn] module.
*)

let mock_conn ?(reader = Lwt_pipe.Maybe_bounded.create ())
    ?(writer = Lwt_pipe.Maybe_bounded.create ()) () =
  let socket =
    let auth_connection =
      P2p_socket.Internal_for_tests.mock_authenticated_connection ()
    in
    P2p_socket.Internal_for_tests.mock ~reader ~writer auth_connection
  in
  let peer_info =
    P2p_peer_state.Info.create ~created:(Time.System.now ()) ~peer_metadata:()
    @@ P2p_peer.Id.of_string_exn
         "4239482738492035" (* Just a dummy 16 bytes long hexa code *)
  in
  let messages = Lwt_pipe.Maybe_bounded.create () in
  let canceler = Lwt_canceler.create () in
  let callback = P2p_protocol.create_private () in
  let version = Network_version.Internal_for_tests.mock () in
  P2p_conn.create
    ~conn:socket
    ~point_info:None
    ~peer_info
    ~messages
    ~canceler
    ~greylister:(fun () -> ())
    ~callback
    ~disable_peer_discovery:true
    version

(** This test checks that when disabling the peer discovery feature,
   [P2p_node.t] connctions can ignore [Bootstrap] and [Advertize]
   messages, being still able to continue consuming user messages. *)
let check_message_consumption_without_peer_discovery () =
  Test.register
    ~__FILE__
    ~title:"p2p msg consumption without peer_discovery"
    ~tags:["p2p"; "conn"; "consumption"; "peer_discovery"]
  @@ fun () ->
  let open Lwt_syntax in
  let reader = Lwt_pipe.Maybe_bounded.create () in

  (* Instantiate the connexion and starts the worker. *)
  let conn = mock_conn ~reader () in

  (* Push a Bootstrap, an Advertise, then a message in the socket. *)
  let messages =
    [P2p_message.Bootstrap; P2p_message.Advertise []; P2p_message.Message ()]
  in
  let* () =
    List.iter_s
      (fun msg -> Lwt_pipe.Maybe_bounded.push reader @@ Ok (0, msg))
      messages
  in

  (* Reads the message in the queue.
     Reading should not hang as one message should be
     retrieved. *)
  let timeout =
    Lwt.catch
      (fun () -> Lwt_unix.timeout 1.0)
      (fun _ -> Lwt.return @@ Error [Timeout])
  in
  let read = P2p_conn.read conn in
  let* res = Lwt.pick [timeout; read] in

  (* Releasing resources to end the test properly. *)
  let* () = P2p_conn.close ~reason:(User "end of the tests") conn in

  match res with
  | Ok () -> unit
  | Error [Timeout] ->
      Test.fail
        "Timeout reached. The message has not been processed. The connection \
         is unexpectedly hanging."
  | Error e ->
      Test.fail
        "Couldn't read message from the connection: %a"
        Error_monad.pp_print_trace
        e

let () = check_message_consumption_without_peer_discovery ()
