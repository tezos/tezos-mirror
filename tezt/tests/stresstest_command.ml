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

(* Testing
   -------
   Component:    Client
   Invocation:   dune exec tezt/tests/main.exe -- --file stresstest_command.ml
   Subject:      Test the [stresstest] client command
*)

(** Tests the [stresstest] client command with explicit values for
    optional arguments [transfers] and [tps]. *)
let test_stresstest_explicit =
  Protocol.register_test ~__FILE__ ~title:"stresstest explicit" ~tags:["client"]
  @@ fun protocol ->
  let* node = Node.init [Synchronisation_threshold 0; Connections 0] in
  let* client = Client.init ~endpoint:Client.(Node node) () in
  let* () = Client.activate_protocol ~protocol client in
  let* _ = Node.wait_for_level node 1 in
  Client.stresstest ~transfers:30 ~tps:25 client

(** Waits for [n] injection request events. *)
let wait_for_n_injections n node =
  let seen = ref 0 in
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "inject" ->
        incr seen ;
        if !seen >= n then Some () else None
    | Some _ | None -> None
  in
  Node.wait_for node "request_completed_notice.v0" filter

(** Tests the [stresstest] client command without providing optional
    arguments [transfers] and [tps]. This means the command won't
    stop (it only stops after [transfers] operations when the argument
    is given), so instead we use {!wait_for_n_injections} to finish
    the test. *)
let test_stresstest_implicit =
  Protocol.register_test ~__FILE__ ~title:"stresstest implicit" ~tags:["client"]
  @@ fun protocol ->
  let* node = Node.init [Synchronisation_threshold 0; Connections 0] in
  let* client = Client.init ~endpoint:Client.(Node node) () in
  let* () = Client.activate_protocol ~protocol client in
  let* _ = Node.wait_for_level node 1 in
  let waiter = wait_for_n_injections 50 node in
  let _ = Client.stresstest client in
  waiter

let register ~protocols =
  test_stresstest_explicit ~protocols ;
  test_stresstest_implicit ~protocols
