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
   Component:    Baker
   Invocation:   dune exec tezt/tests/main.exe -- --file baker_test.ml
   Subject:      Run the baker while performing a lot of transfers
*)

let baker_test ~title ~tags =
  Protocol.register_test ~__FILE__ ~title ~tags @@ fun protocol ->
  let* node, client =
    Client.init_with_protocol `Client ~protocol ~timestamp:Now ()
  in
  let level_2_promise = Node.wait_for_level node 2 in
  let level_3_promise = Node.wait_for_level node 3 in
  let* baker = Baker.init ~protocol node client in
  Log.info "Wait for new head." ;
  Baker.log_events baker ;
  let* _ = level_2_promise in
  Log.info "New head arrive level 2" ;
  let* _ = level_3_promise in
  Log.info "New head arrive level 3" ;
  Lwt.return_unit

let baker_stresstest =
  Protocol.register_test
    ~__FILE__
    ~title:"baker stresstest"
    ~tags:["node"; "baker"; "stresstest"]
  @@ fun protocol ->
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in
  let* _ = Baker.init ~protocol node client in
  let* _ = Node.wait_for_level node 3 in
  (* Use a large tps, to have failing operations too *)
  let* () = Client.stresstest ~tps:25 ~transfers:100 client in
  Lwt.return_unit

let register ~protocols =
  let () = baker_test ~title:"baker test" ~tags:["node"; "baker"] protocols in
  baker_stresstest protocols
