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
   Component: Client
   Invocation: dune exec tezt/long_tests/main.exe -- --file prt_client.ml
   Subject: check regressions in the duration it takes for the client to load.
*)

let load_time = "client load time"

let response_time_test = "get blocks time"

let response_time_measurement = "RPC.get_block response time"

let grafana_panels : Grafana.panel list =
  [
    Row "Test: client";
    Grafana.simple_graph
      ~measurement:load_time
      ~test:load_time
      ~field:"duration"
      ();
    Grafana.simple_graph
      ~measurement:response_time_measurement
      ~test:response_time_test
      ~field:"duration"
      ();
  ]

let client_load_time ~executors () =
  Long_test.register
    ~__FILE__
    ~title:load_time
    ~tags:["client"; "load"]
    ~timeout:(Minutes 2)
    ~executors
  @@ fun () ->
  let client = Client.create () in
  Long_test.time_lwt ~repeat:20 load_time @@ fun () -> Client.version client

let get_blocks_response_time ~executors ~protocol =
  Long_test.register
    ~__FILE__
    ~title:response_time_test
    ~tags:["rpc"]
    ~timeout:(Seconds 20)
    ~executors
  @@ fun () ->
  let* _node, client = Client.init_with_protocol `Client ~protocol () in
  Long_test.time_lwt ~repeat:20 response_time_measurement @@ fun () ->
  let* _ = Client.RPC.call client @@ RPC.get_chain_block () in
  unit

let register ~executors ~protocols =
  client_load_time ~executors () ;
  protocols
  |> List.iter @@ fun protocol -> get_blocks_response_time ~executors ~protocol
