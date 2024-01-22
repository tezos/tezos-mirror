(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Components: Client
   Invocation: dune exec tezt/tests/main.exe -- check_client_events
   Subject: Test that the client shows the contract events in correct order
*)

let test_emit_event protocol =
  let* node = Node.init [Synchronisation_threshold 0; Connections 0] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol_and_wait ~protocol client in
  let* _alias, contract_id =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "emit_events"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.transfer
      ~gas_limit:100_000
      ~fee:Tez.one
      ~amount:Tez.zero
      ~burn_cap:Tez.one
      ~storage_limit:10000
      ~giver:"bootstrap1"
      ~receiver:contract_id
      ~arg:"Unit"
      ~force:true
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* first_manager_operation =
    Client.rpc
      Client.GET
      ["chains"; "main"; "blocks"; "head"; "operations"; "3"; "0"]
      client
  in
  let open JSON in
  let events =
    first_manager_operation |-> "contents" |=> 0 |-> "metadata"
    |-> "internal_operation_results"
  in
  let event = events |=> 0 in
  let assert_prim ~prim ~annots json =
    assert (json |-> "prim" |> as_string = prim) ;
    assert (json |-> "annots" |> as_list |> List.map as_string = annots)
  in
  let assert_type ~annots event =
    let ty = event |-> "type" in
    assert_prim ty ~prim:"or" ~annots:[] ;
    let args = ty |-> "args" in
    assert_prim
      (args |=> 0)
      ~prim:"nat"
      ~annots:(if annots then ["%int"] else []) ;
    assert_prim
      (args |=> 1)
      ~prim:"string"
      ~annots:(if annots then ["%str"] else [])
  in
  assert_type ~annots:false event ;
  let data = event |-> "payload" in
  assert (data |-> "prim" |> as_string = "Right") ;
  assert (data |-> "args" |=> 0 |-> "string" |> as_string = "right") ;
  let tag = event |-> "tag" |> as_string in
  assert (tag = "tag1") ;
  let event = events |=> 1 in
  assert_type ~annots:true event ;
  let data = event |-> "payload" in
  assert (data |-> "prim" |> as_string = "Left") ;
  assert (data |-> "args" |=> 0 |-> "int" |> as_string = "2") ;
  let tag = event |-> "tag" |> as_string in
  assert (tag = "tag2") ;
  return ()

let check_client_events =
  Protocol.register_test
    ~__FILE__
    ~title:"Events: events from client"
    ~tags:["check_client_events"]
    test_emit_event

let register ~protocols = check_client_events protocols
