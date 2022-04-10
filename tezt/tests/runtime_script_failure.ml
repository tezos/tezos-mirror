(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Invocation: dune exec tezt/tests/main.exe -- runtime_script_failure
   Subject: Test that the --force option of the transfer command can be used
            to include an invalid operation in a block
*)

let check_client_force =
  Protocol.register_test
    ~__FILE__
    ~title:"Runtime Script Failure: client force"
    ~tags:["runtime_script_failure"]
  @@ fun protocol ->
  let* node = Node.init [Synchronisation_threshold 0; Connections 0] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol ~protocol client in
  let* _ = Node.wait_for_level node 1 in
  let* contract_id =
    Client.originate_contract
      ~alias:"always_fails"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/always_fails.tz"
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 1)
      client
  in
  let* () = Client.bake_for client in
  let* () =
    Client.transfer
      ~gas_limit:100_000
      ~fee:Tez.one
      ~amount:Tez.zero
      ~burn_cap:Tez.zero
      ~storage_limit:10000
      ~giver:"bootstrap1"
      ~receiver:contract_id
      ~arg:"\"saucisse\""
      ~force:true
      client
  in
  let* () = Client.bake_for client in
  let* first_manager_operation =
    Client.rpc
      Client.GET
      ["chains"; "main"; "blocks"; "head"; "operations"; "3"; "0"]
      client
  in
  let first_operation_result =
    JSON.(
      first_manager_operation |-> "contents" |=> 0 |-> "metadata"
      |-> "operation_result")
  in
  assert (JSON.(first_operation_result |-> "status" |> as_string = "failed")) ;
  let first_failed_script =
    JSON.(first_operation_result |-> "errors" |=> 0 |-> "contract_code")
  in
  (match protocol with
  | Alpha | Ithaca | Jakarta ->
      (* In Alpha and Ithaca this field is deprecated *)
      assert (JSON.(first_failed_script |> as_string = "Deprecated"))
  | Hangzhou ->
      (* In Hangzhou this field contains the failed script, it
         is a sequence of length 3 (parameter, storage, and code). *)
      assert (JSON.(first_failed_script |> as_list |> List.length = 3))) ;
  return ()

let register ~protocols = check_client_force protocols
