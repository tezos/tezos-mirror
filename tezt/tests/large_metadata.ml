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
   Invocation: dune exec tezt/tests/main.exe -- large_metadata
   Subject: Test that large block metadata are not stored.
*)

let metadata_is_stored client exponent =
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
  let length_of_first_error_message =
    JSON.(
      first_operation_result |-> "errors" |=> 1 |-> "with" |-> "bytes"
      |> as_string
      |> fun s ->
      (* One byte is represented as 2 characters in hex. *)
      String.length s / 2)
  in
  assert (length_of_first_error_message = 1 lsl exponent) ;
  unit

let large_metadata_is_not_stored client =
  let* first_manager_operation =
    Client.rpc
      Client.GET
      ["chains"; "main"; "blocks"; "head"; "operations"; "3"; "0"]
      client
  in
  let metadata =
    JSON.(first_manager_operation |-> "metadata" |> as_string_opt)
  in
  match metadata with
  | Some s ->
      assert (s = "too large") ;
      unit
  | None -> Test.fail "metadata should not be stored"

let check_large_metadata =
  Protocol.register_test
    ~__FILE__
    ~title:"Large metadata"
    ~tags:["large_metadata"]
  @@ fun protocol ->
  let* node = Node.init [Synchronisation_threshold 0; Connections 0] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol ~protocol client in
  let* _ = Node.wait_for_level node 1 in
  (* Originate the contract allowing the large metadata generation
     when failing. This contract always fails with a byte sequence
     full of zeros. If the parameter n is less than 2 then the length
     of the error message is 2 bytes, otherwise it is 2^n bytes. See
     "file:./tezt/tests/contracts/proto_alpha/large_error.tz" *)
  let* contract_id =
    Client.originate_contract
      ~alias:"large_error"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/large_error.tz"
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 1)
      client
  in
  let* () = Client.bake_for client in
  let small_exponent = 12 in
  (* Call the contract with a small exponent to make sure that the
     metadata is allowed. As the metadata cap is set to 10_000 bytes
     and as 2^12*|hex_repr|=4_096*2=8_192 does not exceed this limit,
     the metadata should be stored. *)
  let* () =
    Client.transfer
      ~gas_limit:100_000
      ~fee:Tez.one
      ~amount:Tez.zero
      ~burn_cap:Tez.zero
      ~storage_limit:10000
      ~giver:"bootstrap1"
      ~receiver:contract_id
      ~arg:(string_of_int small_exponent)
      ~force:true
      client
  in
  let* () = Client.bake_for client in
  (* All protocols should store the metadata. *)
  let* () = metadata_is_stored client small_exponent in
  (* We now call the contract with a bigger exponent to exceed the
     10_000 limit: 2^13*|hex_repr|=8_192*2=16_384. The contract should
     be sucessfully injected. *)
  let big_exponent = 13 in
  let* () =
    Client.transfer
      ~gas_limit:100_000
      ~fee:Tez.one
      ~amount:Tez.zero
      ~burn_cap:Tez.zero
      ~storage_limit:10000
      ~giver:"bootstrap1"
      ~receiver:contract_id
      ~arg:(string_of_int big_exponent)
      ~force:true
      client
  in
  let* () = Client.bake_for client in
  (* All protocols should not store the metadata. *)
  let* () = large_metadata_is_not_stored client in
  return ()

let register ~protocols = check_large_metadata ~protocols
