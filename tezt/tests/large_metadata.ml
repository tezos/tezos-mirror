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
   Invocation: dune exec tezt/tests/main.exe -- --file large_metadata.ml
   Subject: Test that large operations metadata are not stored.
*)

(* Some contract calls require a hard-coded gas limit because the simulation is
   bypassed (because it fails). We pass the maximum gas limit for operations. *)

let gas_limit = 1_040_000

(* Stands for the different values of the metadata query
   string. Not_provided represents the fact that the argument is not
   provided. *)
type metadata_query_param = Always | Never | Not_provided

let convert_metadata_query_param = function
  | Always -> Some "always"
  | Never -> Some "never"
  | Not_provided -> None

let get_metadata ?(metadata_query_string = Not_provided) client =
  let rpc_args = ["chains"; "main"; "blocks"; "head"; "operations"; "3"; "0"] in
  match convert_metadata_query_param metadata_query_string with
  | Some query ->
      Client.rpc ~query_string:[("metadata", query)] Client.GET rpc_args client
  | None -> Client.rpc Client.GET rpc_args client

let metadata_is_available ?(metadata_query_string = Not_provided) client
    exponent =
  let* first_manager_operation = get_metadata ~metadata_query_string client in
  let first_operation_result =
    JSON.(
      first_manager_operation |-> "contents" |=> 0 |-> "metadata"
      |-> "operation_result")
  in
  assert (JSON.(first_operation_result |-> "status" |> as_string = "failed")) ;
  let length_of_first_error_message =
    JSON.(
      first_operation_result |-> "errors" |=> 1 |-> "with" |-> "string"
      |> as_string |> String.length)
  in
  assert (length_of_first_error_message = 1 lsl exponent) ;
  unit

let metadata_is_pruned client =
  let* first_manager_operation =
    get_metadata ~metadata_query_string:Never client
  in
  let metadata =
    JSON.(first_manager_operation |-> "contents" |=> 0 |-> "metadata" |> as_opt)
  in
  match metadata with
  | Some _ -> Test.fail "metadata should not be returned"
  | None -> unit

let metadata_is_too_large client =
  let* first_manager_operation =
    get_metadata ~metadata_query_string:Not_provided client
  in
  let metadata =
    JSON.(first_manager_operation |-> "metadata" |> as_string_opt)
  in
  match metadata with
  | Some s ->
      assert (s = "too large") ;
      unit
  | None -> Test.fail "metadata should not be stored"

(* Uses the deprecated version of the query paramater to force the
   recomputation of the operations metadata.*)
let metadata_is_available_deprecated ?(force_metadata = false) client exponent =
  let* first_manager_operation =
    Client.RPC.call client
    @@ RPC.get_chain_block_operations_validation_pass
         ~force_metadata
         ~validation_pass:3
         ~operation_offset:0
         ()
  in
  let first_operation_result =
    JSON.(
      first_manager_operation |-> "contents" |=> 0 |-> "metadata"
      |-> "operation_result")
  in
  assert (JSON.(first_operation_result |-> "status" |> as_string = "failed")) ;
  let length_of_first_error_message =
    JSON.(
      first_operation_result |-> "errors" |=> 1 |-> "with" |-> "string"
      |> as_string |> String.length)
  in
  assert (length_of_first_error_message = 1 lsl exponent) ;
  unit

let get_attestation client =
  let* _ =
    Client.RPC.call client
    @@ RPC.get_chain_block_operations_validation_pass
         ~validation_pass:0
         ~operation_offset:0
         ()
  in
  unit

let setup_node ~limit protocol =
  let limit = match limit with None -> [] | Some a -> [a] in
  let* node =
    Node.init ([Node.Synchronisation_threshold 0; Connections 0] @ limit)
  in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol_and_wait ~protocol client in
  (* Originate the contract allowing the large metadata generation
     when failing. This contract always fails with a byte sequence
     full of zeros. If the parameter n is less than 2 then the length
     of the error message is 2 bytes, otherwise it is 2^n bytes. See
     [michelson_test_scripts/mini_scenarios/large_error.tz]. *)
  let* _alias, contract_id =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 1)
      client
      ["mini_scenarios"; "large_error"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  return (contract_id, client, node)

let check_default_limit_metadata =
  Protocol.register_test
    ~__FILE__
    ~title:"Large metadata with default limit"
    ~tags:["large_metadata"; "default"]
  @@ fun protocol ->
  let* contract_id, client, _node = setup_node ~limit:None protocol in
  let small_exponent = 23 in
  (* Call the contract with a small exponent to make sure that the
     metadata is allowed. As the metadata cap is set to 10_000_000 bytes
     and as 2^23=8_388_608 does not exceed this limit, the metadata should
     be stored. *)
  let* () =
    Client.transfer
      ~gas_limit
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
  let* () = Client.bake_for_and_wait client in
  (* All protocols should store the metadata. *)
  let* () = metadata_is_available client small_exponent in
  (* We now call the contract with a bigger exponent to exceed the
     10_000_000 limit: 2^24=16_777_216. The contract should be
     sucessfully injected. *)
  let big_exponent = 24 in
  let* () =
    Client.transfer
      ~gas_limit
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
  let* () = Client.bake_for_and_wait client in
  (* All protocols should not store the metadata. *)
  let* () = metadata_is_too_large client in
  unit

let check_limit_metadata =
  Protocol.register_test
    ~__FILE__
    ~title:"Large metadata with a small limit"
    ~tags:["large_metadata"; "limit"]
  @@ fun protocol ->
  let* contract_id, client, _node =
    setup_node ~limit:(Some (Node.Metadata_size_limit (Some 10_000))) protocol
  in
  let small_exponent = 13 in
  (* Call the contract with a small exponent to make sure that the
     metadata is allowed. As the metadata cap is set to 10_000 bytes
     and as 2^13=8_192 does not exceed this limit, the metadata should
     be stored. *)
  let* () =
    Client.transfer
      ~gas_limit
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
  let* () = Client.bake_for_and_wait client in
  (* The metadata should be present. *)
  let* () = metadata_is_available client small_exponent in
  (* We now call the contract with a bigger exponent to exceed the
     10_000 limit: 2^14=16_384. *)
  let big_exponent = 14 in
  let* () =
    Client.transfer
      ~gas_limit
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
  let* () = Client.bake_for_and_wait client in
  (* The metadata shouldn't be present. *)
  let* () = metadata_is_too_large client in
  unit

let check_unlimited_metadata =
  Protocol.register_test
    ~__FILE__
    ~title:"Large metadata without limit"
    ~tags:["large_metadata"; "unlimited"]
  @@ fun protocol ->
  let* contract_id, client, _node =
    setup_node ~limit:(Some (Node.Metadata_size_limit None)) protocol
  in
  (* We call the contract with a bigger exponent to exceed the
     10_000_000 default limit (2^24=16_777_216) *)
  let big_exponent = 24 in
  let* () =
    Client.transfer
      ~gas_limit
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
  let* () = Client.bake_for_and_wait client in
  (* The metadata should be present. *)
  let* () = metadata_is_available client big_exponent in
  unit

let check_metadata_query_string =
  Protocol.register_test
    ~__FILE__
    ~title:"Check recomputation and pruning of metadata"
    ~tags:["large_metadata"; "query"]
  @@ fun protocol ->
  let* contract_id, client, _node =
    setup_node ~limit:(Some (Node.Metadata_size_limit (Some 10_000))) protocol
  in
  let small_exponent = 13 in
  (* Call the contract with a small exponent to make sure that the
     metadata is allowed. As the metadata cap is set to 10_000 bytes
     and as 2^13=8_192 does not exceed this limit, the metadata should
     be stored. *)
  let* () =
    Client.transfer
      ~gas_limit
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
  let* () = Client.bake_for_and_wait client in
  (* The metadata should be present. *)
  let* () = metadata_is_available client small_exponent in
  (* The metadata should not be returned even if stored. *)
  let* () = metadata_is_pruned client in
  (* We now call the contract with a bigger exponent to exceed the
     10_000 limit: 2^14=16_384. *)
  let big_exponent = 14 in
  let* () =
    Client.transfer
      ~gas_limit
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
  let* () = Client.bake_for_and_wait client in
  (* The metadata shouldn't be present. *)
  let* () = metadata_is_too_large client in
  (* The metadata should be available as we force the node to
     recompute it. *)
  let* () = metadata_is_available_deprecated ~force_metadata:true client 14 in
  let* () = metadata_is_available ~metadata_query_string:Always client 14 in
  (* The metadata should not be returned even if stored. *)
  let* () = metadata_is_pruned client in
  (* Check that attestations can still be queried. *)
  let* () = get_attestation client in
  unit

let register ~protocols =
  check_default_limit_metadata protocols ;
  check_limit_metadata protocols ;
  check_unlimited_metadata protocols ;
  check_metadata_query_string protocols
