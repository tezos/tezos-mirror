(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Native contract
   Invocation:   dune exec tezt/tests/main.exe -- --file native_contracts.ml
   Subject:      Test native contracts integration
*)

let team = Tag.layer1

let register_test = Protocol.register_test ~supports:(Protocol.From_protocol 25)

let find_native_contract_hash client contract =
  Lwt.catch
    (fun () ->
      let* hash =
        Client.RPC.call client
        @@ RPC.get_chain_block_context_raw
             ~value_path:["contracts"; "native_contracts"; contract]
             ()
      in
      return (Some hash))
    (fun _ -> return None)

let test_feature_flag =
  register_test
    ~__FILE__
    ~title:"Test feature flag works"
    ~tags:[team; "feature_flag"]
    ~uses:(fun _ -> [Constant.octez_node])
  @@ fun protocol ->
  let* parameter_file_enabled =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      [(["native_contracts_enable"], `Bool true)]
  in
  let* _node, client =
    Client.init_with_protocol
      ~parameter_file:parameter_file_enabled
      `Client
      ~protocol
      ()
  in
  let* contract = find_native_contract_hash client "clst" in
  if contract = None then
    Test.fail "Contract is not in the storage, the feature has not been enabled" ;
  let* parameter_file_disabled =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      [(["native_contracts_enable"], `Bool false)]
  in
  let* _node, client =
    Client.init_with_protocol
      ~parameter_file:parameter_file_disabled
      `Client
      ~protocol
      ()
  in
  let* contract = find_native_contract_hash client "accumulator" in
  if contract <> None then
    Test.fail "Contract is in the storage while the feature is disabled" ;
  unit

let test_feature_disabled_on_mainnet =
  register_test
    ~__FILE__
    ~title:"Test feature flag is disabled on mainnet"
    ~tags:[team; "feature_flag"; "mainnet"]
    ~uses:(fun _ -> [Constant.octez_node])
  @@ fun protocol ->
  let* parameter_file_mainnet =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, Some Constants_mainnet))
      []
  in
  let* _node, client =
    Client.init_with_protocol
      ~parameter_file:parameter_file_mainnet
      `Client
      ~protocol
      ()
  in
  let* contract = find_native_contract_hash client "accumulator" in
  if contract <> None then
    Test.fail "Contract is in the storage while the feature is disabled" ;
  unit

let register ~protocols =
  test_feature_flag protocols ;
  test_feature_disabled_on_mainnet protocols
